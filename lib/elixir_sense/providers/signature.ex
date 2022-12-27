defmodule ElixirSense.Providers.Signature do
  @moduledoc """
  Provider responsible for introspection information about function signatures.
  """

  alias ElixirSense.Core.Binding
  alias ElixirSense.Core.Introspection
  alias ElixirSense.Core.Metadata
  alias ElixirSense.Core.Source
  alias ElixirSense.Core.State
  alias ElixirSense.Core.TypeInfo

  @type signature_info :: %{
          active_param: non_neg_integer,
          pipe_before: boolean,
          signatures: [Metadata.signature_t()]
        }

  @doc """
  Returns the signature info from the function or type defined in the prefix, if any.
  """
  @spec find(String.t(), State.Env.t(), Metadata.t()) :: signature_info | :none
  def find(prefix, env, metadata) do
    %State.Env{imports: imports, aliases: aliases, module: module, vars: vars, attributes: attributes} = env

    binding_env = %Binding{
      attributes: attributes,
      variables: vars,
      current_module: module
    }

    with %{
          #  candidate: {mod, fun},
           elixir_prefix: elixir_prefix,
          #  npar: npar,
           unfinished_parm: unfinished_parm,
           pipe_before: pipe_before
         } <-
           Source.which_func(prefix, module),
           {:ok, ast} <- Code.Fragment.container_cursor_to_quoted(prefix),
           {_, {:ok, call, npar}} <- Macro.prewalk(ast, nil, &find_call_pre/2),
           {m, f} <- get_mod_fun(call, binding_env),
         {mod, fun, true} <-
           Introspection.actual_mod_fun(
             {m, f},
             imports,
             if(elixir_prefix, do: [], else: aliases),
             module,
             metadata.mods_funs_to_positions,
             metadata.types
           ) do
      signatures = find_signatures({mod, fun}, npar, unfinished_parm, env, metadata)
      %{active_param: npar, pipe_before: pipe_before, signatures: signatures}
    else
      _ ->
        :none
    end
  end

  def find_call_pre(ast, {:ok, call, npar}), do: {ast, {:ok, call, npar}}
  # def find_call_pre({:__cursor__, _, []} = ast, []), do: {ast, nil}
  # def find_call_pre({:__cursor__, _, []} = ast, [candidate | _]), do: {ast, {:ok, candidate}}
  # transform `a |> b(c)` calls into `b(a, c)`
  def find_call_pre({:|>, _, [params_1, {call, meta, params_rest}]}, state) do
    params = [params_1 | params_rest || []]
    find_call_pre({call, meta, params}, state)
  end
  def find_call_pre({{:., _, call}, _, params} = ast, list) when is_list(params) do
    case Enum.find_index(params, &match?({:__cursor__, _, []}, &1)) do
      nil -> {ast, nil}
      npar -> {ast, {:ok, call, npar}}
    end
  end
  # def find_call_pre({:__aliases__, _, _} = ast, state), do: {ast, state}
  # def find_call_pre({:., _, _} = ast, state), do: {ast, state}
  def find_call_pre({atom, _, params} = ast, list) when is_atom(atom) and is_list(params) do
    case Enum.find_index(params, &match?({:__cursor__, _, []}, &1)) do
      nil -> {ast, nil}
      npar -> {ast, {:ok, atom, npar}}
    end
  end
  def find_call_pre(ast, state), do: {ast, state}

  # def find_call_post({{:., _, _call}, _, _} = ast, [_ | rest]), do: {ast, rest}
  # def find_call_post({:__aliases__, _, _} = ast, state), do: {ast, state}
  # def find_call_post({:., _, _} = ast, state), do: {ast, state}
  # def find_call_post({atom, _, _} = ast, [_ | rest]) when is_atom(atom), do: {ast, rest}
  # def find_call_post(ast, state), do: {ast, state}

  def get_mod_fun(atom, _binding_env) when is_atom(atom), do: {nil, atom}
  def get_mod_fun([{:__aliases__, _, list}, fun], binding_env) do
    mod = get_mod(list, binding_env)
    if mod do
      {mod, fun}
    end
  end
  def get_mod_fun([{:__MODULE__, _, nil}, fun], binding_env) do
    if binding_env.current_module not in [nil, Elixir] do
      {binding_env.current_module, fun}
    end
  end
  def get_mod_fun([{:@, _, [{name, _, nil}]}, fun], binding_env) when is_atom(name) do
    case Binding.expand(binding_env, {:attribute, name}) do
      {:atom, atom} ->
        {atom, fun}
      _ -> nil
    end
  end
  def get_mod_fun([{name, _, nil}, fun], binding_env) when is_atom(name) do
    case Binding.expand(binding_env, {:variable, name}) do
      {:atom, atom} ->
        {atom, fun}
      _ -> nil
    end
  end
  def get_mod_fun([atom, fun], _binding_env) when is_atom(atom), do: {atom, fun}
  def get_mod_fun(_, _binding_env), do: nil

  def get_mod([{:__MODULE__, _, nil} | rest], binding_env) do
    if binding_env.current_module not in [nil, Elixir] do
      binding_env.current_module
      |> Module.split()
      |> Kernel.++(rest)
      |> Module.concat()
    end
  end

  def get_mod([{:@, _, [{name, _, nil}]} | rest], binding_env) when is_atom(name) do
    case Binding.expand(binding_env, {:attribute, name}) do
      {:atom, atom} ->
        atom
        |> Module.split()
        |> Kernel.++(rest)
        |> Module.concat()
      _ -> nil
    end
  end

  def get_mod([head | _rest] = list, _binding_env) when is_atom(head) do
    Module.concat(list)
  end

  def get_mod(_list, _binding_env), do: nil

  defp find_signatures({mod, fun}, npar, unfinished_parm, env, metadata) do
    signatures = find_function_signatures({mod, fun}, env, metadata)

    signatures =
      if Metadata.at_module_body?(metadata, env) do
        signatures ++ find_type_signatures({mod, fun}, metadata)
      else
        signatures
      end

    signatures
    |> Enum.filter(fn %{params: params} ->
      params_length = length(params)

      if params_length == 0 do
        not unfinished_parm and npar == 0
      else
        if unfinished_parm do
          params_length >= npar + 1
        else
          params_length > npar
        end
      end
    end)
    |> Enum.sort_by(&length(&1.params))
  end

  defp find_function_signatures({nil, _fun}, _env, _metadata), do: []

  defp find_function_signatures({mod, fun}, env, metadata) do
    signatures =
      case Metadata.get_function_signatures(metadata, mod, fun) do
        [] ->
          Introspection.get_signatures(mod, fun)

        signatures ->
          callback_docs_specs = Metadata.get_docs_specs_from_behaviours(env)

          for signature <- signatures do
            if signature.documentation == nil or signature.spec == nil do
              arity = length(signature.params)

              {spec, doc, _} =
                Metadata.get_doc_spec_from_behaviours(callback_docs_specs, fun, arity)

              %{
                signature
                | documentation: signature.documentation || doc,
                  spec: signature.spec || spec
              }
            else
              signature
            end
          end
      end

    signatures |> Enum.uniq_by(fn sig -> sig.params end)
  end

  defp find_type_signatures({nil, fun}, _metadata) do
    TypeInfo.get_signatures(nil, fun)
  end

  defp find_type_signatures({mod, fun}, metadata) do
    case Metadata.get_type_signatures(metadata, mod, fun) do
      [] -> TypeInfo.get_signatures(mod, fun)
      signature -> signature
    end
  end
end
