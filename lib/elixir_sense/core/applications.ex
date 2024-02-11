# This file includes modified code extracted from the elixir project. Namely:
#
# https://github.com/elixir-lang/elixir/blob/v1.10/lib/iex/lib/iex/autocomplete.ex
#
# The original code is licensed as follows:
#
# Copyright 2012 Plataformatec
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

defmodule ElixirSense.Core.Applications do
  @moduledoc """
  This module contains helper functions for introspecting running OTP applications
  """

  @spec get_modules_from_applications() :: [module]
  # TODO use :code.all_available |> Enum.map(fn {m, _, _} -> :"#{m}" end) on otp 23+
  # as it returns more
  def get_modules_from_applications do
    # :erts app is not loaded by default
    _ = Application.load(:erts)

    for [app] <- loaded_applications(),
        module <- safe_get_modules(app) do
      module
    end
  end

  defp safe_get_modules(app) do
    case :application.get_key(app, :modules) do
      {:ok, modules} -> modules
      :undefined -> []
    end
  end

  defp loaded_applications do
    # If we invoke :application.loaded_applications/0,
    # it can error if we don't call safe_fixtable before.
    # Since in both cases we are reaching over the
    # application controller internals, we choose to match
    # for performance.
    :ets.match(:ac_tab, {{:loaded, :"$1"}, :_})
  end

  def get_application(module) do
    Enum.find_value(:application.loaded_applications(), fn {app, _, _} ->
      case :application.get_key(app, :modules) do
        {:ok, modules} ->
          if module in modules do
            app
          end

        :undefined ->
          nil
      end
    end)
  end
end
