defmodule ElixirSense.ServerTest do
  use ExUnit.Case

  setup_all do
    spawn_link(fn ->
      ElixirSense.Server.start(["7777", "dev"])
    end)

    Process.sleep(50)
    {:ok, socket} = :gen_tcp.connect('localhost', 7777, [:binary, active: false, packet: 4])
    {:ok, socket: socket}
  end

  test "definition request", %{socket: socket} do
    request = %{
      "request_id" => 1,
      "request" => "definition",
      "payload" => %{
        "buffer" => "Enum.to_list",
        "line" => 1,
        "column" => 6
      }
    }
    assert send_request(socket, request) =~ "enum.ex:2523"
  end

  test "signature request", %{socket: socket} do
    request = %{
      "request_id" => 1,
      "request" => "signature",
      "payload" => %{
        "buffer" => "List.flatten(par, ",
        "line" => 1,
        "column" => 18
      }
    }
    assert send_request(socket, request).active_param == 1
  end

  test "quote request", %{socket: socket} do
    request = %{
      "request_id" => 1,
      "request" => "quote",
      "payload" => %{
        "code" => "var = 1",
      }
    }
    assert send_request(socket, request) == "{:=, [line: 1], [{:var, [line: 1], nil}, 1]}"
  end

  test "match request", %{socket: socket} do
    request = %{
      "request_id" => 1,
      "request" => "match",
      "payload" => %{
        "code" => "{var1, var2} = {1, 2}",
      }
    }
    assert send_request(socket, request) == "# Bindings\n\nvar1 = 1\n\nvar2 = 2"
  end

  test "expand request", %{socket: socket} do
    request = %{
      "request_id" => 1,
      "request" => "expand_full",
      "payload" => %{
        "buffer" => "",
        "selected_code" => "unless true, do: false",
        "line" => 1
      }
    }
    assert send_request(socket, request).expand_once == "if(true) do\n  nil\nelse\n  false\nend"
  end

  test "docs request", %{socket: socket} do
    request = %{
      "request_id" => 1,
      "request" => "docs",
      "payload" => %{
        "buffer" => "Enum.to_list",
        "line" => 1,
        "column" => 6
      }
    }
    assert send_request(socket, request).docs.docs =~ "> Enum.to_list"
  end

  test "suggestions request", %{socket: socket} do
    request = %{
      "request_id" => 1,
      "request" => "suggestions",
      "payload" => %{
        "buffer" => "",
        "line" => 1,
        "prefix" => "List."
      }
    }
    assert send_request(socket, request) |> Enum.at(0) == %{type: :hint, value: "List."}
  end

  test "set_context request", %{socket: socket} do
    {_, _, _, env, cwd, _} = ContextLoader.get_state()

    assert env == "dev"

    request = %{
      "request_id" => 1,
      "request" => "set_context",
      "payload" => %{
        "env" => "test",
        "cwd" => cwd
      }
    }
    send_request(socket, request)

    {_, _, _, env, _, _} = ContextLoader.get_state()
    assert env == "test"
  end

  defp send_request(socket, request) do
    data = :erlang.term_to_binary(request)
    send_and_recv(socket, data)
    |> :erlang.binary_to_term
    |> Map.get(:payload)
  end

  defp send_and_recv(socket, data) do
    :ok = :gen_tcp.send(socket, data)
    {:ok, response} = :gen_tcp.recv(socket, 0, 1000)
    response
  end

end
