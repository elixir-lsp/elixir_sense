defmodule ElixirSenseExample.FunctionsWithReturnSpec do
  defstruct [:abc]

  @type t :: %ElixirSenseExample.FunctionsWithReturnSpec{
          abc: %{key: nil}
        }
  @type x :: %{required(:abc) => atom_1, optional(:cde) => atom_1}
  @type atom_1 :: :asd
  @type num :: number

  @spec f01() :: Abc.non_existing()
  def f01(), do: :ok

  @spec f02() :: atom_1
  def f02(), do: :ok

  @spec f03() :: num
  def f03(), do: :ok

  @spec f1() :: t
  def f1(), do: :ok

  @spec f2() :: x
  def f2(), do: :ok

  @spec f3() :: ElixirSenseExample.FunctionsWithReturnSpec.Remote.t()
  def f3(), do: :ok

  @spec f4() :: ElixirSenseExample.FunctionsWithReturnSpec.Remote.x()
  def f4(), do: :ok

  @spec f5() :: %ElixirSenseExample.FunctionsWithReturnSpec{}
  def f5(), do: :ok

  @spec f6() :: %{abc: atom}
  def f6(), do: :ok

  @spec f7() :: %{abc: atom}
  @spec f7() :: nil
  def f7(), do: :ok

  @spec f71(integer) :: %{abc: atom}
  @spec f71(boolean) :: %{abc: atom}
  def f71(_), do: :ok

  @spec f8() :: %{abc: atom} | nil
  def f8(), do: :ok

  @spec f9(a) :: %{abc: atom} when a: integer
  def f9(_), do: :ok

  @spec f91() :: a when a: %{abc: atom}
  def f91(), do: :ok

  @spec f10(integer, integer, any) :: String
  def f10(_ \\ 0, _ \\ 0, _), do: String
end

defmodule ElixirSenseExample.FunctionsWithReturnSpec.Remote do
  defstruct [:abc]
  @type t :: %ElixirSenseExample.FunctionsWithReturnSpec.Remote{}
  @type x :: %{abc: atom}
end
