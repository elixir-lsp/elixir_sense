defmodule ElixirSenseExample.FunctionsWithReturnSpec do
  defstruct [:abc]

  @type t :: %ElixirSenseExample.FunctionsWithReturnSpec{
          abc: %{key: nil}
        }
  @type x :: %{required(:abc) => atom_1, optional(:cde) => atom_1}
  @type atom_1 :: :asd
  @type num :: number
  @type tup :: {:ok, :abc}
  @type int :: 44

  @spec f01() :: Abc.non_existing()
  def f01(), do: :ok

  @spec f02() :: atom_1
  def f02(), do: :ok

  @spec f03() :: num
  def f03(), do: :ok

  @spec f04() :: tup
  def f04(), do: :ok

  @spec f05() :: int
  def f05(), do: :ok

  @spec f1() :: t
  def f1(), do: :ok

  @spec f1x(any) :: t
  def f1x(_a), do: :ok

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
  def f71(_x), do: :ok

  @spec f8() :: %{abc: atom} | nil
  def f8(), do: :ok

  @spec f9(a) :: %{abc: atom} when a: integer
  def f9(_a), do: :ok

  @spec f91() :: a when a: %{abc: atom}
  def f91(), do: :ok

  @spec f10(integer, integer, any) :: String
  def f10(_a \\ 0, _b \\ 0, _c), do: String

  @spec f11 :: {:ok, :some} | {:error, :some_error}
  def f11, do: {:ok, :some}

  @spec f_no_return :: no_return
  def f_no_return, do: :ok

  @spec f_any :: any
  def f_any, do: :ok

  @spec f_term :: term
  def f_term, do: :ok
end

defmodule ElixirSenseExample.FunctionsWithReturnSpec.Remote do
  defstruct [:abc]
  @type t :: %ElixirSenseExample.FunctionsWithReturnSpec.Remote{}
  @type x :: %{abc: atom}
end
