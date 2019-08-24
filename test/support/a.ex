defmodule A do
  def plain_fun do
    B.Callee.fun()
  end

  def other_fun do
    B.Callee.my_fun()
  end
end
