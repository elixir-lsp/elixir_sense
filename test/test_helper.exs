defmodule ExUnitConfig do
  defp otp_older_than(major) do
    {otp_major_version, ""} = Integer.parse(System.build_info()[:otp_release])
    otp_major_version < major
  end

  defp otp_related do
    for major <- 22..25, otp_older_than(major) do
      {:"requires_otp_#{major}", true}
    end
  end

  defp elixir_older_than(minor) do
    !Version.match?(System.build_info().version, ">= 1.#{minor}.0")
  end

  defp elixir_related do
    for minor_version <- 12..15, elixir_older_than(minor_version) do
      {:"requires_elixir_1_#{minor_version}", true}
    end
  end

  def erlang_eep48_supported do
    otp_release = System.otp_release() |> String.to_integer()
    otp_release >= 23
  end

  def excludes do
    [requires_source: true] ++ otp_related() ++ elixir_related()
  end
end

ExUnit.configure(exclude: ExUnitConfig.excludes())
ExUnit.start()

Application.load(:erts)
