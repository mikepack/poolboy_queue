defmodule PoolboyQueue.Mixfile do
  use Mix.Project

  def project do
    [ app: :poolboy_queue,
      version: "0.1.0",
      elixir: "~> 0.12.5",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    []
  end

  defp deps do
    [
      { :poolboy, "~> 1.0", github: "devinus/poolboy" }
    ]
  end
end
