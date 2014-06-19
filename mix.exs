defmodule SaEautomation.Mixfile do
  use Mix.Project

  def project do
    [ app: :nano_ring,
      version: "0.0.2",
      elixir: "~> 0.14.0",
      deps: []
    ]
  end

  def application do
    [ mod: { NanoRing.App,[] },
      applications: [:iex],
      env: [ data_dir: "./data" ] ]
  end
end
