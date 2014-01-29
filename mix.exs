defmodule SaEautomation.Mixfile do
  use Mix.Project

  def project do
    [ app: :nano_ring,
      version: "0.0.1",
      elixir: "~> 0.12.2",
      deps: [],
      ## dev multi nodes configs
      dev1_config: [nano_ring: [data_dir: "./dev1_data"]],
      dev2_config: [nano_ring: [data_dir: "./dev2_data"]],
      dev3_config: [nano_ring: [data_dir: "./dev3_data"]],
      dev4_config: [nano_ring: [data_dir: "./dev4_data"]] 
    ]
  end

  def application do
    [ mod: { NanoRing.App,[] },
      applications: [:iex],
      env: [ data_dir: "./data" ] ]
  end
end
