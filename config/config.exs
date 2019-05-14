use Mix.Config
sname = "#{node()}" |> String.split("@") |> hd

if sname != "nonode" do
  import_config "#{sname}.exs"
else
  [nano_ring: [data_dir: "data", die_after: 100, gossip_after: 1_000]]
end
