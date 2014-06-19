use Mix.Config
sname = "#{node}" |> String.split("@") |> hd

if sname != "nonode" do
  import_config "#{sname}.exs"
else
  [nano_ring: [data_dir: "data"]]
end
