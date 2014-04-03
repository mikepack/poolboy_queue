defmodule PoolboyQueue.Watcher do
  def watch(name) do
    receive do
      {:done, worker} ->
        :poolboy.checkin(name, worker)
    end
    watch(name)
  end
end