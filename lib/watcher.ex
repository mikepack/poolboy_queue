defmodule PoolboyQueue.Watcher do
  def watch do
    receive do
      {:done, worker} ->
        :poolboy.checkin(:hard_workers, worker)
    end
    watch
  end
end