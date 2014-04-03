defmodule PoolboyQueueTest.ArgumentlessWorkers do
  use PoolboyQueue.Queue

  def name, do: :argumentless
  def worker_module, do: PoolboyQueueTest.ArgumentlessWorker
end
