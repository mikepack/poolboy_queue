defmodule PoolboyQueueTest.Supervisor do
  use PoolboyQueue.Queue

  def name, do: :results
  def worker_module, do: PoolboyQueueTest.ResultWorker
end
