defmodule PoolboyQueueTest.HardWorker do
  use PoolboyQueue.Worker

  def perform(results_pid) do
    :gen_server.cast(results_pid, {:push, self})
  end
end
