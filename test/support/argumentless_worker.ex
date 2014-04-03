defmodule PoolboyQueueTest.ArgumentlessWorker do
  use PoolboyQueue.Worker

  def perform do
    send :test_runner, :done
  end
end
