defmodule PoolboyQueueTest.ResultWorkers do
  use PoolboyQueue.Queue, name: :results,
                          worker: PoolboyQueueTest.ResultWorker
end
