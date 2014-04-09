defmodule PoolboyQueueTest.ArgumentlessWorkers do
  use PoolboyQueue.Queue, name: :argumentless,
                          worker: PoolboyQueueTest.ArgumentlessWorker
end
