defmodule PoolboyQueueTest.Supervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    pool_options = [
      name: {:local, :hard_workers},
      worker_module: PoolboyQueueTest.HardWorker,
      size: 1,
      max_overflow: 0
    ]

    children = [
      :poolboy.child_spec(:hard_workers, pool_options, [])
    ]

    supervise(children, strategy: :one_for_one)
  end
end
