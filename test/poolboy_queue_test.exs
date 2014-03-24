defmodule PoolboyQueueTest do
  use ExUnit.Case

  setup do
    results_pid = PoolboyQueueTest.Helpers.setup_queue
    { :ok, results_pid: results_pid }
  end

  test "it runs a job", meta do
    {:ok, queue} = PoolboyQueue.start_link

    PoolboyQueue.enqueue(queue, meta[:results_pid])
    PoolboyQueue.enqueue(queue, meta[:results_pid])

    PoolboyQueue.work(queue)

    :timer.sleep 5

    assert PoolboyQueueTest.Helpers.finished(meta[:results_pid]) == 2
  end
end


defmodule PoolboyQueueTest.Helpers do
  def setup_queue do
    {:ok, results_pid} = PoolboyQueueTest.Results.start_link
    PoolboyQueueTest.Supervisor.start_link
    results_pid
  end

  def finished(results_pid) do
    :gen_server.call(results_pid, :count)
  end
end

defmodule PoolboyQueueTest.HardWorker do
  use PoolboyQueue.Worker
  
  def perform(results_pid) do
    :gen_server.cast(results_pid, {:push, self})
  end
end

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

defmodule PoolboyQueueTest.Results do
  use GenServer.Behaviour

  def start_link do
    :gen_server.start_link(__MODULE__, [], [])
  end

  def init(stack) do
    {:ok, stack}
  end

  def handle_cast({:push, results}, stack) do
    {:noreply, [results|stack]}
  end

  def handle_call(:count, _from, stack) do
    {:reply, Enum.count(stack), stack}
  end
end
