defmodule PoolboyQueue do
  @moduledoc """
  The PoolboyQueue module contains the public API for working with
  user-defined queues. Jobs are pushed into queues via this public
  API and queues can be instructed to begin flushing the jobs.
  """

  use GenServer.Behaviour

  ### Public API

  @doc """
  Starts a worker process that represents a named queue. After starting
  a queue, jobs can be pushed into it. The name of the queue, which is
  defined as part of the PoolboyQueue.Queue API, should be passed as the
  first argument to start_link/1.

      defmodule App.Workers do
        use PoolboyQueue.Queue, name: :workers,
                                worker: App.Worker
      end

      { :ok, queue } = PoolboyQueue.start_link(:workers)
      PoolboyQueue.enqueue(queue)
  """
  def start_link(name) do
    :gen_server.start_link(__MODULE__, {name, [], false}, [])
  end

  @doc """
  The work/1 function tells a queue to begin flushing any pending
  jobs that are enqueued for work. If jobs continue to be pushed
  into the queue after the queue has been started, they will be
  immediately processed.

  Imagine a queue has been registered with name `:workers` and jobs
  need to be pushed into the queue:

      { :ok, queue } = PoolboyQueue.start_link(:workers)

      PoolboyQueue.work(queue)
      PoolboyQueue.enqueue(queue, {:arg1, :arg2})

  If you'd like to defer processing until you've enqueued a number of
  jobs, call work/1 when you're ready:

      { :ok, queue } = PoolboyQueue.start_link(:workers)

      PoolboyQueue.enqueue(queue, {:arg1, :arg2})
      PoolboyQueue.enqueue(queue, {:arg3, :arg4})
      PoolboyQueue.work(queue)
  """
  def work(queue_pid) do
    :gen_server.cast(queue_pid, :work)
  end

  @doc """
  The primary function to put jobs on a queue.

  Jobs can be pushed onto a queue without arguments:

      { :ok, queue } = PoolboyQueue.start_link(:workers)

      PoolboyQueue.enqueue(queue)

  This will invoke the worker module's perform/0 function as a background
  process.

  Workers accepting a single parameter (defining a perform/1 function)
  should be enqueued with a single argument:

      { :ok, queue } = PoolboyQueue.start_link(:workers)

      PoolboyQueue.enqueue(queue, "My arg")

  Workers accepting more than one argument (defining a perform/1 function)
  should be enqueued with a tuple:

      { :ok, queue } = PoolboyQueue.start_link(:workers)

      PoolboyQueue.enqueue(queue, {:arg1, :arg2})
  """
  def enqueue(queue_pid, args \\ nil) do
    :gen_server.cast(queue_pid, {:push, args})
  end


  ### GenServer API
  def init(state) do
    {:ok, state}
  end

  def handle_cast({:push, args}, {name, stack, started}) do
    stack = [args|stack]

    if started do
      :gen_server.cast(self, :work)
    end

    {:noreply, {name, stack, started}}
  end

  def handle_cast(:work, {name, stack, started}) do
    watcher_pid = spawn fn ->
      PoolboyQueue.Watcher.watch(name)
    end
    flush(name, stack, watcher_pid)

    {:noreply, {name, [], true}}
  end

  defp flush(_, [], _) do
  end

  defp flush(name, [args|stack], watcher_pid) do
    case :poolboy.status(name) do
      {:ready, _, _, _} ->
        worker = :poolboy.checkout(name)
        :gen_server.cast(worker, {:work, watcher_pid, args})

        flush(name, stack, watcher_pid)

      {:full, _, _, _} ->
        flush(name, [args|stack], watcher_pid)
    end
  end
end
