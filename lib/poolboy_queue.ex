defmodule PoolboyQueue do
  use GenServer.Behaviour

  # Public API
  def work(queue_pid) do
    :gen_server.cast(queue_pid, :work)
  end

  def enqueue(queue_pid, args \\ nil) do
    :gen_server.cast(queue_pid, {:push, args})
  end

  # GenServer API
  def start_link(name) do
    :gen_server.start_link(__MODULE__, {name, [], false}, [])
  end

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
