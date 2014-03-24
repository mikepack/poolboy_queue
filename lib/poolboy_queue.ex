defmodule PoolboyQueue do
  use GenServer.Behaviour

  # GenServer API
  def start_link do
    :gen_server.start_link(__MODULE__, [], [])
  end

  def init(state) do
    {:ok, state}
  end

  def handle_cast({:push, args}, stack) do
    {:noreply, [args|stack]}
  end

  def handle_cast(:work, stack) do
    watcher_pid = spawn fn ->
      PoolboyQueue.Watcher.watch
    end
    flush(stack, watcher_pid)
    {:noreply, stack}
  end

  # Public API
  def work(queue_pid) do
    :gen_server.cast(queue_pid, :work)
  end

  def enqueue(queue_pid, args) do
    :gen_server.cast(queue_pid, {:push, args})
  end

  defp flush([], _) do
  end

  defp flush([args|stack], watcher_pid) do
    case :poolboy.status(:hard_workers) do
      {:ready, _, _, _} ->
        worker = :poolboy.checkout(:hard_workers)
        :gen_server.cast(worker, {:work, watcher_pid, args})

        flush(stack, watcher_pid)

      {:full, _, _, _} ->
        flush([args|stack], watcher_pid)
    end
  end
end
