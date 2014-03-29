defmodule PoolboyQueue do
  use GenServer.Behaviour

  # Public API
  def work(queue_pid) do
    :gen_server.cast(queue_pid, :work)
  end

  def enqueue(queue_pid, args) do
    :gen_server.cast(queue_pid, {:push, args})
  end

  # GenServer API
  def start_link do
    :gen_server.start_link(__MODULE__, {[], false}, [])
  end

  def init(state) do
    {:ok, state}
  end

  def handle_cast({:push, args}, {stack, started}) do
    stack = [args|stack]

    if started do
      :gen_server.cast(self, :work)
    end

    {:noreply, {stack, started}}
  end

  def handle_cast(:work, {stack, started}) do
    watcher_pid = spawn fn ->
      PoolboyQueue.Watcher.watch
    end
    flush(stack, watcher_pid)

    {:noreply, {[], true}}
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
