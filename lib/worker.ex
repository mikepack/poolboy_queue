defmodule PoolboyQueue.Worker do
  use GenServer.Behaviour

  defmacro __using__(_) do
    quote location: keep do
      def start_link(state) do
        :gen_server.start_link(__MODULE__, state, [])
      end

      def init(state) do
        {:ok, state}
      end

      def handle_cast({:work, watcher_pid, args}, state) do
        perform(args)
        send watcher_pid, {:done, self}
        {:noreply, state}
      end
    end
  end
end