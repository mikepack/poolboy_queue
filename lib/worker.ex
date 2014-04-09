defmodule PoolboyQueue.Worker do
  @moduledoc """
  Use this mixin to define a worker module that can process jobs. A worker
  must define a perform function that is run as part of a background
  process. The perform function is always asynchronous, so its return
  value is always discarded.

      defmodule App.Worker do
        use PoolboyQueue.Worker

        def perform do
          # Do some asynchronous work
        end
      end

  If a worker needs to accept an argument, define a perform/1 function:

      defmodule App.Worker do
        use PoolboyQueue.Worker

        def perform(name) do
          # Do some asynchronous work
        end
      end

  If a worker needs to accept more than one argument, represent all
  arguments as a tuple:

      defmodule App.Worker do
        use PoolboyQueue.Worker

        def perform({type, id}) do
          # Do some asynchronous work
        end
      end
  """

  use GenServer.Behaviour

  defmacro __using__(_) do
    quote do
      def start_link(state) do
        :gen_server.start_link(__MODULE__, state, [])
      end

      def init(state) do
        {:ok, state}
      end

      def handle_cast({:work, watcher_pid, args}, state) do
        if args == nil do
          perform
        else
          perform(args)
        end

        send watcher_pid, {:done, self}
        {:noreply, state}
      end

      def perform do
        raise "Please define a perform/0 method in your worker."
      end

      def perform(_) do
        raise "Please define a perform/1 method in your worker."
      end

      defoverridable [perform: 0, perform: 1]
    end
  end
end