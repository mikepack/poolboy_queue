defmodule PoolboyQueue.Queue do
  @moduledoc """
  Use this mixin to define a queue. A queue is a named background process
  that can accept jobs who would like to perform work. The queue contains
  the state for what jobs are waiting to be executed. Queues are FIFO, so
  jobs that are pushed into the queue first are executed first. Of course,
  workers run concurrently so the orders in which jobs are enqueued does
  not equate to the order in which they finish.

  Imagine you want a queue named `:workers`:

      defmodule App.Workers do
        use PoolboyQueue.Queue, name: :workers,
                                worker: App.HardWorker
      end

  Keep in mind, defining a queue does not start the queue immediately.
  """

  use Supervisor.Behaviour

  defmacro __using__(options) do
    quote do
      def start_link do
        :supervisor.start_link(__MODULE__, [])
      end

      def init([]) do
        pool_options = [
          name: {:local, name},
          worker_module: worker,
          size: 1,
          max_overflow: 0
        ]

        children = [
          :poolboy.child_spec(name, pool_options, [])
        ]

        supervise(children, strategy: :one_for_one)
      end

      defp name do
        unquote(options[:name])
      end

      defp worker do
        unquote(options[:worker])
      end
    end
  end
end