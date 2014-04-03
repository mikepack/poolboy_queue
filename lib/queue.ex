defmodule PoolboyQueue.Queue do
  use Supervisor.Behaviour

  defmacro __using__(_) do
    quote do

      def start_link do
        :supervisor.start_link(__MODULE__, [])
      end

      def init([]) do
        pool_options = [
          name: {:local, name},
          worker_module: worker_module,
          size: 1,
          max_overflow: 0
        ]

        children = [
          :poolboy.child_spec(name, pool_options, [])
        ]

        supervise(children, strategy: :one_for_one)
      end
    end
  end
end