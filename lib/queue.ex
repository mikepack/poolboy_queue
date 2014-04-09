defmodule PoolboyQueue.Queue do
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