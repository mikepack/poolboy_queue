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
