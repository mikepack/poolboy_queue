defmodule PoolboyQueueTest.Helpers do
  def setup_queue do
    {:ok, results_pid} = PoolboyQueueTest.Results.start_link
    PoolboyQueueTest.Supervisor.start_link
    results_pid
  end

  def finished(results_pid) do
    :gen_server.call(results_pid, :count)
  end
end