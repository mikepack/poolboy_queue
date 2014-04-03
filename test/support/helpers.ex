defmodule PoolboyQueueTest.Helpers do
  def setup_queue do
    {:ok, results_pid} = PoolboyQueueTest.Results.start_link
    PoolboyQueueTest.ResultWorkers.start_link
    PoolboyQueueTest.ArgumentlessWorkers.start_link
    results_pid
  end

  def finished(results_pid) do
    :gen_server.call(results_pid, :count)
  end
end