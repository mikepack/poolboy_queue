defmodule PoolboyQueueTest do
  use ExUnit.Case

  setup do
    results_pid = PoolboyQueueTest.Helpers.setup_queue
    { :ok, queue } = PoolboyQueue.start_link(:results)
    { :ok, results_pid: results_pid, queue: queue }
  end

  test "it runs jobs", meta do
    PoolboyQueue.enqueue(meta[:queue], meta[:results_pid])
    PoolboyQueue.enqueue(meta[:queue], meta[:results_pid])

    PoolboyQueue.work(meta[:queue])

    :timer.sleep 5

    assert PoolboyQueueTest.Helpers.finished(meta[:results_pid]) == 2
  end

  test "it doesn't run jobs until told", meta do
    PoolboyQueue.enqueue(meta[:queue], meta[:results_pid])

    :timer.sleep 5

    assert PoolboyQueueTest.Helpers.finished(meta[:results_pid]) == 0
  end

  test "jobs can continue to be added", meta do
    PoolboyQueue.enqueue(meta[:queue], meta[:results_pid])

    PoolboyQueue.work(meta[:queue])

    PoolboyQueue.enqueue(meta[:queue], meta[:results_pid])

    :timer.sleep 5

    assert PoolboyQueueTest.Helpers.finished(meta[:results_pid]) == 2
  end

  test "argument-less jobs can be added to the queue" do
    Process.register(self, :test_runner)

    {:ok, queue} = PoolboyQueue.start_link(:argumentless)

    PoolboyQueue.work(queue)
    PoolboyQueue.enqueue(queue)

    assert_receive :done
  end
end
