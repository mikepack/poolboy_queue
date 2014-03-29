defmodule PoolboyQueueTest do
  use ExUnit.Case

  setup do
    results_pid = PoolboyQueueTest.Helpers.setup_queue
    { :ok, results_pid: results_pid }
  end

  test "it runs jobs", meta do
    {:ok, queue} = PoolboyQueue.start_link

    PoolboyQueue.enqueue(queue, meta[:results_pid])
    PoolboyQueue.enqueue(queue, meta[:results_pid])

    PoolboyQueue.work(queue)

    :timer.sleep 5

    assert PoolboyQueueTest.Helpers.finished(meta[:results_pid]) == 2
  end

  test "it doesn't run jobs until told", meta do
    {:ok, queue} = PoolboyQueue.start_link

    PoolboyQueue.enqueue(queue, meta[:results_pid])

    :timer.sleep 5

    assert PoolboyQueueTest.Helpers.finished(meta[:results_pid]) == 0
  end

  test "jobs can continue to be added", meta do
    {:ok, queue} = PoolboyQueue.start_link

    PoolboyQueue.enqueue(queue, meta[:results_pid])

    PoolboyQueue.work(queue)

    PoolboyQueue.enqueue(queue, meta[:results_pid])

    :timer.sleep 5

    assert PoolboyQueueTest.Helpers.finished(meta[:results_pid]) == 2
  end
end
