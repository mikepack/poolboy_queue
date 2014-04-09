# Poolboy Queue

## A scrawny queue for Poolboy

[![Build Status](https://travis-ci.org/mikepack/poolboy_queue.svg)](https://travis-ci.org/mikepack/poolboy_queue)

Poolboy Queue allows you to enqueue jobs to be run in [Poolboy](https://github.com/devinus/poolboy) workers. You may disregard pool overflows and just start working.


## Usage

Create a worker module:

```elixir
defmodule App.HardWorker do
  use PoolboyQueue.Worker

  def perform(results_pid) do
    IO.puts "Working on #{num}."
  end
end
```

Create your pool of workers (named `:hard_workers`) as you would normally with Poolboy:

```elixir
defmodule App.PoolboySupervisor do
  use PoolboyQueue.Queue, name: :hard_workers,
                          worker: App.HardWorker
end
```

<!-- Notice we set the pool `size` to 1 and the `max_overflow` to 0, effectively giving us a single worker process. -->

<!-- 
You'll likely want your application to supervise your `PoolboySupervisor`, so add it to your **mix.exs** file:

```elixir
def application do
  [ applications: [:],
    mod: { ConsumerElixir, [] } ]
end
```
 -->

Create a queue for this pool and push two jobs into it:

```elixir
{:ok, queue} = PoolboyQueue.start_link(:hard_workers)

PoolboyQueue.enqueue(queue, { :worker_arg1, 123 })
PoolboyQueue.enqueue(queue, { :worker_arg1, 456 })

PoolboyQueue.waiting #=> 2

PoolboyQueue.working #=> 0

PoolboyQueue.work(queue)
#=> Working on 123
#=> Working on 456

PoolboyQueue.enqueue(queue, { :worker_arg1, 789 })
#=> Working on 789
```

Although our pool contains only one worker, we've enqueued two jobs. Poolboy Queue will continue running jobs until either all workers are consumed or the queue is empty.


## Why

Poolboy is great at telling you when you've run out of available workers, but it doesn't allow you to "schedule for later" in the case that the pool is full. You'll need to wait for an available worker.

Poolboy handles full pools in two ways. By default, it will block the current process and wait for an available worker, failing after a timeout:

```elixir
worker = :poolboy.checkout(:hard_workers)
# Timeout, loosing worker
```

Or, Poolboy can return immediately telling you the pool is full:

```elixir
worker = :poolboy.checkout(:hard_workers, false)
#=> :full
```

Somtimes it's useful to just queue up a series of jobs without having to worry about the availability of workers. When a worker becomes available, schedule the next job in the queue.


# TODO

- Remove timers from tests
- Remove perform placeholders from PoolboyQueue.Worker
- Unit tests around worker exceptions
- Unit tests around PoolboyQueue.Queue options
- Unit test enqueuing jobs for a queue has not been started
- Add a function to stop a queue from working
- Add queue size options to PoolboyQueue.Queue


## Caveats

Poolboy Queue is not a mature or production-ready project. Interleaving processes, atomicity, OTP compliance and the like have not been considered. Pull requests welcome.