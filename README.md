# Poolboy Queue - A scrawny queue for Poolboy

Poolboy Queue allows you to enqueue jobs to be run in [Poolboy](https://github.com/devinus/poolboy) workers. You may disregard pool overflows and just start working.


## Usage

Create a worker module:

```elixir
defmodule App.HardWorker do
  use GenServer.Behaviour

  def start_link(state) do
    :gen_server.start_link(__MODULE__, state, [])
  end

  def init(state) do
    {:ok, state}
  end

  def handle_cast({:worker_arg1, num}, state) do
    IO.puts "Working on #{num}."
    # Do some work here
    {:noreply, state}
  end
end
```

Create your pool of workers (named `:hard_workers`) as you would normally with Poolboy:

```elixir
defmodule App.PoolboySupervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    pool_options = [
      name: {:local, :hard_workers},
      worker_module: App.HardWorker,
      size: 1,
      max_overflow: 0
    ]

    children = [
      :poolboy.child_spec(:hard_workers, pool_options, [])
    ]

    supervise(children, strategy: :one_for_one)
  end
end
```

Notice we set the pool `size` to 1 and the `max_overflow` to 0, effectively giving us a single worker process.

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
{:ok, queue} = PoolboyQueue.start_link

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

Poolboy is great at telling you when you've run out of available workers, but it doesn't allow you to "schedule for later" in the case that the pool has dried up. You'll need to wait for an available worker.

It's useful to just queue up a job without having to worry about the availability of workers. When a worker becomes available, schedule the next job in the queue.


# TODO

- Support argument-less workers
- Remove timers from tests
- Inline docs
- Support numerous queues


## Caveats

Poolboy Queue is not a mature or production-ready project. Interleaving processes, atomicity, OTP compliance and the like have not been considered. Pull requests welcome.