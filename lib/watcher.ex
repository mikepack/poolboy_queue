defmodule PoolboyQueue.Watcher do
  @moduledoc """
  This module is used for internal maintenance of Poolboy workers. When
  workers are finished, they call watch/1 on this module to indicate
  the worker is free to accept a new job.
  """

  @doc """
  Wait for any jobs to finish so the worker can be checked back into
  Poolboy.
  """
  def watch(name) do
    receive do
      {:done, worker} ->
        :poolboy.checkin(name, worker)
    end
    watch(name)
  end
end