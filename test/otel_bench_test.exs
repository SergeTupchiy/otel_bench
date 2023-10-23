defmodule OtelBenchTest do
  use ExUnit.Case
  doctest OtelBench

  test "greets the world" do
    assert OtelBench.hello() == :world
  end
end
