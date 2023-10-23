defmodule OtelBench do
  @moduledoc """
  Documentation for `OtelBench`.
  """

  def ets_traverse_bench do
    res = :otel_resource.create(%{}, :undefined)
    Benchee.run(
      %{
        "ets_delete" => {fn _input -> :otel_ets_traverse.export_delete_table(:ets_delete_tab, res) end,
                         before_each:
                         fn input ->
                           :otel_ets_traverse.new_ets(:ets_delete_tab)
                           :otel_ets_traverse.insert_spans(:ets_delete_tab, input)
                         end},
        "ets_take" => {fn _input -> :otel_ets_traverse.export_take_from_table(:ets_take_from_tab, res) end,
                       before_each:
                       fn input ->
                         :otel_ets_traverse.insert_spans(:ets_take_from_tab, input)
                       end},
        "ets_fix_take" => {fn _input -> :otel_ets_traverse.export_fix_take_from_table(:ets_fix_take_from_tab, res) end,
                           before_each:
                           fn input ->
                             :otel_ets_traverse.insert_spans(:ets_fix_take_from_tab, input)
                           end}
      },
      inputs: %{
        "10K" => 10_000,
        "20K" => 20_000,
        "100K" => 100_000
      },
      before_scenario: fn input ->
        :otel_ets_traverse.new_ets(:ets_take_from_tab)
        :otel_ets_traverse.new_ets(:ets_fix_take_from_tab)
        input
      end,
      time: 60
    )
  end

  def ets_insert_bench do
    :otel_ets_insert.new_ets_set(:ets_insert_set)
    :otel_ets_insert.new_ets_duplicate_bag(:ets_insert_dup_bag)

    Benchee.run(
      %{
        "ets_set" => {fn input -> :otel_ets_insert.gen_and_insert_spans(:ets_insert_set, input) end,
                      after_each:
                      fn _input ->
                        :ets.delete_all_objects(:ets_insert_set)
                      end},
        "ets_duplicate_bag" => {fn input -> :otel_ets_insert.gen_and_insert_spans(:ets_insert_dup_bag, input) end,
                                after_each:
                                fn _input ->
                                  :ets.delete_all_objects(:ets_insert_dup_bag)
                                end},
      },
      inputs: %{
        "1" => 1,
        "100" => 100,
        "1K" => 1_000
      },
      time: 60,
      parallel: 1000
    )
  end

end
