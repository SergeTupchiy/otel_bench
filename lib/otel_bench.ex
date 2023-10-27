defmodule OtelBench do
  @moduledoc """
  Documentation for `OtelBench`.
  """

  def ets_traverse_bench do
    res = :otel_resource.create(%{}, :undefined)

    Benchee.run(
      %{
        "ets_delete" =>
          {fn _input -> :otel_ets_traverse.export_delete_table(:ets_delete_tab, res) end,
           before_each: fn input ->
             :otel_ets_traverse.new_ets(:ets_delete_tab)
             :otel_ets_traverse.insert_spans(:ets_delete_tab, input)
           end},
        "ets_take" =>
          {fn _input -> :otel_ets_traverse.export_take_from_table(:ets_take_from_tab, res) end,
           before_each: fn input ->
             :otel_ets_traverse.insert_spans(:ets_take_from_tab, input)
           end},
        "ets_fix_take" =>
          {fn _input ->
             :otel_ets_traverse.export_fix_take_from_table(:ets_fix_take_from_tab, res)
           end,
           before_each: fn input ->
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
        "ets_set" =>
          {fn input -> :otel_ets_insert.gen_and_insert_spans(:ets_insert_set, input) end,
           after_each: fn _input ->
             :ets.delete_all_objects(:ets_insert_set)
           end},
        "ets_duplicate_bag" =>
          {fn input -> :otel_ets_insert.gen_and_insert_spans(:ets_insert_dup_bag, input) end,
           after_each: fn _input ->
             :ets.delete_all_objects(:ets_insert_dup_bag)
           end}
      },
      inputs: %{
        "1" => 1,
        "100" => 100,
        "1K" => 1_000
      },
      time: 60,
      parallel: 500
    )
  end

  def ets_insert_bench_less_cleanup do
    :otel_ets_insert.new_ets_set(:ets_insert_set)
    :otel_ets_insert.new_ets_duplicate_bag(:ets_insert_dup_bag)

    Benchee.run(
      %{
        "ets_set" =>
          {fn _input -> :otel_ets_insert.gen_and_insert_span(:ets_insert_set) end,
           after_scenario: fn _input ->
             :ets.delete_all_objects(:ets_insert_set)
           end},
        "ets_duplicate_bag" =>
          {fn _input -> :otel_ets_insert.gen_and_insert_span(:ets_insert_dup_bag) end,
           after_scenario: fn _input ->
             :ets.delete_all_objects(:ets_insert_dup_bag)
           end}
      },
      inputs: %{
        "1" => 1
      },
      time: 30,
      parallel: 1000
    )
  end

  def ets_insert_to_large_tab_bench do
    :otel_ets_insert.new_ets_set(:ets_insert_set)
    :otel_ets_insert.new_ets_duplicate_bag(:ets_insert_dup_bag)

    Benchee.run(
      %{
        "ets_duplicate_bag" =>
          {fn _input -> :otel_ets_insert.gen_and_insert_span(:ets_insert_dup_bag) end,
           before_scenario: fn _input ->
             :ets.delete_all_objects(:ets_insert_dup_bag)
             :otel_ets_insert.gen_and_insert_spans(:ets_insert_dup_bag, 100_000)
           end,
           after_scenario: fn _input -> :ets.delete_all_objects(:ets_insert_dup_bag) end},
        "ets_set" =>
          {fn _input -> :otel_ets_insert.gen_and_insert_span(:ets_insert_set) end,
           before_scenario: fn _input ->
             :ets.delete_all_objects(:ets_insert_set)
             :otel_ets_insert.gen_and_insert_spans(:ets_insert_set, 100_000)
           end,
           after_scenario: fn _input -> :ets.delete_all_objects(:ets_insert_set) end}
      },
      inputs: %{
        "1" => 1
      },
      time: 10,
      parallel: 100
    )
  end

  def ets_insert_to_large_tab_set_bench do
    :otel_ets_insert.new_ets_set(:ets_insert_set)

    Benchee.run(
      %{
        "ets_set" =>
          {fn _input -> :otel_ets_insert.gen_and_insert_span(:ets_insert_set) end,
           before_scenario: fn _input ->
             :ets.delete_all_objects(:ets_insert_set)
             :otel_ets_insert.gen_and_insert_spans(:ets_insert_set, 100_000)
           end,
           after_scenario: fn _input -> :ets.delete_all_objects(:ets_insert_set) end}
      },
      inputs: %{
        "1" => 1
      },
      time: 15,
      parallel: 100
    )
  end

  def ets_insert_to_large_tab_bag_bench do
    :otel_ets_insert.new_ets_duplicate_bag(:ets_insert_dup_bag)

    Benchee.run(
      %{
        "ets_duplicate_bag" =>
          {fn _input -> :otel_ets_insert.gen_and_insert_span(:ets_insert_dup_bag) end,
           before_scenario: fn _input ->
             :ets.delete_all_objects(:ets_insert_dup_bag)
             :otel_ets_insert.gen_and_insert_spans(:ets_insert_dup_bag, 100_000)
           end,
           after_scenario: fn _input -> :ets.delete_all_objects(:ets_insert_dup_bag) end}
      },
      inputs: %{
        "1" => 1
      },
      time: 15,
      parallel: 100
    )
  end

  def ets_size_bench do
    Benchee.run(
      %{
        "ets_size" =>
          {fn tid -> :ets.info(tid, :size) >= 0 end,
           before_scenario: fn input_opts ->
             :lists.foreach(fn p -> :erlang.garbage_collect(p) end, :erlang.processes())
             tid = :otel_ets_insert.new_ets_tab(:ets_size_test, input_opts)
             :otel_ets_insert.gen_and_insert_spans(tid, 100_000)
             tid
           end,
           after_scenario: fn tid -> :ets.delete(tid) end},
        "ets_size_empty_tab" =>
          {fn tid -> :ets.info(tid, :size) >= 0 end,
           before_scenario: fn input_opts ->
             :lists.foreach(fn p -> :erlang.garbage_collect(p) end, :erlang.processes())
             :otel_ets_insert.new_ets_tab(:ets_size_test, input_opts)
           end,
           after_scenario: fn tid -> :ets.delete(tid) end},
        "ets_first" =>
          {fn tid -> :ets.first(tid) != :"$end_of_table" end,
           before_scenario: fn input_opts ->
             :lists.foreach(fn p -> :erlang.garbage_collect(p) end, :erlang.processes())
             tid = :otel_ets_insert.new_ets_tab(:ets_size_test, input_opts)
             :otel_ets_insert.gen_and_insert_spans(tid, 100_000)
             tid
           end,
           after_scenario: fn tid -> :ets.delete(tid) end},
        "ets_first_empty_tab" =>
          {fn tid -> :ets.first(tid) != :"$end_of_table" end,
           before_scenario: fn input_opts ->
             :lists.foreach(fn p -> :erlang.garbage_collect(p) end, :erlang.processes())
             :otel_ets_insert.new_ets_tab(:ets_size_test, input_opts)
           end,
           after_scenario: fn tid -> :ets.delete(tid) end}
      },
      inputs: %{
        "dup bag write_concurrency decentralized" => [
          :duplicate_bag,
          write_concurrency: true,
          decentralized_counters: true
        ],
        "dup bag write_concurrency" => [
          :duplicate_bag,
          write_concurrency: true,
          decentralized_counters: false
        ],
        "dup bag" => [:duplicate_bag, write_concurrency: false, decentralized_counters: false],
        "set write_concurrency decentralized" => [
          :set,
          write_concurrency: true,
          decentralized_counters: true
        ],
        "set write_concurrency" => [:set, write_concurrency: true, decentralized_counters: false],
        "set" => [:set, write_concurrency: false, decentralized_counters: false]
      },
      time: 10
    )
  end

  def ets_insert do
    :otel_ets_insert.new_ets_tab(:set, [
      :set,
      write_concurrency: false,
      decentralized_counters: false
    ])

    :otel_ets_insert.new_ets_tab(:set_write_conc, [
      :set,
      write_concurrency: true,
      decentralized_counters: false
    ])

    :otel_ets_insert.new_ets_tab(:set_write_conc_decentr, [
      :set,
      write_concurrency: true,
      decentralized_counters: true
    ])

    :otel_ets_insert.new_ets_tab(
      :dupbag,
      [:duplicate_bag, write_concurrency: false, decentralized_counters: false],
      false
    )

    :otel_ets_insert.new_ets_tab(
      :dupbag_write_conc,
      [:duplicate_bag, write_concurrency: true, decentralized_counters: false],
      false
    )

    :otel_ets_insert.new_ets_tab(
      :dupbag_write_conc_decentr,
      [:duplicate_bag, write_concurrency: true, decentralized_counters: true],
      false
    )

    :otel_ets_insert.new_ets_tab(:dupbag_uniq_key, [
      :duplicate_bag,
      write_concurrency: false,
      decentralized_counters: false
    ])

    :otel_ets_insert.new_ets_tab(:dupbag_write_conc_uniq_key, [
      :duplicate_bag,
      write_concurrency: true,
      decentralized_counters: false
    ])

    :otel_ets_insert.new_ets_tab(:dupbag_write_conc_decentr_uniq_key, [
      :duplicate_bag,
      write_concurrency: true,
      decentralized_counters: true
    ])

    Benchee.run(
      %{
        "set" =>
          {fn span -> :ets.insert(:set, span) end,
           before_each: fn _ -> :otel_ets_insert.generate_span() end,
           after_scenario: fn _ ->
             :ets.delete_all_objects(:set)
           end},
        "set write_concurrency" =>
          {fn span -> :ets.insert(:set_write_conc, span) end,
           before_each: fn _ -> :otel_ets_insert.generate_span() end,
           after_scenario: fn _ ->
             :ets.delete_all_objects(:set_write_conc)
           end},
        "set write_concurrency decentralized_counters" =>
          {fn span -> :ets.insert(:set_write_conc_decentr, span) end,
           before_each: fn _ -> :otel_ets_insert.generate_span() end,
           after_scenario: fn _ ->
             :ets.delete_all_objects(:set_write_conc_decentr)
           end},
        "dup bag" =>
          {fn span -> :ets.insert(:dupbag, span) end,
           before_each: fn _ -> :otel_ets_insert.generate_span() end,
           after_scenario: fn _ ->
             :ets.delete_all_objects(:dupbag)
           end},
        "dup bag write_concurrency" =>
          {fn span -> :ets.insert(:dupbag_write_conc, span) end,
           before_each: fn _ -> :otel_ets_insert.generate_span() end,
           after_scenario: fn _ ->
             :ets.delete_all_objects(:dupbag_write_conc)
           end},
        "dup bag write_concurrency decentralized_counters" =>
          {fn span -> :ets.insert(:dupbag_write_conc_decentr, span) end,
           before_each: fn _ -> :otel_ets_insert.generate_span() end,
           after_scenario: fn _ ->
             :ets.delete_all_objects(:dupbag_write_conc_decentr)
           end},
        "dup bag uniq key" =>
          {fn span -> :ets.insert(:dupbag_uniq_key, span) end,
           before_each: fn _ -> :otel_ets_insert.generate_span() end,
           after_scenario: fn _ ->
             :ets.delete_all_objects(:dupbag_uniq_key)
           end},
        "dup bag write_concurrency uniq key" => {
          fn span -> :ets.insert(:dupbag_write_conc_uniq_key, span) end,
          before_each: fn _ -> :otel_ets_insert.generate_span() end,
          after_scenario: fn _ ->
            :ets.delete_all_objects(:dupbag_write_conc_uniq_key)
          end
        },
        "dup bag write_concurrency decentralized_counters uniq key" => {
          fn span -> :ets.insert(:dupbag_write_conc_decentr_uniq_key, span) end,
          before_each: fn _ -> :otel_ets_insert.generate_span() end,
          after_scenario: fn _ ->
            :ets.delete_all_objects(:dupbag_write_conc_decentr_uniq_key)
          end
        }
      },
      before_scenario: fn _ ->
        :lists.foreach(fn p -> :erlang.garbage_collect(p) end, :erlang.processes())
      end,
      time: 10,
      parallel: 100
    )
  end
end
