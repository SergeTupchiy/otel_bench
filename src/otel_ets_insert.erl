-module(otel_ets_insert).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

%% init bench
-export([new_ets_tab/2,
         new_ets_set/1,
         new_ets_duplicate_bag/1,
         create_spans/1,
         generate_span/0
        ]).

%% run bench
-export([insert_spans/2,
         gen_and_insert_span/1,
         gen_and_insert_spans/2
        ]).

new_ets_set(Name) ->
     ets:new(Name, [public, named_table, set,
                   {write_concurrency,true},
                   {keypos, #span.trace_id}]).

new_ets_duplicate_bag(Name) ->
    ets:new(Name, [public, named_table, duplicate_bag,
                   {write_concurrency,true},
                   {keypos, #span.instrumentation_scope}]).

new_ets_tab(Name, Opts) ->
    Opts1 = case lists:member(set, Opts) of
                true ->
                    [{keypos, #span.trace_id} | Opts];
                false ->
                    [{keypos, #span.instrumentation_scope} | Opts]
            end,
    ets:new(Name, [public | Opts1]).

create_spans(N) ->
    [generate_span() || _ <- lists:seq(1, N)].

gen_and_insert_span(Tab) ->
    ets:insert(Tab, generate_span()).

insert_spans(Tab, Spans) ->
    lists:foreach(fun(Span) -> ets:insert(Tab, Span) end, Spans).

gen_and_insert_spans(Tab, N) ->
    {Pid, Mon} = spawn_monitor(
                   fun() -> lists:foreach(
                              fun(_) -> ets:insert(Tab, generate_span()) end,
                              lists:seq(1, N))
                   end),
    receive {'DOWN', Mon, process, Pid, _} ->
            ok
    end.


generate_span() ->
    #span{trace_id = otel_id_generator:generate_trace_id(),
          span_id = otel_id_generator:generate_span_id(),
          name = "test_span",
          attributes = otel_attributes:new(undefined, 10, 10),
          events = otel_events:new(10,10,10),
          links = otel_links:new([], 10, 10, 10),
          trace_flags = 1,
          is_recording = true,
          start_time = erlang:monotonic_time(),
          end_time = erlang:monotonic_time(),
          instrumentation_scope = #instrumentation_scope{name = "test"}}.
