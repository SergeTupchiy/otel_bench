-module(otel_ets_traverse).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").


%% init bench
-export([new_ets/1,
         insert_spans/2
        ]).

%% run bench
-export([export_delete_table/2,
         export_take_from_table/2,
         export_fix_take_from_table/2
        ]).

new_ets(Name) ->
    ets:new(Name, [public, named_table, duplicate_bag,
                   {write_concurrency,true},
                   {keypos, #span.instrumentation_scope}]).

export_delete_table(Tab, Resource) ->
    {Pid, Mon} = spawn_monitor(fun() ->
                                       _ = to_proto(Tab, Resource),
                                       _ = ets:delete(Tab)
                               end),
    receive {'DOWN', Mon, process, Pid, _} ->
            ok
    end.

export_take_from_table(Tab, Resource) ->
    {Pid, Mon} = spawn_monitor(fun() -> _ = to_proto_take(Tab, Resource) end),
    receive {'DOWN', Mon, process, Pid, _} ->
            ok
    end.

export_fix_take_from_table(Tab, Resource) ->
    {Pid, Mon} = spawn_monitor(fun() -> _ = to_proto_fix_take(Tab, Resource) end),
    receive {'DOWN', Mon, process, Pid, _} ->
            ok
    end.

insert_spans(T, N) ->
    lists:foreach(fun(_) -> ets:insert(T, generate_span())  end, lists:seq(1,N)).

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

%% NOTE: all of the below code is copied from opentelemetry-erlang/apps/opentelemetry_exporter/src/otel_otlp_traces.erl

%% curent implementation: traverse and not-safe table delete

to_proto(Tab, Resource) ->
    case to_proto_by_instrumentation_scope(Tab) of
        [] ->
            empty;
        InstrumentationScopeSpans ->
            Attributes = otel_resource:attributes(Resource),
            ResourceSpans = #{resource => #{attributes => otel_otlp_common:to_attributes(Attributes),
                                            dropped_attributes_count => otel_attributes:dropped(Attributes)},
                              scope_spans => InstrumentationScopeSpans},
            case otel_resource:schema_url(Resource) of
                undefined ->
                    #{resource_spans => [ResourceSpans]};
                SchemaUrl ->
                    #{resource_spans => [ResourceSpans#{schema_url => SchemaUrl}]}
            end
    end.

to_proto_by_instrumentation_scope(Tab) ->
    Key = ets:first(Tab),
    to_proto_by_instrumentation_scope(Tab, Key).

to_proto_by_instrumentation_scope(_Tab, '$end_of_table') ->
    [];
to_proto_by_instrumentation_scope(Tab, InstrumentationScope) ->
    InstrumentationScopeSpans = lists:foldl(fun(Span, Acc) ->
                                                      [to_proto(Span) | Acc]
                                              end, [], ets:lookup(Tab, InstrumentationScope)),
    InstrumentationScopeSpansProto = otel_otlp_common:to_instrumentation_scope_proto(InstrumentationScope),
    [InstrumentationScopeSpansProto#{spans => InstrumentationScopeSpans}
    | to_proto_by_instrumentation_scope(Tab, ets:next(Tab, InstrumentationScope))].

%% export by taking the first record from the table without safe_fixtable

to_proto_take(Tab, Resource) ->
    case to_proto_by_instrumentation_scope_take(Tab) of
        [] ->
            empty;
        InstrumentationScopeSpans ->
            Attributes = otel_resource:attributes(Resource),
            ResourceSpans = #{resource => #{attributes => otel_otlp_common:to_attributes(Attributes),
                                            dropped_attributes_count => otel_attributes:dropped(Attributes)},
                              scope_spans => InstrumentationScopeSpans},
            case otel_resource:schema_url(Resource) of
                undefined ->
                    #{resource_spans => [ResourceSpans]};
                SchemaUrl ->
                    #{resource_spans => [ResourceSpans#{schema_url => SchemaUrl}]}
            end
    end.

to_proto_by_instrumentation_scope_take(Tab) ->
    Key = ets:first(Tab),
    to_proto_by_instrumentation_scope_take(Tab, Key).

to_proto_by_instrumentation_scope_take(_Tab, '$end_of_table') ->
    [];
to_proto_by_instrumentation_scope_take(Tab, InstrumentationScope) ->
    InstrumentationScopeSpans = lists:foldl(fun(Span, Acc) ->
                                                      [to_proto(Span) | Acc]
                                              end, [], ets:take(Tab, InstrumentationScope)),
    InstrumentationScopeSpansProto = otel_otlp_common:to_instrumentation_scope_proto(InstrumentationScope),
    [InstrumentationScopeSpansProto#{spans => InstrumentationScopeSpans}
     %% ets:first/1 because next can't be used after removing the previous key
    | to_proto_by_instrumentation_scope_take(Tab, ets:first(Tab))].

%% export by fixing and traversing the table with ets:take/2

to_proto_fix_take(Tab, Resource) ->
    case to_proto_by_instrumentation_scope_fix_take(Tab) of
        [] ->
            empty;
        InstrumentationScopeSpans ->
            Attributes = otel_resource:attributes(Resource),
            ResourceSpans = #{resource => #{attributes => otel_otlp_common:to_attributes(Attributes),
                                            dropped_attributes_count => otel_attributes:dropped(Attributes)},
                              scope_spans => InstrumentationScopeSpans},
            case otel_resource:schema_url(Resource) of
                undefined ->
                    #{resource_spans => [ResourceSpans]};
                SchemaUrl ->
                    #{resource_spans => [ResourceSpans#{schema_url => SchemaUrl}]}
            end
    end.

to_proto_by_instrumentation_scope_fix_take(Tab) ->
    Key = ets:first(Tab),
    to_proto_by_instrumentation_scope_fix_take(Tab, Key).

to_proto_by_instrumentation_scope_fix_take(_Tab, '$end_of_table') ->
    [];
to_proto_by_instrumentation_scope_fix_take(Tab, InstrumentationScope) ->
    true = ets:safe_fixtable(Tab, true),
    try
        InstrumentationScopeSpans = lists:foldl(fun(Span, Acc) ->
                                                        [to_proto(Span) | Acc]
                                                end, [], ets:take(Tab, InstrumentationScope)),
        InstrumentationScopeSpansProto = otel_otlp_common:to_instrumentation_scope_proto(InstrumentationScope),
        [InstrumentationScopeSpansProto#{spans => InstrumentationScopeSpans}
        | to_proto_by_instrumentation_scope_fix_take(Tab, ets:next(Tab, InstrumentationScope))]
    after
        true = ets:safe_fixtable(Tab, false)
    end.


to_proto(#span{trace_id=TraceId,
               span_id=SpanId,
               tracestate=TraceState,
               parent_span_id=MaybeParentSpanId,
               name=Name,
               kind=Kind,
               start_time=StartTime,
               end_time=EndTime,
               attributes=Attributes,
               events=TimedEvents,
               links=Links,
               status=Status,
               trace_flags=_TraceFlags,
               is_recording=_IsRecording}) when is_integer(TraceId),
                                                is_integer(SpanId) ->
    ParentSpanId = case MaybeParentSpanId of _ when is_integer(MaybeParentSpanId) -> <<MaybeParentSpanId:64>>; _ -> <<>> end,
    #{name                     => otel_otlp_common:to_binary(Name),
      trace_id                 => <<TraceId:128>>,
      span_id                  => <<SpanId:64>>,
      parent_span_id           => ParentSpanId,
      %% eqwalizer:ignore have to have tracestate as type '_' for matchspecs
      trace_state              => otel_tracestate:encode_header(TraceState),
      kind                     => to_otlp_kind(Kind),
      %% eqwalizer:ignore have to allow value '$2' for matchspecs
      start_time_unix_nano     => opentelemetry:timestamp_to_nano(StartTime),
      %% eqwalizer:ignore have to allow value '_' for matchspecs
      end_time_unix_nano       => opentelemetry:timestamp_to_nano(EndTime),
      %% eqwalizer:ignore have to allow value '_' for matchspecs
      attributes               => otel_otlp_common:to_attributes(Attributes),
      dropped_attributes_count => otel_attributes:dropped(Attributes),
      events                   => to_events(otel_events:list(TimedEvents)),
      dropped_events_count     => otel_events:dropped(TimedEvents),
      links                    => to_links(otel_links:list(Links)),
      dropped_links_count      => otel_links:dropped(Links),
      %% eqwalizer:ignore have to allow value '_' for matchspecs
      status                   => to_status(Status)}.

-spec to_status(opentelemetry:status() | undefined) -> opentelemetry_exporter_trace_service_pb:status().
to_status(#status{code=Code,
                  message=Message}) ->
    #{code => to_otlp_status(Code),
      message => Message};
to_status(_) ->
    #{}.

-spec to_events([#event{}]) -> [opentelemetry_exporter_trace_service_pb:event()].
to_events(Events) ->
    [#{time_unix_nano => opentelemetry:timestamp_to_nano(Timestamp),
       name => otel_otlp_common:to_binary(Name),
       attributes => otel_otlp_common:to_attributes(Attributes)}
     || #event{system_time_native=Timestamp,
               name=Name,
               attributes=Attributes} <- Events].

-spec to_links([#link{}]) -> [opentelemetry_exporter_trace_service_pb:link()].
to_links(Links) ->
    [#{trace_id => <<TraceId:128>>,
       span_id => <<SpanId:64>>,
       trace_state => otel_tracestate:encode_header(TraceState),
       attributes => otel_otlp_common:to_attributes(Attributes),
       dropped_attributes_count => 0} || #link{trace_id=TraceId,
                                               span_id=SpanId,
                                               attributes=Attributes,
                                               tracestate=TraceState} <- Links].

-spec to_otlp_kind(atom()) -> opentelemetry_exporter_trace_service_pb:'span.SpanKind'().
to_otlp_kind(?SPAN_KIND_INTERNAL) ->
    'SPAN_KIND_INTERNAL';
to_otlp_kind(?SPAN_KIND_SERVER) ->
    'SPAN_KIND_SERVER';
to_otlp_kind(?SPAN_KIND_CLIENT) ->
    'SPAN_KIND_CLIENT';
to_otlp_kind(?SPAN_KIND_PRODUCER) ->
    'SPAN_KIND_PRODUCER';
to_otlp_kind(?SPAN_KIND_CONSUMER) ->
    'SPAN_KIND_CONSUMER';
to_otlp_kind(_) ->
    'SPAN_KIND_UNSPECIFIED'.

-spec to_otlp_status(atom()) -> opentelemetry_exporter_trace_service_pb:'status.StatusCode'().
to_otlp_status(?OTEL_STATUS_UNSET) ->
    'STATUS_CODE_UNSET';
to_otlp_status(?OTEL_STATUS_OK) ->
    'STATUS_CODE_OK';
to_otlp_status(?OTEL_STATUS_ERROR) ->
    'STATUS_CODE_ERROR'.
