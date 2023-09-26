-module(capi_woody_event_handler).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("woody/src/woody_defs.hrl").

-behaviour(woody_event_handler).

-export([handle_event/4]).

-spec handle_event(Event, RpcId, Meta, Opts) -> ok when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta :: woody_event_handler:event_meta(),
    Opts :: woody:options().

handle_event(
    Event = ?EV_CALL_SERVICE,
    RpcID = #{span_id := WoodySpanId},
    Meta = #{service := Service, function := Function},
    Opts
) ->
    SpanName = <<"client ", (atom_to_binary(Service))/binary, ":", (atom_to_binary(Function))/binary>>,
    ok = proc_span_start(WoodySpanId, SpanName, #{kind => ?SPAN_KIND_CLIENT}),
    scoper_woody_event_handler:handle_event(Event, RpcID, Meta, Opts);
handle_event(Event = ?EV_SERVICE_RESULT, RpcID = #{span_id := WoodySpanId}, Meta, Opts) ->
    scoper_woody_event_handler:handle_event(Event, RpcID, Meta, Opts),
    proc_span_end(WoodySpanId);
handle_event(
    Event = ?EV_INVOKE_SERVICE_HANDLER,
    RpcID = #{span_id := WoodySpanId},
    Meta = #{service := Service, function := Function},
    Opts
) ->
    SpanName = <<"server ", (atom_to_binary(Service))/binary, ":", (atom_to_binary(Function))/binary>>,
    ok = proc_span_start(WoodySpanId, SpanName, #{kind => ?SPAN_KIND_SERVER}),
    scoper_woody_event_handler:handle_event(Event, RpcID, Meta, Opts);
handle_event(Event = ?EV_SERVICE_HANDLER_RESULT, RpcID = #{span_id := WoodySpanId}, Meta, Opts) ->
    scoper_woody_event_handler:handle_event(Event, RpcID, Meta, Opts),
    proc_span_end(WoodySpanId);
handle_event(Event, RpcID, Meta, Opts) ->
    scoper_woody_event_handler:handle_event(Event, RpcID, Meta, Opts).

%%

proc_span_start(SpanKey, SpanName, Opts) ->
    Tracer = opentelemetry:get_application_tracer(?MODULE),
    SpanCtx = otel_tracer:start_span(Tracer, SpanName, Opts),
    spans_push(SpanKey, SpanCtx).

proc_span_end(SpanKey) ->
    case spans_pop(SpanKey) of
        undefined ->
            ok;
        SpanCtx ->
            _ = otel_span:end_span(SpanCtx, undefined),
            ok
    end.

%% Otel Spans helpers

-define(SPAN_STACK, otel_span_stack).

spans_pop(DiscriminatorKey) ->
    case spans_get() of
        %% Spans stack is empty; its only membery is original span with which it has been initialized
        [{_, RootSpanCtx}] ->
            _ = otel_tracer:set_current_span(RootSpanCtx),
            undefined;
        %% Normal scenario case when spans are being popped in orderly fashion
        [{Key, SpanCtx} | Stack] when DiscriminatorKey =:= Key orelse DiscriminatorKey =:= undefined ->
            do_pop(SpanCtx, Stack);
        %% Somehow in-process spans are in misorder;
        %% try to fix via popping other spans if given key exists in stack
        Stack ->
            case find_and_split(DiscriminatorKey, [], Stack) of
                {error, notfound} ->
                    %% Do nothing
                    undefined;
                {ok, SpanCtx, SpanCtxsToEnd, FixedStack} ->
                    %% End wrong spans
                    _EndedSpans = [otel_span:end_span(SC, undefined) || {_, SC} <- SpanCtxsToEnd],
                    do_pop(SpanCtx, FixedStack)
            end
    end.

find_and_split(_Key, _WrongSpanCtxs, []) ->
    {error, notfound};
find_and_split(Key, WrongSpanCtxs, [{Key, SpanCtx} | FixedStack]) ->
    {ok, SpanCtx, lists:reverse(WrongSpanCtxs), FixedStack};
find_and_split(Key, WrongSpanCtxs, [{_OtherKey, SpanCtx} | Stack]) ->
    find_and_split(Key, [SpanCtx | WrongSpanCtxs], Stack).

do_pop(SpanCtx, Stack) ->
    {_, ParentSpanCtx} = hd(Stack),
    _ = otel_tracer:set_current_span(ParentSpanCtx),
    ok = spans_put(Stack),
    SpanCtx.

spans_push(DiscriminatorKey, SpanCtx) ->
    ok = spans_put([{DiscriminatorKey, SpanCtx} | spans_get()]),
    _ = otel_tracer:set_current_span(SpanCtx),
    ok.

spans_get() ->
    genlib:define(erlang:get(?SPAN_STACK), [{undefined, otel_tracer:current_span_ctx()}]).

spans_put(Stack) ->
    _ = erlang:put(?SPAN_STACK, Stack),
    ok.
