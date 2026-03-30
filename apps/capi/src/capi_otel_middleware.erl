-module(capi_otel_middleware).

%% TODO Adopt https://github.com/cogini/opentelemetry_xray
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-behaviour(cowboy_middleware).

-export([execute/2]).

-behaviour(otel_propagator_text_map).

-export([fields/1]).
-export([inject/4]).
-export([extract/5]).

-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req} when Req :: cowboy_req:req(), Env :: cowboy_middleware:env().
execute(#{headers := Headers} = Req, Env) ->
    Propagator =
        {otel_propagator_text_map_composite, [
            %% First try getting OTEL context from x-ray trace header
            ?MODULE,
            %% But if trace context headers are present, then get current span context from there
            otel_propagator:builtin_to_module(tracecontext)
        ]},
    OtelCtx = otel_propagator_text_map:extract_to(otel_ctx:new(), Propagator, maps:to_list(Headers)),
    %% Implicitly puts OTEL context into process dictionary.
    %% Since cowboy does not reuse process for other requests, we don't care
    %% about cleaning it up.
    _Token = otel_ctx:attach(OtelCtx),
    {ok, Req, Env}.

-define(HEADER_KEY, <<"x-amzn-trace-id">>).

-spec fields(otel_propagator_text_map:propagator_options()) -> [unicode:latin1_binary()].
fields(_) ->
    [?HEADER_KEY].

-spec inject(
    otel_ctx:t(),
    otel_propagator:carrier(),
    otel_propagator_text_map:carrier_set(),
    otel_propagator_text_map:propagator_options()
) -> no_return().
inject(_Ctx, _Carrier, _CarrierSet, _Options) ->
    erlang:error(not_implemented).

-spec extract(
    otel_ctx:t(),
    otel_propagator:carrier(),
    otel_propagator_text_map:carrier_keys(),
    otel_propagator_text_map:carrier_get(),
    otel_propagator_text_map:propagator_options()
) -> otel_ctx:t().
extract(Ctx, Carrier, _CarrierKeysFun, CarrierGet, _Options) ->
    case CarrierGet(?HEADER_KEY, Carrier) of
        undefined ->
            Ctx;
        XRayTrace ->
            SpanCtx0 = otel_tracer:from_remote_span(0, 0, 0),
            SpanCtx1 = lists:foldl(fun decode/2, SpanCtx0, string:split(string:trim(XRayTrace), ";", all)),
            case SpanCtx1 of
                undefined ->
                    Ctx;
                _ ->
                    otel_tracer:set_current_span(Ctx, SpanCtx1)
            end
    end.

%%

decode(<<"Root=1-", Timestamp:8/binary, "-", RootId:24/binary>>, SpanCtx) ->
    TraceId = binary_to_integer(<<Timestamp/binary, RootId/binary>>, 16),
    SpanCtx#span_ctx{trace_id = TraceId, hex_trace_id = otel_utils:encode_hex(<<TraceId:128>>)};
decode(<<"Parent=", ParentId:16/binary>>, SpanCtx) ->
    SpanId = binary_to_integer(ParentId, 16),
    SpanCtx#span_ctx{span_id = SpanId, hex_span_id = otel_utils:encode_hex(<<SpanId:64>>)};
decode(<<"Sampled=0">>, SpanCtx) ->
    SpanCtx#span_ctx{trace_flags = 0};
decode(<<"Sampled=1">>, SpanCtx) ->
    SpanCtx#span_ctx{trace_flags = 1};
decode(_Value, SpanCtx) ->
    SpanCtx.
