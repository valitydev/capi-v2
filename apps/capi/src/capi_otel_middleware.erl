-module(capi_otel_middleware).

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
            case decode(string:trim(XRayTrace)) of
                undefined ->
                    Ctx;
                SpanCtx ->
                    otel_tracer:set_current_span(Ctx, SpanCtx)
            end
    end.

%%

decode(
    %% NOTE Version is expected to be always single char "1"
    <<"Root=", _Version:1/binary, "-", Timestamp:8/binary, "-", RootId:24/binary, ";Parent=", ParentId:16/binary,
        ";Sampled=", Sampled:1/binary, _/binary>>
) ->
    try
        TraceId = binary_to_integer(<<Timestamp/binary, RootId/binary>>, 16),
        SpanId = binary_to_integer(ParentId, 16),
        TraceFlags =
            case Sampled of
                <<"1">> -> 1;
                <<"0">> -> 0;
                _ -> error(badarg)
            end,
        otel_tracer:from_remote_span(TraceId, SpanId, TraceFlags)
    catch
        %% to integer from base 16 string failed
        error:badarg ->
            undefined
    end;
decode(_) ->
    undefined.
