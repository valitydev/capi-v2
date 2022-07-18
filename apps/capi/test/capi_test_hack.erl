-module(capi_test_hack).

-include_lib("damsel/include/dmsl_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-include_lib("capi_dummy_data.hrl").

-export([get_invoice_mock/0]).

-spec get_invoice_mock() -> _.
get_invoice_mock() ->
    {invoicing, fun('Get', _) -> {ok, ?PAYPROC_INVOICE} end}.
