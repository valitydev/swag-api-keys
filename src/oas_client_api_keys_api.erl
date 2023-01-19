%% -*- mode: erlang -*-
-module(oas_client_api_keys_api).

%% generated methods
-export([get_api_key/2]).
-export([get_api_key/3]).
-export([issue_api_key/2]).
-export([issue_api_key/3]).
-export([list_api_keys/2]).
-export([list_api_keys/3]).
-export([revoke_api_key/2]).
-export([revoke_api_key/3]).
-spec get_api_key(Endpoint :: oas_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_api_key(Endpoint, Params) ->
    get_api_key(Endpoint, Params, []).

-spec get_api_key(Endpoint :: oas_client:endpoint(), Params :: map(), Opts :: oas_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_api_key(Endpoint, Params, Opts) ->
    process_response(oas_client_procession:process_request(
        get,
        oas_client_utils:get_url(Endpoint, "/orgs/:partyId/api-keys/:apiKeyId"),
        Params,
        get_request_spec(get_api_key),
        Opts
    ), get_api_key).
-spec issue_api_key(Endpoint :: oas_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
issue_api_key(Endpoint, Params) ->
    issue_api_key(Endpoint, Params, []).

-spec issue_api_key(Endpoint :: oas_client:endpoint(), Params :: map(), Opts :: oas_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
issue_api_key(Endpoint, Params, Opts) ->
    process_response(oas_client_procession:process_request(
        post,
        oas_client_utils:get_url(Endpoint, "/orgs/:partyId/api-keys"),
        Params,
        get_request_spec(issue_api_key),
        Opts
    ), issue_api_key).
-spec list_api_keys(Endpoint :: oas_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_api_keys(Endpoint, Params) ->
    list_api_keys(Endpoint, Params, []).

-spec list_api_keys(Endpoint :: oas_client:endpoint(), Params :: map(), Opts :: oas_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
list_api_keys(Endpoint, Params, Opts) ->
    process_response(oas_client_procession:process_request(
        get,
        oas_client_utils:get_url(Endpoint, "/orgs/:partyId/api-keys"),
        Params,
        get_request_spec(list_api_keys),
        Opts
    ), list_api_keys).
-spec revoke_api_key(Endpoint :: oas_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
revoke_api_key(Endpoint, Params) ->
    revoke_api_key(Endpoint, Params, []).

-spec revoke_api_key(Endpoint :: oas_client:endpoint(), Params :: map(), Opts :: oas_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
revoke_api_key(Endpoint, Params, Opts) ->
    process_response(oas_client_procession:process_request(
        put,
        oas_client_utils:get_url(Endpoint, "/orgs/:partyId/api-keys/:apiKeyId/status"),
        Params,
        get_request_spec(revoke_api_key),
        Opts
    ), revoke_api_key).
process_response({ok, Code, Headers, RespBody}, OperationID) ->
    try oas_client_procession:process_response(
        get_response_spec(OperationID, Code),
        RespBody
    ) of
        {ok, Resp} ->
            {ok, Code, Headers, Resp};
        Error ->
            Error
    catch
        error:invalid_response_code ->
            {error, {invalid_response_code, Code}}
    end;
process_response(Error, _) ->
    Error.


-spec get_request_spec(OperationID :: oas_client:operation_id()) ->
    Spec :: oas_client_procession:request_spec() | no_return().

get_request_spec('get_api_key') ->
    [
        {'partyId', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40 }, {min_length, 1 }, true, {required, true }]
        }},
        {'apiKeyId', #{
            source => binding,
            rules  => [{type, 'ApiKeyID'}, true, {required, true }]
        }}
    ];
get_request_spec('issue_api_key') ->
    [
        {'partyId', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40 }, {min_length, 1 }, true, {required, true }]
        }},
        {'ApiKey', #{
            source => body,
            rules  => [schema, {required, false }]
        }}
    ];
get_request_spec('list_api_keys') ->
    [
        {'partyId', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40 }, {min_length, 1 }, true, {required, true }]
        }},
        {'status', #{
            source => qs_val,
            rules  => [{type, 'ApiKeyStatus'}, true, {required, false }]
        }}
    ];
get_request_spec('revoke_api_key') ->
    [
        {'partyId', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40 }, {min_length, 1 }, true, {required, true }]
        }},
        {'apiKeyId', #{
            source => binding,
            rules  => [{type, 'ApiKeyID'}, true, {required, true }]
        }},
        {'binary', #{
            source => body,
            rules  => [schema, {required, false }]
        }},
        {'apiKeyRevokeToken', #{
            source => qs_val,
            rules  => [{type, 'RevokeToken'}, true, {required, false }]
        }}
    ].

-spec get_response_spec(OperationID :: oas_client:operation_id(), Code :: oas_client_procession:code()) ->
    Spec :: oas_client_procession:response_spec() | no_return().

get_response_spec('get_api_key', 200) ->
    {'ApiKey', 'ApiKey'};
get_response_spec('get_api_key', 400) ->
    {'inline_response_400', 'inline_response_400'};
get_response_spec('get_api_key', 403) ->
    undefined;
get_response_spec('get_api_key', 404) ->
    undefined;
get_response_spec('issue_api_key', 200) ->
    {'inline_response_200_1', 'inline_response_200_1'};
get_response_spec('issue_api_key', 400) ->
    {'inline_response_400', 'inline_response_400'};
get_response_spec('issue_api_key', 403) ->
    undefined;
get_response_spec('list_api_keys', 200) ->
    {'inline_response_200', 'inline_response_200'};
get_response_spec('list_api_keys', 400) ->
    {'inline_response_400', 'inline_response_400'};
get_response_spec('list_api_keys', 403) ->
    undefined;
get_response_spec('revoke_api_key', 204) ->
    undefined;
get_response_spec('revoke_api_key', 400) ->
    {'inline_response_400', 'inline_response_400'};
get_response_spec('revoke_api_key', 403) ->
    undefined;
get_response_spec('revoke_api_key', 404) ->
    undefined;
get_response_spec(_, _) ->
    error(invalid_response_code).
