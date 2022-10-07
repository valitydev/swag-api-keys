%% -*- mode: erlang -*-

%% basic handler
-module(oas_server_api_keys_handler).

%% Cowboy REST callbacks
-export([allowed_methods/2]).
-export([init/2]).
-export([allow_missing_post/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([delete_resource/2]).
-export([is_authorized/2]).
-export([known_content_type/2]).
-export([malformed_request/2]).
-export([valid_content_headers/2]).
-export([valid_entity_length/2]).

%% Handlers
-export([handle_request_json/2]).

-record(state, {
    operation_id  :: oas_server:operation_id(),
    logic_handler :: module(),
    swagger_handler_opts :: oas_server_router:swagger_handler_opts(),
    context       :: oas_server:request_context()
}).

-type state()              :: state().
-type content_type()       :: {binary(), binary(), '*' | [{binary(), binary()}]}.
-type processed_response() :: {stop, cowboy_req:req(), state()}.

%% Cowboy REST callbacks

-spec init(Req :: cowboy_req:req(), Opts :: oas_server_router:init_opts()) ->
    {cowboy_rest, Req :: cowboy_req:req(), State :: state()}.

init(Req, {_Operations, LogicHandler, SwaggerHandlerOpts} = InitOpts) ->
    OperationID    = oas_server_utils:get_operation_id(Req, InitOpts),

    error_logger:info_msg("Attempt to process operation: ~p", [OperationID]),

    State = #state{
        operation_id  = OperationID,
        logic_handler = LogicHandler,
        swagger_handler_opts = SwaggerHandlerOpts,
        context       = #{cowboy_req => Req}
    },
    {cowboy_rest, Req, State}.

-spec allowed_methods(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: [binary()], Req :: cowboy_req:req(), State :: state()}.

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetApiKey'
    }
) ->
    {[<<"GET">>], Req, State};
allowed_methods(
    Req,
    State = #state{
        operation_id = 'IssueApiKey'
    }
) ->
    {[<<"POST">>], Req, State};
allowed_methods(
    Req,
    State = #state{
        operation_id = 'ListApiKeys'
    }
) ->
    {[<<"GET">>], Req, State};
allowed_methods(
    Req,
    State = #state{
        operation_id = 'RevokeApiKey'
    }
) ->
    {[<<"PUT">>], Req, State};
allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: true | {false, AuthHeader :: iodata()},
        Req   :: cowboy_req:req(),
        State :: state()
    }.
is_authorized(
    Req0,
    State = #state{
        operation_id  = 'GetApiKey' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
is_authorized(
    Req0,
    State = #state{
        operation_id  = 'IssueApiKey' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
is_authorized(
    Req0,
    State = #state{
        operation_id  = 'ListApiKeys' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
is_authorized(
    Req0,
    State = #state{
        operation_id  = 'RevokeApiKey' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
is_authorized(Req, State) ->
    { {false, <<"">>}, Req, State}.

-spec content_types_accepted(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: [{content_type(), AcceptResource :: atom()}],
        Req   :: cowboy_req:req(),
        State :: state()
    }.

content_types_accepted(Req, State) ->
    {
        [{
            {<<"application">>, <<"json">>, [{<<"charset">>, <<"utf-8">>}]},
            handle_request_json
        }],
        Req,
        State
    }.

-spec valid_content_headers(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: boolean(), Req :: cowboy_req:req(), State :: state()}.
valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetApiKey'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};
valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'IssueApiKey'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};
valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'ListApiKeys'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};
valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'RevokeApiKey'
    }
) ->
    Headers = [],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};
valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: [{content_type(), ProvideResource :: atom()}],
        Req   :: cowboy_req:req(),
        State :: state()
    }.

content_types_provided(Req, State) ->
    {
        [{
            {<<"application">>, <<"json">>, '*'},
            handle_request_json
        }],
        Req,
        State
    }.

-spec charsets_provided(Req :: cowboy_req:req(), State :: state()) ->
    {Charsets :: [binary()], Req :: cowboy_req:req(), State :: state()}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

-spec malformed_request(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: boolean(), Req :: cowboy_req:req(), State :: state()}.

malformed_request(Req, State = #state{context = Context}) ->
    PeerResult = oas_server_handler_api:determine_peer(Req),
    case PeerResult of
        {ok, Peer} ->
            Context1 = Context#{peer => Peer},
            State1   = State#state{context = Context1},
            {false, Req, State1};
        {error, Reason} ->
            error_logger:error_msg("Unable to determine client peer: ~p", [Reason]),
            {true, Req, State}
    end.

-spec allow_missing_post(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: false, Req :: cowboy_req:req(), State :: state()}.

allow_missing_post(Req, State) ->
    {false, Req, State}.

-spec delete_resource(Req :: cowboy_req:req(), State :: state()) ->
    processed_response().

delete_resource(Req, State) ->
    handle_request_json(Req, State).

-spec known_content_type(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: true, Req :: cowboy_req:req(), State :: state()}.

known_content_type(Req, State) ->
    {true, Req, State}.

-spec valid_entity_length(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: true, Req :: cowboy_req:req(), State :: state()}.

valid_entity_length(Req, State) ->
    %% @TODO check the length
    {true, Req, State}.


%% Handlers

-spec handle_request_json(Req :: cowboy_req:req(), State :: state()) ->
    processed_response().

handle_request_json(
    Req0,
    State = #state{
        operation_id  = OperationID,
        logic_handler = LogicHandler,
        swagger_handler_opts = SwaggerHandlerOpts,
        context       = Context
    }
) ->
    ValidationOpts = maps:get(validation_opts, SwaggerHandlerOpts, #{}),
    case populate_request(LogicHandler, OperationID, Req0, ValidationOpts) of
        {ok, Populated, Req1} ->
            {Status, Resp} = handle_request(LogicHandler, OperationID, Populated, Context),
            ok = validate_response(Status, Resp, OperationID, ValidationOpts),
            process_response(ok, encode_response(Resp), Req1, State);
        {error, Reason, Req1} ->
            process_response(error, Reason, Req1, State)
    end.


%% Internal

populate_request(LogicHandler, OperationID, Req, ValidationOpts) ->
    Spec = get_request_spec(OperationID),
    oas_server_handler_api:populate_request(LogicHandler, OperationID, Spec, Req, ValidationOpts).

handle_request(LogicHandler, OperationID, Populated, Context) ->
    oas_server_logic_handler:handle_request(LogicHandler, OperationID, Populated, Context).

validate_response(error, _, _, _) ->
    ok;
validate_response(ok, {Code, _Headers, Body}, OperationID, ValidationOpts) ->
    Spec = get_response_spec(OperationID, Code),
    oas_server_handler_api:validate_response(OperationID, Spec, Body, ValidationOpts).

encode_response(Resp) ->
    oas_server_handler_api:encode_response(Resp).

process_response(Status, Result, Req0, State = #state{operation_id = OperationID}) ->
    Req = oas_server_handler_api:process_response(Status, Result, Req0, OperationID),
    {stop, Req, State}.

validate_headers(_, Req) ->
    {true, Req}.

-spec get_request_spec(OperationID :: oas_server:operation_id()) ->
    Spec :: oas_server_handler_api:request_spec() | no_return().

get_request_spec('GetApiKey') ->
    [
        {'orgId', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40 }, {min_length, 1 }, true, {required, true }]
        }},
{'apiKeyId', #{
            source => binding,
            rules  => [{type, 'ApiKeyID'}, true, {required, true }]
        }}
    ];
get_request_spec('IssueApiKey') ->
    [
        {'orgId', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40 }, {min_length, 1 }, true, {required, true }]
        }},
{'ApiKey', #{
            source => body,
            rules  => [schema, {required, false }]
        }}
    ];
get_request_spec('ListApiKeys') ->
    [
        {'orgId', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40 }, {min_length, 1 }, true, {required, true }]
        }},
{'status', #{
            source => qs_val,
            rules  => [{type, 'ApiKeyStatus'}, true, {required, false }]
        }}
    ];
get_request_spec('RevokeApiKey') ->
    [
        {'orgId', #{
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
        }}
    ].

-spec get_response_spec(OperationID :: oas_server:operation_id(), Code :: cowboy:http_status()) ->
    Spec :: oas_server_handler_api:response_spec() | no_return().

get_response_spec('GetApiKey', 200) ->
    {'ApiKey', 'ApiKey'};
get_response_spec('GetApiKey', 400) ->
    {'inline_response_400', 'inline_response_400'};
get_response_spec('GetApiKey', 403) ->
    undefined;
get_response_spec('GetApiKey', 404) ->
    undefined;
get_response_spec('IssueApiKey', 200) ->
    {'inline_response_200_1', 'inline_response_200_1'};
get_response_spec('IssueApiKey', 400) ->
    {'inline_response_400', 'inline_response_400'};
get_response_spec('IssueApiKey', 403) ->
    undefined;
get_response_spec('IssueApiKey', 404) ->
    undefined;
get_response_spec('ListApiKeys', 200) ->
    {'inline_response_200', 'inline_response_200'};
get_response_spec('ListApiKeys', 400) ->
    {'inline_response_400', 'inline_response_400'};
get_response_spec('ListApiKeys', 403) ->
    undefined;
get_response_spec('ListApiKeys', 404) ->
    undefined;
get_response_spec('RevokeApiKey', 204) ->
    undefined;
get_response_spec('RevokeApiKey', 400) ->
    {'inline_response_400', 'inline_response_400'};
get_response_spec('RevokeApiKey', 403) ->
    undefined;
get_response_spec('RevokeApiKey', 404) ->
    undefined;
get_response_spec(OperationID, Code) ->
    error({invalid_response_code, OperationID, Code}).
