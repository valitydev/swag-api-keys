-module(oas_server_logic_handler).

-export([handle_request/4]).
-export([map_error/2]).

-type operation_id()    :: oas_server:operation_id().
-type api_key()         :: oas_server:api_key().
-type auth_context()    :: oas_server:auth_context().
-type object()          :: oas_server:object().
-type request_context() :: oas_server:request_context().
-type handler_opts(T)   :: oas_server:handler_opts(T).
-type logic_handler(T)  :: oas_server:logic_handler(T).
-type response()        :: oas_server:response().

-type validation_error() :: oas_server_validation:error().
-type error_type()      :: validation_error.
-type error_message()   :: oas_server:error_reason().

-export_type([error_type/0]).

%% Behaviour definition



-callback handle_request(operation_id(), object(), request_context(), handler_opts(_)) ->
    {ok | error, response()}.

-callback map_error(error_type(), validation_error()) ->
    error_message().

%% API

-spec handle_request(logic_handler(_), operation_id(), object(), request_context()) ->
    {ok | error, response()}.

handle_request(Handler, OperationID, Request, Context) ->
    {Module, Opts} = get_mod_opts(Handler),
    Module:handle_request(OperationID, Request, Context, Opts).

-spec map_error(module(), {error_type(), validation_error()}) ->
    error_message().

map_error(Handler, {Type, Error}) ->
    {Module, _Opts} = get_mod_opts(Handler),
    Module:map_error(Type, Error).


%% Internal functions

get_mod_opts(ModOpts= {_, _}) ->
    ModOpts;
get_mod_opts(Module) ->
    {Module, undefined}.
