-module(oas_server_custom_validator).
-export([validate_param/4]).
-export([validate_schema/3]).

-type param_rule()   :: oas_server_param_validator:param_rule().
-type schema_rule()  :: oas_server_schema_validator:schema_rule().
-type value()        :: oas_server:value().

-type validation_opts() :: oas_server_validation:validation_opts().
-type param_context()      :: oas_server_param_validator:context().
-type schema_context()     :: oas_server_schema_validator:context().

-type validate_param_result() ::
    ok | {ok, term()} | pass | error | {error, Error :: term()}.

-type validate_schema_result() ::
    jesse_state:state() | pass | no_return().

%% BEHAVIOUR

-callback validate_param(param_rule(), value(), param_context()) ->
    validate_param_result().

-callback validate_schema(schema_rule(), value(), schema_context(), jesse_state:state()) ->
    validate_schema_result().

%% API

-spec validate_param(param_rule(), value(), param_context(), validation_opts()) ->
    validate_param_result().
validate_param(Rule, Value, Meta, ValidationOpts) ->
    case get_validatior(ValidationOpts) of
        undefined -> pass;
        Module -> Module:validate_param(Rule, Value, Meta)
    end.

-spec validate_schema(schema_rule(), value(), jesse_state:state()) ->
    validate_schema_result().
validate_schema(Rule, Value, JesseState) ->
    Meta = get_schema_context(JesseState),
    ValidationOpts = get_schema_opts(JesseState),
    case get_validatior(ValidationOpts) of
        undefined -> pass;
        Module -> Module:validate_schema(Rule, Value, Meta, JesseState)
    end.

%%

get_schema_context(JesseState) ->
    #{validation_meta := Meta} = jesse_state:get_validator_state(JesseState),
    CurrentPath = lists:reverse(jesse_state:get_current_path(JesseState)),
    maps:merge(
        #{current_path => CurrentPath},
        Meta
    ).

get_schema_opts(JesseState) ->
    maps:with([custom_validator], jesse_state:get_validator_state(JesseState)).

get_validatior(ValidationOpts) ->
    maps:get(custom_validator, ValidationOpts, undefined).
