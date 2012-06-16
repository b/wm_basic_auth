%% @author Benjamin Black <b@b3k.us>
%% @copyright 2012 Benjamin Black.

%% @doc wm_basic_auth - RFC 2617 HTTP Basic auth implementation for Webmachine
%%                      http://tools.ietf.org/html/rfc2617

-module(wm_basic_auth).
-author('Benjamin Black <b@b3k.us>').
-export([is_authorized/3, get_username/1, always_true_auth/3, always_false_auth/3]).

challenge(Realm) -> "Basic realm=" ++ "\"" ++ Realm ++ "\"".

parse_auth_data(ReqData) ->
    case wrq:get_req_header("Authorization", ReqData) of
        undefined ->
            {error, "missing authorization header"};
        "Basic " ++ EncodedParams ->
            parse_params(EncodedParams);
        _ ->
            {error, "malformed authorization header data"}
    end.

parse_params(EncodedParams) ->
    Params = base64:decode_to_string(EncodedParams),
    case string:tokens(Params, ":") of
        [Username, Password] ->
            {ok, {Username, Password}};
        _ ->
            {error, "bad authorization params"}
    end.

is_authorized_response(ok, _Realm) -> true;
is_authorized_response(_, Realm) -> challenge(Realm).

is_authorized(ReqData, Realm, PasswordFun) ->
    case parse_auth_data(ReqData) of
        {ok, {Username, Password}} ->
            is_authorized_response(PasswordFun(Realm, Username, Password), Realm);
        {error, _} ->
            challenge(Realm)
    end.

get_username(ReqData) ->
    case parse_auth_data(ReqData) of
        {ok, {Username, _}} ->
            Username;
        {error, Reason} -> {error, Reason}
    end.

always_true_auth(_, _, _) -> ok.

always_false_auth(_, _, _) -> {error, "Always false"}.
