%% @author Benjamin Black <b@b3k.us>
%% @copyright 2012 Benjamin Black.

%% @doc wm_basic_auth - RFC 2617 HTTP Basic auth implementation for Webmachine
%%                      http://tools.ietf.org/html/rfc2617

-module(wm_basic_auth).
-author('Benjamin Black <b@b3k.us>').
-export([is_authorized/3, always_true_auth/3, always_false_auth/3]).

challenge(Realm) -> "Basic realm=" ++ "\"" ++ Realm ++ "\"".

parse_params(EncodedParams) ->
	Params = base64:decode_to_string(EncodedParams),
	string:tokens(Params, ":").

is_authorized_response(ok, _Realm) -> true;
is_authorized_response(_, Realm) -> challenge(Realm).

is_authorized(ReqData, Realm, PasswordFun) ->
	case wrq:get_req_header("Authorization", ReqData) of
		undefined -> challenge(Realm);
		"Basic " ++ EncodedParams ->
			case parse_params(EncodedParams) of
				[Username, Password] ->
					is_authorized_response(PasswordFun(Realm, Username, Password), Realm);
				_ -> challenge(Realm)
			end;
		_ -> challenge(Realm)
	end.

always_true_auth(_, _, _) -> true.

always_false_auth(_, _, _) -> false.