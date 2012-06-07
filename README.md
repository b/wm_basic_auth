### wm_basic_auth

Use Webmachine.  Use HTTP Basic authentication over TLS/SSL.  Done.


### Using wm_basic_auth

--add dep to rebar.config

	{deps, [{wm_basic_auth, "*", {git, "git://github.com/b/basic_auth", "HEAD"}}]}.
	

--implement password_fun/3



--call wm_basic_auth:is_authorized/3 from webmachine:is_authorized/2



### Example resource

	-module(basic_auth_resource).
	-export([init/1, to_html/2]).
	-export([is_authorized/2, authorize/3]).

	-include_lib("webmachine/include/webmachine.hrl").

	-record(state, {realm, params}).

	init([]) -> {ok, #state{realm="testrealm@b3k.us"}}.

	is_authorized(ReqData, State=#state{realm=Realm}) ->
	  Response = wm_basic_auth:is_authorized(ReqData, Realm, fun ?MODULE:authorize/3),
	  {Response, ReqData, State}.

	to_html(ReqData, State) ->
	    {"<html><body>Hello, new world</body></html>", ReqData, State}.

	authorize(_Realm, Username, Password) ->
	  	case Password == proplists:get_value(Username,
	    			                         [{"foo", "bar"}, {"baz", "bal"}]) of
			true -> ok;
			false -> error
		end.

Add the example resource to your webmachine app, compile, and start it up.  You can test
authentication easily using curl:

	curl -v --user foo:bar http://localhost:8000/

### Blame

wm_basic_auth is made by Benjamin Black and can be found at https://github.com/b/wm_basic_auth.
