%%%-------------------------------------------------------------------
%%% File    : mod_prebind.erl
%%% Purpose : Provide an HTTP interface to prebind BOSH sessions.
%%%
%%%-------------------------------------------------------------------
-module(mod_prebind).

%%% XMPP/Jabber Globals
-include("jlib.hrl").

%%% ejabberd Globals
-include("ejabberd.hrl").

%%% HTTP Record
-include("web/ejabberd_http.hrl").

-define(JABBER_DOMAIN, "example.org").

-export([
  process/2
]).

process([], #request{ method = 'POST',
                      auth = Auth,
                      data = Data,
                      ip = IP}) ->

  ?DEBUG("Incoming data: ~s", [Data]).

process(_Path, _Request) ->
    ?DEBUG("Bad Request: ~p", [_Request]),
    {403, [], "Forbidden"}.