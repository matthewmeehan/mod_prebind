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

%%% HTTP Globals
-include("/root/ejabberd/src/web/http_bind.hrl").

%%% HTTP Record
-include("web/ejabberd_http.hrl").


-define(JABBER_DOMAIN, "example.org").

-export([process/2]).


process([], #request{ method = 'POST',
                      %% auth = Auth,
                      data = Data,
                      ip = IP}) ->
  ?DEBUG("Incoming data: ~s", [Data]),
  process_request(Data, IP);

process(_Path, _Request) ->
    ?DEBUG("Bad Request: ~p", [_Request]),
    {400, ?HEADER, "Bad Request."}.

process_request(Data, IP) ->
  case authorized(Data) of 
    {Username, Server} ->
      bind(Username, Server, IP);
    _ ->
      ?DEBUG("Unauthorized Request ~s", [Data])
  end.
  
authorized(Data) ->
    case Data of 
      {Username, Token} ->
        case jlib:string_to_jid(Username) of
          error ->
              unauthorized;
          #jid{user = U, server = S} ->
              case ejabberd_auth:check_password(U, S, Token) of
                  true ->
                      {U, S};
                  false ->
                      unauthorized
              end
        end;
      _ ->
        unauthorized
    end.

bind(Username, Server, IP) ->
  ?DEBUG("Starting Pre-Bind Process", []).