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
-include("http_bind.hrl").

%%% HTTP Record
-include("ejabberd_http.hrl").


-define(JABBER_DOMAIN, "example.org").

-behaviour(gen_mod).

-export([
    start/2,
    stop/1,
    process/2
]).


%%%----------------------------------------------------------------------
%%% REQUEST HANDLERS
%%%----------------------------------------------------------------------

process([Username,Token], #request{ method = 'POST',
                      %% auth = Auth,
                      %% data = Data,
                      ip = IP}) ->
  ?DEBUG("Incoming Username: ~s", [Username]),
  ?DEBUG("Incoming Token: ~s", [Token]),
  process_request(Username, Token, IP);

process(_Path, _Request) ->
    ?DEBUG("Bad Request: ~p", [_Request]),
    {400, [], {xmlelement, "code", [],
                [{xmlcdata, "400 Bad Request"}]}}.

process_request(Username, Token, IP) ->
  case authorized(Username, Token) of 
    {U, S} ->
      bind(U, S, Token, IP);
    _ ->
      ?DEBUG("Unauthorized Request", [])
  end.

%%%----------------------------------------------------------------------
%%% PROCESSING
%%%----------------------------------------------------------------------

authorized(Username, Token) ->
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
    end.


bind(User, Server, Token, IP) ->
  ?DEBUG("Starting Pre-Bind Process For ~s", [User]),
  Rid = list_to_integer(randoms:get_string()),
  Rid1 = integer_to_list(Rid + 1),
  {xmlelement, "body", Attrs1, _} = process_request("<body rid='"++Rid1++"' xmlns='http://jabber.org/protocol/httpbind' to='"++Server++"' xml:lang='en' wait='60' hold='1' window='5' content='text/xml; charset=utf-8' ver='1.6' xmpp:version='1.0' xmlns:xmpp='urn:xmpp:xbosh'/>", IP),
  {value, {_, Sid}} = lists:keysearch("sid", 1, Attrs1),
  {value, {_, AuthID}} = lists:keysearch("authid", 1, Attrs1),
  Rid2 = integer_to_list(Rid + 2),
  Auth = base64:encode_to_string(AuthID++[0]++User++[0]++Token),
  process_request("<body rid='"++Rid2++"' xmlns='http://jabber.org/protocol/httpbind' sid='"++Sid++"'><auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='PLAIN'>"++Auth++"</auth></body>", IP),
  Rid3 = integer_to_list(Rid + 3),
  process_request("<body rid='"++Rid3++"' xmlns='http://jabber.org/protocol/httpbind' sid='"++Sid++"' to='"++Server++"' xml:lang='en' xmpp:restart='true' xmlns:xmpp='urn:xmpp:xbosh'/>", IP),
  Rid4 = integer_to_list(Rid + 4),
  {_,_,_,[{_,_,_,[{_,_,_,[{_,_,_,[{_,SJID}]}]}]}]} = process_request("<body rid='"++Rid4++"' xmlns='http://jabber.org/protocol/httpbind' sid='"++Sid++"'><iq type='set'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></iq></body>", IP),
  Rid5 = integer_to_list(Rid + 5),
  process_request("<body rid='"++Rid5++"' xmlns='http://jabber.org/protocol/httpbind' sid='"++Sid++"'><iq type='set'><session xmlns='urn:ietf:params:xml:ns:xmpp-session'/></iq></body>", IP),
  binary_to_list(SJID) ++ "\n" ++ Sid ++ "\n" ++ integer_to_list(Rid + 6).

process_request(Request, IP) ->
    {_, _, Response} = ejabberd_http_bind:process_request(Request, IP),
    xml_stream:parse_element(lists:flatten(Response)).



%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok. 

