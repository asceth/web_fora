%%%-------------------------------------------------------------------
%%% File    : web_fora.erl
%%% Author  : asceth <machinist@asceth.com>
%%% Description : Standalone OTP application to serve forum requests.
%%%
%%% Created : 27 Sep 2008 by asceth <machinist@asceth.com>
%%%-------------------------------------------------------------------
-module(web_fora).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% External Hooks
-export([request/1]).

%% Logging
-include("logger.hrl").

-record(state, {web_exchange, prefix}).

-define(SERVER, ?MODULE).
-define(SUB_APPS, [<<"categories">>, <<"forums">>, <<"topics">>, <<"posts">>, <<"polls">>]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Options) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).


%%====================================================================
%% External API
%%====================================================================
%%--------------------------------------------------------------------
request(Session) ->
  PathTokens = web_session:flash_lookup(Session, "path_tokens"),
  do_request(lists:reverse(PathTokens), Session).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Options) ->
  WebExchange = proplists:get_value(web_exchange, Options),
  Prefix = proplists:get_value(prefix, Options),
  web_router:load_bindings(WebExchange, routes(Prefix)),
  {ok, #state{web_exchange=WebExchange, prefix=Prefix}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |case Req:get_cookie_value("_session_id") of
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%%% Categories
%% index, create
do_request(["categories"|_Rest], Session) ->
  case web_session:flash_lookup(Session, "method") of
    get ->
      web_fora_categories:index(Session);
    post ->
      web_fora_categories:create(Session)
  end;
%% new
do_request(["new", "categories"|_Rest], Session) ->
  web_fora_categories:new(Session);
%% show, update, destroy
do_request([CategoryId, "categories"|_Rest], Session) ->
  case web_session:flash_lookup(Session, "method") of
    get ->
      web_fora_categories:show(CategoryId, Session);
    put ->
      web_fora_categories:update(CategoryId, Session);
    delete ->
      web_fora_categories:destroy(CategoryId, Session)
  end;
%% edit
do_request(["edit", CategoryId, "categories"|_Rest], Session) ->
  {session, web_fora_categories:edit(CategoryId, Session), view_tokens, ["web_fora", "categories", "edit"]};
do_request(_PathTokens, Session) ->
  Session.

routes() ->
  routes(<<"">>).
routes(Prefix) when is_list(Prefix) ->
  routes(list_to_binary(Prefix));
routes(Prefix) when is_binary(Prefix) ->
  lists:flatten([category_routes(Prefix)]).

category_routes(Prefix) ->
  [{<<"get.request", Prefix/binary, ".categories">>, % index
    fun web_fora_categories:index/1},
   {<<"get.request_view", Prefix/binary, ".categories">>, % index view
    fun web_fora_categories:index_view/1},
   {<<"get.request", Prefix/binary, ".categories.new">>, % new
    fun web_fora_categories:new/1},
   {<<"get.request_view", Prefix/binary, ".categories.new">>, % new view
    fun web_fora_categories:new/1},
   {<<"post.request", Prefix/binary, ".categories">>, % create
    fun web_fora_categories:create/1},
   {<<"get.request", Prefix/binary, ".categories.*">>, % show
    fun web_fora_categories:show/1},
   {<<"get.request_view", Prefix/binary, ".categories.*">>, % show view
    fun web_fora_categories:show_view/1},
   {<<"get.request", Prefix/binary, ".categories.*.edit">>, % edit
    fun web_fora_categories:edit/1},
   {<<"get.request_view", Prefix/binary, ".categories.*.edit">>, % edit view
    fun web_fora_categories:edit_view/1},
   {<<"put.request", Prefix/binary, ".categories.*">>, % update
    fun web_fora_categories:update/1},
   {<<"delete.request", Prefix/binary, ".categories.*">>, % delete
    fun web_fora_categories:delete/1}].

