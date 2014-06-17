-module(gol_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {game_id, world, size}).


init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.


websocket_init(_TransportName, Req, _Opts) ->
	{ok, Req, #state{}}.


websocket_handle({text, <<"store">>}, Req, State) ->
    io:format("STORE REQUEST!~n~n"),
    {ok, Sr} = application:get_env(gol, sr),
    R = dets:insert(Sr, {State#state.game_id, State}),
    io:format("DETS: ~p~n", [R]),
    {ok, Req, State};
websocket_handle({text, GameId}, Req, State) ->
    Size = ets:lookup_element(games, GameId, 2),
    {World, HtmlWorld} = gol:create_world(Size),
    erlang:start_timer(100, self(), next_generation),
    {reply, {text, HtmlWorld}, Req, State#state{game_id = GameId, size = Size, world = World}}.


websocket_info({timeout, _Ref, next_generation}, Req, State) ->
    {NextGeneration, HtmlNextGeneration} = gol:create_next_generation(State#state.size, State#state.world),
    erlang:start_timer(100, self(), next_generation),
    {reply, {text, HtmlNextGeneration}, Req, State#state{world = NextGeneration}}.


websocket_terminate(_Reason, _Req, _State) ->
	ok.
