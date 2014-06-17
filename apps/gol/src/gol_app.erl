-module(gol_app).

-behaviour(application).

-export([start/0]).
-export([start/2]).
-export([stop/1]).


start() ->
    application:ensure_all_started(gol).


start(_StartType, _StartArgs) ->
    Port = 9090,
    Routes = [
              {"/", gol_main_handler, []},
              {"/game", gol_game_handler, []},
              {"/ws", gol_ws_handler, []},
              {"/static/[...]", cowboy_static, {priv_dir, gol, "static"}}
             ],

    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
    ets:new(games, [set, public, named_table]),
    io:format("Look http://localhost:~p~n", [Port]),
    gol_sup:start_link().


stop(_State) ->
    ok.
