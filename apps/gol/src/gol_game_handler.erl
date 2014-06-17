-module(gol_game_handler).

-export([init/3]).
-export([rest_init/2]).
-export([content_types_provided/2]).
-export([allowed_methods/2]).
-export([game_page/2]).
-export([resource_exists/2]).

-record(rs_state, {game_id}).


init(_Type, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.


rest_init(Req, _State) ->
    {GameId, Req2} = cowboy_req:qs_val(<<"id">>, Req),
    State = #rs_state{game_id = GameId},
    {ok, Req2, State}.


allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.


content_types_provided(Req, State) ->
    {[{<<"text/html">>, game_page}], Req, State}.


resource_exists(Req, #rs_state{game_id = GameId} = State) ->
    case ets:lookup(games, GameId) of
        [{GameId, _Size}] ->
            {true, Req, State};
        [] ->
            io:format("GameId: ~p not found~n", [GameId]),
            {false, Req, State}
    end.


game_page(Req, State) ->
    GamePath = code:priv_dir(gol) ++ "/game.html",
    {ok, GamePage} = file:read_file(GamePath),
	{GamePage, Req, State}.
