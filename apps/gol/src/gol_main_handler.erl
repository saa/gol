-module(gol_main_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).
-export([index_page/2]).
-export([start_game/2]).

-define(DEFAULT_SIZE, 100).

init(_Type, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.


allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.


content_types_provided(Req, State) ->
    {[{<<"text/html">>, index_page}], Req, State}.


content_types_accepted(Req, State) ->
	{[{<<"application/x-www-form-urlencoded">>, start_game}], Req, State}.


index_page(Req, State) ->
    IndexPath = code:priv_dir(gol) ++ "/index.html",
    {ok, IndexPage} = file:read_file(IndexPath),
	{IndexPage, Req, State}.


start_game(Req, State) ->
    {ok, Data, Req2} = cowboy_req:body_qs(Req),
    case check_size_val(Data) of
        {ok, Size} ->
            GameId = get_random_string(10),
            ets:insert(games, {GameId, Size}),
            {{true, <<$/, <<"game">>/binary, $?, <<"id=">>/binary, GameId/binary>>}, Req2, State};
        {error, Reason} ->
            io:format("Error, reason: ~s~n", [Reason]),
            Req3 = cowboy_req:set_resp_body(Reason, Req2),
            {ok, Req4} = cowboy_req:reply(400, Req3),
            {halt, Req4, State}
    end.


check_size_val([{<<"size">>, <<"">>}]) ->
    io:format("Use default size: ~p~n", [?DEFAULT_SIZE]),
    {ok, ?DEFAULT_SIZE};
check_size_val([{<<"size">>, Size}]) ->
    try
        Size2 = binary_to_integer(Size),
        io:format("Use size from request: ~p~n", [Size2]),
        {ok, Size2}
    catch
        _:_ ->
            {error, <<"Wrong size type">>}
    end.


get_random_string(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                         "abcdefghijklmnopqrstuvwxyz"
                         "0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) ->
                [element(crypto:rand_uniform(Len, ChrsSize), Chrs) | R] end,
    list_to_binary(lists:foldl(F, "", lists:seq(1, Len))).
