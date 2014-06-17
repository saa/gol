-module(gol).

-compile([export_all]).

-export([create_world/1]).
-export([create_next_generation/2]).


create_world(Size) ->
    create_world(Size, 0).

create_world(Size, _CellAlive) ->
    World = init_world(Size),
    ColoredWorld = paint_cell(World),
    {World, generate_html_table(Size, ColoredWorld)}.


create_next_generation(Size, World) ->
    NextGeneration = [Cell#{ state := get_state(Cell, World) } || Cell <- World],
    ColoredNextGeneration = paint_cell(NextGeneration),
    {NextGeneration, generate_html_table(Size, ColoredNextGeneration)}.


get_state(#{ state := State } = Cell, World) ->
    Neighbors = get_neighbors(Cell, World),
    LiveNeighbors = length(lists:filter(fun(#{ state := S }) -> S =:= alive end, Neighbors)),
    BoolState = case State of
                    alive -> (LiveNeighbors < 2) orelse (LiveNeighbors > 3);
                    dead -> LiveNeighbors =/= 3
                end,
    case BoolState of
        true ->
            dead;
        false ->
            alive
    end.


is_neighbor(#{ x := Xm, y := Ym } = Cm, #{ x := X, y := Y } = C) ->
    %% io:format("Xm: ~p X: ~p Ym: ~p Y: ~p~n", [Xm, X, Ym, Y]),
    Xd = abs(Xm - X),
    Yd = abs(Ym - Y),
    (Xd =< 1) andalso (Yd =< 1) andalso (Cm =/= C).


get_neighbors(Cell, World) ->
    lists:filter(fun(Cm) -> is_neighbor(Cm, Cell) end, World).


init_world(Size) ->
    init_world(Size, Size, Size, []).

init_world(0, 0, _Size, World) ->
    [#{x => 0, y => 0, state => get_random_state()} | World];
init_world(X, 0, Size, World) ->
    init_world(X - 1, Size, Size, [#{x => X, y => 0, state => get_random_state()} | World]);
init_world(X, Y, Size, World) ->
    init_world(X, Y - 1, Size, [#{x => X, y => Y, state => get_random_state()} | World]).


get_random_state() ->
    case random:uniform(5) =:= 1 of
        true ->
            alive;
        false ->
            dead
    end.


generate_html_table(Size, World) ->
    iolist_to_binary(ehtml:ehtml_expand(generate_rows(0, Size, World))).

generate_rows(Acc, Size, _World) when Acc =:= Size ->
    [];
generate_rows(Acc, Size, World) when Acc < Size ->
    [generate_row(Acc, Size, World) | generate_rows(Acc + 1, Size, World)].

generate_row(Acc, Size, World) ->
    {tr, [], generate_cells(0, Acc, Size, World)}.


generate_cells(X, _Y, Size, _World) when X =:= Size ->
    [];
generate_cells(X, Y, Size, World) when X < Size ->
    [generate_cell(X, Y, World) | generate_cells(X + 1, Y, Size, World)].

generate_cell(X, Y, World) ->
    case lists:filter(fun(#{ x := Xcell, y := Ycell }) -> (Xcell =:= X) andalso (Ycell =:= Y) end, World) of
        [#{ color := Color }] ->
            {td, [{bgcolor, Color}]};
        _ ->
            io:format("Error. Cell not found in world.")
    end.


paint_cell(World) ->
    paint_cell(World, []).

paint_cell([], Acc) ->
    Acc;
paint_cell([#{ state := alive } = Cell | Cells], Acc) ->
    paint_cell(Cells, [Cell#{ color => green} | Acc]);
paint_cell([#{ state := dead } = Cell | Cells], Acc) ->
    paint_cell(Cells, [Cell#{ color => black} | Acc]).
