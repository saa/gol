%% This code from yaws - https://github.com/klacke/yaws/blob/master/src/yaws_api.erl
-module(ehtml).

-export([ehtml_expand/1]).


ehtml_expand(Ch) when Ch >= 0, Ch =< 255 -> Ch;
ehtml_expand(Bin) when is_binary(Bin) -> Bin;


ehtml_expand({Tag}) ->
    ["<", atom_to_list(Tag), " />"];
ehtml_expand({pre_html, X}) -> X;
ehtml_expand({Mod, Fun, Args})
  when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ehtml_expand(Mod:Fun(Args));
ehtml_expand({Tag, Attrs}) ->
    NL = ehtml_nl(Tag),
    [NL, "<", atom_to_list(Tag), ehtml_attrs(Attrs), "></",
     atom_to_list(Tag), ">"];
ehtml_expand({Tag, Attrs, Body}) when is_atom(Tag) ->
    Ts = atom_to_list(Tag),
    NL = ehtml_nl(Tag),
    [NL, "<", Ts, ehtml_attrs(Attrs), ">", ehtml_expand(Body), "</", Ts, ">"];
ehtml_expand([H|T]) -> [ehtml_expand(H)|ehtml_expand(T)];
ehtml_expand([]) -> [];
ehtml_expand(Fun) when is_function(Fun) ->
    ehtml_expand(Fun()).
ehtml_attrs([]) -> [];
ehtml_attrs([Attribute|Tail]) when is_atom(Attribute) ->
    [[$ |atom_to_list(Attribute)]|ehtml_attrs(Tail)];
ehtml_attrs([Attribute|Tail]) when is_list(Attribute) ->
    [" ", Attribute|ehtml_attrs(Tail)];
ehtml_attrs([{Name, {Mod, Fun, Args}} | Tail])
  when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ehtml_attrs([{Name,  Mod:Fun(Args)} | Tail]);
ehtml_attrs([{Name, Value} | Tail]) when is_function(Value) ->
    ehtml_attrs([{Name, Value()} | Tail]);
ehtml_attrs([{Name, Value} | Tail]) ->
    ValueString = [$", if
                           is_atom(Value) -> atom_to_list(Value);
                           is_list(Value) -> Value;
                           is_integer(Value) -> integer_to_list(Value);
                           is_float(Value) -> float_to_list(Value)
                       end, $"],
    [[$ |atom_to_list(Name)], [$=|ValueString]|ehtml_attrs(Tail)];
ehtml_attrs([{check, Name, {Mod, Fun, Args}} | Tail])
  when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ehtml_attrs([{check, Name,  Mod:Fun(Args)} | Tail]);
ehtml_attrs([{check, Name, Value} | Tail]) when is_function(Value) ->
    ehtml_attrs([{check, Name, Value()} | Tail]);
ehtml_attrs([{check, Name, Value} | Tail]) ->
    Val = if
               is_atom(Value) -> atom_to_list(Value);
              is_list(Value) -> Value;
              is_integer(Value) -> integer_to_list(Value);
              is_float(Value) -> float_to_list(Value)
          end,
    Q = case deepmember($", Val) of
            true -> $';
            false -> $"
        end,
    ValueString = [Q,Value,Q],
    [[$ |atom_to_list(Name)], [$=|ValueString]|ehtml_attrs(Tail)].


ehtml_nl(a) -> [];
ehtml_nl(br) -> [];
ehtml_nl(span) -> [];
ehtml_nl(em) -> [];
ehtml_nl(strong) -> [];
ehtml_nl(dfn) -> [];
ehtml_nl(code) -> [];
ehtml_nl(samp) -> [];
ehtml_nl(kbd) -> [];
ehtml_nl(var) -> [];
ehtml_nl(cite) -> [];
ehtml_nl(abbr) -> [];
ehtml_nl(acronym) -> [];
ehtml_nl(q) -> [];
ehtml_nl(sub) -> [];
ehtml_nl(sup) -> [];
ehtml_nl(ins) -> [];
ehtml_nl(del) -> [];
ehtml_nl(img) -> [];
ehtml_nl(tt) -> [];
ehtml_nl(i) -> [];
ehtml_nl(b) -> [];
ehtml_nl(big) -> [];
ehtml_nl(small) -> [];
ehtml_nl(strike) -> [];
ehtml_nl(s) -> [];
ehtml_nl(u) -> [];
ehtml_nl(font) -> [];
ehtml_nl(basefont) -> [];
ehtml_nl(input) -> [];
ehtml_nl(button) -> [];
ehtml_nl(object) -> [];
ehtml_nl(_) -> "\n".


deepmember(_C,[]) ->
    false;
deepmember(C,[C|_Cs]) ->
    true;
deepmember(C,[L|Cs]) when is_list(L) ->
    case deepmember(C,L) of
        true  -> true;
        false -> deepmember(C,Cs)
    end;
deepmember(C,[N|Cs]) when C /= N ->
    deepmember(C, Cs).
