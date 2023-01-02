-module(bf).
-export([tokenize/1, transform/1, execute/1]).

main(_) ->
    Token = tokenize(io:get_line("")),
    Commands = transform(Token),
    Result = execute(Commands),

    io:format("~ncommands: ~p~nresult: ~p~n", [Commands, Result]).

tokenize(Data) ->
    tokenize(Data, []).
tokenize([], Commands) ->
    lists:reverse(Commands);
tokenize([$+ | Rest], Commands) ->
    tokenize(Rest, [value_increment | Commands]);
tokenize([$- | Rest], Commands) ->
    tokenize(Rest, [value_decrement | Commands]);
tokenize([$. | Rest], Commands) ->
    tokenize(Rest, [output | Commands]);
tokenize([$< | Rest], Commands) ->
    tokenize(Rest, [pointer_decrement | Commands]);
tokenize([$> | Rest], Commands) ->
    tokenize(Rest, [pointer_increment | Commands]);
tokenize([$[ | Rest], Commands) ->
    tokenize(Rest, [loop_start | Commands]);
tokenize([$] | Rest], Commands) ->
    tokenize(Rest, [loop_end | Commands]);
tokenize([_ | Rest], Commands) ->
    tokenize(Rest, Commands).

transform(Data) ->
    transform(Data, 0, [], []).
transform([], _, Result, _) ->
    lists:reverse(Result);
transform([loop_start | Rest], Index, Result, Loops) ->
    transform(Rest, Index + 1, [{loop_start, -1} | Result], [Index | Loops]);
transform([loop_end | Rest], Index, Result, Loops) ->
    [StartIndex | LoopsRest] = Loops,
    NewResult = update_list(Result, abs(length(Result) - StartIndex - 1), {loop_start, Index}),
    transform(Rest, Index + 1, [{loop_end, StartIndex} | NewResult], LoopsRest);
transform([Head | Rest], Index, Result, Loops) ->
    transform(Rest, Index + 1, [Head | Result], Loops).

execute(Commands) ->
    execute(Commands, Commands, 0, []).
execute(_, [], _, Result) ->
    Result;
execute(Commands, [Head | Rest], PointerIndex, Data) ->
    NewData = if
        length(Data) =< PointerIndex ->
            Data ++ [0];
        true ->
            Data
    end,
    Value = lists:nth(PointerIndex + 1, NewData),

    case Head of
        value_increment ->
            execute(Commands, Rest, PointerIndex, update_list(NewData, PointerIndex, Value + 1));
        value_decrement ->
            execute(Commands, Rest, PointerIndex, update_list(NewData, PointerIndex, Value - 1));
        output ->
            io:format("~c", [Value]),
            execute(Commands, Rest, PointerIndex, NewData);
        pointer_increment -> execute(Commands, Rest, PointerIndex + 1, NewData);
        pointer_decrement -> execute(Commands, Rest, PointerIndex - 1, NewData);
        {loop_start, LoopEndIndex} when Value == 0 ->
            execute(Commands, lists:sublist(Commands, LoopEndIndex + 1, length(Commands)), PointerIndex, NewData);
        {loop_start, _} ->
            execute(Commands, Rest, PointerIndex, NewData);
        {loop_end, LoopStartIndex} when Value /= 0 ->
            execute(Commands, lists:sublist(Commands, LoopStartIndex + 1, length(Commands)), PointerIndex, NewData);
        {loop_end, _} ->
            execute(Commands, Rest, PointerIndex, NewData);
        _ -> execute(Commands, Rest, PointerIndex, NewData)
    end.

update_list(List, Index, Value) ->
    lists:sublist(List, 1, Index) ++ [Value] ++ lists:sublist(List, Index + 2, length(List)).
