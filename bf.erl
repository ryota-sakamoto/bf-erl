-module(bf).
-export([parse/1, transform/1, execute/1]).

main(_) ->
    Prased = parse(io:get_line("")),
    Commands = transform(Prased),
    Result = execute(Commands),

    io:format("~ncommands: ~p~nresult: ~p~n", [Commands, Result]).

parse(Data) ->
    parse(Data, []).
parse([_], Commands) ->
    lists:reverse(Commands);
parse([$+ | Rest], Commands) ->
    parse(Rest, [value_increment | Commands]);
parse([$- | Rest], Commands) ->
    parse(Rest, [value_decrement | Commands]);
parse([$. | Rest], Commands) ->
    parse(Rest, [output | Commands]);
parse([$< | Rest], Commands) ->
    parse(Rest, [pointer_decrement | Commands]);
parse([$> | Rest], Commands) ->
    parse(Rest, [pointer_increment | Commands]);
parse([$[ | Rest], Commands) ->
    parse(Rest, [loop_start | Commands]);
parse([$] | Rest], Commands) ->
    parse(Rest, [loop_end | Commands]);
parse([_ | Rest], Commands) ->
    parse(Rest, Commands).

transform(Data) ->
    transform(Data, 0, [], []).
transform([_], _, Result, _) ->
    lists:reverse(Result);
transform([Head | Rest], Index, Result, Loops) ->
    case Head of
        loop_start ->
            transform(Rest, Index + 1, [{loop_start, -1} | Result], [Index | Loops]);
        loop_end ->
            [StartIndex | LoopsRest] = Loops,
            NewResult = update_list(Result, abs(length(Result) - StartIndex - 1), {loop_start, Index}),
            transform(Rest, Index + 1, [{loop_end, StartIndex} | NewResult], LoopsRest);
        _ ->
            transform(Rest, Index + 1, [Head | Result], Loops)
    end.

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
