-module(tests).
-include_lib("eunit/include/eunit.hrl").

tokenize_test() ->
   [
      value_increment,
      value_decrement,
      output,
      pointer_decrement,
      pointer_increment,
      loop_start,
      loop_end
   ] = bf:tokenize("+-.<>[]").

transform_test() ->
   [
      {loop_start, 2},
      {value_increment, 2},
      {loop_end, 0},
      {loop_start, 8},
      {pointer_decrement, 1},
      {loop_start, 7},
      {value_decrement, 1},
      {loop_end, 5},
      {loop_end, 3}
   ] = bf:transform([
      loop_start,
      value_increment,
      value_increment,
      loop_end,
      loop_start,
      pointer_decrement,
      loop_start,
      value_decrement,
      loop_end,
      loop_end
   ]).

execute_test() ->
   #{
      data := [0, 2],
      outputs := [2]
   } = bf:execute([
      {value_increment, 1},
      {pointer_increment, 1},
      {value_increment, 1},
      {value_increment, 1},
      {pointer_decrement, 1},
      {value_decrement, 1},
      {pointer_increment, 1},
      output
   ]).
