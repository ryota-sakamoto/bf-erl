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
      value_increment,
      {loop_end, 0},
      {loop_start, 8},
      pointer_decrement,
      {loop_start, 7},
      value_decrement,
      {loop_end, 5},
      {loop_end, 3}
   ] = bf:transform([
      loop_start,
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
      value_increment,
      pointer_increment,
      value_increment,
      value_increment,
      pointer_decrement,
      value_decrement,
      pointer_increment,
      output
   ]).
