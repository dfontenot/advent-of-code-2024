#!/usr/bin/env -S swipl -f -q
:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(prolog_stack)).

:- initialization main.

computer_data([]) --> eos.
computer_data([(Left, Right)|Ls]) -->
  string_without("m", _), "mul(",
  integer(Left),
  ",",
  integer(Right),
  ")",
  computer_data(Ls).
computer_data(L) -->
  [_],
  computer_data(L).

mult_tuples([], []) :- true.
mult_tuples([(Left, Right)|Ls], [Num|Ls2]) :-
  Num is Left * Right,
  mult_tuples(Ls, Ls2).

main :-
  phrase_from_file(computer_data(Instructions), 'data/day3.txt'),
  mult_tuples(Instructions, Multiplied),
  foldl(plus, Multiplied, 0, Res),
  format("~w~n", [Res]),
  halt(0).
