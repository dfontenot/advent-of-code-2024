#!/usr/bin/env -S swipl -f -q
:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(prolog_stack)).

:- initialization main.

computer_data([]) --> eos.
computer_data([(Left, Right)|Ls]) -->
  "mul(",
  integer(Left),
  ",",
  integer(Right),
  ")",
  computer_data(Ls).
computer_data(L) -->
  [_],
  computer_data(L).

toggled_computer_data([]) --> eos.
toggled_computer_data([(Left, Right)|Ls]) -->
  "mul(",
  integer(Left),
  ",",
  integer(Right),
  ")",
  toggled_computer_data(Ls).
toggled_computer_data([Instruction|Ls]) -->
  "do()", { Instruction = 'do' }, toggled_computer_data(Ls).
toggled_computer_data([Instruction|Ls]) -->
  "don't()", { Instruction = 'dont' }, toggled_computer_data(Ls).
toggled_computer_data(L) -->
  [_],
  toggled_computer_data(L).

mult_tuples_dont([], []) :- true.
mult_tuples_dont(['do'|Ls], Ls2) :-
  mult_tuples(Ls, Ls2).
mult_tuples_dont(['dont'|Ls], Ls2) :-
  mult_tuples_dont(Ls, Ls2).
mult_tuples_dont([_|Ls], Ls2) :-
  mult_tuples_dont(Ls, Ls2).

mult_tuples([], []) :- true.
mult_tuples(['do'|Ls], Ls2) :-
  mult_tuples(Ls, Ls2).
mult_tuples(['dont'|Ls], Ls2) :-
  mult_tuples_dont(Ls, Ls2).
mult_tuples([(Left, Right)|Ls], [Num|Ls2]) :-
  Num is Left * Right,
  mult_tuples(Ls, Ls2).

main :-
  phrase_from_file(toggled_computer_data(Instructions), 'data/day3.txt'),
  mult_tuples(Instructions, Multiplied),
  foldl(plus, Multiplied, 0, Res),
  format("~w~n", [Res]),
  halt(0).
