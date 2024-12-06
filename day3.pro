#!/usr/bin/env -S swipl -f -q
:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(prolog_stack)).

%:- initialization main.

skip_to(Code) -->
  string_without(Code, _), Code.

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
computer_data(_) --> [].

main :-
  phrase_from_file(computer_data(Instructions), 'data/day3example.txt'),
  halt(0).
