#!/usr/bin/env -S swipl -f -q
:- use_module(library(dcg/basics)).

:- initialization main.

line((Left, Right)) -->
  integer(Left), blanks, integer(Right).

lines([]) --> eos.
lines([L|Ls]) -->
  line(L), eol, lines(Ls).

main :-
  phrase_from_file(lines(Lines), "data/day1.txt"),
  format(Lines),
  halt(0).
