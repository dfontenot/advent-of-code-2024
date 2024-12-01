#!/usr/bin/env -S swipl -f -q
:- use_module(library(dcg/basics)).

:- initialization main.

line((Left, Right)) -->
  integer(Left), blanks, integer(Right).

lines([]) --> eos.
lines([L|Ls]) -->
  line(L), eol, lines(Ls).

tuples_to_lists([], ([], [])).
tuples_to_lists([(X, Y) | Ts], (L1, L2)) :-
    tuples_to_lists(Ts, (L1s, L2s)),
    L1 = [X | L1s],
    L2 = [Y | L2s].

main :-
  phrase_from_file(lines(Lines), "data/day1.txt"),
  tuples_to_lists(Lines, (L1, L2)),
  format('~W', L1),
  halt(0).
