#!/usr/bin/env -S swipl -f -q
:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(prolog_stack)).

:- initialization main.

line([V, W, X, Y, Z]) -->
  integer(V), blank, integer(W), blank, integer(X), blank, integer(Y), blank, integer(Z).

lines([]) --> eos.
lines([L|Ls]) -->
  line(L), eol, lines(Ls).

distance(Dist, Left, Right) :-
  Signed is Left - Right,
  Dist is abs(Signed).

distance_rule(X, Y, Increasing, Decreasing, PiecewiseOk) :-
  distance(PiecewiseDistance, X, Y),
  PiecewiseOk = ( PiecewiseDistance >= 1, PiecewiseDistance =< 3 -> true ; false),
  Increasing = (X < Y -> true ; false),
  Decreasing = (X > Y -> true ; false).

is_safe_helper([_ | []], AllIncreasing, AllDecreasing, PiecewiseOk) :-
  ( AllIncreasing ; AllDecreasing ; PiecewiseOk ).
is_safe_helper([X, Y | Rst], AllIncreasing, AllDecreasing, PiecewiseOk) :-
  ( AllIncreasing ; AllDecreasing ; PiecewiseOk ),
  distance_rule(X, Y, NextAllIncreasing, NextAllDecreasing, NextPiecewiseOk),
  NewAllIncreasing = (AllIncreasing, NextAllIncreasing -> true ; false),
  NewAllDecreasing = (AllDecreasing, NextAllDecreasing -> true ; false),
  NewPiecewiseOk = (PiecewiseOk, NextPiecewiseOk -> true ; false),
  is_safe_helper([Y | Rst], NewAllIncreasing, NewAllDecreasing, NewPiecewiseOk).

is_safe([]) :- true.
is_safe([_ | []]) :- fail.
is_safe([X, Y | []]) :-
  distance_rule(X, Y, AllIncreasing, AllDecreasing, _),
  ( AllIncreasing ; AllDecreasing ).
is_safe([X, Y | Rst]) :-
  distance_rule(X, Y, AllIncreasing, AllDecreasing, PiecewiseOk),
  is_safe_helper([Y | Rst], AllIncreasing, AllDecreasing, PiecewiseOk).

main :-
  phrase_from_file(lines(Lines), 'data/day2example.txt'),
  include(is_safe, Lines, SafeReports),
  length(SafeReports, Res),
  format('~w~n', Res),
  halt(0).
