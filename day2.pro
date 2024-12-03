#!/usr/bin/env -S swipl -f -q
:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(prolog_stack)).

%:- initialization main.

line([V, W, X, Y, Z]) -->
  integer(V), blank, integer(W), blank, integer(X), blank, integer(Y), blank, integer(Z).

lines([]) --> eos.
lines([L|Ls]) -->
  line(L), eol, lines(Ls).

distance(Dist, Left, Right) :-
  Signed is Left - Right,
  Dist is abs(Signed).

distance_rule(X, Y) :-
  distance(PiecewiseDistance, X, Y),
  PiecewiseDistance >= 1,
  PiecewiseDistance =< 3.

is_safe_helper([]) :- true.
is_safe_helper([_ | []]) :- true.
is_safe_helper([X, Y | Rst]) :-
  distance_rule(X, Y),
  is_safe_helper([Y | Rst]).

is_safe([]) :- true.
is_safe([_ | []]) :- true.
is_safe(Lst) :-
  ( sort(0, @<, Lst, Lst); sort(0, @>, Lst, Lst) ),
  is_safe_helper(Lst).

main :-
  phrase_from_file(lines(Lines), 'data/day2example.txt'),
  include(is_safe, Lines, SafeReports),
  length(SafeReports, Res),
  format('~w~n', Res),
  halt(0).
