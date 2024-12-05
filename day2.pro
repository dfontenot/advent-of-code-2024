#!/usr/bin/env -S swipl -f -q
:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(prolog_stack)).

:- initialization main.

line([L|Ls]) --> integer(L), " ", line(Ls).
line([L]) --> integer(L).

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
  ( sort(0, @=<, Lst, Lst); sort(0, @>=, Lst, Lst) ),
  is_safe_helper(Lst).

% succeeds if lists are the same and left has at most 1 nil replacing a value
one_nil_allowed([], []) :- true.
one_nil_allowed([X], [Y]) :-
  X = Y;
  X = nil.
one_nil_allowed([X|Rst1], [Y|Rst2]) :-
  ( X = Y, one_nil_allowed(Rst1, Rst2) );
  ( X = nil, maplist(=, Rst1, Rst2) ).

is_safeish([]) :- true.
is_safeish(Lst) :-
  one_nil_allowed(LstOneNil, Lst),
  select(nil, LstOneNil, LstNoNil),
  is_safe(LstNoNil).

main :-
  phrase_from_file(lines(Lines), 'data/day2.txt'),
  include(is_safeish, Lines, SafeReports),
  length(SafeReports, Res),
  format('~w~n', Res),
  halt(0).
