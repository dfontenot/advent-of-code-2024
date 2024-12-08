#!/usr/bin/env -S swipl -f -q
:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(prolog_stack)).
:- use_module(library(lists)).

%:- initialization main.

line([Letter|Ls]) -->
  [Char],
  { char_code(Letter, Char), atom_codes('XMAS', AllowedCodes), member(Char, AllowedCodes) },
  line(Ls).
line(_) -->
  eol.

lines([]) --> eos.
lines([L|Ls]) -->
  line(L), lines(Ls).

list_first([], _) :- false.
list_first([L|_], First) :- L = First.

main :-
  phrase_from_file(lines(Wordsearch), 'data/day4example.txt'),
  length(Wordsearch, LinesLen),
  list_first(Wordsearch, FirstLine),
  length(FirstLine, ColsLen),
  format('~w~w~n', [LinesLen, ColsLen]),
  halt(0).
