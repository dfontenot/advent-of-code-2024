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

wordsearch_get((Row, Col), (RowCount, ColCount), Dict, Res) :-
  Row < RowCount,
  Col < ColCount,
  Row >= 0,
  Col >= 0,
  Idx is (ColCount * Row) + Col,
  get_dict(Idx, Dict, Res).

wordsearch_dict_(Letter, (Idx, Dict), (NewIdx, NewDict)) :-
  put_dict(Idx, Dict, Letter, NewDict),
  NewIdx is Idx + 1.

wordsearch_dict([], Dict) :- Dict = wordsearch{}.
wordsearch_dict(Rows, Dict) :-
  flatten(Rows, FlatWordsearch),
  foldl(wordsearch_dict_, FlatWordsearch, (0, wordsearch{}), (_, Dict)).

xmas_count_at_loc_((Row, Col), GridDims, Dict, (RowDir, ColDir)) :-
  wordsearch_get((Row, Col), GridDims, Dict, 'X'),
  wordsearch_get((Row + RowDir, Col + ColDir), GridDims, Dict, 'M'),
  wordsearch_get((Row + RowDir * 2, Col + ColDir * 2), GridDims, Dict, 'A'),
  wordsearch_get((Row + RowDir * 3, Col + ColDir * 3), GridDims, Dict, 'S').

xmas_count_at_loc(Loc, GridDims, Dict, Count) :-
  Directions = [(1, 1), (1, 0), (0, 1), (-1, 0), (0, -1), (-1, -1), (-1, 1), (1, -1)],
  include(xmas_count_at_loc_(Loc, GridDims, Dict), Directions, Successes),
  length(Successes, Count).

main :-
  phrase_from_file(lines(Wordsearch), 'data/day4example.txt'),
  length(Wordsearch, RowCount),
  list_first(Wordsearch, FirstRow),
  length(FirstRow, ColCount),
  wordsearch_dict(Wordsearch, Dict),
  %xmas_count_at_loc((0,5), (RowCount,ColCount), Dict, Res),
  %xmas_count_at_loc((4,0), (RowCount,ColCount), Dict, Res),
  wordsearch_get((2, 0), (RowCount, ColCount), Dict, Res),
  format('~w~n', [Res]),
  halt(0).
