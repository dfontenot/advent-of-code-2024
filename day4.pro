#!/usr/bin/env -S swipl -f -q
:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(prolog_stack)).
:- use_module(library(lists)).

:- initialization main.

wordsearch_char(Char) -->
  [FoundChar],
  { char_code(Char, FoundChar), atom_codes('XMAS', AllowedCodes), member(FoundChar, AllowedCodes), ! }.

rest_of_line([Char|Cs]) -->
  wordsearch_char(Char),
  rest_of_line(Cs).
rest_of_line([]) --> [].

line([Char|Cs]) -->
  wordsearch_char(Char),
  rest_of_line(Cs),
  eol, !.

wordsearch([]) --> eos.
wordsearch([L|Ls]) -->
  line(L), wordsearch(Ls), !.

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

xmas_count_at_loc(GridDims, Dict, Loc, Count) :-
  Directions = [(1, 1), (1, 0), (0, 1), (-1, 0), (0, -1), (-1, -1), (-1, 1), (1, -1)],
  include(xmas_count_at_loc_(Loc, GridDims, Dict), Directions, Successes),
  length(Successes, Count).

lst_tuple([], _) :- false.
lst_tuple([_|[]], _) :- false.
lst_tuple([H|T], (L,R)) :-
  length(T, 1),
  H = L,
  T = [R].

% ref: https://stackoverflow.com/a/49503900, https://stackoverflow.com/a/72426112
cart_product((RowCount, ColCount), Lst) :-
  RowCountFence is RowCount - 1,
  ColCountFence is ColCount - 1,
  findall(X, between(0, RowCountFence, X), Rows),
  findall(X, between(0, ColCountFence, X), Cols),
  findall([X,Y], (member(X, Rows), member(Y, Cols)), Lsts),
  maplist(lst_tuple, Lsts, Lst).

main :-
  phrase_from_file(wordsearch(Wordsearch), 'data/day4.txt'),
  length(Wordsearch, RowCount),
  nth0(0, Wordsearch, FirstRow),
  length(FirstRow, ColCount),
  wordsearch_dict(Wordsearch, Dict),
  cart_product((RowCount, ColCount), Positions),
  maplist(xmas_count_at_loc((RowCount, ColCount), Dict), Positions, XmasCounts),
  sum_list(XmasCounts, FinalCount),
  format('~w~n', [FinalCount]),
  halt(0).
