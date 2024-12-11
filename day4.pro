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

x_mas_count_at_loc_((Row, Col), GridDims, Dict) :-
  wordsearch_get((Row, Col), GridDims, Dict, 'A'),
  wordsearch_get((Row - 1, Col - 1), GridDims, Dict, 'M'),
  wordsearch_get((Row - 1, Col + 1), GridDims, Dict, 'S'),
  wordsearch_get((Row + 1, Col - 1), GridDims, Dict, 'M'),
  wordsearch_get((Row + 1, Col + 1), GridDims, Dict, 'S').

xmas_count_at_loc(GridDims, Dict, Loc, Count) :-
  Directions = [(1, 1), (1, 0), (0, 1), (-1, 0), (0, -1), (-1, -1), (-1, 1), (1, -1)],
  include(xmas_count_at_loc_(Loc, GridDims, Dict), Directions, Successes),
  length(Successes, Count).

x_mas_count_at_loc(GridDims, Dict, Loc, Count) :-
  ( x_mas_count_at_loc_(Loc, GridDims, Dict) -> Count is 1 ; Count is 0 ).

lst_tuple([], _) :- false.
lst_tuple([_|[]], _) :- false.
lst_tuple([H|T], (L,R)) :-
  length(T, 1),
  H = L,
  T = [R].

% ref: https://stackoverflow.com/a/49503900, https://stackoverflow.com/a/72426112
cart_product(((RowStart, RowEnd), (ColStart, ColEnd)), Lst) :-
  findall(X, between(RowStart, RowEnd, X), Rows),
  findall(X, between(ColStart, ColEnd, X), Cols),
  findall([X,Y], (member(X, Rows), member(Y, Cols)), Lsts),
  maplist(lst_tuple, Lsts, Lst).

part_1(RowCount, ColCount, Dict) :-
  RowEnd is RowCount - 1,
  ColEnd is ColCount - 1,
  cart_product(((0, RowEnd), (0, ColEnd)), Positions),
  maplist(xmas_count_at_loc((RowCount, ColCount), Dict), Positions, XmasCounts),
  sum_list(XmasCounts, FinalCount),
  format('~w~n', [FinalCount]).

part_2(RowCount, ColCount, Dict) :-
  RowEnd is RowCount - 2,
  ColEnd is ColCount - 2,
  cart_product(((1, RowEnd), (1, ColEnd)), Positions),
  maplist(x_mas_count_at_loc((RowCount, ColCount), Dict), Positions, XmasCounts),
  sum_list(XmasCounts, FinalCount),
  format('~w~n', [FinalCount]).

main :-
  phrase_from_file(wordsearch(Wordsearch), 'data/day4.txt'),
  length(Wordsearch, RowCount),
  nth0(0, Wordsearch, FirstRow),
  length(FirstRow, ColCount),
  wordsearch_dict(Wordsearch, Dict),
  %part_1(RowCount, ColCount, Dict),
  part_2(RowCount, ColCount, Dict),
  halt(0).
