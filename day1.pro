#!/usr/bin/env -S swipl -f -q
:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(prolog_stack)).

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

distance(Dist, Left, Right) :-
  Signed is Left - Right,
  Dist is abs(Signed).

distances_list(Unsorted, Distances) :-
  tuples_to_lists(Unsorted, (L1Unsorted, L2Unsorted)),
  sort(0, @=<, L1Unsorted, L1Sorted),
  sort(0, @=<, L2Unsorted, L2Sorted),
  maplist(distance, Distances, L1Sorted, L2Sorted).

main_part_1 :-
  phrase_from_file(lines(Lines), 'data/day1.txt'),
  distances_list(Lines, Distances),
  foldl(plus, Distances, 0, Res),
  format('~w~n', Res),
  halt(0).

increment_occurrences(Occurrence, Occurrences, OccurrencesOut) :-
  ( get_dict(Occurrence, Occurrences, Count) -> NewCount is Count + 1 ; NewCount is 1 ),
  put_dict(Occurrence, Occurrences, NewCount, OccurrencesOut).

get_similarity(Occurrences, Val, Similarity) :-
  ( get_dict(Val, Occurrences, Count) -> FoundCount is Count ; FoundCount is 0 ),
  Similarity is FoundCount * Val.

similarities_list(Lines, Similarities) :-
  tuples_to_lists(Lines, (L1, L2)),
  dict_create(L2Occurrences, occurrences, []),
  foldl(increment_occurrences, L2, L2Occurrences, AllOccurrences),
  maplist(get_similarity(AllOccurrences), L1, Similarities).

main :-
  phrase_from_file(lines(Lines), 'data/day1.txt'),
  similarities_list(Lines, Similarities),
  foldl(plus, Similarities, 0, Res),
  format('~w~n', Res),
  halt(0).
