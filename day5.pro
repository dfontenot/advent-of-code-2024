#!/usr/bin/env -S swipl -f -q
:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(prolog_stack)).
:- use_module(library(lists)).

:- initialization main.

member_(List, Elem) :- member(Elem, List).

lst_equal_length(Lst1, Lst2) :-
  length(Lst1, Lst1Len),
  length(Lst2, Lst2Len),
  Lst1Len =:= Lst2Len.

page_identifier(EndsWith, Id) -->
  { atom_codes('0123456789', AllowedCodes), ! },
  string_without(EndsWith, IdChars),
  {
    include(member_(AllowedCodes), IdChars, Filtered),
    lst_equal_length(IdChars, Filtered),
    atom_string(Id, IdChars)
  }.

parse_rule(Left, Right) -->
  page_identifier("|", Left), "|", page_identifier("\n", Right), eol.

parse_rules([(PrintRuleLeft, PrintRuleRight)|Rst]) -->
  parse_rule(PrintRuleLeft, PrintRuleRight), !, parse_rules(Rst).
parse_rules([]) --> eol.

parse_page_update_line([UpdateId|[]]) -->
  page_identifier("\n", UpdateId), !, eol.
parse_page_update_line([UpdateId|Rst]) -->
  page_identifier(",", UpdateId), ",", parse_page_update_line(Rst).

parse_page_updates([]) --> eos, !.
parse_page_updates([UpdateIds|Rst]) -->
  parse_page_update_line(UpdateIds), parse_page_updates(Rst).

printer(PrintRules, PageUpdates) -->
  parse_rules(PrintRules), parse_page_updates(PageUpdates).

main :-
  phrase_from_file(printer(PrintOrderRules, PageUpdates), 'data/day5example.txt'),
  format('~w ~w~n', [PrintOrderRules, PageUpdates]),
  halt(0).
