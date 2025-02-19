#!/usr/bin/env -S swipl -f -q
:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(prolog_stack)).
:- use_module(library(lists)).

%:- initialization main.

:- dynamic page_ordering_rule/2.

% left atom in sequence before right atom is ok
setup_print_ordering_rules([]).
setup_print_ordering_rules([(Left, Right)|Rst]) :-
  assertz(page_ordering_rule(Left, Right)),
  setup_print_ordering_rules(Rst).

member_(List, Elem) :- member(Elem, List).

lst_equal_length(Lst1, Lst2) :-
  length(Lst1, Lst1Len),
  length(Lst2, Lst2Len),
  Lst1Len =:= Lst2Len.

lst_middle_([], _, _, _) :- false.
lst_middle_([Num|_], LstMiddle, Middle, Idx) :-
  Idx =:= LstMiddle,
  Middle = Num.
lst_middle_([_|Rst], LstMiddle, Middle, Idx) :-
  NextIdx is Idx + 1,
  lst_middle_(Rst, LstMiddle, Middle, NextIdx).
lst_middle(Lst, Middle) :-
  length(Lst, LstLen),
  (LstLen mod 2) =:= 1,
  LstMiddle is ceil(LstLen / 2),
  lst_middle_(Lst, LstMiddle, Middle, 1).

dfs(Node, Node, Path) :- Path = [Node].
dfs(Node, Goal, Path) :-
  dfs_(Node, Goal, [Node], PathFound),
  Path = [Node|PathFound].

dfs_(Node, Node, _, Path) :- Path = [Node].
dfs_(Node, Goal, _, Path) :-
  page_ordering_rule(Node, Goal),
  Path = [Goal].
dfs_(Node, Goal, SeenList, Path) :-
  bagof(R, (page_ordering_rule(Node, R), not(member(R, SeenList))), Options),
  member(Option, Options),
  dfs_(Option, Goal, [Option|SeenList], PathContinued),
  Path = [Option|PathContinued].

% does there exist a rule that has this number in it at all
rule_for_page_number(Number) :-
  page_ordering_rule(Number, _);
  page_ordering_rule(_, Number).

page_ordering_rule_(_, []) :- true.
% debug clause
page_ordering_rule_(NewPageNumber, _) :-
  not(rule_for_page_number(NewPageNumber)),
  format('precondition failed page number ~w is not in the rules list~n'),
  fail.
page_ordering_rule_(NewPageNumber, SeenNumber) :-
  page_ordering_rule(SeenNumber, NewPageNumber).

update_in_correct_order_([], _) :- true.
update_in_correct_order_([PageNumber|Rst], Context) :-
  maplist(page_ordering_rule_(PageNumber), Context),
  update_in_correct_order_(Rst, [PageNumber|Context]).

update_in_correct_order([]) :- true.
update_in_correct_order([FirstPageUpdate|Rst]) :-
  update_in_correct_order_(Rst, [FirstPageUpdate]).

can_end_with(Bag, Left, Filtered) :-
  bagof(R, page_ordering_rule(Left, R), RightNumbers),
  bagof(N, (member(N, RightNumbers), member(N, Bag)), Filtered).

% out of Bag, Filtered is what is allowed for the starting page update
can_start_with(Bag, Filtered) :-
  bagof(L, page_ordering_rule(L, _), LeftNumbers),
  bagof(N, (member(N, LeftNumbers), member(N, Bag)), Filtered).

correct_order_([Num|[]], Left, [Answer|[]]) :-
  page_ordering_rule(Left, Num),
  Answer = Num.
correct_order_(Bag, Left, [AnswerH|AnswerT]) :-
  can_end_with(Bag, Left, Filtered),
  member(Next, Filtered),
  delete(Bag, Next, BagRst),
  AnswerH = Next,
  correct_order_(BagRst, Next, AnswerT).

correct_order(Bag, [AnswerH|AnswerT]) :-
  can_start_with(Bag, Starters),
  member(Starter, Starters),
  delete(Bag, Starter, BagRst),
  AnswerH = Starter,
  correct_order_(BagRst, Starter, AnswerT).

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

setup_rules_from_file(PrintOrderRules, PageUpdates) :-
  phrase_from_file(printer(PrintOrderRules, PageUpdates), 'data/day5.txt'),
  setup_print_ordering_rules(PrintOrderRules).

part_1 :-
  setup_rules_from_file(_PrintOrderRules, PageUpdates),
  include(update_in_correct_order, PageUpdates, CorrectPageUpdates),
  maplist(lst_middle, CorrectPageUpdates, MiddlePagesOnly),
  maplist(atom_number, MiddlePagesOnly, MiddlePageNums),
  sum_list(MiddlePageNums, Answer),
  format('~w~n', [Answer]),
  halt(0).

main :-
  setup_rules_from_file(_PrintOrderRules, PageUpdates),
  exclude(update_in_correct_order, PageUpdates, IncorrectPageUpdates),
  format('incorrect ordered ~w~n', [IncorrectPageUpdates]),
  coverage(maplist(correct_order, IncorrectPageUpdates, CorrectedPageUpdates)),
  show_coverage([dir(cov)]),
  format('corrected ~w~n', [CorrectedPageUpdates]),
  maplist(lst_middle, CorrectedPageUpdates, MiddlePagesOnly),
  maplist(atom_number, MiddlePagesOnly, MiddlePageNums),
  format('middle page numbers ~w~n', [MiddlePageNums]),
  sum_list(MiddlePageNums, Answer),
  format('~w~n', [Answer]),
  halt(0).
