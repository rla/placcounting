:- module(reporting, [
    report/1
]).

:- use_module(analysis).
:- use_module(database).
:- use_module(library(apply)).

report(Account):-
    account_lines(Account, Lines),
    maplist(print_line, Lines),
    debit_total(Account, Debit),
    credit_total(Account, Credit),
    format('Debit: ~2f~n', [ Debit ]),
    format('Credit: ~2f~n', [ Credit ]),
    balance(Account, Debit, Credit, Balance),
    format('Balance: ~2f~n', [ Balance ]).

print_line(Line):-
    Line =.. [line,Date|Args],
    format_date(Date, DateStr),
    format('~w ~w ~w ~2f ~w~n', [DateStr|Args]).

format_date(Y-M-D, Atom):- !,
    Date = date(Y, M, D, 0, 0, 0, 0, 'UTC', false),
    format_time(atom(Atom), '%Y-%m-%d', Date).

format_date(Y-M, Atom):-
    Date = date(Y, M, 1, 0, 0, 0, 0, 'UTC', false),
    format_time(atom(Atom), '%Y-%m', Date).