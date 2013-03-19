:- module(analysis, [
    balance/3,
    balance/2,
    balance/4,
    debit_total/2,
    credit_total/2,
    debit_total/3,
    credit_total/3,
    lines_amount/2,
    line_amount/2,
    month_lines/3,
    month_line/3,
    debit_line/3,
    credit_line/3,
    account_lines/2,
    account_line/2,
    debit_line/2,
    credit_line/2,
    change/3
]).

:- use_module(library(apply)).
:- use_module(database).

%% balance(+Account, +Month, -Balance) is det.
%
% Calculates the balance for the given account
% and month.

balance(Account, Month, Balance):-
    debit_total(Account, Month, Debit),
    credit_total(Account, Month, Credit),
    balance(Account, Debit, Credit, Balance).

%% balance(+Account, -Balance) is det.
%
% Calculates the balance for the given account.

balance(Account, Balance):-
    debit_total(Account, Debit),
    credit_total(Account, Credit),
    balance(Account, Debit, Credit, Balance).

%% balance(+Account, +Debit, +Credit, -Balance) is det.
%
% Calculates balance using precalculated total
% credit and total debit.

balance(Account, Debit, Credit, Balance):-
    account(Account, _, Type),
    change(Type, debit, DSign),
    change(Type, credit, CSign), !,
    DebitTerm =.. [DSign, 0, Debit],
    CreditTerm =.. [CSign, 0, Credit],
    Balance is DebitTerm + CreditTerm.

%% debit_total(+Account, -Total) is det.
%
% Calculates the toal sum of amounts of
% the debit lines for the given account.

debit_total(Account, Total):-
    findall(Line, debit_line(Account, Line), Lines),
    lines_amount(Lines, Total).

%% credit_total(+Account, -Total) is det.
%
% Calculates the toal sum of amounts of
% the credit lines for the given account.

credit_total(Account, Total):-
    findall(Line, credit_line(Account, Line), Lines),
    lines_amount(Lines, Total).

%% debit_total(+Account, +Month, -Total) is det.
%
% Calculates the toal sum of amounts of
% the debit lines for the given account and month.

debit_total(Account, Month, Total):-
    findall(Line, debit_line(Account, Month, Line), Lines),
    maplist(line_amount, Lines, Amounts),
    sum_list(Amounts, Total).

%% credit_total(+Account, +Month, -Total) is det.
%
% Calculates the toal sum of amounts of
% the credit lines for the given account and month.

credit_total(Account, Month, Total):-
    findall(Line, credit_line(Account, Month, Line), Lines),
    lines_amount(Lines, Total).

%% lines_amount(+Lines:list, -Total) is det.
%
% Calculates the total sum of amounts of
% the given list of lines.

lines_amount(Lines, Total):-
    maplist(line_amount, Lines, Amounts),
    sum_list(Amounts, Total).

%% line_amount(+Line, -Amount) is det.
%
% Extracts Amount from the line.

line_amount(line(_, _, _, Amount, _), Amount).

%% month_lines(+Account, +Month, -Lines) is det.
%
% Retrieves all lines using month_line/3. Sorts
% lines by date.

month_lines(Account, Month, Sorted):-
    findall(Line, month_line(Account, Month, Line), Lines),
    sort(Lines, Sorted).

%% month_line(?Account, ?Month, ?Line) is nondet.
%
% Retrieves those line/5 clauses that debit or credit
% to the given account on the given month.
% See also database:line/5.

month_line(Account, Month, Line):-
    credit_line(Account, Month, Line).

month_line(Account, Month, Line):-
    debit_line(Account, Month, Line).

%% debit_line(?Account, ?Month, ?Line) is nondet.
%
% Retrieves those line/5 clauses that debit to
% the given account on the given month.
% See also database:line/5.

debit_line(Account, Month, Line):-
    Line = line(_-Month-_, _, _, _, _),
    debit_line(Account, Line).

%% credit_line(?Account, ?Month, ?Line) is nondet.
%
% Retrieves those line/5 clauses that credit to
% the given account on the given month.
% See also database:line/5.

credit_line(Account, Month, Line):-
    Line = line(_-Month-_, _, _, _, _),
    credit_line(Account, Line).

%% account_lines(+Account, Lines:list) is det.
%
% Retrieves account lines sorted by date.
% See also account_line/2.

account_lines(Account, Lines):-
    findall(Line, account_line(Account, Line), Tmp),
    sort(Tmp, Lines).

%% account_line(?Account, ?Line) is nondet.
%
% Retrieves line that credits or debits to
% the given account.

account_line(Account, Line):-
    debit_line(Account, Line).

account_line(Account, Line):-
    credit_line(Account, Line).

%% debit_line(?Account, ?Line) is nondet.
%
% Retrieves those line/5 clauses that debit to
% the given account. See also database:line/5.

debit_line(Account, Line):-
    line(Date, Account, Credit, Amount, Desc),
    Line = line(Date, Account, Credit, Amount, Desc).

%% credit_line(?Account, ?Line) is nondet.
%
% Retrieves those line/5 clauses that credit to
% the given account. See also database:line/5.

credit_line(Account, Line):-
    line(Date, Debit, Account, Amount, Desc),
    Line = line(Date, Debit, Account, Amount, Desc).

%% change(?Type, ?DorC, ?Sign) is nondet.
%
% Gives the effect of debit or credit on
% the given account type.

change(asset, debit, (+)).
change(asset, credit, (-)).
change(liability, debit, (-)).
change(liability, credit, (+)).
change(income, debit, (-)).
change(income, credit, (+)).
change(expense, debit, (+)).
change(expense, credit, (-)).
change(equity, debit, (-)).
change(equity, credit, (+)).
