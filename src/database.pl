:- module(database, [
    read_all/1,
    account/3,
    transaction/3,
    transaction_line/5,
    line/5,
    print_stat/0
]).

:- use_module(library(apply)).
:- use_module(library(gensym)).
:- use_module(library(yamltiny)).

%% account(?Code, ?Name, ?Type) is nondet.
%
% Describes accounts in the system. Type is one
% of: asset, liability, expense, income, equity.
% Types are currently used for calculating the effects
% of debits and credits (increasing or decreasing).

:- dynamic(account/3).

%% transaction(?TxId, ?Desc, ?Date) is nondet.
%
% Describes single transaction. A transaction is a logical
% collection of debit/credit lines. TxId is a generated
% identifier to relate lines with the transaction.

:- dynamic(transaction/3).

%% transaction_line(?TxId, ?Debit, ?Credit, ?Amount, ?Desc) is nondet.
%
% Describes single transaction line belonging to the
% transaction identified by TxId. For easier use, consider
% using line/5.

:- dynamic(transaction_line/5).

%% line(?Date, ?Debit, ?Credit, ?Amount, ?Desc) is nondet.
%
% Helper predicate to work with transaction lines. Desc is
% in the format 'TxDesc (LineDesc)' when the line description
% is entered. Otherwise it is same as the transaction description. 

line(Date, Debit, Credit, Amount, Desc):-
    transaction_line(TxId, Debit, Credit, Amount, DescLine),
    transaction(TxId, DescTx, Date),
    (
        DescLine = '',
        Desc = DescTx
    ;
        DescLine \= '',
        format(atom(Desc), '~w (~w)', [DescTx, DescLine]) 
    ).

%% read_all(+Directory) is det.
%
% Reads all data from the given directory.
% All .yml files other than accounts.yml, reports.yml
% and settings.yml are considered to contain transaction
% records. Outputs statistics about the final database.

read_all(Directory):-
    clear_database,
    read_accounts(Directory),
    directory_files(Directory, Entries),
    exclude(special_file, Entries, Files),
    include(yaml_file, Files, YamlFiles),    
    maplist(read_transactions(Directory), YamlFiles),
    print_stat.

%% print_stat is det.
%
% Outputs statistics about the current database.
    
print_stat:-
    findall(_, account(_, _, _), Accounts),
    findall(_, transaction(_, _, _), Transactions),
    findall(_, transaction_line(_, _, _, _, _), Lines),
    length(Accounts, AccountCount),
    length(Transactions, TransactionCount),
    length(Lines, LineCount),
    format('total accounts: ~w~n', [ AccountCount ]),
    format('total transactions: ~w~n', [ TransactionCount ]),
    format('total lines: ~w~n', [ LineCount ]).

%% read_transactions(+Directory, +File) is det.
%
% Reads all transactions and lines and asserts
% them from the given file from the given directory.
    
read_transactions(Directory, File):-
    atomic_list_concat([ Directory, File ], '/', Name),
    format('reading ~w~n', [ Name ]),
    yamltiny_read(Name, Docs),
    length(Docs, Count),
    format('asserting ~w transactions~n', [ Count ]),
    maplist(assert_transaction, Docs).

%% read_accounts(+Directory) is det.
%
% Reads the file describing accounts. The file
% name is accounts.yml.

read_accounts(Directory):-
    atomic_list_concat([ Directory, 'accounts.yml' ], '/', Name),
    format('reading ~w~n', [ Name ]),
    yamltiny_read(Name, Docs),
    length(Docs, Count),
    format('asserting ~w accounts~n', [ Count ]),
    maplist(assert_account, Docs).

%% assert_account(+YamlHash) is det.
%
% Asserts the account entry. Must contain
% properties code, name and type. Throws
% invalid_account_entry(YamlHash) when the account
% cannot be asserted.
    
assert_account(hash(Doc)):-
    memberchk(code-Code, Doc),
    memberchk(name-Name, Doc),
    memberchk(type-Type, Doc),
    assert(account(Code, Name, Type)), !.
    
assert_account(Doc):-
    throw(invalid_account_entry(Doc)).

%% assert_transaction(+YamlHash) is det.
%
% Asserts the transaction entry. YamlHash
% must contain properties desc, date and lines.
% Throws invalid_transaction_entry(YamlHash)
% when the transaction cannot be assertes. 
    
assert_transaction(hash(Tx)):-
    gensym(tx, TxId),
    memberchk(desc-Desc, Tx),
    memberchk(date-DateStr, Tx),
    date_term(DateStr, Date),
    assert(transaction(TxId, Desc, Date)),
    memberchk(lines-array(Lines), Tx),
    maplist(assert_line(TxId), Lines), !.
    
assert_transaction(Tx):-
    throw(invalid_transaction_entry(Tx)).

%% assert_line(+TxId, +YamlHash) is det.
%
% Asserts the debit/credit line for the transaction
% given by TxId. YamlHash must contain properties
% debit, credit, sum and may contain property desc.
% Throws invalid_line_entry(YamlHash) when the line
% cannot be asserted. Also applies check_account_exists/1
% for both debit and credit accounts.
    
assert_line(TxId, hash(Line)):-
    memberchk(debit-Debit, Line),
    memberchk(credit-Credit, Line),
    memberchk(sum-Sum, Line),
    check_account_exists(Debit),
    check_account_exists(Credit),
    atom_number(Sum, Amount),
    (memberchk(desc-Desc, Line) -> true; Desc = '' ),
    assert(transaction_line(TxId, Debit, Credit, Amount, Desc)), !.
    
assert_line(_, Line):-
    throw(invalid_line_entry(Line)).

%% check_account_exists(+Account) is det.
%
% Checks that the given account exists.
% Used before asserting the transaction line.
    
check_account_exists(Account):-
    account(Account, _, _), !.
    
check_account_exists(Account):-
    throw(account_does_not_exist(Account)).    

%% clear_database is det.
%
% Clears all account, transaction and
% transaction_line entries.
    
clear_database:-
    format('cleaning database~n'),
    retractall(account(_, _, _)),
    retractall(transaction(_, _, _)),
    retractall(transaction_line(_, _, _, _, _)).
    
special_file('accounts.yml').
special_file('reports.yml').
special_file('settings.yml').
special_file('..').
special_file('.').

yaml_file(Name):-
    atom_concat(_, '.yml', Name).

%% date_term(+Atom, -Term) is det.
%
% Parses ISO8601 date into term Year-Month-Day.

date_term(Atom, Y-M-D):-
    parse_time(Atom, Stamp),
    stamp_date_time(Stamp, DateTime, 'UTC'),
    DateTime = date(Y, M, D, _, _, _, _, _, _).
