:- use_module(database).
:- use_module(analysis).
:- use_module(reporting).

main:-
    current_prolog_flag(argv, Argv),
    (append(_, [--,Directory|_], Argv) ->
        read_all(Directory),
        hello
    ;
        usage,
        halt(1)
    ).
    
usage:-
    format('placcounting <directory name>~n').

hello:-
    format('accounting system started.~n').