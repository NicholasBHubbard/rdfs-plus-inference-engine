#!/usr/bin/env -S swipl -q -f none -g main

%   Author: Nicholas Hubbard
%   Email:  nicholashubbard@posteo.net
%   WWW:    https://github.com/NicholasBHubbard/rdf_inference_engine

:- use_module(ttl_read_write).
:- use_module(inference).
:- use_module(contradiction).

main :-
    current_prolog_flag(argv, ARGV),
    main(ARGV),
    halt(0).

main([]).
main([File|Files]) :-
    retractall(rdf(_,_,_)),
    parse_and_assert_ttlFile(File),
    make_all_inferences,
    check_for_contradictions,
    write_new_ttlFile(File),
    main(Files).
