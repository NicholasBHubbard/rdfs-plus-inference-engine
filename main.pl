%   Author: Nicholas Hubbard
%   Email:  nhub73@keemail.me
%   WWW:    https://github.com/NicholasBHubbard/rdf_inference_engine

:- use_module(ttl_read_write).
:- use_module(inference).
:- use_module(contradiction).

main(File) :-
    parse_and_assert_ttlFile(File),
%    make_all_inferences, LAST THING THAT NEEDS TO BE DONE
    check_for_contradictions,
    write_new_ttlFile.
