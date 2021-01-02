%   Author: Nicholas Hubbard
%   Email:  nhub73@keemail.me
%   WWW:    https://github.com/NicholasBHubbard/rdf_inference_engine

:- autoload(library(semweb/rdf11),[rdf/3,rdf_assert/3]).
:- autoload(library(semweb/turtle),[rdf_read_turtle/3]).
:- autoload(library(yall),[(>>)/2]).

:- use_module(inference,[infer/1]).
:- use_module(queries).

main(File) :-
    parse_and_assert_ttlFile(File),
    make_all_inferences.
    %% check_for_contradictions,
    %% write_new_file.

%!  parse_and_assert_ttlFile(+File)
%
%   Parse File using library(semweb/turtle)'s rdf_read_turtle/3 which gives a 
%   list of rdf/3 triples. Then use my_rdf_assert_list to assert all of the 
%   parsed triples.

parse_and_assert_ttlFile(File) :-
    rdf_read_turtle(File,AllTriples,[]), % [] means no special options.
    maplist([rdf(S,P,O)]>>rdf_assert(S,P,O),AllTriples).

%!  make_all_inferences.
%
%   make_all_inferences/0 simply seperates out the rdf/3 triples that represent
%   RDFS-Plus constructs into a list and then passes the list to 
%   make_all_inferences/1. 

make_all_inferences :-
    all_known_rdfsplus_triples(RDFSPlus),
    make_all_inferences(RDFSPlus).

make_all_inferences([]).
make_all_inferences([RDF|RDFs]). 
