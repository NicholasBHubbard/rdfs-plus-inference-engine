%   Author: Nicholas Hubbard
%   Email:  nicholashubbard@posteo.net
%   WWW:    https://github.com/NicholasBHubbard/rdf_inference_engine

:- module(ttl_read_write,
          [ parse_and_assert_ttlFile/1,   % +File
            write_new_ttlFile/0
          ]).
                         
:- autoload(library(semweb/turtle),[rdf_read_turtle/3]).
:- autoload(library(semweb/rdf_turtle_write),[rdf_save_turtle/2]).
:- autoload(library(semweb/rdf11),[rdf_assert/3]).
:- autoload(library(yall),[(>>)/2]).


%!  parse_and_assert_ttlFile(+File)
%
%   Parse File using library(semweb/turtle)'s rdf_read_turtle/3 which gives a 
%   list of rdf/3 triples.

parse_and_assert_ttlFile(File) :-
    rdf_read_turtle(File,AllTriples,[]), % [] means no special options.
    maplist([rdf(S,P,O)]>>rdf_assert(S,P,O),AllTriples).

%!  write_new_ttlFile(+OutFile)
%
%   Write all known rdf/3 triples to a file named OutFile.

write_new_ttlFile(File) :-
    rdf_save_turtle(OutFile,[]).
