:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_prefixes)).
:- use_module(library(semweb/turtle)).

[rdfsplus_facts].
             
%!  parse_all_triples(+TurtleFile, -ListOfTriples)
%
%   True if ListOfTriples is a list containing every triple that
%   is defined in TurtleFiel.

parse_all_triples(File,Triples) :- 
    rdf_read_turtle(File,Triples,[]).

%!  separate_rdfsplus(+AllTriples, -PairOfLists)
%
%   True if the first list in PairOfLists is all of the RDFS-Plus
%   triples from AllTriples AND the second list in PairOfLists is
%   all of the triples from AllTriples that are not RDFS-Plus.

% separate_rdfsplus(List,(RDFSPlus,Rest)) :-
%     all_rdfsplus_constructs
