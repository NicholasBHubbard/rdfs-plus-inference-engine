%   Author: Nicholas Hubbard
%   Email:  nhub73@keemail.me
%   WWW:    https://github.com/NicholasBHubbard/rdf_inference_engine

:- autoload(library(semweb/rdf_prefixes)).
:- autoload(library(semweb/rdf11)).
:- autoload(library(semweb/turtle)).

:- [rdfsplus_constructs].

%!  ttlFile_separatedTriples(+TurtleFile, -RDFSPlus, -Rest).
%
%   True if RDFSPlus is all of the triples from TurtleFile that represent
%   RDFS-Plus construcs and Rest is all of the triples in TurtleFile
%   that do not represent RDFS-Plus constructs.

ttlFile_rdfsplus_rest(TtlFile, RDFSPlus, Rest) :-
    ttlFile_allTriples(TtlFile,AllTriples),
    partition(is_rdfsplus_construct,AllTriples,RDFSPlus,Rest).

%!  ttlFile_allTriples(+TurtleFile, -RDFList).
%
%   True if ListOfTriples is a list containing every triple that
%   is defined in TurtleFile.

ttlFile_allTriples(TtlFile,AllTriples) :- 
    rdf_read_turtle(TtlFile,AllTriples,[]). % [] means no special options.


