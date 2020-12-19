%   Author: Nicholas Hubbard
%   Email:  nhub73@keemail.me
%   WWW:    https://github.com/NicholasBHubbard/rdf_inference_engine

:- autoload(library(semweb/rdf_prefixes)).
:- autoload(library(semweb/rdf11)).
:- autoload(library(semweb/turtle)).

:- [rdfsplus_constructs].


                 /*******************************
                 *      ENTIRE FILE PARSING     *
                 *******************************/

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


                 /*******************************
                 *   CLASS MEMBERSHIP PARSING   *
                 *******************************/

rdfClass_allRdf_members(Class,AllRDF,Members)
    include(rdf_represents_member_of_class(Class),AllRDF,MemberRDF).

%!  rdfClass_allRdf_members(+Class, +AllRDF, -RDFList) 
%
%   True if RDFList is a list containing every triple from AllRDF that is
%   of the form 'rdf(X,rdf:type,Class)'. The difference between this
%   predicate and rdfClass_allRdf_members/3 is that RDFList is a list of
%   complete rdf/3 triples, whereas the former predicate is a list of
%   atoms that represent the X in 'rdf(X,rdf:type,Class)'.

rdfClass_allRdf_membershipRdf(Class,AllRDF,MemberRDF) :-
    include(rdf_represents_member_of_class(Class),AllRDF,MemberRDF).

%!  rdf_represents_member_of_class(+Class, ?RDF) 
%
%   True if RDF is a triple of the form 'rdf(X,rdf:type,Class)'.

rdf_represents_member_of_class(Class,rdf(_,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',Class)).
