%   Author: Nicholas Hubbard
%   Email:  nhub73@keemail.me
%   WWW:    https://github.com/NicholasBHubbard/rdf_inference_engine

:- module(queries,
          [ all_known_rdf/1,                  % -RDFList
            all_rdf_containing/2,             % +X, -RDFList
            number_of_known_rdf_triples/1,    % -Int
            all_known_rdfsplus_triples/1,     % -RDFList
            class_members/2,                  % +rdfClass, -Members
            property_relatedRdf/2,            % +rdfProperty, -RDFList
            property_subjects/2,              % +rdfProperty, -Subjects
            property_objects/2                % +rdfProperty, -Objects
          ]).

:- autoload(library(semweb/rdf_prefixes),[rdf_meta/1]).
:- autoload(library(semweb/rdf11),[rdf/3]).

:- use_module(my_prelude,[list_length/2]).


                 /*******************************
                 *        GENERAL QUERIES       *
                 *******************************/

%!  all_known_rdf(-RDFList)
%
%   True if RDFList is every rdf/3 triple that has been asserted.

all_known_rdf(RDFList) :-
    findall(rdf(S,P,O),rdf(S,P,O),RDFList).

%!  all_rdf_containing(+X, -RDFList)
%
%   True if RDFList is a list containing all of the known rdf/3 triples that
%   contain X in either it's subject, predicate, or object.

all_rdf_containing(X,RDFList) :-
    all_known_rdf(AllRDF),
    include(rdf_contains(X),AllRDF,RDFList).

rdf_contains(X,rdf(S,P,O)) :-
    ( X = S
    ; X = P
    ; X = O
    ).

%!  number_of_known_rdf_triples(-Int).
%
%   True if int is the number of asserted rdf/3 triples.

number_of_known_rdf_triples(Int) :-
    all_known_rdf(AllRDF),
    list_length(AllRDF,Int).

%!  all_known_rdfsplus_triples(-RDFList)
%
%   True if RDFList is a list containing all the rdf/3 triples that represent
%   RDFS-Plus constructs.

all_known_rdfsplus_triples(RDFList) :-
    all_known_rdf(AllRDF),
    include(is_rdfsplus,AllRDF,RDFList).


                 /*******************************
                 *        CLASS MEMBERSHIP      *
                 *******************************/

%!  class_members(+Class, -Members)
%
%   True if Members is a list containing all the X's from rdf triples of the 
%   form 'rdf(X, rdf:type, Class)'.

:- rdf_meta(rdfsClass_members(r,t)). 
                                     
class_members(Class,Members) :-
    findall(X,rdf(X,rdf:type,Class),Members).
    

                 /*******************************
                 *           PROPERTIES         *
                 *******************************/

%!  property_relatedRdf(+Property,-RDFList)
%
%   True if RDFList contains every known rdf/3 triple of the form 
%   rdf(X,Property,Y).

property_relatedRdf(Property,RDFList) :-
    findall(rdf(X,Property,Y),rdf(X,Property,Y),RDFList).

%!  property_subjects(+Property, -Subjects)
%
%   True if Subjects is a list of all X's from all known rdf/3 triples of the  
%   form rdf(X,Property,_).

property_subjects(Property,Subjects) :-
    findall(X,rdf(X,Property,_),Subjects).

%!  property_objects(+Property, -Objects)
%
%   True if Objects is a list of all Y's from all known rdf/3 triples of the  
%   form rdf(_,Property,Y).

property_objects(Property,Objects) :-
    findall(Y,rdf(_,Property,Y),Objects).


                 /*******************************
                 *          MISC HELPERS        *
                 *******************************/

%!  is_rdfsplus(+RDF)
%
%   True if RDF is an rdf/3 triple representing an RDFS-Plus construct.

:- rdf_meta(is_rdfsplus(t)).

is_rdfsplus(rdf(_,rdfs:subClassOf,_)) :- !.
is_rdfsplus(rdf(_,rdfs:subPropertyOf,_)) :- !.
is_rdfsplus(rdf(_,rdfs:domain,_)) :- !.
is_rdfsplus(rdf(_,rdfs:range,_)) :- !.
is_rdfsplus(rdf(_,owl:equivalentProperty,_)) :- !.
is_rdfsplus(rdf(_,owl:equivalentClass,_)) :- !.
is_rdfsplus(rdf(_,owl:sameAs,_)) :- !.
is_rdfsplus(rdf(_,owl:inverseOf,_)) :- !.
is_rdfsplus(rdf(_,rdf:type,owl:'SymmetricProperty',_)) :- !.
is_rdfsplus(rdf(_,rdf:type,owl:'FunctionalProperty')) :- !.
is_rdfsplus(rdf(_,rdf:type,owl:'InverseFunctionalProperty')) :- !.
is_rdfsplus(rdf(_,rdf:type,owl:'TransitiveProperty')).
