%   Author: Nicholas Hubbard
%   Email:  nhub73@keemail.me
%   WWW:    https://github.com/NicholasBHubbard/rdf_inference_engine

:- autoload(library(semweb/rdf_prefixes),
            [rdf_register_prefix/2,
             rdf_meta/1
            ]).
:- autoload(library(semweb/rdf11),[rdf/3,rdf_assert/3]).
:- autoload(library(semweb/turtle),[rdf_read_turtle/3]).

/*
  This module deals with all of the parsing of .ttl files. Please note that the
  usage of the words 'RDF' and 'Triple' are used interchangeably. Explain the 
  rdf_meta/1 predicate.
*/


                 /*******************************
                 *        GENERAL PARSING       *
                 *******************************/

%!  parse_and_assert_ttlFile(+File)
%
%   Parse File using library(semweb/turtle)'s rdf_read_turtle/3 which gives a 
%   list of rdf/3 triples. Then use my_rdf_assert_list to assert all of the 
%   parsed triples.

parse_and_assert_ttlFile(File) :-
    rdf_read_turtle(File,AllTriples,[]), % [] means no special options.
    rdf_assert_list(AllTriples).
    
%!  all_known_rdf(-RDFList)
%
%   True if RDFList is every rdf/3 triple that has been asserted.

all_known_rdf(RDFList) :-
    findall(rdf(S,P,O),rdf(S,P,O),RDFList).


                 /*******************************
                 *        CLASS MEMBERSHIP      *
                 *******************************/

%!  rdfClass_allRdf_members(+Class, -Members)
%
%   True if Members is a list containing all the X's from rdf triples of the 
%   form 'rdf(X, rdf:type, Class)'.

:- rdf_meta(rdfsClass_members(r,t)). 
                                     
rdf:class_members(Class,Members) :-
    findall(X,rdf(X,rdf:type,Class),Members).
    

                 /*******************************
                 *           PROPERTIES         *
                 *******************************/

%!  all_related_by_property(+Property,-RDFList)
%
%   True if RDFList contains every known rdf/3 triple of the form 
%   rdf(X,Property,Y).

rdf:property_relatedRdf(Property,RDFList) :-
    findall(rdf(X,Property,Y),rdf(X,Property,Y),RDFList).

%!  property_subjects(+Property, -Subjects)
%
%   True if Subjects is a list of all X's from all known rdf/3 triples of the  
%   form rdf(X,Property,Y).

property_subjects(Property,Subjects) :-
    findall(X,rdf(X,Property,_),Subjects).

                 /*******************************
                 *          MISC HELPERS        *
                 *******************************/

%!  rdf_assert_list(+RDFList)
%
%   Assert every rdf/3 triple in RDFList. 

rdf_assert_list([]).
rdf_assert_list([rdf(S,P,O)|RDFs]) :-
    rdf_assert(S,P,O),
    rdf_assert_list(RDFs).

%!  is_rdfsplus_construct(+RDF).
%
%   True if either the subject, predicate, or object of RDF is an RDFS-Plus 
%   construct.

is_rdfsplus_construct(rdf(S,P,O)) :-
    all_rdfsplus_constructs(Constructs),
    ( memberchk(P,Constructs)
    ; memberchk(O,Constructs)
    ; memberchk(S,Constructs)
    ).

%!  all_rdfsplus_constructs(-List) is det.
%
%   True if List is all of the defined constructs of RDFS-Plus.

all_rdfsplus_constructs(List) :-
    List = ['http://www.w3.org/2000/01/rdf-schema#subClassOf',
            'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',
            'http://www.w3.org/2000/01/rdf-schema#domain',
            'http://www.w3.org/2000/01/rdf-schema#range',
            'http://www.w3.org/2002/07/owl#equivalentClass',
            'http://www.w3.org/2002/07/owl#equivalentProperty',
            'http://www.w3.org/2002/07/owl#FunctionalProperty',
            'http://www.w3.org/2002/07/owl#InverseFunctionalProperty',
            'http://www.w3.org/2002/07/owl#sameAs',
            'http://www.w3.org/2002/07/owl#inverseOf',
            'http://www.w3.org/2002/07/owl#SymmetricProperty',
            'http://www.w3.org/2002/07/owl#TransitiveProperty'].
