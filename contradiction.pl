%   Author: Nicholas Hubbard
%   Email:  nhub73@keemail.me
%   WWW:    https://github.com/NicholasBHubbard/rdf_inference_engine

:- autoload(library(semweb/rdf11),[rdf/3,rdf_assert/3]).
:- autoload(library(semweb/rdf_prefixes),[rdf_meta/1]).
:- autoload(library(yall),[(>>)/2]).
:- autoload(library(error),[resource_error/1]).

:- use_module(inference).
:- use_module(queries).

:- rdf_meta(contradiction_check(t)).

contradiction_check(rdf(Sub,rdfs:subClassOf,Super)) :-
    class_members(Sub,SubMembers),
    (  maplist({Super}/[X]>>rdf(X,rdf:type,Super),SubMembers)
    -> true
    ;  write("error: "), 
    ).

%% contradiction_check(rdf(Sub,rdfs:subPropertyOf,Super)).

%% contradiction_check(rdf(Property,rdfs:domain,Class)).

%% contradiction_check(rdf(Property,rdfs:range,Class)).

%% contradiction_check(rdf(Class1,owl:equivalentClass,Class2)).

%% contradiction_check(rdf(Property1,owl:equivalentProperty,Property2)).

%% contradiction_check(rdf(X,owl:sameAs,Y)).

%% contradiction_check(rdf(Property1,owl:inverseOf,Property2)).

%% contradiction_check(rdf(Property,rdf:type,owl:'FunctionalProperty')).
        
%% contradiction_check(rdf(Property,rdf:type,owl:'InverseFunctionalProperty')).

%% contradiction_check(rdf(Property,rdf:type,owl:'SymmetricProperty')).

%% contradiction_check(rdf(Property,rdf:type,owl:'TransitiveProperty')).
