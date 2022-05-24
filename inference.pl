%   Author: Nicholas Hubbard
%   Email:  nicholashubbard@posteo.net
%   WWW:    https://github.com/NicholasBHubbard/rdf_inference_engine

:- module(inference,
          [ make_all_inferences/0
          ]).

:- autoload(library(semweb/rdf11),[rdf/3,rdf_assert/3,rdf_reachable/3]).
:- autoload(library(yall),[(>>)/2]).

:- use_module(queries).
:- use_module(my_prelude,[tail_of/2]).


                 /*******************************
                 *     CULMINATING PREDICATES   *
                 *******************************/

make_all_inferences :-
    true.

%   infer(+RDF)
%
%   Used as the culmination of all the individual constructs. Must be passed an
%   rdf/3 triple that represents an RDFS-Plus construct.

:- rdf_meta(infer(t)).

infer(rdf(Sub,rdfs:subClassOf,Super)) :-
    infer_rdfs:subClassOf(Sub,Super), !.

infer(rdf(Sub,rdfs:subPropertyOf,Super)) :-
    infer_rdfs:subPropertyOf(Sub,Super), !.

infer(rdf(Property,rdfs:domain,Class)) :-
    infer_rdfs:domain(Property,Class), !.

infer(rdf(Property,rdfs:range,Class)) :-
    infer_rdfs:range(Property,Class), !.

infer(rdf(Class1,owl:equivalentClass,Class2)) :-
    infer_owl:equivalentClass(Class1,Class2), !.

infer(rdf(Property1,owl:equivalentProperty,Property2)) :-
    infer_owl:equivalentProperty(Property1,Property2), !.

infer(rdf(X,owl:sameAs,Y)) :-
    infer_owl:sameAs(X,Y), !.

infer(rdf(Property1,owl:inverseOf,Property2)) :-
    infer_owl:inverseOf(Property1,Property2), !.

infer(rdf(Property,rdf:type,owl:'FunctionalProperty')) :-
    infer_owl:functionalProperty(Property), !.
        
infer(rdf(Property,rdf:type,owl:'InverseFunctionalProperty')) :-
    infer_owl:inverseFunctionalProperty(Property), !.

infer(rdf(Property,rdf:type,owl:'SymmetricProperty')) :-
    infer_owl:symmetricProperty(Property), !.

infer(rdf(Property,rdf:type,owl:'TransitiveProperty')) :-
    infer_owl:transitiveProperty(Property).


                 /*******************************
                 *         RDFS-Plus Rules      *
                 *******************************/

%!  infer_rdfs:subClassOf(+SubClass, +SuperClass)
%
%   For all members X of SubClass assert a new rdf/3 triple of the form 
%   rdf(X,rdf:type,SuperClass).

infer_rdfs:subClassOf(Sub,Super) :-
    class_members(Sub,SubMembers),
    maplist(assert_x_rdf:type(Super),SubMembers).

%!  infer_rdfs:subPropertyOf(+SubProperty, +SuperProperty)
%
%   For all known rdf/3 triples of the form rdf(X,SubProperty,Y) assert a new 
%   triple rdf(X,SuperProperty,Y).

infer_rdfs:subPropertyOf(Sub,Super) :-
    property_relatedRdf(Sub,RDFRelatedBySub),
    maplist({Super}/[rdf(X,_,Y)]>>rdf_assert(X,Super,Y),RDFRelatedBySub). 

%!  infer_rdfs:domain(+Property, +Domain)
%
%   For all known rdf/3 triples of the form rdf(X,Property,Y) assert a new rdf/3
%   triple of the form rdf(X,rdf:type,Domain).

infer_rdfs:domain(Property,Class) :-
    property_subjects(Property,Subjects),
    maplist(assert_x_rdf:type(Class),Subjects).

%!  infer_rdfs:range(+Property, +Range)
%
%   For all known rdf/3 triples of the form rdf(X,Property,Y) assert a new rdf/3
%   triple of the form rdf(Y,rdf:type,Range).

infer_rdfs:range(Property,Class) :-
    property_objects(Property,Objects),
    maplist(assert_x_rdf:type(Class),Objects).

%!  infer_owl:equivalentClass(+Class1, +Class2)
%
%   For all members X of Class1 assert a new rdf/3 triple of the form
%   rdf(X,rdf:type,Class12 AND for all members Y of Class2 assert a rdf/3 triple
%   of the form rdf(Y,rdf:type,Class1).

infer_owl:equivalentClass(Class1,Class2) :-
    infer_rdfs:subClassOf(Class1,Class2),
    infer_rdfs:subClassOf(Class2,Class1).

%!  infer_owl:equivalentProperty(+Property1, +Property2)
%
%   For all rdf/3 triples of the form rdf(X,Property1,Y) assert a new triple 
%   rdf(X,Property2,Y) AND for all triples of the form rdf(X,Property2,Y) assert
%   a new triple rdf(X,Property1,Y).

infer_owl:equivalentProperty(Property1,Property2) :-
    infer_rdfs:subPropertyOf(Property1,Property2),
    infer_rdfs:subPropertyOf(Property2,Property1).

%!  infer_owl:sameAs(+X, +Y)
%
%   For all rdf/3 triples containing an X assert the same triple but with X
%   replaced by Y. Likewise for all rdf/3 triples containing a Y assert the same
%   triple but with the Y replaced with X.

infer_owl:sameAs(X,Y) :-
    all_rdf_containing(X,XRdf),
    maplist(assert_with_y_insteadof_x(X,Y),XRdf),
    all_rdf_containing(Y,YRdf),
    maplist(assert_with_y_insteadof_x(Y,X),YRdf).

assert_with_y_insteadof_x(X,Y,rdf(X,X,X)) :-
    rdf_assert(Y,Y,Y), !.
assert_with_y_insteadof_x(X,Y,rdf(X,X,O)) :-
    rdf_assert(Y,Y,O), !.
assert_with_y_insteadof_x(X,Y,rdf(S,X,X)) :-
    rdf_assert(S,Y,Y), !.
assert_with_y_insteadof_x(X,Y,rdf(X,P,X)) :-
    rdf_assert(Y,P,Y), !.
assert_with_y_insteadof_x(X,Y,rdf(X,P,O)) :-
    rdf_assert(Y,P,O), !.
assert_with_y_insteadof_x(X,Y,rdf(S,X,O)) :-
    rdf_assert(S,Y,O), !.
assert_with_y_insteadof_x(X,Y,rdf(S,P,X)) :-
    rdf_assert(S,P,Y).

%!  infer_owl:functionalProperty(+Property)
%
%   For every pair of rdf/3 triples of the form rdf(X,Property,Y) and
%   rdf(X,Property,Z), infer that 'Y owl:sameAs Z'.

infer_owl:functionalProperty(Property) :-
    findall(rdf(X,Property,Y),lambda_functional_property(X,Property,Y),_).

lambda_functional_property(X,Property,Y) :- 
    rdf(X,Property,Y),
    rdf(X,Property,Z),
    Y \= Z,
    infer_owl:sameAs(Y,Z).
    
%!  infer_owl:inverseFunctionalProperty(+Property)
%
%   For every pair of rdf/3 triples of the form rdf(Y,Property,X) and
%   rdf(Z,Property,X), infer that 'Y owl:sameAs Z'.

infer_owl:inverseFunctionalProperty(Property) :-
    findall(rdf(Y,Property,X),lambda_inverse_functional(Y,Property,X),_).

lambda_inverse_functional(Y,Property,X) :- 
    rdf(Y,Property,X),
    rdf(Z,Property,X),
    Y \= Z,
    infer_owl:sameAs(Y,Z).
    
%!  infer_owl:inverseOf(+Property1, +Property2)
%
%   For all rdf/3 triples of the form rdf(X,Property1,Y) assert a new triple 
%   rdf(Y,Property2,X).

infer_owl:inverseOf(P1,P2) :-
    property_relatedRdf(P1,RelatedByP1),
    maplist(apply_inverseOf_rules(P2),RelatedByP1).
    
apply_inverseOf_rules(Property,rdf(S,_,O)) :-
    rdf_assert(O,Property,S).

%!  infer_owl:symmetricProperty(+Property)
%
%   Infer 'Property owl:inverseOf Property'.

infer_owl:symmetricProperty(Property) :-
    infer_owl:inverseOf(Property,Property).

%!  infer_owl:transitiveProperty(+Property)
%
%   For all pairs of rdf/3 triples of the form rdf(X,Property,Y) and
%   rdf(Y,Property,Z), infer a new triple of the form rdf(X,Property,Z).

infer_owl:transitiveProperty(Property) :-
    findall(_,rdf_assert_transitive_closure_of(Property),_).

rdf_assert_transitive_closure_of(Property) :-
    rdf(X,Property,_),
    findall(O,rdf_reachable(X,Property,O),Objects0),
    tail_of(Objects0,Objects), % First item is X, but we want the irreflexive 
                               % transitive closure. Without this step we get
                               % the REFLEXIVE transitive closure. 
    maplist(rdf_assert(X,Property),Objects).


                 /*******************************
                 *            HELPERS           *
                 *******************************/

%!  assert_x_rdf:type(+Class, +Member)
%
%   Assert a new rdf/3 triple of the form rdf(Member,rdf:type,Class).

:- rdf_meta(assert_x_rdf:type(r,t)).

assert_x_rdf:type(Class,X) :-
    rdf_assert(X,rdf:type,Class).                                    
