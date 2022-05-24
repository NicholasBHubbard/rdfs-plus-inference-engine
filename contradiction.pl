%   Author: Nicholas Hubbard
%   Email:  nicholashubbard@posteo.net
%   WWW:    https://github.com/NicholasBHubbard/rdf_inference_engine

:- module(contradiction,
          [check_for_contradictions/0
          ]).

:- autoload(library(semweb/rdf11),[rdf/3,rdf_assert/3,rdf_reachable/3]).
:- autoload(library(semweb/rdf_prefixes),[rdf_meta/1]).
:- autoload(library(yall),[(>>)/2]).

:- use_module(queries).
:- use_module(my_prelude,[tail_of/2]).


                 /*******************************
                 *     CULMINATING PREDICATES   *
                 *******************************/

check_for_contradictions :-
    all_known_rdfsplus(RDFSPlus),
    maplist(check,RDFSPlus).

%   check(+RDF)
%
%   Used as the culmination of all the individual constructs. Must be passed an
%   rdf/3 triple that represents an RDFS-Plus construct.

:- rdf_meta(check(t)).

check(rdf(Sub,rdfs:subClassOf,Super)) :-
    check_rdfs:subClassOf(Sub,Super), !.

check(rdf(Sub,rdfs:subPropertyOf,Super)) :-
    check_rdfs:subPropertyOf(Sub,Super), !.

check(rdf(Property,rdfs:domain,Class)) :-
    check_rdfs:domain(Property,Class), !.

check(rdf(Property,rdfs:range,Class)) :-
    check_rdfs:range(Property,Class), !.

check(rdf(Class1,owl:equivalentClass,Class2)) :-
    check_owl:equivalentClass(Class1,Class2), !.

check(rdf(Property1,owl:equivalentProperty,Property2)) :-
    check_owl:equivalentProperty(Property1,Property2), !.

check(rdf(X,owl:sameAs,Y)) :-
    check_owl:sameAs(X,Y), !.

check(rdf(Property1,owl:inverseOf,Property2)) :-
    check_owl:inverseOf(Property1,Property2), !.

check(rdf(Property,rdf:type,owl:'FunctionalProperty')) :-
    check_owl:functionalProperty(Property), !.
        
check(rdf(Property,rdf:type,owl:'InverseFunctionalProperty')) :-
    check_owl:inverseFunctionalProperty(Property), !.

check(rdf(Property,rdf:type,owl:'SymmetricProperty')) :-
    check_owl:symmetricProperty(Property), !.

check(rdf(Property,rdf:type,owl:'TransitiveProperty')) :-
    check_owl:transitiveProperty(Property).


                 /*******************************
                 *       CONSTRUCT ANALYSIS     *
                 *******************************/

%!  check_rdfs:subClassOf(+Sub, +Super)
%
%   Fail if there exists a rdf/3 triple of the form rdf(X,rdf:type,Sub) but
%   there does not exist a triple of the form rdf(X,rdf:type,Super).

:- rdf_meta(check_rdfs:subClassOf(t)).

check_rdfs:subClassOf(Sub,Super) :-
    class_members(Sub,SubMembers),
    (  maplist({Super}/[X]>>rdf(X,rdf:type,Super),SubMembers)
    -> true
    ;  rdf(ErrorMember,rdf:type,Sub), \+ rdf(ErrorMember,rdf:type,Super)
       -> format(' CONTRADICTION!
                 ~n found the triple "~w" 
                 ~n BUT
                 ~n "~w" is a member of "~w" 
                 ~n but is not a member of "~w" ~n',
                 [rdf(Sub,rdfs:subClassOf,Super),ErrorMember,Sub,Super]),
          fail
    ; write("THIS SHOULD NEVER HAPPEN (check_rdfs:subClassOf)"),
      fail
    ).

%!  check_rdfs:subPropertyOf(+Sub, +Super)
%
%   Fail if there exists a rdf/3 triple of the form rdf(X,Sub,Y) but there does 
%   not exist a triple of the form rdf(X,Super,Y).

check_rdfs:subPropertyOf(Sub,Super) :-
    property_relatedRdf(Sub,RelatedBySub),
    (  maplist({Super}/[rdf(X,_,Y)]>>rdf(X,Super,Y),RelatedBySub)
    -> true
    ;  rdf(SomeX,Sub,SomeY), \+ rdf(SomeX,Super,SomeY)
       -> format(' CONTRADICTION!
                 ~n found the triple "~w"
                 ~n BUT
                 ~n there exists the triple "~w" 
                 ~n yet the triple "~w" is not present ~n',
                 [rdf(Sub,rdfs:subClassOf,Super),rdf(SomeX,Sub,SomeY),
                 rdf(SomeX,Super,SomeY)]),
          fail
    ; write("THIS SHOULD NEVER HAPPEN (check_rdfs:subPropertyOf)"),
      fail
    ).
                 
%!  check_rdfs:domain(+Property, +Domain)
%
%   Fail if there exists a rdf/3 triple of the form rdf(X,Property,Y) for which
%   there is no triple of the form rdf(X,rdf:type,Domain).

:- rdf_meta(check_rdfs:domain(t)).

check_rdfs:domain(Property,Domain) :-
    property_relatedRdf(Property,RelatedByProperty),
    (  maplist({Domain}/[rdf(X,_,_)]>>rdf(X,rdf:type,Domain),RelatedByProperty)
    -> true
    ;  rdf(SomeX,Property,SomeY), \+ rdf(SomeX,rdf:type,Domain)
       -> format(' CONTRADICTION!
                 ~n found the triple "~w"
                 ~n BUT
                 ~n there exists the triple "~w" 
                 ~n yet the triple "~w" is not present ~n',
                 [rdf(Property,rdfs:domain,Domain),rdf(SomeX,Property,SomeY),
                  rdf(SomeX,rdf:type,Domain)]),
          fail
    ; write("THIS SHOULD NEVER HAPPEN (check_rdfs:domain)"),
      fail
    ).

%!  check_rdfs:range(+Property, +Range)
%
%   Fail if there exists a rdf/3 triple of the form rdf(X,Property,Y) for which
%   there is no triple of the form rdf(X,rdf:type,Domain).

:- rdf_meta(check_rdfs:range(t)).

check_rdfs:range(Property,Range) :-
    property_relatedRdf(Property,RelatedByProperty),
    (  maplist({Range}/[rdf(_,_,Y)]>>rdf(Y,rdf:type,Range),RelatedByProperty)
    -> true
    ;  rdf(SomeX,Property,SomeY), \+ rdf(SomeY,rdf:type,Range)
       -> format(' CONTRADICTION!
                 ~n found the triple "~w"
                 ~n BUT
                 ~n there exists the triple "~w" 
                 ~n yet the triple "~w" is not present ~n',
                 [rdf(Property,rdfs:range,Range),rdf(SomeX,Property,SomeY),
                  rdf(SomeY,rdf:type,Range)]),
          fail
    ; write("THIS SHOULD NEVER HAPPEN (check_rdfs:range)"),
      fail
    ).

%!  check_rdfs:equivalentClass(+Class1, +Class2)
%
%   Fail if Class1 and Class2 are not rdfs:subClass's of each other. Error 
%   messages happen in 'rdfs:subClassOf/2'.

check_owl:equivalentClass(Class1,Class2) :-
    check_rdfs:subClassOf(Class1,Class2),
    check_rdfs:subClassOf(Class2,Class1).

%!  check_rdfs:equivalentProperty(+Property1, +Property2)
%
%   Fail is Property1 and Property2 are not rdfs:subProperty's of each other.
%   Error messages happen in 'check_rdfs:subPropertyOf/2'.

check_owl:equivalentProperty(Property1,Property2) :-
    check_rdfs:subPropertyOf(Property1,Property2),
    check_rdfs:subPropertyOf(Property2,Property1).

%!  check_owl:inverseOf(+Property1, +Property2)
%
%   Fail if there exists a rdf/3 triple of the form rdf(X,Property1,Y) but there
%   is no triple of the form rdf(Y,Property2,X).

check_owl:inverseOf(P1,P2) :-
    property_relatedRdf(P1,RelatedByP1),
    (  maplist({P2}/[rdf(X,_,Y)]>>rdf(Y,P2,X),RelatedByP1)
    -> true
    ;  rdf(X,P1,Y), \+ rdf(Y,P2,X)
       -> format(' CONTRADICTION!
                 ~n found triple "~w"
                 ~n BUT
                 ~n there exists the triple "~w"
                 ~n yet the triple "~w" is not present ~n',
                 [rdf(P1,owl:inverseOf,P2),rdf(X,P1,Y),rdf(Y,P2,X)]),
          fail
    ; write("THIS SHOULD NEVER HAPPEN (check_owl:range)"),
      fail
    ).

%!  check_owl:symmetricProperty(+Property)
%
%   Fail if Property is not it's own inverse. 

check_owl:symmetricProperty(Property) :-
    check_owl:inverseOf(Property,Property).

%!  check_owl:functionalProperty(+Property)
%
%   Fail if there exists a pair of rdf/3 triples, rdf(X,Property,Y) and
%   rdf(X,Property,Z) in which there is any triple containing a Y without an
%   equivalent triple but with Z in the place of X.

check_owl:functionalProperty(Property) :-
    findall(rdf(X,Property,Y),lambda_check_func_property(X,Property,Y),_).

lambda_check_func_property(X,Property,Y) :-
    rdf(X,Property,Y),
    rdf(X,Property,Z),
    Y \= Z,
    check_owl:sameAs(Y,Z).

%!  check_owl:inverseFunctionalProperty(+Property)
%
%   Fail if there exists a pair of rdf/3 triples, rdf(Y,Property,X), and
%   rdf(Z,Property,X) in which Y is not the sameAs Z.

check_owl:inverseFunctionalProperty(Property) :-
    findall(rdf(X,Property,Y),lambda_check_inv_func_property(X,Property,Y),_).
    
lambda_check_inv_func_property(Y,Property,X) :-
    rdf(Y,Property,X),
    rdf(Z,Property,X),
    Y \= Z,
    check_owl:sameAs(Y,Z).

%!  check_owl:transitiveProperty(+Property)
%
%   Fail if there exists a pair of rdf/3 triples of the form rdf(X,Property,Y) 
%   and rdf(Y,Property,Z) yet there does not exist the triple rdf(X,Property,Z).

check_owl:transitiveProperty(Property) :-
    findall(X,rdf(X,Property,_),PropertySubjects),
    check_transitive_closure_of(PropertySubjects,Property).

check_transitive_closure_of([],_).
check_transitive_closure_of([X|Xs],Property) :-
    (  findall(O,rdf_reachable(X,Property,O),Objects0),
       tail_of(Objects0,Objects), % we want irreflexive transitive closure and
                               % first elem of Objects0 is X.
       maplist(rdf(X,Property),Objects)
    -> true
    ;  format(' CONTRADICTION!
              ~n found triple "~w"
              ~n BUT
              ~n cannot complete the irreflexive transitive closure',
              [rdf(X,Property,_)]),
       fail
    ),
    check_transitive_closure_of(Xs,Property).

%!  check_owl:sameAs(+X, +Y)
%
%   Fail if there is any X in the subject, predicate, or object slot of an 
%   rdf/3 triple in which the equivalent triple with Y replaced by X does not
%   exist. Same statement must also hold in the vica-versa scenario.

check_owl:sameAs(X,Y) :-
    (  all_rdf_containing(X,XContaining),
       maplist(exists_y_equivalent_triple(X,Y),XContaining),
       all_rdf_containing(Y,YContaining),
       maplist(exists_y_equivalent_triple(Y,X),YContaining)
    -> true
    ;  no_y_equivalent_triple(X,Y,RDF1)
       -> format(' CONTRADICTION!
                 ~n found triple "~w"
                 ~n BUT
                 ~n the triple "~w" has no sameAs equivalent ~n',
                 [rdf(X,owl:sameAs,Y),RDF1]),
          fail
       ;  no_y_equivalent_triple(Y,X,RDF2)
          -> format(' CONTRADICTION!
                     ~n found triple "~w"
                     ~n BUT
                     ~n the triple "~w" has no sameAs equivalent ~n',
                    [rdf(X,owl:sameAs,Y),RDF2]),
             fail
    ; write("THIS SHOULD NEVER HAPPEN (check_owl:sameAs)"),
      fail
    ).
    
exists_y_equivalent_triple(X,Y,rdf(X,X,X)) :-
    rdf(Y,Y,Y), !.
exists_y_equivalent_triple(X,Y,rdf(X,X,O)) :-
    rdf(Y,Y,O), !.
exists_y_equivalent_triple(X,Y,rdf(S,X,X)) :-
    rdf(S,Y,Y), !.
exists_y_equivalent_triple(X,Y,rdf(X,P,X)) :-
    rdf(Y,P,Y), !.
exists_y_equivalent_triple(X,Y,rdf(X,P,O)) :-
    rdf(Y,P,O), !.
exists_y_equivalent_triple(X,Y,rdf(S,X,O)) :-
    rdf(S,Y,O), !.
exists_y_equivalent_triple(X,Y,rdf(S,P,X)) :-
    rdf(S,P,Y).

no_y_equivalent_triple(X,Y,RDF) :-
    rdf(X,X,X), \+ rdf(Y,Y,Y), RDF = rdf(X,X,X), !.
no_y_equivalent_triple(X,Y,RDF) :-
    rdf(X,X,O), \+ rdf(Y,Y,O), RDF = rdf(X,X,O), !.
no_y_equivalent_triple(X,Y,RDF) :-
    rdf(S,X,X), \+ rdf(S,Y,Y), RDF = rdf(S,X,X), !.
no_y_equivalent_triple(X,Y,RDF) :-
    rdf(X,P,X), \+ rdf(Y,P,Y), RDF = rdf(X,P,X), !.
no_y_equivalent_triple(X,Y,RDF) :-
    rdf(X,P,O), \+ rdf(Y,P,O), RDF = rdf(X,P,O), !.
no_y_equivalent_triple(X,Y,RDF) :-
    rdf(S,X,O), \+ rdf(S,Y,O), RDF = rdf(S,X,O), !.
no_y_equivalent_triple(X,Y,RDF) :-
    rdf(S,P,X), \+ rdf(S,P,Y), RDF = rdf(S,P,X).
