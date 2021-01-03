%   Author: Nicholas Hubbard
%   Email:  nhub73@keemail.me
%   WWW:    https://github.com/NicholasBHubbard/rdf_inference_engine

:- autoload(library(semweb/rdf11),[rdf/3,rdf_assert/3]).
:- autoload(library(semweb/rdf_prefixes),[rdf_meta/1]).
:- autoload(library(yall),[(>>)/2]).

:- use_module(inference).
:- use_module(queries).

:- rdf_meta(contradiction_check(t)).

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
                 ~n but is not a member of "~w"',
                 [rdf(Sub,rdfs:subClassOf,Super),ErrorMember,Sub,Super]),
          fail
    ; write("THIS SHOULD NEVER HAPPEN (check_rdfs:subClassOf)"),
      fail
    ).

%!  check_rdfs:subPropertyOf(+Sub, +Super)
%
%   Fail if there exists a rdf/3 triple of the form rdf(X,Sub,Y) but there does 
%   not exist a triple of the form rdf(X,Super,Y).

:- rdf_meta(check_rdfs:subPropertyOf(t)).

check_rdfs:subPropertyOf(Sub,Super) :-
    property_relatedRdf(Sub,RelatedBySub),
    (  maplist({Super}/[rdf(X,_,Y)]>>rdf(X,Super,Y),RelatedBySub)
    -> true
    ;  rdf(SomeX,Sub,SomeY), \+ rdf(SomeX,Super,SomeY)
       -> format(' CONTRADICTION!
                 ~n found the triple "~w"
                 ~n BUT
                 ~n there exists the triple "~w" 
                 ~n yet the triple "~w" is not present',
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
                 ~n yet the triple "~w" is not present',
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
                 ~n yet the triple "~w" is not present',
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

check_rdfs:equivalentClass(Class1,Class2) :-
    check_rdfs:subClassOf(Class1,Class2),
    check_rdfs:subClassOf(Class2,Class1).

%!  check_rdfs:equivalentProperty(+Property1, +Property2)
%
%   Fail is Property1 and Property2 are not rdfs:subProperty's of each other.
%   Error messages happen in 'check_rdfs:subPropertyOf/2'.

check_rdfs:equivalentProperty(Property1,Property2) :-
    check_rdfs:subPropertyOf(Property1,Property2),
    check_rdfs:subPropertyOf(Property2,Property1).

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
    ;  (

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



    
    
