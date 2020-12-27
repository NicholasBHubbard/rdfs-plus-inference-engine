%   Author: Nicholas Hubbard
%   Email:  nhub73@keemail.me
%   WWW:    https://github.com/NicholasBHubbard/rdf_inference_engine

:- autoload(library(semweb/rdf11),[rdf/3,rdf_assert/3]).
:- autoload(library(yall),[(>>)/2]).

:- [parsing].


%!  infer_rdfs:subClassOf(+SubClass, +SuperClass)
%
%   For all members X of SubClass assert rdf/3 triples of the form 
%   rdf(X,rdf:type,SuperClass).

:- rdf_meta(infer_rdfs:subClassOf(r,t)).

infer_rdfs:subClassOf(Sub,Super) :-
    rdf:class_members(Sub,SubMembers),
    maplist(assert_x_rdf:type(Super),SubMembers).

%!  infer_rdfs:subPropertyOf(+SubProperty, +SuperProperty)
%
%   For all known rdf/3 triples of the form rdf(X,SubProperty,Y) assert a new 
%   triple rdf(X,SuperProperty,Y).

infer_rdfs:subPropertyOf(Sub,Super) :-
    rdf:property_relatedRdf(Sub,RDFRelatedBySub),
    maplist({Super}/[rdf(X,_,Y)]>>rdf_assert(X,Super,Y),
            RDFRelatedBySub). 

%!  infer_rdfs:domain(+Property, +Domain)
%
%   For all known rdf/3 triples of the form rdf(X,Property,Y) assert a new rdf/3
%   triple of the form rdf(X,rdf:type,Domain).

infer_rdfs:domain(Property,Domain) :-
    property_subjects(Property,Subjects),
    maplist(assert_x_rdf:type(Domain),Subjects).

%!  infer_rdfs:range(+Property, +Range)
%
%   For all known rdf/3 triples of the form rdf(X,Property,Y) assert a new rdf/3
%   triple of the form rdf(Y,rdf:type,Range).

infer_rdfs:range(Property,Range) :-
    property_objects(Property,Objects),
    maplist(assert_x_rdf:type(Range),Objects).


                 /*******************************
                 *            HELPERS           *
                 *******************************/

:- rdf_meta(assert_x_rdf:type(r,t)).

assert_x_rdf:type(Type,X) :-
    rdf_assert(X,rdf:type,Type).                                    
