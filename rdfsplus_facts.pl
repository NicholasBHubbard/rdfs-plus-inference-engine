%   Author: Nicholas Hubbard
%   Email:  nhub73@keemail.me
%   WWW:    https://github.com/NicholasBHubbard/rdf_inference_engine

%!  all_rdfsplus_constructs(-List)
%
%   True if List is all of the defined constructs of RDFS-Plus

all_rdfsplus_constructs(List) :-
    List = [ 'http://www.w3.org/2000/01/rdf-schema#subClassOf',
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
             'http://www.w3.org/2002/07/owl#TransitivePropert'
           ].
