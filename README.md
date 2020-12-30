# -*- mode:org;mode:auto-fill;fill-column:80 -*-
#+title: RDFS-Plus inference engine
#+author: Nicholas Hubbard

* Overview
Static inference engine powered by SWI-Prolog for the RDFS-Plus modelling language
defined in [[https://workingontologist.org/index.html][Semantic Web for the Working Ontologist]]. 

* Features
  + Static inference engine 
  + Parse [[https://en.wikipedia.org/wiki/Turtle_(syntax)][turtle files]] into pure rdf
  + Performs contradiction analysis
  + /tbd/

* Workflow
Simply run your turtle files against NAME_TBD and you will either get error
messages about all the mistakes you have made, or you will get a new file with a
/.inferred/ extenstion that is a copy of the original file with all
inferred triples appended.

* TODO TODO 
** TODO Modeling Constructs: [12/12]
    - [X] rdfs:subClassOf
    - [X] rdfs:subPropertyOf
    - [X] rdfs:domain
    - [X] rdfs:range
    - [X] owl:equivalentClass
    - [X] owl:equivalentProperty
    - [X] owl:FunctionalProperty
    - [X] owl:InverseFunctionalProperty
    - [X] owl:sameAs
    - [X] owl:inverseOf
    - [X] owl:SymmetricProperty
    - [X] owl:TransitiveProperty

             
