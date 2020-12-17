%   Author: Nicholas Hubbard
%   Email:  nhub73@keemail.me
%   WWW:    https://github.com/NicholasBHubbard/rdf_inference_engine

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_prefixes)).

%!  init_prefixes is det
%
%   These are the only prefixes that need to be initialized for
%   RDFS-Plus. Other prefixes are irrelevant to the inference
%   Engine.

init_prefixes :-
    rdf_register_prefix(rdf,'http://www.w3.org/1999/02/22-rdf-syntax-ns#'),
    rdf_register_prefix(rdfs,'http://www.w3.org/2000/01/rdf-schema#'),
    rdf_register_prefix(owl,'http://www.w3.org/2002/07/owl#').
    
%   

