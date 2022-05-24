%   Author: Nicholas Hubbard
%   Email:  nicholashubbard@posteo.net

:- module(my_prelude,
	  [ head_of/2,                          % ?List, ?Elem
	    tail_of/2,                          % ?List, ?Elem
	    second_elem_of/2,                   % ?List, ?Elem
	    third_elem_of/2,                    % ?List, ?Elem
	    fourth_elem_of/2,                   % ?List, ?Elem
	    fifth_elem_of/2,                    % ?List, ?Elem
	    fifth_elem_of/2,                    % ?List, ?Elem
	    list_index0_elem/3,                 % ?List, ?Index, ?Elem
	    list_index1_elem/3,                 % ?List, ?Index, ?Elem
	    list_length/2,                      % ?List, ?Length:int
	    list_length_minus_1/2,              % ?List, ?Length:int
	    list_partitioned_on/3,              % +Delimiter, +List, -ListOfLists
	    list_splat_on/3,                    % +Delimiter, +List, -ListOfLists
	    list_without_duplicates/2,          % +List, -List
            list_elem_occurrences/3,            % ?elem, ?List, ?Int
	    list_without_last_elem/2,           % ?List, ?List
	    prefix_list/2,                      % ?List, ?List
	    sublist/2,                          % ?List, ?List
	    same_length/3,                      % ?List, ?List, ?List
	    different_lengths/2,                % ?List, ?List
	    different_lengths/3,                % ?List, ?List, ?List
	    lists_chopped_to_equal_length/4,    % ?List, ?List, ?List, ?List
	    lists_zipped/3,                     % ?List, ?List, ?ListOfPairs
	    lists_intersection/3,               % +List, +List, -List
	    list_without_first_n_elems/3,       % ?List, ?Int, ?List
	    lists_disjoint/2,                   % +List, +List
	    list_list_difference/3,             % +List, +List, -List
	    list_cardinality/2,                 % +List, -Int
	    add/3,                              % ?Int, ?Int, ?Int
	    subtract/3,                         % ?Int, ?Int, ?Int
	    multiply/3,                         % ?Int, ?Int, ?Int
	    integer_divide/3,                   % ?Int, ?Int, ?Int
            succ_of/2,                          % ?Int, ?Int
            pred_of/2,                          % ?Int, ?Int
	    factorial_of/2,                     % ?Int, ?Int
	    gcd/3,                              % +Int, +Int, -Int
	    between/3,                          % ?Int, ?Int, ?Int
	    read_file_as_list_of_lines/2,       % +File, -List
	    writeln_with_dot/1,                 % +Term
	    writeln_with_dot/2,                 % +Strem, +Term
            op(900, fx, $-),                   
	    ($-)/1,                             % +Goal
            op(900, fx, $),                     
	    ($)/1,                              % +Goal
            op(900, fx, @),                     
	    (@)/1,                              % +Goal
            op(900, fx, *),                     
	    (*)/1,                              % +Goal
            op(1100,xfy,xor),		        
	    xor/2,                              % +Goal, +Goal
            just/2,                             % ?Term, ?Term
	    number_string_padded/2,             % +RealNumber, -String
	    number_number_of_digits/2,          % +RealNumber, -Integer
	    digit_int/2                         % ?Char, ?Integer
	  ]).
:- use_module(library(clpfd)).
:- use_module(library(error)).
:- use_module(library(reif)).

               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %        LIST ALGORITHMS
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!  head_of(?List, ?Elem) is det.
%
%   True if Elem is the first element of List
%
%   Ex: head_of([1,2,3],1).

head_of([X|_],X).

%!  tail_of(?List1, ?List2) is det.
%
%   True if List2 is the tail of List1.
%
%   Ex: tail_of([1,2,3],[2,3]).

tail_of([_|Xs],Xs).

%!  second_elem_of(?List, ?Elem) is det.
%
%   True if Elem is the second element of List.
%
%   Ex: second_elem([1,2,3],2).

second_elem_of(Xs,X) :-
    tail_of(Xs,Ys1),
    head_of(Ys1,X).

%!  third_elem_of(?List, ?Elem) is det.
%
%   True if Elem is the third element of List.

third_elem_of(Xs,X) :-
    tail_of(Xs,Ys1),
    tail_of(Ys1,Ys2),
    head_of(Ys2,X).

%!  fourth_elem_of(?List, ?Elem) is det.
%
%   True if Elem is the fourth element of List.

fourth_elem_of(Xs,X) :-
    tail_of(Xs,Ys1),
    tail_of(Ys1,Ys2),
    tail_of(Ys2,Ys3),
    head_of(Ys3,X).

%!  fifth_elem_of(?List, ?Elem) is det.
%
%   True if Elem is the fifth element of List.

fifth_elem_of(Xs,X) :-
    tail_of(Xs,Ys1),
    tail_of(Ys1,Ys2),
    tail_of(Ys2,Ys3),
    tail_of(Ys3,Ys4),
    head_of(Ys4,X).

%!  list_index0_elem(?List, ?Index, ?Elem) is semidet.
%
%   True if Elem is the Index'th element of List. Index count starts at 0.

list_index0_elem(List,Index,Elem) :-
    Index #>= 0,
    list_index0_elem_star(List,Index,0,Elem).

list_index0_elem_star([Elem|_],Index,Index,Elem) :- !.
list_index0_elem_star([_|Tail],Index,Counter,Elem) :-
    succ_of(Counter,CounterPlus1),
    list_index0_elem_star(Tail,Index,CounterPlus1,Elem).

%!  list_index1_elem(?List, ?Index, ?Elem) is det.
%
%   True if Elem is the Index'th element of List. Index count starts at 1.

list_index1_elem(List,Index,Elem) :-
    Index #>= 1,
    list_index1_elem_star(List,Index,1,Elem).

list_index1_elem_star([Elem|_],Index,Index,Elem) :- !.
list_index1_elem_star([_|Tail],Index,Counter,Elem) :-
    succ_of(Counter,CounterPlus1),
    list_index1_elem_star(Tail,Index,CounterPlus1,Elem).

%!  list_length(?List, ?Length:int) is det.
%
%   True if Int is the number of elements in List. list_length/2 is
%   just a more declaritive name for the built-in predicate length/2.
%   length/2 has a complex implementation so it is easiest to just
%   call length/2 directly instead of reimplementing.

list_length(List,Length) :- length(List,Length).

%!  list_length_minus_1(?List, ?Length:int) is det.
%
%   True if Int is the number of elements in List subtracted by 1.

list_length_minus_1(List,LengthMinus1) :-
    list_length(List,Length),
    pred_of(Length,LengthMinus1).

%!  list_partitioned_on(+Delimiter,+List,-ListOfLists) is det.
%
%   True if ListOfLists is the result of partitioning List on Delimiter.
%
%   Ex: list_partioned_on(100,[1,2,100,3,4,100,5,6],[[1,2],[3,4],[5,6]].

list_partitioned_on(Delimiter,List,Solution) :-
    must_be(list,List),
    list_partitioned_on_star(Delimiter,List,[],Solution,[]).

list_partitioned_on_star(_,[],Seen,Solution,Saw) :-
    append(Saw,[Seen],Solution).
list_partitioned_on_star(Delim,[X|Xs],Seen,Sol,HaveSaw) :-
    X \= Delim,
    append(Seen,[X],NowSeen),
    list_partitioned_on_star(Delim,Xs,NowSeen,Sol,HaveSaw).
list_partitioned_on_star(Delim,[Delim|Xs],Seen,Sol,Saw) :-
    append(Saw,[Seen],HaveNowSaw),
    list_partitioned_on_star(Delim,Xs,[],Sol,HaveNowSaw).

%!  list_splat_on(+Delimiter,+List,-ListOfLists) is det.
%
%   True if ListOfLists is the result of splitting List on Delimiter.
%   The maximum length of ListOfLists is 2, which is what differentiates
%   this predicate from list_partitioned_on/3. If Delimiter is not an
%   element of List then ListOfLists = [List].
%
%   Ex: list_splat_on(100,[1,2,100,3,4,100,5,6],[[1,2],[3,4,100,5,6]].
%   Ex: list_splat_on(4,[1,2,3],[[1,2,3]].

list_splat_on(Delimiter,List,Solution) :-
    must_be(constant,Delimiter),
    must_be(list,List),
    list_splat_on_star(Delimiter,List,[],Solution).

list_splat_on_star(_,[],Seen,[Seen]).
list_splat_on_star(Delim,[X|Xs],Seen,Solution) :-
    X \= Delim,
    append(Seen,[X],NowSeen),
    list_splat_on_star(Delim,Xs,NowSeen,Solution).
list_splat_on_star(Delim,[Delim|Xs],Seen,[Seen|[Xs]]).

%!  list_without_duplicates(+List1, -List2) is det.
%
%   True if List2 is a sorted list with all the elements of List1 but has
%   no duplicate elements. 
%
%   Ex: list_without_duplicates([1,1,2,2,3,3],[1,2,3]).

list_without_duplicates(List1,List2) :-
    must_be(list,List1),
    list_without_duplicates_star(List1,List2).

list_without_duplicates_star([],[]).
list_without_duplicates_star([X|Xs],Ys) :-
    (memberchk(X,Xs) -> Ys=Acc ; Ys = [X|Acc]),
    list_without_duplicates_star(Xs,Acc).

%!  list_elem_occurrences(?Elem, ?List, ?Int) is det.
%
%   True if Int is the number of Elem's in List. This is not
%   my code, I found it on stack overflow from the user false.

list_elem_occurrences([],_,0).
list_elem_occurrences([X|Xs],Elem,N0) :-
    N0 #>= 0,
    if_(X = Elem, D = 1, D = 0),
    N0 #= N1 + D,
    list_elem_occurrences(Xs,Elem,N1).

%!  list_without_last_elem(?List1, ?List2) is det.
%
%   True if List2 is the same as List1 except it does not contain List1's last
%   element. List2' length is one less than List1's length.

list_without_last_elem(List,Solution) :-
    list_without_last_elem_star(List,[],Solution).
    
list_without_last_elem_star([_],Solution,Solution) :- !.
list_without_last_elem_star([X|Xs],Acc,Solution) :-
    append(Acc,[X],Next),
    list_without_last_elem_star(Xs,Next,Solution).

%!  prefix_list(?List1, ?List2) is nondet.
%
%   True if List1 is a prefix list of List2. [1,2] is a prefix of [1,2,3] but
%   [2,1] is not. [2,3] is also not a prefix list. Any list is a prefix of itself.
%   The empty list is a prefix of any list.

prefix_list([],_).
prefix_list([X|Ps],[X|Ls]) :- prefix_list(Ps,Ls).

%!  sublist(?List1,?List2) is nondet.
%
%   True if List1 occurs as a prefix at any point within List2. [1,2] is a sublist of
%   [1,2,3]. The difference though between prefix_list/2 and this predicated is that
%   [2,3] is a sublist of [1,2,3]. Any list is a sublist of itself. The empty list is
%   a sublist of any list.

sublist(Sub,Xs) :- prefix_list(Sub,Xs).
sublist(Sub,[_|Xs]) :- sublist(Sub,Xs).

%!  same_lengths(?List1, ?List2, ?List3).
%
%   True if List1, List2 and List3 all have the same length.

same_length([],[],[]).
same_length([_|L1s],[_|L2s],[_|L3s]) :-
    same_length(L1s,L2s,L3s).

%!  different_lengths(?List1, ?List2) is semidet.
%
%   True if List1 is a different length than List2.

different_lengths(List1,List2) :-
    list_length(List1,Length1),
    list_length(List2,Length2),
    Length1 \= Length2.

%!  different_lengths(?List1, ?List2, ?List3) is semidet.
%
%   True if List1, List2, and List3 all have different lengths.

different_lengths(List1,List2,List3) :-
    list_length(List1,Length1),
    list_length(List2,Length2),
    list_length(List3,Length3),
    Length1 \= Length2,
    Length1 \= Length3,
    Length2 \= Length3.


%!  lists_chopped_to_equal_length(?List1, ?List2, ?Chopped1:list, ?Chopped2:list) is det.
%
%   True if Chopped1 and Chopped2 are the result of chopping List1 and List2 to
%   the same length by cutting off the end of the longer list.
%
%   Ex: lists_chopped_to_equal_length([1,2],[3,4,5,6],[1,2],[3,4]).

lists_chopped_to_equal_length(List1,List2,List1,List2) :-
    same_length(List1,List2),
    !.
lists_chopped_to_equal_length(List1,List2,[],[]) :-
    ( List1 = [] 
    ; List2 = []
    ),
    !.
lists_chopped_to_equal_length_([X|Xs],[Y|Ys],[X|Acc1],[Y|Acc2]) :-
    lists_chopped_to_equal_length_(Xs,Ys,Acc1,Acc2).

%!  lists_zipped(?List1, ?List2, ?ListOfPairs) is nondet.
%
%   True if ListOfPairs is the result of Zipping List1 and List2. List1 and
%   List2 are first chopped to the same lengths before they are zipped, meaning
%   the length of ListOfPairs is equal to the length of the shorter list.
%
%   Ex: lists_zipped([1,2],[3,4,5,6],[(1,2),(2,3)]).

lists_zipped(L1,L2,Zipped) :- 
    lists_chopped_to_equal_length(L1,L2,Chopped1,Chopped2),
    lists_zipped_star(Chopped1,Chopped2,Zipped).

lists_zipped_star([],[],[]).
lists_zipped_star([X|Xs],[Y|Ys],[(X,Y)|Acc]) :-
    lists_zipped_star(Xs,Ys,Acc).

%!  lists_intersection(+List1, +List2, -CommonElems).
%
%   True if CommonElems is all of the elements that are contained
%   in both List1 and List2.
%
%   Ex: lists_intersection([1,2,3],[2,3,4],[2,3]).

lists_intersection(List1,List2,Intersection) :-
    must_be(list,List1),
    must_be(list,List2),
    lists_intersection_star(List1,List2,Intersection).
	   
lists_intersection_star([],_,[]).
lists_intersection_star(_,[],[]).
lists_intersection_star([X|Xs],Ys,[X|Acc]) :-
    memberchk(X,Ys),
    lists_intersection_star(Xs,Ys,Acc).
lists_intersection_star([X|Xs],Ys,Acc) :-
    \+ memberchk(X,Ys),
    lists_intersection_star(Xs,Ys,Acc).

%!  list_without_first_n_elems(?List1, ?Int, ?List2).
%
%   True if List2 is the result of removing the first Int element
%   from List1.

list_without_first_n_elems(List,N,[]) :-
    list_length(List,Length),
    N #>= Length,
    !.
list_without_first_n_elems(List,N,NewList) :-
    list_without_first_n_elems_star(List,N,NewList).

list_without_first_n_elems_star(List,0,List) :- !.
list_without_first_n_elems_star([_|Xs],N,NewList) :-
    N #> 0,
    NMinus1 #= N - 1,
    list_without_first_n_elems_star(Xs,NMinus1,NewList).

%!  lists_disjoint(+List1, +List2).
%
%   True if List1 and List2 share zero common elements.
%
%   Ex: lists_disjoint([1,2,3],[4,5,6]).

lists_disjoint(List1,List2) :-
    must_be(list,List1),
    must_be(list,List2),
    lists_intersection(List1,List2,[]).
    
%!  list_list_difference(+List1, +List2, -List3) is det.
%
%   True if List3 is all of the elements that are members of List1 but
%   are not elements of List2.
%
%   Ex: list_list_difference([1,2,3],[3,4,5],[1,2]).

list_list_difference([],_,[]).
list_list_difference([X|Xs],Ys,[X|Acc]) :-
    \+ member(X,Ys),
    !,
    list_list_difference(Xs,Ys,Acc).
list_list_difference([X|Xs],Ys,Acc) :-
    member(X,Ys),
    !,
    list_list_difference(Xs,Ys,Acc).

%!  list_cardinality(+List, -Cardinality:int) is det.
%
%   True if Cardinality is the number of unique elements in List.
%
%   Ex: list_cardinality([1,1,2,2,3,3],3).

list_cardinality(List,Cardinality) :-
    list_without_duplicates(List,ListNoDupes),
    list_length(ListNoDupes,Cardinality).

               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %          ARITHMETIC
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!  add(?Integer1, ?Integer2, ?Solution:int).

add(X,Y,Z) :- Z #= X + Y.

%!  subtract(?Integer1, ?Integer2, ?Solution:int).

subtract(X,Y,Z) :- Z #= X - Y.

%!  multiply(?Integer1, ?Integer2, ?Solution:int).

multiply(X,Y,Z) :- Z #= X * Y.

%!  integer_divide(?Integer1, ?Integer2, ?Solution:int).

integer_divide(X,Y,Z) :- Z #= X // Y.


%!  succ_of(?Integer1, ?Integer2) is det.
%
%   True if Integer2 is the result of adding one to Integer1.

succ_of(X,Y) :- Y #= X+1.

%!  pred_of(?Integer1, ?Integer2) is det.
%
%   True if Integer2 is the result of subtracting one to Integer1.

pred_of(X,Y) :- Y #= X-1.

%!  factorial_of(?Integer1, ?Integer2) is det.
%
%   True if Integer2 is the factorial of Integer1.

factorial_of(0,1).
factorial_of(N,Fact) :-
    N #> 0,
    Fact #> 0,
    NMinus1 #= N - 1,
    Fact #= N * Fact1,
    factorial_of(NMinus1,Fact1).

%!  gcd(+Integer1, +Integer2, -Integer3) is det.
%
%   True if Integer3 is the greatest common divisor of
%   Integer1 and Integer2. This is not my code as it comes
%   rosettacode.org.

gcd(X, 0, X):- !.
gcd(0, X, X):- !.
gcd(X, Y, D):- X =< Y, !, Z is Y - X, gcd(X, Z, D).
gcd(X, Y, D):- gcd(Y, X, D).

%!  Between(?Integer1, ?Integer2, ?Integer3) is det.
%
%   True if Integer3 is greater than or equal to Integer1 and
%   is less than or equal to Integer2.

between(Lower,Upper,X) :-
    X #>= Lower,
    X #=< Upper.

               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %              IO
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!  read_file_as_list_of_lines(+File, -List).
%
%   True if List is a list where individual elements of List
%   represent exactly one line of File.

read_file_as_list_of_lines(File,ListOfLines) :-
    open(File,read,Stream),
    stream_as_list_of_lines(Stream,ListOfLines),
    !,
    close(Stream).

stream_as_list_of_lines(Stream,[]) :-
    at_end_of_stream(Stream).
stream_as_list_of_lines(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    stream_as_list_of_lines(Stream,L).

%!  writeln_with_dot(+Term).
%
%   Same behavior as writeln/1 except adds a '.' at the end of the line.

writeln_with_dot(Term) :-
    write(Term),
    write(.),
    nl.

%!  writeln_with_dot(+Strem, +Term).
%
%   Same behavior as writeln/2 except adds a '.' at the end of the line.

writeln_with_dot(Stream,Term) :-
    write(Stream,Term),
    write(Stream,.),
    nl(Stream).

               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %            DEBUGGER
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%   This code comes from the user "false" on StackOverflow.

:- op(900, fx, [@,$,$-]).

%!  $-(+Goal).
%
%   Means only signal exceptions going through this goal.

$-(G_0) :-
   catch(G_0, Ex, ( portray_clause(exception:Ex:G_0), throw(Ex) ) ).

%!  $(+Goal).
%
%   Same as '$-' except also shows call and exit.

$(G_0) :-
   portray_clause(call:G_0),
   $-G_0,
   portray_clause(exit:G_0).

%!  @(+Goal).
%
%   Assures that there is at least one answer, and if not,
%   it is reported and an exception is thrown.

@(G_0) :-
   (   $-G_0
   *-> true
   ;   portray_clause(badfail:G_0),
       throw(goal_failed(G_0))
   ).

%!  *(+Goal).
%
%   Removes the goal. This is for generalizing a program doing program
%   modification/program-slicing in a pure monotonic program.

:- op(950, fy, *).
*(_).

:- op(1100,xfy,xor).		  
xor(A,B) :- A, \+B, !.
xor(A,B) :- \+A, B, !.	


    
    
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               %          MISCELLANEOUS
               %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!  just(?Term, ?Term)
%
%   this predicate is useful for pattern matching with lambdas.

just(X,X).

%!  number_string_padded(+Number, -String) is det.
%
%   Same as number_string except single digit numbers are prepended with a '0'.
%
%   Ex: number_string_padded(2,"02").
%   Ex: number_string_padded(11,"11").

number_string_padded(Number,String) :-
    Number #< 10,
    number_string(Number,S),
    string_concat("0",S,String),
    !.
number_string_padded(Number,String) :-
    Number #>= 10,
    number_string(Number,String),
    !.
number_string_padded(Number,String) :-
    number_string(Number,String),
    !.

%!  number_number_of_digits(+Number, -Integer) is det.
%
%   True if Integer is the amount of digits in Number. Supports both integers and
%   decimal numbers.
%
%   Ex: number_number_of_digits(123,3).
%   Ex: number_number_of_digits(-123.45,5).

number_number_of_digits(Int,NumberOfDigits) :-
    number_string(Int,S),
    string_chars(S,S1),
    exclude((=('.')),S1,S2),
    exclude((=('-')),S2,S3),
    string_length(S3,NumberOfDigits).

% self explanatory

digit_int('0',0).
digit_int('1',1).
digit_int('2',2).
digit_int('3',3).
digit_int('4',4).
digit_int('5',5).
digit_int('6',6).
digit_int('7',7).
digit_int('8',8).
digit_int('9',9).
