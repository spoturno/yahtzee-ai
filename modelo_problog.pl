1/6::dado1(1); 1/6::dado1(2); 1/6::dado1(3); 1/6::dado1(4); 1/6::dado1(5); 1/6::dado1(6).
1/6::dado2(1); 1/6::dado2(2); 1/6::dado2(3); 1/6::dado2(4); 1/6::dado2(5); 1/6::dado2(6).
1/6::dado3(1); 1/6::dado3(2); 1/6::dado3(3); 1/6::dado3(4); 1/6::dado3(5); 1/6::dado3(6).
1/6::dado4(1); 1/6::dado4(2); 1/6::dado4(3); 1/6::dado4(4); 1/6::dado4(5); 1/6::dado4(6).
1/6::dado5(1); 1/6::dado5(2); 1/6::dado5(3); 1/6::dado5(4); 1/6::dado5(5); 1/6::dado5(6).

% Helper predicate to check if a value is in a list
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% Yahtzee: 5 of a kind
yahtzee :- dado1(X), dado2(X), dado3(X), dado4(X), dado5(X).

four_of_a_kind :- dado1(X), dado2(X), dado3(X), dado4(X), dado5(Y), X \= Y.
four_of_a_kind :- dado1(X), dado2(X), dado3(X), dado5(X), dado4(Y), X \= Y.
four_of_a_kind :- dado1(X), dado2(X), dado5(X), dado4(X), dado3(Y), X \= Y.
four_of_a_kind :- dado1(X), dado5(X), dado3(X), dado4(X), dado2(Y), X \= Y.
four_of_a_kind :- dado5(X), dado2(X), dado3(X), dado4(X), dado1(Y), X \= Y.
four_of_a_kind :- yahtzee.

full_house :- dado1(X), dado2(X), dado3(X), dado4(A), dado5(A), X \= A.
full_house :- dado1(X), dado2(X), dado3(A), dado4(X), dado5(A), X \= A.
full_house :- dado1(X), dado2(A), dado3(X), dado4(X), dado5(A), X \= A.
full_house :- dado1(A), dado2(X), dado3(X), dado4(X), dado5(A), X \= A.
full_house :- dado1(A), dado2(X), dado3(X), dado4(A), dado5(X), X \= A.
full_house :- dado1(A), dado2(X), dado3(A), dado4(X), dado5(X), X \= A.
full_house :- dado1(A), dado2(A), dado3(X), dado4(X), dado5(X), X \= A.
full_house :- dado1(X), dado2(X), dado3(A), dado4(A), dado5(X), X \= A.
full_house :- dado1(X), dado2(A), dado3(A), dado4(X), dado5(X), X \= A.
full_house :- dado1(X), dado2(A), dado3(X), dado4(A), dado5(X), X \= A.

three_of_a_kind :- dado1(X), dado2(X), dado3(X), dado4(A), dado5(B), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(X), dado2(X), dado3(X), dado4(B), dado5(A), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(X), dado2(X), dado3(B), dado4(X), dado5(A), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(X), dado2(X), dado3(A), dado4(X), dado5(B), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(X), dado2(B), dado3(X), dado4(X), dado5(A), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(X), dado2(A), dado3(X), dado4(X), dado5(B), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(A), dado2(X), dado3(X), dado4(X), dado5(B), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(B), dado2(X), dado3(X), dado4(X), dado5(A), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(A), dado2(X), dado3(X), dado4(B), dado5(X), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(B), dado2(X), dado3(X), dado4(A), dado5(X), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(A), dado2(X), dado3(B), dado4(X), dado5(X), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(B), dado2(X), dado3(A), dado4(X), dado5(X), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(A), dado2(B), dado3(X), dado4(X), dado5(X), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(B), dado2(A), dado3(X), dado4(X), dado5(X), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(X), dado2(X), dado3(A), dado4(B), dado5(X), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(X), dado2(X), dado3(B), dado4(A), dado5(X), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(X), dado2(A), dado3(B), dado4(X), dado5(X), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(X), dado2(B), dado3(A), dado4(X), dado5(X), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(X), dado2(A), dado3(X), dado4(B), dado5(X), X \= A, X \= B, A \= B.
three_of_a_kind :- dado1(X), dado2(B), dado3(X), dado4(A), dado5(X), X \= A, X \= B, A \= B.
three_of_a_kind :- full_house.
three_of_a_kind :- four_of_a_kind.


small_straight :- dado1(X1), dado2(X2), dado3(X3), dado4(X4), dado5(X5),
    (member(1, [X1, X2, X3, X4, X5]), member(2, [X1, X2, X3, X4, X5]), member(3, [X1, X2, X3, X4, X5]), member(4, [X1, X2, X3, X4, X5])
    ;
    member(2, [X1, X2, X3, X4, X5]), member(3, [X1, X2, X3, X4, X5]), member(4, [X1, X2, X3, X4, X5]), member(5, [X1, X2, X3, X4, X5])
    ;
    member(3, [X1, X2, X3, X4, X5]), member(4, [X1, X2, X3, X4, X5]), member(5, [X1, X2, X3, X4, X5]), member(6, [X1, X2, X3, X4, X5])).

large_straight :- dado1(X1), dado2(X2), dado3(X3), dado4(X4), dado5(X5),
    (member(1, [X1, X2, X3, X4, X5]), member(2, [X1, X2, X3, X4, X5]), member(3, [X1, X2, X3, X4, X5]), member(4, [X1, X2, X3, X4, X5]), member(5, [X1, X2, X3, X4, X5])
    ;
    member(2, [X1, X2, X3, X4, X5]), member(3, [X1, X2, X3, X4, X5]), member(4, [X1, X2, X3, X4, X5]), member(5, [X1, X2, X3, X4, X5]), member(6, [X1, X2, X3, X4, X5])).


evidence(dado1(6)).
evidence(dado4(6)).
evidence(dado5(6)).
query(yahtzee).
query(four_of_a_kind).
query(full_house).
query(three_of_a_kind).
query(small_straight).
query(large_straight).
