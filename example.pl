:- use_module(library(smw)).

:- dynamic(pred/4).
:- dynamic(pred/3).


pred('PTA2', 'has operand', 'Vocal gain').
pred('PTA2', 'has temporal location', 'Last control exam').
pred('Last control exam', 'has objective', 'Left ear').
pred('Left ear', 'has device', 'IC').

% facts
pred(prop123, 'hasType', 'PTA2').
pred(prop123, 'has value', n).
%[...]



















askAnat :-
    % Login in Semantic MediaWiki using login
    login('http://dbns1.npsrr.unipd.it/anatomia_maratonave/api.php', 'ForAPI', '20D9-3lt&FDi', _Token),

    % To submit a query use ask_query
    ask_query(pred, 'http://dbns1.npsrr.unipd.it/anatomia_maratonave', 'Atleti', ['Tempo minimo::>0'], ['Nazione', 'Genere', 'Tempo minimo'], [0, 1000], L),

    write(L), nl.

%  If you have the Prolog export format installed use get_predicates (same syntax),
%  the result will be a set of asserted predicates pred(Subject, Predicate, Object).

gt55Anat :-
    smw:pred(Id, Nazione, Genere, Tempo),
    Tempo \= '"Tempo minimo"',
    atom_number(Tempo, N),
    N > 5.5,
    format('~a (~a, ~a): ~a~n', [Id, Nazione, Genere, N]),
    fail.
gt55Anat.

lt35F :-
    smw:pred(Id, Nazione, Genere, Tempo),
    Tempo \= '"Tempo minimo"',
    atom_number(Tempo, N),
    N < 3.5, Genere = 'F',
    format('~a (~a, ~a): ~a~n', [Id, Nazione, Genere, N]),
    fail.
lt35F.

notITA_Anat :-
    smw:pred(Id, Nazione, Genere, Tempo),
    Nazione \= 'ITA',
    format('~a (~a, ~a): ~a~n', [Id, Nazione, Genere, Tempo]),
    fail.
notITA_Anat.
