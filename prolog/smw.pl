:- module(smw, [example/0, login/4, get_predicates/6, get_predicates/7, get_predicates/8, ask_query/6, ask_query/7, ask_query/8]).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(url)).
:- use_module(library(http/http_cookie)).


% e.g.: get_token('http://localhost/wiki/api.php', TokenEncoded, Token)
get_token(URL, Token) :-
    http_post(URL, form([
                        action='query',
                        meta='tokens',
                        type='login',
                        format='json'
                    ]) , Request, []),
    Request = json(L),
    member(M, L),
    M = (query=json([tokens=json([logintoken=Token])])).
    %www_form_encode(Token, TokenEncoded).

% e.g.: login('http://localhost/wiki/api.php', 'Username', 'Pwd', Token).
login(URL, User, Pwd, Token) :-
    get_token(URL, Token),
    %Token='29590a3037d325be70b93fb8258ed29257448cfb%2B%5C',
    http_post(URL, form([
                        action='clientlogin',
                        username=User,
                        format='json',
                        rememberMe=1,
                        loginreturnurl=URL,
                        logintoken = Token,
                        password = Pwd
                    ]) , Request, []),
            (Request = json([clientlogin=json([(status=Status),
                    (message=Msg),
                    (messagecode=_)])|_]) ->
              format('~w: ~w', [Status, Msg]),
              fail
            ;
              Request = json([clientlogin=json([status=Status,
                            username=Username])|_]),
              format('~w: user ~w', [Status, Username])
            ).

get_smw_export(URL, Category, Filters, Printouts, [Offset, Limit], UserPwd, Pred, Format, L) :-
    format(atom(URL1), '~w/api.php', [URL]),
    (UserPwd \= [] ->
        [Username, Pwd] = UserPwd,
        login(URL1, Username, Pwd, _Token)
    ;
        true
    ),
    format(atom(URL2), '~w/index.php?title=Special:Ask', [URL]),
    (Filters = [] ->
        format(atom(CategoryAndFilters), '[[Category:~w]]', [Category])
    ;
        atomic_list_concat(Filters, ']]\r\n[[', FiltersList),
        format(atom(CategoryAndFilters), '[[Category:~w]]\r\n[[~w]]', [Category, FiltersList])
    ),
    atomic_list_concat(Printouts, '\r\n', PrintoutsList),
    http_post(URL2, form([
                        %_action	"submit"
                        q=CategoryAndFilters,
                        po=PrintoutsList,
                        'p[format]'=Format,
                        'p[pname]'=Pred,
                        'p[limit]'=Limit,
                        'p[offset]'=Offset
                    ]) , Response, []),
    (sub_atom(Response, _From, 2, _After, '<!') ->
      write('Error while retrieving predicates: check that you are logged and that the query are correct.'), nl,
      fail
    ;
      atomic_list_concat(L, '\n', Response)
    ).

assert_atom(A) :-
    atom_to_term(A, T, []),
    %format('Asserting ~w~n', [T]),
    assertz(T).

% once logged in the session is maintained
% e.g.: ask_query('http://localhost/wiki', 'Samplings',
%           ['Has Patient=Parent', 'Number=number'],
%           [0, 50], ['Username', 'Pwd',], L).
get_predicates(Pred, URL, Category, Filters, Printouts, L) :-
    get_predicates(Pred, URL, Category, Filters,  Printouts, [0, 100], [], L).
get_predicates(Pred, URL, Category, Filters, Printouts, [Offset, Limit], L) :-
    get_predicates(Pred, URL, Category, Filters, Printouts, [Offset, Limit], [], L).
get_predicates(Pred, URL, Category, Filters, Printouts, [Offset, Limit], UserPwd, L) :-
    get_smw_export(URL, Category, Filters, Printouts, [Offset, Limit], UserPwd, Pred, 'prolog', L) ,
    maplist(assert_atom, L),
    length(L, N),
    format('~w predicates loaded.~n', [N]).

assert_list(Pred, StringList) :-
    atomic_list_concat(L, ',', StringList),
    P =.. [Pred|L],
    assertz(P).

ask_query(Pred, URL, Category, Filters, Printouts, L) :-
    ask_query(Pred, URL, Category, Filters, Printouts, [0, 100], [], L).
ask_query(Pred, URL, Category, Filters, Printouts, [Offset, Limit], L) :-
    ask_query(Pred, URL, Category, Filters, Printouts, [Offset, Limit], [], L).
ask_query(Pred, URL, Category, Filters, Printouts, [Offset, Limit], UserPwd, L) :-
    get_smw_export(URL, Category, Filters, Printouts, [Offset, Limit], UserPwd, Pred, 'csv', L) ,
      maplist(assert_list(Pred), L),
      length(L, N),
      format('~w predicates loaded.~n', [N]).

% e.g.: return get_preds:predicate(S, V, O).
example :-
    write('SMW pack loaded. Short guide:'), nl, nl,
    write('Login in Semantic MediaWiki using login, for example: '), nl,
    write('?- login(''http://localhost/wiki/api.php'', ''Username'', ''Pwd'', _Token).'), nl, nl,
    write('once logged in the session persists a while.'), nl, nl,
    write('To submit a query use ask_query, for example:'), nl,
    write('?- ask_query(pred, ''http://localhost/wiki'', ''Category'', [''Prop1::>50''], [''Prop1'', ''Prop2''], _L).'), nl, nl,
    write('the result will be a set of asserted predicates of the form pred/N where N is the number of printouts + 1 (the subject).'), nl, nl,
    write('If you have the Prolog export format installed use get_predicates (same syntax),'), nl,
    write('the result will be a set of asserted predicates pred(Subject, Predicate, Object).'), nl.

:- initialization(example).
