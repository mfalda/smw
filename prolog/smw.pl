:- module(smw, [example/0, login/4, ask_query/4, ask_query/5, ask_query/6]).

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

assert_atom(A) :-
    atom_to_term(A, T, []),
    %format('Asserting ~w~n', [T]),
    assertz(T).

% once logged in the session is maintained
% e.g.: ask_query('http://localhost/wiki', 'Samplings',
%           ['Has Patient=Parent', 'Number=number'],
%           [0, 50], ['Username', 'Pwd',], L).
ask_query(URL, Category, Printouts, L) :-
    ask_query(URL, Category, Printouts, [0, 100], [], L).
ask_query(URL, Category, Printouts, [Offset, Limit], L) :-
    ask_query(URL, Category, Printouts, [Offset, Limit], [], L).
ask_query(URL, Category, Printouts, [Offset, Limit], UserPwd, L) :-
    atomic_list_concat([URL, 'api.php'], '/', URL1),
    (UserPwd \= [] ->
        [Username, Pwd] = UserPwd,
        login(URL1, Username, Pwd, _Token)
    ; true),
    atomic_list_concat([URL, 'index.php?title=Special:Ask'], '/', URL2),
    atomic_list_concat(['[[Category:', Category, ']]'], '', Category1),
    atomic_list_concat(Printouts, '\r\n', Printouts1),
    http_post(URL2, form([
                       %_action	"submit"
                       q=Category1,
                       po=Printouts1,
                       'p[format]'='prolog',
                       'p[limit]'=Limit,
                       'p[offset]'=Offset
                    ]) , Response, []),
    (sub_atom(Response, _From, 2, _After, '<!') ->
      write('Error while retrieving predicates: check that you are logged and that the query are correct.'), nl,
      fail
    ;
      atomic_list_concat(L, '\n', Response),
      maplist(assert_atom, L),
      length(L, N),
      format('~w predicates loaded.~n', [N])
    ).

% e.g.: return get_preds:predicate(S, V, O).
example :-
    write('Login in Semantic MediaWiki using login, for example: '), nl,
    write('login(''http://localhost/wiki/api.php'', ''Username'', ''Pwd'', _Token).'), nl, nl,
    write('Once logged in the session persists a while.'), nl,
    write('To submit a query use ask_query, for example:'), nl,
    write('ask_query(''http://localhost/wiki'', ''Category'', [''Prop1'', ''Prop2''], _L).'), nl.
