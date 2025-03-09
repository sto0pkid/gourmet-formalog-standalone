pengines:  ?- listing.
listing.

more_solutions(ID, Choice, State, Time) :-
    pengine_request(Event),
    more_solutions(Event, ID, Choice, State, Time).

pengine_thread(Pengine, Thread) :-
    current_pengine(Pengine,
                    _Parent,
                    Thread,
                    _URL,
                    _Application,
                    _Destroy),
    Thread\==0,
    !.

:- multifile prepare_module/3.


send_message(queue(Queue), Event, _) :-
    thread_send_message(Queue, pengine_request(Event)).
send_message(pengine(Pengine), Event, Options) :-
    (   pengine_remote(Pengine, Server)
    ->  remote_pengine_send(Server,
                            Pengine,
                            Event,
                            Options)
    ;   pengine_thread(Pengine, Thread)
    ->  thread_send_message(Thread, pengine_request(Event))
    ;   existence_error(pengine, Pengine)
    ).

extra_load_options(Pengine, Options) :-
    pengine_not_sandboxed(Pengine),
    !,
    Options=[].
extra_load_options(_, [sandboxed(true)]).

:- multifile prepare_goal/3.


discard_post_data(Request) :-
    option(method(post), Request),
    !,
    setup_call_cleanup(open_null_stream(NULL),
                       http_read_data(Request,
                                      _,
                                      [to(stream(NULL))]),
                       close(NULL)).
discard_post_data(_).

ip_pattern([], []).
ip_pattern([*], _) :-
    !.
ip_pattern([S|T0], [N|T]) :-
    number_string(N, S),
    ip_pattern(T0, T).

local_pengine_create(Options) :-
    thread_self(Self),
    option(application(Application), Options, pengine_sandbox),
    create(Self, Child, Options, local, Application),
    option(alias(Name), Options, Child),
    assert(child(Name, Child)).

:- dynamic pengine_user/2.
:- volatile pengine_user/2.


pengine_remote(Pengine, URL) :-
    current_pengine(Pengine,
                    _Parent,
                    0,
                    URL,
                    _Application,
                    _Destroy).

:- multifile write_result/3.


delay_message(Target, Event, Options) :-
    option(delay(Delay), Options),
    !,
    alarm(Delay,
          send_message(Target, Event, Options),
          _AlarmID,
          [remove(true)]).
delay_message(Target, Event, Options) :-
    true,
    send_message(Target, Event, Options).

var_binding(Bindings, Var, Binding) :-
    member(Binding, Bindings),
    arg(2, Binding, V),
    V==Var,
    !.

json_lang(json) :-
    !.
json_lang(Format) :-
    sub_atom(Format, 0, _, _, 'json-').

:- dynamic pengine_data/2.
:- volatile pengine_data/2.


more_solutions(stop, ID, _Choice, _State, _Time) :-
    !,
    debug(pengine(transition), '~q: 6 = ~q => 7', [ID, stop]),
    destroy_or_continue(stop(ID)).
more_solutions(next, ID, _Choice, _State, Time) :-
    !,
    debug(pengine(transition), '~q: 6 = ~q => 3', [ID, next]),
    statistics(cputime, T0),
    nb_setarg(1, Time, T0),
    fail.
more_solutions(next(Count), ID, _Choice, State, Time) :-
    Count>0,
    !,
    debug(pengine(transition), '~q: 6 = ~q => 3', [ID, next(Count)]),
    nb_setarg(1, State, Count),
    statistics(cputime, T0),
    nb_setarg(1, Time, T0),
    fail.
more_solutions(ask(Goal, Options), ID, Choice, _State, _Time) :-
    !,
    debug(pengine(transition), '~q: 6 = ~q => 3', [ID, ask(Goal)]),
    prolog_cut_to(Choice),
    ask(ID, Goal, Options).
more_solutions(destroy, ID, _Choice, _State, _Time) :-
    !,
    debug(pengine(transition), '~q: 6 = ~q => 1', [ID, destroy]),
    pengine_terminate(ID).
more_solutions(Event, ID, Choice, State, Time) :-
    debug(pengine(transition),
          '~q: 6 = ~q => 6',
          [ID, protocol_error(Event)]),
    pengine_reply(error(ID, error(protocol_error, _))),
    more_solutions(ID, Choice, State, Time).

:- meta_predicate pengine_rpc(+,+,:).

pengine_rpc(URL, Query, M:Options0) :-
    translate_local_sources(Options0, Options1, M),
    (   option(timeout(_), Options1)
    ->  Options=Options1
    ;   setting(time_limit, Limit),
        Options=[timeout(Limit)|Options1]
    ),
    term_variables(Query, Vars),
    Template=..[v|Vars],
    State=destroy(true),
    setup_call_catcher_cleanup(pengine_create(
                                              [ ask(Query),
                                                template(Template),
                                                server(URL),
                                                id(Id)
                                              | Options
                                              ]),
                               wait_event(Template,
                                          State,
                                          [listen(Id)|Options]),
                               Why,
                               pengine_destroy_and_wait(State,
                                                        Id,
                                                        Why)).

:- multifile not_sandboxed/2.


pengine_send2(self, Event, Options) :-
    !,
    thread_self(Queue),
    delay_message(queue(Queue), Event, Options).
pengine_send2(Name, Event, Options) :-
    child(Name, Target),
    !,
    delay_message(pengine(Target), Event, Options).
pengine_send2(Target, Event, Options) :-
    delay_message(pengine(Target), Event, Options).

:- multifile event_to_json/3.


pengine_rpc_output(ID, Term) :-
    output(ID, Term),
    !.
pengine_rpc_output(_ID, Term) :-
    print(Term).

wait_and_output_result(Pengine, Queue, Format, TimeLimit) :-
    (   catch(thread_get_message(Queue,
                                 pengine_event(_, Event),
                                 [timeout(TimeLimit)]),
              Error,
              true)
    ->  (   var(Error)
        ->  debug(pengine(wait), 'Got ~q from ~q', [Event, Queue]),
            ignore(destroy_queue_from_http(Pengine,
                                           Event,
                                           Queue)),
            output_result(Format, Event)
        ;   output_result(Format, died(Pengine))
        )
    ;   time_limit_exceeded(Pengine, Format)
    ).

pengine_rpc(URL, Query) :-
    pengine_rpc(URL, Query, []).

http_pengine_pull_response(Request) :-
    reply_options(Request, [get]),
    !.
http_pengine_pull_response(Request) :-
    http_parameters(Request,
                    [id(ID, []), format(Format, [default(prolog)])]),
    (   (   pengine_queue(ID, Queue, TimeLimit, _)
        ->  true
        ;   output_queue(ID, Queue, _),
            TimeLimit=0
        )
    ->  wait_and_output_result(ID,
                               Queue,
                               Format,
                               TimeLimit)
    ;   http_404([], Request)
    ).

fix_stream(Name) :-
    is_cgi_stream(Name),
    !,
    debug(pengine(stream), '~w is a CGI stream!', [Name]),
    set_stream(user_output, alias(Name)).
fix_stream(_).

remote_pengine_pull_response(BaseURL, ID, Options) :-
    remote_send_rec(BaseURL,
                    pull_response,
                    ID,
                    [],
                    Reply,
                    Options),
    thread_self(Queue),
    pengine_reply(Queue, Reply).

:- dynamic output_queue/3.
:- volatile output_queue/3.

output_queue('007740cc-df85-494e-b1b4-726e1512997e', <message_queue>(0x7f24a002c6d0), 1579493151.2733).
output_queue('8b99d416-e1bc-4a19-b21d-9208d388bb8f', <message_queue>(0x7f24a0035270), 1579492956.76602).
output_queue('c2ab2cf1-dea8-4acf-ac3f-bec0070be82f', <message_queue>(0x7f24a002ebe0), 1579492948.58728).
output_queue('8b2401d6-3582-43a1-99f4-e76a24e708ad', <message_queue>(0x7f24a002d060), 1579492934.54604).

sync_destroy_queue_from_http(ID, Queue) :-
    (   output_queue(ID, Queue, _)
    ->  destroy_queue_if_empty(Queue)
    ;   thread_peek_message(Queue,
                            pengine_event(_, output(_, _)))
    ->  debug(pengine(destroy),
              'Delay destruction of ~p because of output',
              [Queue]),
        get_time(Now),
        asserta(output_queue(ID, Queue, Now))
    ;   message_queue_destroy(Queue),
        asserta(output_queue_destroyed(Queue))
    ).

match_peer(_, Allowed) :-
    memberchk(*, Allowed),
    !.
match_peer(_, []) :-
    !,
    fail.
match_peer(Request, Allowed) :-
    http_peer(Request, Peer),
    debug(pengine(allow), 'Peer: ~q, Allow: ~q', [Peer, Allowed]),
    (   memberchk(Peer, Allowed)
    ->  true
    ;   member(Pattern, Allowed),
        match_peer_pattern(Pattern, Peer)
    ).

process_event(create(_ID, Features), Template, State, Options) :-
    memberchk(answer(First), Features),
    process_event(First, Template, State, Options).
process_event(error(_ID, Error), _Template, _, _Options) :-
    throw(Error).
process_event(failure(_ID, _Time), _Template, _, _Options) :-
    fail.
process_event(prompt(ID, Prompt), Template, State, Options) :-
    pengine_rpc_prompt(ID, Prompt, Reply),
    pengine_send(ID, input(Reply)),
    wait_event(Template, State, Options).
process_event(output(ID, Term), Template, State, Options) :-
    pengine_rpc_output(ID, Term),
    pengine_pull_response(ID, Options),
    wait_event(Template, State, Options).
process_event(debug(ID, Message), Template, State, Options) :-
    debug(pengine(debug), '~w', [Message]),
    pengine_pull_response(ID, Options),
    wait_event(Template, State, Options).
process_event(success(_ID, Solutions, _Proj, _Time, false), Template, _, _Options) :-
    !,
    member(Template, Solutions).
process_event(success(ID, Solutions, _Proj, _Time, true), Template, State, Options) :-
    (   member(Template, Solutions)
    ;   pengine_next(ID, Options),
        wait_event(Template, State, Options)
    ).
process_event(destroy(ID, Event), Template, State, Options) :-
    !,
    retractall(child(_, ID)),
    nb_setarg(1, State, false),
    debug(pengine(destroy), 'State: ~p~n', [State]),
    process_event(Event, Template, State, Options).
process_event(success(ID, Solutions, Time, More), Template, State, Options) :-
    process_event(success(ID,
                          Solutions,
                          _Proj,
                          Time,
                          More),
                  Template,
                  State,
                  Options).

:- meta_predicate pengine_event_loop(1,+).

pengine_event_loop(Closure, Options) :-
    child(_, _),
    !,
    pengine_event(Event),
    (   option(autoforward(all), Options)
    ->  forall(child(_, ID), pengine_send(ID, Event))
    ;   true
    ),
    pengine_event_loop(Event, Closure, Options).
pengine_event_loop(_, _).

http_pengine_abort(Request) :-
    reply_options(Request, [get]),
    !.
http_pengine_abort(Request) :-
    http_parameters(Request,
                    [id(ID, []), format(Format, [default(prolog)])]),
    (   pengine_thread(ID, _Thread),
        pengine_queue(ID, Queue, TimeLimit, _)
    ->  broadcast(pengine(abort(ID))),
        abort_pending_output(ID),
        pengine_abort(ID),
        wait_and_output_result(ID,
                               Queue,
                               Format,
                               TimeLimit)
    ;   http_404([], Request)
    ).

:- thread_local child/2.
:- dynamic child/2.
:- volatile child/2.


pengine_uuid(Id) :-
    uuid(Id, [version(4)]).

reply_options(Request, Allowed) :-
    option(method(options), Request),
    !,
    cors_enable(Request, [methods(Allowed)]),
    format('Content-type: text/plain\r\n'),
    format('~n').

:- multifile prompt/3.


to_atomic(Obj, Atomic) :-
    atom(Obj),
    !,
    Atomic=Obj.
to_atomic(Obj, Atomic) :-
    number(Obj),
    !,
    Atomic=Obj.
to_atomic(Obj, Atomic) :-
    string(Obj),
    !,
    Atomic=Obj.
to_atomic(Obj, Atomic) :-
    term_string(Obj, Atomic).

:- meta_predicate pengine_create(:).

pengine_create(M:Options0) :-
    translate_local_sources(Options0, Options, M),
    (   select_option(server(BaseURL), Options, RestOptions)
    ->  remote_pengine_create(BaseURL, RestOptions)
    ;   local_pengine_create(Options)
    ).

:- multifile authentication_hook/3.


abort_pending_output(Pengine) :-
    forall(pengine_replying(Pengine, Thread),
           abort_output_thread(Thread)).

:- multifile current_application/1.

current_application(pengine_sandbox).
current_application(larkc_pengine_app).

sync_destroy_queue_from_pengine(ID, Queue) :-
    (   retract(output_queue_destroyed(Queue))
    ->  true
    ;   get_time(Now),
        asserta(output_queue(ID, Queue, Now))
    ),
    retractall(pengine_queue(ID, Queue, _, _)).

replace_blobs(Blob, Atom) :-
    blob(Blob, Type),
    Type\==text,
    !,
    format(atom(Atom), '~p', [Blob]).
replace_blobs(Term0, Term) :-
    compound(Term0),
    !,
    compound_name_arguments(Term0, Name, Args0),
    maplist(replace_blobs, Args0, Args),
    compound_name_arguments(Term, Name, Args).
replace_blobs(Term, Term).

ip_term(Peer, Pattern) :-
    split_string(Peer, ".", "", PartStrings),
    ip_pattern(PartStrings, Pattern).

:- dynamic pengine_queue/4.
:- volatile pengine_queue/4.

pengine_queue('2007e915-63e9-4838-8a91-b58145d8a4bc', <message_queue>(0x7f2494010420), 300, 1579493115.8117273).
pengine_queue('40601176-69ab-43e4-b612-8d5cdab8c57a', <message_queue>(0x7f2494014660), 300, 1579493101.0289116).
pengine_queue('75416896-12ce-45bf-becb-997fd36c37b9', <message_queue>(0x7f249400cc50), 300, 1579493086.8165538).
pengine_queue('28ee9535-d3a3-4c13-83fa-11cf359646b6', <message_queue>(0x7f249400ff80), 300, 1579492971.8008718).
pengine_queue('2a1da4d1-6499-4b71-8ecd-3cefbc2aa0ba', <message_queue>(0x7f2490024290), 300, 1579492951.6519272).
pengine_queue('869ce1de-6f3c-40a5-add8-a6fa452e9080', <message_queue>(0x7f24a0030e90), 300, 1579492916.0689569).
pengine_queue('e54f6ab4-8e3c-4fa6-bba3-3f187343f1db', <message_queue>(0x7f24a0033710), 300, 1579492869.5037484).

http_pengine_destroy_all(Request) :-
    reply_options(Request, [get]),
    !.
http_pengine_destroy_all(Request) :-
    http_parameters(Request, [ids(IDsAtom, [])]),
    atomic_list_concat(IDs, ',', IDsAtom),
    forall(member(ID, IDs), pengine_destroy(ID, [force(true)])),
    reply_json("ok").

keep_source(Pengine, ID, SrcText) :-
    get_pengine_application(Pengine, Application),
    setting(Application:debug_info, true),
    !,
    to_string(SrcText, SrcString),
    assertz(pengine_data(Pengine, source(ID, SrcString))).
keep_source(_, _, _).

options_to_dict(Options, Dict) :-
    select_option(ask(Ask), Options, Options1),
    select_option(template(Template), Options1, Options2),
    !,
    no_numbered_var_in(Ask+Template),
    findall(AskString-TemplateString,
            ask_template_to_strings(Ask,
                                    Template,
                                    AskString,
                                    TemplateString),
            [AskString-TemplateString]),
    options_to_dict(Options2, Dict0),
    '.'(Dict0,
        put(_{ask:AskString, template:TemplateString}),
        A),
    Dict=A.
options_to_dict(Options, Dict) :-
    maplist(prolog_option, Options, Options1),
    dict_create(Dict, _, Options1).

match_peer_pattern(Pattern, Peer) :-
    ip_term(Pattern, IP),
    ip_term(Peer, IP),
    !.

copy_flag(Module, Application, Flag) :-
    current_prolog_flag(Application:Flag, Value),
    !,
    set_prolog_flag(Module:Flag, Value).
copy_flag(_, _, _).

create_option_type(ask(_), term).
create_option_type(template(_), term).
create_option_type(application(_), atom).

get_pengine_module(Pengine, Pengine).

pengine_send(Target, Event) :-
    pengine_send(Target, Event, []).

:- meta_predicate findnsols_no_empty(+,?,0,-).

findnsols_no_empty(N, Template, Goal, List) :-
    findnsols(N, Template, Goal, List),
    List\==[].

goal_expansion(random_delay, Expanded) :-
    (   debugging(pengine(delay))
    ->  Expanded=do_random_delay
    ;   Expanded=true
    ).

authenticate(Request, Application, UserOptions) :-
    authentication_hook(Request, Application, User),
    !,
    must_be(ground, User),
    UserOptions=[user(User)].
authenticate(_, _, []).

list_in_module(M, PI) :-
    listing(M:PI).

pengine_unregister(Id) :-
    thread_self(Me),
    (   current_pengine(Id, Queue, Me, http, _, _)
    ->  with_mutex(pengine,
                   sync_destroy_queue_from_pengine(Id, Queue))
    ;   true
    ),
    retractall(current_pengine(Id,
                               _,
                               Me,
                               _,
                               _,
                               _)),
    retractall(pengine_user(Id, _)),
    retractall(pengine_data(Id, _)).

pengine_main_loop(ID) :-
    catch(guarded_main_loop(ID), abort_query, pengine_aborted(ID)).

no_numbered_var_in(Term) :-
    sub_term(Sub, Term),
    subsumes_term('$VAR'(_), Sub),
    !,
    domain_error(numbered_vars_free_term, Term).
no_numbered_var_in(_).

:- meta_predicate solve(+,?,0,+).

solve(Chunk, Template, Goal, ID) :-
    prolog_current_choice(Choice),
    State=count(Chunk),
    statistics(cputime, Epoch),
    Time=time(Epoch),
    nb_current('$variable_names', Bindings),
    filter_template(Template, Bindings, Template2),
    '$current_typein_module'(CurrTypeIn),
    (   '$set_typein_module'(ID),
        call_cleanup(catch(findnsols_no_empty(State,
                                              Template2,
                                              set_projection(Goal,
                                                             Bindings),
                                              Result),
                           Error,
                           true),
                     query_done(Det, CurrTypeIn)),
        arg(1, Time, T0),
        statistics(cputime, T1),
        CPUTime is T1-T0,
        (   var(Error)
        ->  projection(Projection),
            (   var(Det)
            ->  pengine_reply(success(ID,
                                      Result,
                                      Projection,
                                      CPUTime,
                                      true)),
                more_solutions(ID, Choice, State, Time)
            ;   !,
                destroy_or_continue(success(ID,
                                            Result,
                                            Projection,
                                            CPUTime,
                                            false))
            )
        ;   !,
            (   Error==abort_query
            ->  throw(Error)
            ;   destroy_or_continue(error(ID, Error))
            )
        )
    ;   !,
        arg(1, Time, T0),
        statistics(cputime, T1),
        CPUTime is T1-T0,
        destroy_or_continue(failure(ID, CPUTime))
    ).
solve(_, _, _, _).

http_pengine_create(Request) :-
    reply_options(Request, [post]),
    !.
http_pengine_create(Request) :-
    memberchk(content_type(CT), Request),
    sub_atom(CT, 0, _, _, 'application/json'),
    !,
    http_read_json_dict(Request, Dict),
    dict_atom_option(format, Dict, Format, prolog),
    dict_atom_option(application, Dict, Application, pengine_sandbox),
    http_pengine_create(Request,
                        Application,
                        Format,
                        Dict).
http_pengine_create(Request) :-
    Optional=[optional(true)],
    OptString=[string|Optional],
    Form=[format(Format, [default(prolog)]), application(Application, [default(pengine_sandbox)]), chunk(_, [integer, default(1)]), solutions(_, [oneof([all, 
 chunked]), default(chunked)]), ask(_, OptString), template(_, OptString), src_text(_, OptString), disposition(_, OptString), src_url(_, Optional)],
    http_parameters(Request, Form),
    form_dict(Form, Dict),
    http_pengine_create(Request,
                        Application,
                        Format,
                        Dict).

:- dynamic output_queue_destroyed/1.


pengine_property2(self(Id), Id) :-
    current_pengine(Id,
                    _Parent,
                    _Thread,
                    _URL,
                    _Application,
                    _Destroy).
pengine_property2(module(Id), Id) :-
    current_pengine(Id,
                    _Parent,
                    _Thread,
                    _URL,
                    _Application,
                    _Destroy).
pengine_property2(alias(Alias), Id) :-
    child(Alias, Id),
    Alias\==Id.
pengine_property2(thread(Thread), Id) :-
    current_pengine(Id,
                    _Parent,
                    Thread,
                    _URL,
                    _Application,
                    _Destroy),
    Thread\==0.
pengine_property2(remote(Server), Id) :-
    current_pengine(Id,
                    _Parent,
                    0,
                    Server,
                    _Application,
                    _Destroy).
pengine_property2(application(Application), Id) :-
    current_pengine(Id,
                    _Parent,
                    _Thread,
                    _Server,
                    Application,
                    _Destroy).
pengine_property2(destroy(Destroy), Id) :-
    current_pengine(Id,
                    _Parent,
                    _Thread,
                    _Server,
                    _Application,
                    Destroy).
pengine_property2(parent(Parent), Id) :-
    current_pengine(Id,
                    Parent,
                    _Thread,
                    _URL,
                    _Application,
                    _Destroy).
pengine_property2(source(SourceID, Source), Id) :-
    pengine_data(Id, source(SourceID, Source)).

prolog_option(Option0, Option) :-
    create_option_type(Option0, term),
    !,
    Option0=..[Name, Value],
    format(string(String), '~k', [Value]),
    Option=..[Name, String].
prolog_option(Option, Option).

pengine_unregister_remote(Id) :-
    retractall(current_pengine(Id,
                               _Parent,
                               0,
                               _,
                               _,
                               _)).

translate_local_sources(OptionsIn, Options, Module) :-
    translate_local_sources(OptionsIn,
                            Sources,
                            Options2,
                            Module),
    (   Sources==[]
    ->  Options=Options2
    ;   Sources=[Source]
    ->  Options=[src_text(Source)|Options2]
    ;   atomics_to_string(Sources, Source)
    ->  Options=[src_text(Source)|Options2]
    ).

http_pengine_send(Request) :-
    reply_options(Request, [get, post]),
    !.
http_pengine_send(Request) :-
    http_parameters(Request,
                    
                    [ id(ID, [type(atom)]),
                      event(EventString, [optional(true)]),
                      format(Format, [default(prolog)])
                    ]),
    get_pengine_module(ID, Module),
    (   current_module(Module)
    ->  catch(( read_event(Request,
                           EventString,
                           Module,
                           Event0,
                           Bindings),
                fix_bindings(Format,
                             Event0,
                             Bindings,
                             Event1)
              ),
              Error,
              true),
        (   var(Error)
        ->  debug(pengine(event), 'HTTP send: ~p', [Event1]),
            (   pengine_thread(ID, Thread)
            ->  pengine_queue(ID, Queue, TimeLimit, _),
                true,
                broadcast(pengine(send(ID, Event1))),
                thread_send_message(Thread,
                                    pengine_request(Event1)),
                wait_and_output_result(ID,
                                       Queue,
                                       Format,
                                       TimeLimit)
            ;   atom(ID)
            ->  pengine_died(Format, ID)
            ;   http_404([], Request)
            )
        ;   output_result(Format, error(ID, Error))
        )
    ;   debug(pengine(event), 'Pengine module ~q vanished', [Module]),
        discard_post_data(Request),
        pengine_died(Format, ID)
    ).

anon(Name=_) :-
    sub_atom(Name, 0, _, _, '_'),
    sub_atom(Name, 1, 1, _, Next),
    char_type(Next, prolog_var_start).

create(Queue, Child, Options, local, Application) :-
    !,
    pengine_child_id(Child),
    create0(Queue, Child, Options, local, Application).
create(Queue, Child, Options, URL, Application) :-
    pengine_child_id(Child),
    catch(create0(Queue,
                  Child,
                  Options,
                  URL,
                  Application),
          Error,
          create_error(Queue, Child, Error)).

create_wait_and_output_result(Pengine, Queue, Format, TimeLimit, Dict) :-
    get_dict(solutions, Dict, all),
    !,
    between(1, infinite, Page),
    (   catch(thread_get_message(Queue,
                                 pengine_event(_, Event),
                                 [timeout(TimeLimit)]),
              Error,
              true)
    ->  (   var(Error)
        ->  debug(pengine(wait),
                  'Page ~D: got ~q from ~q',
                  [Page, Event, Queue]),
            (   destroy_queue_from_http(Pengine, Event, Queue)
            ->  !,
                output_result(Format, page(Page, Event))
            ;   is_more_event(Event)
            ->  pengine_thread(Pengine, Thread),
                thread_send_message(Thread, pengine_request(next)),
                output_result(Format,
                              page(Page, Event),
                              Dict),
                fail
            ;   !,
                output_result(Format,
                              page(Page, Event),
                              Dict)
            )
        ;   !,
            output_result(Format, died(Pengine))
        )
    ;   !,
        time_limit_exceeded(Pengine, Format)
    ),
    !.
create_wait_and_output_result(Pengine, Queue, Format, TimeLimit, _Dict) :-
    wait_and_output_result(Pengine,
                           Queue,
                           Format,
                           TimeLimit).

form_dict(Form, Dict) :-
    form_values(Form, Pairs),
    dict_pairs(Dict, _, Pairs).

probe(create, URL) :-
    !,
    http_open(URL, Stream, [method(options)]),
    close(Stream).
probe(_, _).

pengine_died(Format, Pengine) :-
    output_result(Format,
                  error(Pengine,
                        error(existence_error(pengine, Pengine), _))).

:- meta_predicate pengine_process_event(+,1,-,+).

pengine_process_event(create(ID, T), Closure, Continue, Options) :-
    debug(pengine(transition), '~q: 1 = /~q => 2', [ID, create(T)]),
    (   select(answer(First), T, T1)
    ->  ignore(call(Closure, create(ID, T1))),
        pengine_process_event(First,
                              Closure,
                              Continue,
                              Options)
    ;   ignore(call(Closure, create(ID, T))),
        Continue=true
    ).
pengine_process_event(output(ID, Msg), Closure, true, _Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 4', [ID, output(Msg)]),
    ignore(call(Closure, output(ID, Msg))),
    pengine_pull_response(ID, []).
pengine_process_event(debug(ID, Msg), Closure, true, _Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 4', [ID, debug(Msg)]),
    ignore(call(Closure, debug(ID, Msg))),
    pengine_pull_response(ID, []).
pengine_process_event(prompt(ID, Term), Closure, true, _Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 5', [ID, prompt(Term)]),
    ignore(call(Closure, prompt(ID, Term))).
pengine_process_event(success(ID, Sol, _Proj, _Time, More), Closure, true, _) :-
    debug(pengine(transition),
          '~q: 3 = /~q => 6/2',
          [ID, success(Sol, More)]),
    ignore(call(Closure, success(ID, Sol, More))).
pengine_process_event(failure(ID, _Time), Closure, true, _Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 2', [ID, failure]),
    ignore(call(Closure, failure(ID))).
pengine_process_event(error(ID, Error), Closure, Continue, _Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 2', [ID, error(Error)]),
    (   call(Closure, error(ID, Error))
    ->  Continue=true
    ;   forall(child(_, Child), pengine_destroy(Child)),
        throw(Error)
    ).
pengine_process_event(stop(ID), Closure, true, _Options) :-
    debug(pengine(transition), '~q: 7 = /~q => 2', [ID, stop]),
    ignore(call(Closure, stop(ID))).
pengine_process_event(destroy(ID, Event), Closure, Continue, Options) :-
    pengine_process_event(Event, Closure, _, Options),
    pengine_process_event(destroy(ID),
                          Closure,
                          Continue,
                          Options).
pengine_process_event(destroy(ID), Closure, true, _Options) :-
    retractall(child(_, ID)),
    debug(pengine(transition), '~q: 1 = /~q => 0', [ID, destroy]),
    ignore(call(Closure, destroy(ID))).

rebind_cycles(@(Reply, Bindings), Reply) :-
    is_list(Bindings),
    !,
    maplist(bind, Bindings).
rebind_cycles(Reply, Reply).

to_string(String, String) :-
    string(String),
    !.
to_string(Atom, String) :-
    atom_string(Atom, String),
    !.

consider_queue_gc :-
    predicate_property(output_queue(_, _, _),
                       number_of_clauses(N)),
    N>100,
    (   last_gc(Time),
        get_time(Now),
        Now-Time>5*60
    ->  true
    ;   \+ last_gc(_)
    ).

:- dynamic current_pengine/6.
:- volatile current_pengine/6.

current_pengine('2007e915-63e9-4838-8a91-b58145d8a4bc', <message_queue>(0x7f2494010420), <thread>(19,0x7f24a4030e70), http, pengine_sandbox, "false").
current_pengine('40601176-69ab-43e4-b612-8d5cdab8c57a', <message_queue>(0x7f2494014660), <thread>(18,0x7f24a402f4a0), http, pengine_sandbox, true).
current_pengine('75416896-12ce-45bf-becb-997fd36c37b9', <message_queue>(0x7f249400cc50), <thread>(10,0x7f24a40266d0), http, pengine_sandbox, true).
current_pengine('28ee9535-d3a3-4c13-83fa-11cf359646b6', <message_queue>(0x7f249400ff80), <thread>(13,0x7f24a4027f00), http, pengine_sandbox, "false").
current_pengine('2a1da4d1-6499-4b71-8ecd-3cefbc2aa0ba', <message_queue>(0x7f2490024290), <thread>(12,0x7f24a4024b00), http, pengine_sandbox, true).
current_pengine('869ce1de-6f3c-40a5-add8-a6fa452e9080', <message_queue>(0x7f24a0030e90), <thread>(17,0x7f24a402cb30), http, pengine_sandbox, true).
current_pengine('e54f6ab4-8e3c-4fa6-bba3-3f187343f1db', <message_queue>(0x7f24a0033710), <thread>(16,0x7f24a402b180), http, pengine_sandbox, true).

create0(Queue, Child, Options, URL, Application) :-
    (   current_application(Application)
    ->  true
    ;   existence_error(pengine_application, Application)
    ),
    (   URL\==http
    ->  aggregate_all(count, child(_, _), Count),
        setting(Application:slave_limit, Max),
        (   Count>=Max
        ->  throw(error(resource_error(max_pengines), _))
        ;   true
        )
    ;   true
    ),
    partition(pengine_create_option,
              Options,
              PengineOptions,
              RestOptions),
    thread_create_in_pool(Application,
                          pengine_main(Queue,
                                       PengineOptions,
                                       Application),
                          ChildThread,
                          [at_exit(pengine_done)|RestOptions]),
    option(destroy(Destroy), PengineOptions, true),
    pengine_register_local(Child,
                           ChildThread,
                           Queue,
                           URL,
                           Application,
                           Destroy),
    thread_send_message(ChildThread, pengine_registered(Child)),
    (   option(id(Id), Options)
    ->  Id=Child
    ;   true
    ).

pengine_src_text(Src, Module) :-
    pengine_self(Self),
    format(atom(ID), 'pengine://~w/src', [Self]),
    extra_load_options(Self, Options),
    setup_call_cleanup(open_chars_stream(Src, Stream),
                       load_files(Module:ID,
                                  
                                  [ stream(Stream),
                                    module(Module),
                                    silent(true)
                                  | Options
                                  ]),
                       close(Stream)),
    keep_source(Self, ID, Src).

remote_pengine_create(BaseURL, Options) :-
    partition(pengine_create_option,
              Options,
              PengineOptions0,
              RestOptions),
    (   option(ask(Query), PengineOptions0),
        \+ option(template(_Template), PengineOptions0)
    ->  PengineOptions=[template(Query)|PengineOptions0]
    ;   PengineOptions=PengineOptions0
    ),
    options_to_dict(PengineOptions, PostData),
    remote_post_rec(BaseURL,
                    create,
                    PostData,
                    Reply,
                    RestOptions),
    arg(1, Reply, ID),
    (   option(id(ID2), Options)
    ->  ID=ID2
    ;   true
    ),
    option(alias(Name), Options, ID),
    assert(child(Name, ID)),
    (   (   functor(Reply, create, _)
        ;   functor(Reply, output, _)
        )
    ->  option(application(Application),
               PengineOptions,
               pengine_sandbox),
        option(destroy(Destroy), PengineOptions, true),
        pengine_register_remote(ID,
                                BaseURL,
                                Application,
                                Destroy)
    ;   true
    ),
    thread_self(Queue),
    pengine_reply(Queue, Reply).

pengine_ask(ID, Query, Options) :-
    partition(pengine_ask_option,
              Options,
              AskOptions,
              SendOptions),
    pengine_send(ID, ask(Query, AskOptions), SendOptions).

send_error(error(Formal, context(prolog_stack(Frames), Message))) :-
    is_list(Frames),
    !,
    with_output_to(string(Stack),
                   print_prolog_backtrace(current_output, Frames)),
    pengine_self(Self),
    replace_blobs(Formal, Formal1),
    replace_blobs(Message, Message1),
    pengine_reply(error(Self,
                        error(Formal1,
                              context(prolog_stack(Stack),
                                      Message1)))).
send_error(Error) :-
    pengine_self(Self),
    replace_blobs(Error, Error1),
    pengine_reply(error(Self, Error1)).

pengine_request(Request) :-
    pengine_self(Self),
    get_pengine_application(Self, Application),
    setting(Application:idle_limit, IdleLimit),
    thread_self(Me),
    (   thread_get_message(Me,
                           pengine_request(Request),
                           [timeout(IdleLimit)])
    ->  true
    ;   Request=destroy
    ).

process_create_option(Application, src_text(Text)) :-
    !,
    pengine_src_text(Text, Application).
process_create_option(Application, src_url(URL)) :-
    !,
    pengine_src_url(URL, Application).
process_create_option(_, _).

read_prolog_reply(In, Reply) :-
    set_stream(In, encoding(utf8)),
    read(In, Reply0),
    rebind_cycles(Reply0, Reply).

prep_module(Module, Application, Options) :-
    maplist(copy_flag(Module, Application), [var_prefix]),
    forall(prepare_module(Module, Application, Options), true),
    setup_call_cleanup('$set_source_module'(OldModule, Module),
                       maplist(process_create_option(Module),
                               Options),
                       '$set_source_module'(OldModule)).

:- dynamic last_gc/1.


translate_local_source(src_predicates(PIs), Source, M) :-
    must_be(list, PIs),
    with_output_to(string(Source),
                   maplist(list_in_module(M), PIs)).
translate_local_source(src_list(Terms), Source, _) :-
    must_be(list, Terms),
    with_output_to(string(Source),
                   forall(member(Term, Terms),
                          format('~k .~n', [Term]))).
translate_local_source(src_text(Source), Source, _).

pengine_not_sandboxed(ID) :-
    pengine_user(ID, User),
    pengine_property(ID, application(App)),
    not_sandboxed(User, App),
    !.

empty_queue :-
    pengine_parent(Queue),
    empty_queue(Queue, 0, Discarded),
    debug(pengine(abort), 'Abort: discarded ~D messages', [Discarded]).

wrap_first_answer(ID, Event0, CreateEvent) :-
    wrap_first_answer_in_create_event(CreateEvent,
                                      [answer(Event0)]),
    arg(1, CreateEvent, ID),
    !,
    retract(wrap_first_answer_in_create_event(CreateEvent,
                                              [answer(Event0)])).
wrap_first_answer(_ID, Event, Event).

pengine_main(Parent, Options, Application) :-
    fix_streams,
    thread_get_message(pengine_registered(Self)),
    nb_setval(pengine_parent, Parent),
    pengine_register_user(Options),
    set_prolog_flag(mitigate_spectre, true),
    catch(in_temporary_module(Self,
                              pengine_prepare_source(Application,
                                                     Options),
                              pengine_create_and_loop(Self,
                                                      Application,
                                                      Options)),
          prepare_source_failed,
          pengine_terminate(Self)).

get_pengine_application(Pengine, Application) :-
    current_pengine(Pengine,
                    _Parent,
                    _,
                    _URL,
                    Application,
                    _Destroy),
    !.

pengine_register_user(Options) :-
    option(user(User), Options),
    !,
    pengine_self(Me),
    asserta(pengine_user(Me, User)).
pengine_register_user(_).

:- public pengine_done/0.

pengine_done :-
    thread_self(Me),
    (   thread_property(Me, status(exception('$aborted')))
    ->  pengine_self(Pengine),
        pengine_reply(destroy(Pengine, abort(Pengine))),
        thread_detach(Me)
    ;   true
    ),
    forall(child(_Name, Child), pengine_destroy(Child)),
    pengine_self(Id),
    pengine_unregister(Id).

create_error(Queue, Child, Error) :-
    pengine_reply(Queue, error(Child, Error)).

:- multifile output/2.


template_bindings(Var, Bindings, A, B) :-
    var(Var),
    C=A,
    !,
    D=C,
    (   var_binding(Bindings, Var, Binding),
        E=D
    ->  E=[Binding|B]
    ;   D=B
    ).
template_bindings([H|T], Bindings, A, B) :-
    !,
    C=A,
    template_bindings(H, Bindings, C, D),
    template_bindings(T, Bindings, D, B).
template_bindings(Compoound, Bindings, A, B) :-
    compound(Compoound),
    !,
    compound_name_arguments(Compoound, _, Args),
    C=A,
    template_bindings(Args, Bindings, C, B).
template_bindings(_, _, A, A).

pengine_destroy(ID) :-
    pengine_destroy(ID, []).

pengine_send(Target, Event, Options) :-
    must_be(atom, Target),
    pengine_send2(Target, Event, Options).

output_result(Format, Event) :-
    arg(1, Event, Pengine),
    thread_self(Thread),
    setup_call_cleanup(asserta(pengine_replying(Pengine, Thread),
                               Ref),
                       catch(output_result(Format, Event, _{}),
                             pengine_abort_output,
                             true),
                       erase(Ref)).

ask(ID, Goal, Options) :-
    catch(prepare_goal(ID, Goal, Goal1, Options),
          Error,
          true),
    !,
    (   var(Error)
    ->  option(template(Template), Options, Goal),
        option(chunk(N), Options, 1),
        solve(N, Template, Goal1, ID)
    ;   pengine_reply(error(ID, Error)),
        guarded_main_loop(ID)
    ).

dict_to_options(Dict, Application, CreateOptions) :-
    dict_pairs(Dict, _, Pairs),
    pairs_create_options(Pairs, Application, CreateOptions).

pengine_src_url(URL, Module) :-
    pengine_self(Self),
    uri_encoded(path, URL, Path),
    format(atom(ID), 'pengine://~w/url/~w', [Self, Path]),
    extra_load_options(Self, Options),
    (   get_pengine_application(Self, Application),
        setting(Application:debug_info, false)
    ->  setup_call_cleanup(http_open(URL, Stream, []),
                           ( set_stream(Stream, encoding(utf8)),
                             load_files(Module:ID,
                                        
                                        [ stream(Stream),
                                          module(Module)
                                        | Options
                                        ])
                           ),
                           close(Stream))
    ;   setup_call_cleanup(http_open(URL, TempStream, []),
                           ( set_stream(TempStream, encoding(utf8)),
                             read_string(TempStream, _, Src)
                           ),
                           close(TempStream)),
        setup_call_cleanup(open_chars_stream(Src, Stream),
                           load_files(Module:ID,
                                      
                                      [ stream(Stream),
                                        module(Module)
                                      | Options
                                      ]),
                           close(Stream)),
        keep_source(Self, ID, Src)
    ).

ask_template_to_strings(Ask, Template, AskString, TemplateString) :-
    numbervars(Ask+Template, 0, _),
    WOpts=[numbervars(true), ignore_ops(true), quoted(true)],
    format(string(AskTemplate),
           '~W\n~W',
           [Ask, WOpts, Template, WOpts]),
    split_string(AskTemplate, "\n", "", [AskString, TemplateString]).

pengine_debug(Format, Args) :-
    pengine_parent(Queue),
    pengine_self(Self),
    catch(safe_goal(format(atom(_), Format, Args)),
          E,
          true),
    (   var(E)
    ->  format(atom(Message), Format, Args)
    ;   message_to_string(E, Message)
    ),
    pengine_reply(Queue, debug(Self, Message)).

pengine_terminate(ID) :-
    pengine_reply(destroy(ID)),
    thread_self(Me),
    thread_detach(Me).

abort_output_thread(Thread) :-
    catch(thread_signal(Thread, throw(pengine_abort_output)),
          error(existence_error(thread, _), _),
          true).

empty_queue(Queue, C0, C) :-
    thread_get_message(Queue, _Term, [timeout(0)]),
    !,
    C1 is C0+1,
    empty_queue(Queue, C1, C).
empty_queue(_, C, C).

filter_template(Template0, Bindings, Template) :-
    is_dict(Template0, swish_default_template),
    !,
    dict_create(Template, swish_default_template, Bindings).
filter_template(Template, _Bindings, Template).

pengine_reply(_Queue, _Event0) :-
    nb_current(pengine_idle_limit_exceeded, true),
    !.
pengine_reply(Queue, Event0) :-
    arg(1, Event0, ID),
    wrap_first_answer(ID, Event0, Event),
    true,
    debug(pengine(event), 'Reply to ~p: ~p', [Queue, Event]),
    (   pengine_self(ID)
    ->  get_pengine_application(ID, Application),
        setting(Application:idle_limit, IdleLimit),
        debug(pengine(reply),
              'Sending ~p, timout: ~q',
              [Event, IdleLimit]),
        (   thread_send_message(Queue,
                                pengine_event(ID, Event),
                                [timeout(IdleLimit)])
        ->  true
        ;   thread_self(Me),
            debug(pengine(reply),
                  'pengine_reply: timeout for ~q (thread ~q)',
                  [ID, Me]),
            nb_setval(pengine_idle_limit_exceeded, true),
            thread_detach(Me),
            abort
        )
    ;   thread_send_message(Queue, pengine_event(ID, Event))
    ).

http_pengine_create(Request, Application, Format, Dict) :-
    current_application(Application),
    !,
    allowed(Request, Application),
    authenticate(Request, Application, UserOptions),
    dict_to_options(Dict, Application, CreateOptions0),
    append(UserOptions, CreateOptions0, CreateOptions),
    pengine_uuid(Pengine),
    message_queue_create(Queue, [max_size(25)]),
    setting(Application:time_limit, TimeLimit),
    get_time(Now),
    asserta(pengine_queue(Pengine, Queue, TimeLimit, Now)),
    broadcast(pengine(create(Pengine,
                             Application,
                             CreateOptions))),
    create(Queue,
           Pengine,
           CreateOptions,
           http,
           Application),
    create_wait_and_output_result(Pengine,
                                  Queue,
                                  Format,
                                  TimeLimit,
                                  Dict),
    gc_abandoned_queues.
http_pengine_create(_Request, Application, Format, _Dict) :-
    Error=existence_error(pengine_application, Application),
    pengine_uuid(ID),
    output_result(Format, error(ID, error(Error, _))).

pengine_abort(Name) :-
    (   child(Name, Pengine)
    ->  true
    ;   Pengine=Name
    ),
    (   pengine_remote(Pengine, Server)
    ->  remote_pengine_abort(Server, Pengine, [])
    ;   pengine_thread(Pengine, Thread),
        debug(pengine(abort), 'Signalling thread ~p', [Thread]),
        catch(thread_signal(Thread, throw(abort_query)), _, true)
    ).

pengine_register_local(Id, Thread, Queue, URL, Application, Destroy) :-
    asserta(current_pengine(Id,
                            Queue,
                            Thread,
                            URL,
                            Application,
                            Destroy)).

is_more_event(success(_Id, _Answers, _Projection, _Time, true)).
is_more_event(create(_, Options)) :-
    memberchk(answer(Event), Options),
    is_more_event(Event).

:- meta_predicate pengine_event_loop(+,1,+).

pengine_event_loop(Event, Closure, Options) :-
    pengine_process_event(Event,
                          Closure,
                          Continue,
                          Options),
    (   Continue==true
    ->  pengine_event_loop(Closure, Options)
    ;   true
    ).

set_projection(Goal, Bindings) :-
    b_setval('$variable_names', Bindings),
    call(Goal).

form_values([], []).
form_values([H|T], Pairs) :-
    arg(1, H, Value),
    nonvar(Value),
    !,
    functor(H, Name, _),
    Pairs=[Name-Value|PairsT],
    form_values(T, PairsT).
form_values([_|T], Pairs) :-
    form_values(T, Pairs).

pengine_application(Application) :-
    throw(error(context_error(nodirective,
                              pengine_application(Application)),
                _)).

translate_local_sources([], [], [], _).
translate_local_sources([H0|T], [S0|S], Options, M) :-
    nonvar(H0),
    translate_local_source(H0, S0, M),
    !,
    translate_local_sources(T, S, Options, M).
translate_local_sources([H|T0], S, [H|T], M) :-
    translate_local_sources(T0, S, T, M).

pengine_next(ID, Options) :-
    select_option(chunk(Count), Options, Options1),
    !,
    pengine_send(ID, next(Count), Options1).
pengine_next(ID, Options) :-
    pengine_send(ID, next, Options).

query_done(true, CurrTypeIn) :-
    '$set_typein_module'(CurrTypeIn).

allowed(Request, Application) :-
    setting(Application:allow_from, Allow),
    match_peer(Request, Allow),
    setting(Application:deny_from, Deny),
    \+ match_peer(Request, Deny),
    !.
allowed(Request, _Application) :-
    memberchk(request_uri(Here), Request),
    throw(http_reply(forbidden(Here))).

current_pengine_application(Application) :-
    current_application(Application).

pengine_respond(Pengine, Input, Options) :-
    pengine_send(Pengine, input(Input), Options).

pengine_stop(ID, Options) :-
    pengine_send(ID, stop, Options).

fix_streams :-
    fix_stream(current_output).

destroy_queue_if_empty(Queue) :-
    thread_peek_message(Queue, _),
    !.
destroy_queue_if_empty(Queue) :-
    retractall(output_queue(_, Queue, _)),
    message_queue_destroy(Queue).

projection(Projection) :-
    nb_current('$variable_names', Bindings),
    !,
    maplist(var_name, Bindings, Projection).
projection([]).

pengine_ask_option(template(_)).
pengine_ask_option(chunk(_)).
pengine_ask_option(bindings(_)).
pengine_ask_option(breakpoints(_)).

:- thread_local wrap_first_answer_in_create_event/2.
:- dynamic wrap_first_answer_in_create_event/2.
:- volatile wrap_first_answer_in_create_event/2.


:- dynamic pengine_replying/2.


server_url(Server, Action, Params, URL) :-
    uri_components(Server, Components0),
    uri_query_components(Query, Params),
    uri_data(path, Components0, Path0),
    atom_concat('pengine/', Action, PAction),
    directory_file_path(Path0, PAction, Path),
    uri_data(path, Components0, Path, Components),
    uri_data(search, Components, Query),
    uri_components(URL, Components).

pengine_event(Event, Options) :-
    thread_self(Self),
    option(listen(Id), Options, _),
    (   thread_get_message(Self,
                           pengine_event(Id, Event),
                           Options)
    ->  true
    ;   Event=timeout
    ),
    update_remote_destroy(Event).

is_destroy_event(destroy(_)).
is_destroy_event(destroy(_, _)).
is_destroy_event(create(_, Options)) :-
    memberchk(answer(Event), Options),
    is_destroy_event(Event).

event_term_to_json_data(Event, JSON, Lang) :-
    event_to_json(Event, JSON, Lang),
    !.
event_term_to_json_data(success(ID, Bindings0, Projection, Time, More), json{data:Bindings, event:success, id:ID, more:More, projection:Projection, time:Time}
}, json) :-
    !,
    term_to_json(Bindings0, Bindings).
event_term_to_json_data(destroy(ID, Event), json{data:JSON, event:destroy, id:ID}, Style) :-
    !,
    event_term_to_json_data(Event, JSON, Style).
event_term_to_json_data(create(ID, Features0), JSON, Style) :-
    !,
    (   select(answer(First0), Features0, Features1)
    ->  event_term_to_json_data(First0, First, Style),
        Features=[answer(First)|Features1]
    ;   Features=Features0
    ),
    dict_create(JSON, json, [event(create), id(ID)|Features]).
event_term_to_json_data(destroy(ID, Event), json{data:JSON, event:destroy, id:ID}, Style) :-
    !,
    event_term_to_json_data(Event, JSON, Style).
event_term_to_json_data(error(ID, ErrorTerm), Error, _Style) :-
    !,
    Error0=json{data:Message, event:error, id:ID},
    add_error_details(ErrorTerm, Error0, Error),
    message_to_string(ErrorTerm, Message).
event_term_to_json_data(failure(ID, Time), json{event:failure, id:ID, time:Time}, _) :-
    !.
event_term_to_json_data(EventTerm, json{event:F, id:ID}, _) :-
    functor(EventTerm, F, 1),
    !,
    arg(1, EventTerm, ID).
event_term_to_json_data(EventTerm, json{data:JSON, event:F, id:ID}, _) :-
    functor(EventTerm, F, 2),
    arg(1, EventTerm, ID),
    arg(2, EventTerm, Data),
    term_to_json(Data, JSON).

pairs_create_options([], _, []) :-
    !.
pairs_create_options([N-V0|T0], App, [Opt|T]) :-
    Opt=..[N, V],
    pengine_create_option(Opt),
    N\==user,
    !,
    (   create_option_type(Opt, atom)
    ->  atom_string(V, V0)
    ;   V=V0
    ),
    pairs_create_options(T0, App, T).
pairs_create_options([_|T0], App, T) :-
    pairs_create_options(T0, App, T).

template(_, Template, Options0, Options) :-
    select_option(template(Template), Options0, Options),
    !.
template(Bindings, Template, Options, Options) :-
    dict_create(Template, swish_default_template, Bindings).

pengine_user(User) :-
    pengine_self(Me),
    pengine_user(Me, User).

pengine_input(Prompt, Term) :-
    pengine_self(Self),
    pengine_parent(Parent),
    pengine_reply(Parent, prompt(Self, Prompt)),
    pengine_request(Request),
    (   Request=input(Input)
    ->  Term=Input
    ;   Request==destroy
    ->  abort
    ;   throw(error(protocol_error, _))
    ).

pengine_rpc_prompt(ID, Prompt, Term) :-
    prompt(ID, Prompt, Term0),
    !,
    Term=Term0.
pengine_rpc_prompt(_ID, Prompt, Term) :-
    setup_call_cleanup(prompt(Old, Prompt),
                       read(Term),
                       prompt(_, Old)).

pengine_create_and_loop(Self, Application, Options) :-
    setting(Application:slave_limit, SlaveLimit),
    CreateEvent=create(Self, [slave_limit(SlaveLimit)|Extra]),
    (   option(ask(Query0), Options)
    ->  asserta(wrap_first_answer_in_create_event(CreateEvent,
                                                  Extra)),
        (   string(Query0)
        ->  (   option(template(TemplateS), Options)
            ->  Ask2=Query0-TemplateS
            ;   Ask2=Query0
            ),
            catch(ask_to_term(Ask2,
                              Self,
                              Query,
                              Template,
                              Bindings),
                  Error,
                  true),
            (   var(Error)
            ->  true
            ;   send_error(Error),
                throw(prepare_source_failed)
            )
        ;   Query=Query0,
            option(template(Template), Options, Query),
            option(bindings(Bindings), Options, [])
        ),
        option(chunk(Chunk), Options, 1),
        pengine_ask(Self,
                    Query,
                    
                    [ template(Template),
                      chunk(Chunk),
                      bindings(Bindings)
                    ])
    ;   Extra=[],
        pengine_reply(CreateEvent)
    ),
    pengine_main_loop(Self).

http_pengine_ping(Request) :-
    reply_options(Request, [get]),
    !.
http_pengine_ping(Request) :-
    http_parameters(Request,
                    
                    [ id(Pengine, []),
                      format(Format, [default(prolog)])
                    ]),
    (   pengine_thread(Pengine, Thread),
        catch(thread_statistics(Thread, Stats), _, fail)
    ->  output_result(Format, ping(Pengine, Stats))
    ;   output_result(Format, died(Pengine))
    ).

remote_post_rec(Server, Action, Data, Reply, Options) :-
    server_url(Server, Action, [], URL),
    probe(Action, URL),
    http_open(URL, Stream, [post(json(Data))|Options]),
    call_cleanup(read_prolog_reply(Stream, Reply),
                 close(Stream)).

remote_pengine_abort(BaseURL, ID, Options) :-
    remote_send_rec(BaseURL, abort, ID, [], Reply, Options),
    thread_self(Queue),
    pengine_reply(Queue, Reply).

destroy_queue_from_http(ID, _, Queue) :-
    output_queue(ID, Queue, _),
    !,
    destroy_queue_if_empty(Queue).
destroy_queue_from_http(ID, Event, Queue) :-
    debug(pengine(destroy), 'DESTROY? ~p', [Event]),
    is_destroy_event(Event),
    !,
    message_queue_property(Queue, size(Waiting)),
    debug(pengine(destroy),
          'Destroy ~p (waiting ~D)',
          [Queue, Waiting]),
    with_mutex(pengine, sync_destroy_queue_from_http(ID, Queue)).

gc_abandoned_queues :-
    consider_queue_gc,
    !,
    get_time(Now),
    (   output_queue(_, Queue, Time),
        Now-Time>15*60,
        retract(output_queue(_, Queue, Time)),
        message_queue_destroy(Queue),
        fail
    ;   retractall(last_gc(_)),
        asserta(last_gc(Now))
    ).
gc_abandoned_queues.

remote_send_rec(Server, Action, ID, [event=Event], Reply, Options) :-
    !,
    server_url(Server, Action, [id=ID], URL),
    http_open(URL,
              Stream,
              [post(prolog(Event))|Options]),
    call_cleanup(read_prolog_reply(Stream, Reply),
                 close(Stream)).
remote_send_rec(Server, Action, ID, Params, Reply, Options) :-
    server_url(Server,
               Action,
               [id=ID|Params],
               URL),
    http_open(URL, Stream, Options),
    call_cleanup(read_prolog_reply(Stream, Reply),
                 close(Stream)).

remote_pengine_send(BaseURL, ID, Event, Options) :-
    remote_send_rec(BaseURL,
                    send,
                    ID,
                    [event=Event],
                    Reply,
                    Options),
    thread_self(Queue),
    pengine_reply(Queue, Reply).

:- meta_predicate pengine_prepare_source(:,+).

pengine_prepare_source(Module:Application, Options) :-
    setting(Application:program_space, SpaceLimit),
    set_module(Module:program_space(SpaceLimit)),
    delete_import_module(Module, user),
    add_import_module(Module, Application, start),
    catch(prep_module(Module, Application, Options),
          Error,
          true),
    (   var(Error)
    ->  true
    ;   send_error(Error),
        throw(prepare_source_failed)
    ).

disable_client_cache :-
    format('Cache-Control: no-cache, no-store, must-revalidate\r\nPragma: no-cache\r\nExpires: 0\r\n').

pengine_create_option(src_text(_)).
pengine_create_option(src_url(_)).
pengine_create_option(application(_)).
pengine_create_option(destroy(_)).
pengine_create_option(ask(_)).
pengine_create_option(template(_)).
pengine_create_option(bindings(_)).
pengine_create_option(chunk(_)).
pengine_create_option(alias(_)).
pengine_create_option(user(_)).

pengine_property(Id, Prop) :-
    nonvar(Id),
    nonvar(Prop),
    pengine_property2(Id, Prop),
    !.
pengine_property(Id, Prop) :-
    pengine_property2(Prop, Id).

prepare_goal(ID, Goal0, Module:Goal, Options) :-
    option(bindings(Bindings), Options, []),
    b_setval('$variable_names', Bindings),
    (   prepare_goal(Goal0, Goal1, Options)
    ->  true
    ;   Goal1=Goal0
    ),
    get_pengine_module(ID, Module),
    setup_call_cleanup('$set_source_module'(Old, Module),
                       expand_goal(Goal1, Goal),
                       '$set_source_module'(_, Old)),
    (   pengine_not_sandboxed(ID)
    ->  true
    ;   get_pengine_application(ID, App),
        setting(App:safe_goal_limit, Limit),
        catch(call_with_time_limit(Limit,
                                   safe_goal(Module:Goal)),
              E,
              true)
    ->  (   var(E)
        ->  true
        ;   E=time_limit_exceeded
        ->  throw(error(sandbox(time_limit_exceeded, Limit), _))
        ;   throw(E)
        )
    ).

do_random_delay :-
    Delay is random(20)/1000,
    sleep(Delay).

pengine_destroy_and_wait(destroy(true), Id, Why) :-
    !,
    debug(pengine(rpc), 'Destroying RPC because of ~p', [Why]),
    pengine_destroy(Id),
    wait_destroy(Id, 10).
pengine_destroy_and_wait(_, _, Why) :-
    debug(pengine(rpc), 'Not destroying RPC (~p)', [Why]).

destroy_event(destroy(_)).
destroy_event(destroy(_, _)).
destroy_event(create(_, Features)) :-
    memberchk(answer(Answer), Features),
    !,
    nonvar(Answer),
    destroy_event(Answer).

fix_bindings(Format, ask(Goal, Options0), Bindings, ask(Goal, NewOptions)) :-
    json_lang(Format),
    !,
    exclude(anon, Bindings, NamedBindings),
    template(NamedBindings, Template, Options0, Options1),
    select_option(chunk(Paging), Options1, Options2, 1),
    NewOptions=[template(Template), chunk(Paging), bindings(NamedBindings)|Options2].
fix_bindings(_, Command, _, Command).

:- public add_error_details/3.

add_error_details(Error, JSON0, JSON) :-
    add_error_code(Error, JSON0, JSON1),
    add_error_location(Error, JSON1, JSON).

wait_event(Template, State, Options) :-
    pengine_event(Event, Options),
    debug(pengine(event), 'Received ~p', [Event]),
    process_event(Event, Template, State, Options).

pengine_aborted(ID) :-
    thread_self(Self),
    debug(pengine(abort), 'Aborting ~p (thread ~p)', [ID, Self]),
    empty_queue,
    destroy_or_continue(abort(ID)).

update_remote_destroy(Event) :-
    destroy_event(Event),
    arg(1, Event, Id),
    pengine_remote(Id, _Server),
    !,
    pengine_unregister_remote(Id).
update_remote_destroy(_).

pengine_pull_response(Pengine, Options) :-
    pengine_remote(Pengine, Server),
    !,
    remote_pengine_pull_response(Server, Pengine, Options).
pengine_pull_response(_ID, _Options).

pengine_output(Term) :-
    pengine_self(Me),
    pengine_reply(output(Me, Term)).

pengine_register_remote(Id, URL, Application, Destroy) :-
    thread_self(Queue),
    asserta(current_pengine(Id,
                            Queue,
                            0,
                            URL,
                            Application,
                            Destroy)).

dict_atom_option(Key, Dict, Atom, Default) :-
    (   get_dict(Key, Dict, String)
    ->  atom_string(Atom, String)
    ;   Atom=Default
    ).

output_result(prolog, Event, _) :-
    !,
    format('Content-type: text/x-prolog; charset=UTF-8~n~n'),
    write_term(Event,
               
               [ quoted(true),
                 ignore_ops(true),
                 fullstop(true),
                 blobs(portray),
                 portray_goal(portray_blob),
                 nl(true)
               ]).
output_result(Lang, Event, Dict) :-
    write_result(Lang, Event, Dict),
    !.
output_result(Lang, Event, _) :-
    json_lang(Lang),
    !,
    (   event_term_to_json_data(Event, JSON, Lang)
    ->  cors_enable,
        disable_client_cache,
        reply_json(JSON)
    ;   assertion(event_term_to_json_data(Event, _, Lang))
    ).
output_result(Lang, _Event, _) :-
    domain_error(pengine_format, Lang).

:- public portray_blob/2.

portray_blob(Blob, _Options) :-
    blob(Blob, Type),
    writeq('$BLOB'(Type)).

pengine_reply(Event) :-
    pengine_parent(Queue),
    pengine_reply(Queue, Event).

destroy_or_continue(Event) :-
    arg(1, Event, ID),
    (   pengine_property(ID, destroy(true))
    ->  thread_self(Me),
        thread_detach(Me),
        pengine_reply(destroy(ID, Event))
    ;   pengine_reply(Event),
        garbage_collect,
        trim_stacks,
        guarded_main_loop(ID)
    ).

wait_destroy(Id, _) :-
    \+ child(_, Id),
    !.
wait_destroy(Id, N) :-
    pengine_event(Event, [listen(Id), timeout(10)]),
    !,
    (   destroy_event(Event)
    ->  retractall(child(_, Id))
    ;   succ(N1, N)
    ->  wait_destroy(Id, N1)
    ;   debug(pengine(rpc), 'RPC did not answer to destroy ~p', [Id]),
        pengine_unregister_remote(Id),
        retractall(child(_, Id))
    ).

var_name(Name=_, Name).

add_error_location(error(_, file(Path, Line, -1, _CharNo)), Term0, Term) :-
    atom(Path),
    integer(Line),
    !,
    '.'(Term0,
        put(_{location:_{file:Path, line:Line}}),
        A),
    Term=A.
add_error_location(error(_, file(Path, Line, Ch, _CharNo)), Term0, Term) :-
    atom(Path),
    integer(Line),
    integer(Ch),
    !,
    '.'(Term0,
        put(_{ location:_{ ch:Ch,
                           file:Path,
                           line:Line
                         }
             }),
        A),
    Term=A.
add_error_location(_, Term, Term).

pengine_event(Event) :-
    pengine_event(Event, []).

pengine_parent(Parent) :-
    nb_getval(pengine_parent, Parent).

pengine_destroy(Name, Options) :-
    (   child(Name, ID)
    ->  true
    ;   ID=Name
    ),
    option(force(true), Options),
    !,
    (   pengine_thread(ID, Thread)
    ->  catch(thread_signal(Thread, abort),
              error(existence_error(thread, _), _),
              true)
    ;   true
    ).
pengine_destroy(ID, _) :-
    catch(pengine_send(ID, destroy),
          error(existence_error(pengine, ID), _),
          retractall(child(_, ID))).

read_event(_Request, EventString, Module, Event, Bindings) :-
    nonvar(EventString),
    !,
    term_string(Event,
                EventString,
                [variable_names(Bindings), module(Module)]).
read_event(Request, _EventString, Module, Event, Bindings) :-
    option(method(post), Request),
    http_read_data(Request,
                   Event,
                   
                   [ content_type('application/x-prolog'),
                     module(Module),
                     variable_names(Bindings)
                   ]).

add_error_code(error(existence_error(Type, Obj), _), Error0, Error) :-
    atom(Type),
    !,
    to_atomic(Obj, Value),
    '.'(Error0,
        put(_{arg1:Type, arg2:Value, code:existence_error}),
        A),
    Error=A.
add_error_code(error(Formal, _), Error0, Error) :-
    callable(Formal),
    !,
    functor(Formal, Code, _),
    '.'(Error0, put(code, Code), A),
    Error=A.
add_error_code(_, Error, Error).

time_limit_exceeded(Pengine, Format) :-
    call_cleanup(pengine_destroy(Pengine, [force(true)]),
                 output_result(Format,
                               destroy(Pengine,
                                       error(Pengine,
                                             time_limit_exceeded)))).

bind(Var=Value) :-
    Var=Value.

guarded_main_loop(ID) :-
    pengine_request(Request),
    (   Request=destroy
    ->  debug(pengine(transition), '~q: 2 = ~q => 1', [ID, destroy]),
        pengine_terminate(ID)
    ;   Request=ask(Goal, Options)
    ->  debug(pengine(transition), '~q: 2 = ~q => 3', [ID, ask(Goal)]),
        ask(ID, Goal, Options)
    ;   debug(pengine(transition), '~q: 2 = ~q => 2', [ID, protocol_error]),
        pengine_reply(error(ID, error(protocol_error, _))),
        guarded_main_loop(ID)
    ).

pengine_child_id(Child) :-
    (   nonvar(Child)
    ->  true
    ;   pengine_uuid(Child)
    ).

ask_to_term(Ask-Template, Module, Ask1, Template1, Bindings) :-
    !,
    format(string(AskTemplate), 't((~s),(~s))', [Template, Ask]),
    term_string(t(Template1, Ask1),
                AskTemplate,
                [variable_names(Bindings0), module(Module)]),
    phrase(template_bindings(Template1, Bindings0), Bindings).
ask_to_term(Ask, Module, Ask1, Template, Bindings1) :-
    term_string(Ask1,
                Ask,
                [variable_names(Bindings), module(Module)]),
    exclude(anon, Bindings, Bindings1),
    dict_create(Template, swish_default_template, Bindings1).

pengine_self(Id) :-
    thread_self(Thread),
    current_pengine(Id,
                    _Parent,
                    Thread,
                    _URL,
                    _Application,
                    _Destroy).
true.

pengines:  ?- [1;75r[J[19C[ content_type('application/x-prolog'),[37D
module(Module),[15D
variable_names(Bindings)[26D
]).

add_error_code(error(existence_error(Type, Obj), _), Error0, Error) :-atom(Type),
to_atomic(Obj, Value),[22D
'.'(Error0,[7D
put(_{arg1:Type, arg2:Value, code:existence_error}),[52D
A),[7D
Error=A.
add_error_code(error(Formal, _), Error0, Error) :-[46D
callable(Formal),[17D

functor(Formal, Code, _),[25D
'.'(Error0, put(code, Code), A),[32D
Error=A.
add_error_code(_, Error, Error).

time_limit_exceeded(Pengine, Format) :-[35D
call_cleanup(pengine_destroy(Pengine, [force(true)]),[40D
output_result(Format,[7D
destroy(Pengine,[8D
error(Pengine,[8D
time_limit_exceeded)))).

bind(Var=Value) :-[14D
Var=Value.

guarded_main_loop(ID) :-[20D
pengine_request(Request),[25D
(   Request=destroy[19D
->  debug(pengine(transition), '~q: 2 = ~q => 1', [ID, destroy]),[61D
pengine_terminate(ID)[25D
;   Request=ask(Goal, Options)[30D
->  debug(pengine(transition), '~q: 2 = ~q => 3', [ID, ask(Goal)]),[63D
ask(ID, Goal, Options)[26D
;   debug(pengine(transition), '~q: 2 = ~q => 2', [ID, protocol_error]),[68D
pengine_reply(error(ID, error(protocol_error, _))),[51D
guarded_main_loop(ID)[25D
).

pengine_child_id(Child) :-[22D
(   nonvar(Child)[17D
->  true[8D
;   pengine_uuid(Child)[23D
).

ask_to_term(Ask-Template, Module, Ask1, Template1, Bindings) :-[59D

format(string(AskTemplate), 't((~s),(~s))', [Template, Ask]),[61D
term_string(t(Template1, Ask1),[19D
AskTemplate,[12D
[variable_names(Bindings0), module(Module)]),[57D
phrase(template_bindings(Template1, Bindings0), Bindings).
ask_to_term(Ask, Module, Ask1, Template, Bindings1) :-[50D
term_string(Ask1,[5D
Ask,[4D
[variable_names(Bindings), module(Module)]),[56D
exclude(anon, Bindings, Bindings1),[35D
dict_create(Template, swish_default_template, Bindings1).

pengine_self(Id) :-[15D
thread_self(Thread),[20D
current_pengine(
_Parent,[8D
Thread,[7D
_URL,[5D
_Application,[13D
_Destroy).
true.

pengines:  ?- [1;18r[Jask_to_term(Ask, Module, Ask1, Template, Bindings1) :-term_string(Ask1,[5D
Ask,[4D
[variable_names(Bindings), module(Module)]),exclude(anon, Bindings, Bindings1),dict_create(Template, swish_default_template, Bindings1).

pengine_self(Id) :-thread_self(Thread),[20D
current_pengine(
_Parent,[8D
Thread,[7D
_URL,[5D
_Application,[13D
_Destroy).
true.

pengines:  ?- 
