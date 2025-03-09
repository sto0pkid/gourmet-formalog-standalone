:- use_module('/var/lib/myfrdcsa/codebases/minor/formalog-pengines/client-server/client/larkc_client').

query_formalog_pengines_agent(Agent,Host,Query,Result) :-
	Scheme = 'http',
	hasPenginePort(Agent,Port),
	query_formalog_pengines_server_on_port(Scheme,Host,Port,Query,Result).

query_formalog_pengines_server_on_port(Scheme,Host,Port,Query,Result) :-
	larkc_client:query_formalog_pengines(Scheme,Host,Port,Query,Result).

query_agent(Agent,Host,Message,Result) :-
	hasPenginePort(Agent,Port),
	larkc_client:query_formalog_pengines('http',Host,Port,Message,Result).

query_agent_bindings(Agent,Host,Vars,Message,Results) :-
	query_agent(Agent,Host,Message,Result),
	findall(Vars,(member(Entry,Result),Message = Entry),Results).
