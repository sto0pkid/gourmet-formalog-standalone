% formalogPenginesCall(Executable,Result) :-
% 	term_variables(Executable,Variables),
% 	findnsols(1,fail,Executable,Tmp),
% 	transform_result(Executable,Result).

formalogPenginesCall(TmpExecutable,[Variables,Result]) :-
	view([tmpExecutable,TmpExecutable]),
	correctLists(TmpExecutable,Executable),
	view([corrected,Executable]),
	copy_term(Executable,Copy),
	numbervars(Copy,0,End),
	with_output_to(atom(A),write_canonical(Copy)),
	%% atom_to_term(A,Term,[]),
	writeln(A),
	term_variables(Executable,Variables),
	view([variables,Variables]),
	findnsols(1,Variables,Executable,TmpResult),
	viewIf([pretransformresults,TmpResult]),
	transform_result(TmpResult,Result),
	viewIf([posttransformresults,Result]).

correctLists(Input,Output) :-
	(   (	compound(Input), compound_name_arity(Input,Pred,0)) ->
	    (	Pred = '[|]' -> Output = [] ; Output = Input ) ;
	    (	atom(Input) -> Output = Input ;
		(   is_list(Input) ->
		    findall(Result,(member(Item,Input),correctLists(Item,Result)),Output) ;
		    (	var(Input) -> Output = Input ; 
			(   (	Input =.. [Pred|Args], (Pred = '[|]' ;  Pred = '_prolog_list')) -> 
			    findall(Result,(member(Item,Args),correctLists(Item,Result)),Output) ;
			    (	Input =.. [Pred|Args] ->
				(   
				    findall(Result,(member(Item,Args),correctLists(Item,Result)),TmpOutput),
				    Output =.. [Pred|TmpOutput]
				) ;
				Output = Input
			    )
			)
		    )
		)
	    )
	).


%% transform_result(Input,Input).

:- module(pengines).

%% /var/lib/myfrdcsa/codebases/internal/freekbs2/KBS2/ImportExport/Mod/Prolog.pm

transform_result(Input,Output) :-
	(   compound(Input), compound_name_arity(Input,Pred,0)) ->
	Output = '$COMPOUND_ZERO_ARITY'(Pred) ;
	(   not(nonvar(Input)) -> (with_output_to(atom(Var),system:write_term(Input,[])),Output = '$VARIABLE'(Var)) ;
	    (	number(Input) -> Output = '$NUMBER'(Input) ;
		(   is_list(Input) -> findall(SubOutput,(member(SubInput,Input),transform_result(SubInput,SubOutput)),Output) ;
		    (	(   Input =.. [P|Args] ) ->
			(   
			    findall(SubOutput,(member(SubInput,Args),transform_result(SubInput,SubOutput)),TmpOutput),
			    Output =.. [P|TmpOutput]
			) ;  
			Output = Input)))).

:- multifile sandbox:safe_primitive/1.

testFormalogPenginesCall :-
	formalogPenginesCall(updateWSM('[|]'(context,'Org::FRDCSA::WSM::TestContext',updates,'[|]'('plan-active'())),RESULTS),MYRESULT),
	view([results,RESULTS,myresult,MYRESULTS]),
	hasTruthValue(wsmHolds2('plan-active'(),'Org::FRDCSA::WSM::TestContext'),X),!,
	view([x,X]).

sandbox:safe_primitive(user:transform_result(_, _)).
sandbox:safe_primitive(user:formalogPenginesCall(_, _)).
