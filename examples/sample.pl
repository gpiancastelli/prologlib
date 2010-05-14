% BinProlog x.xx Copyright (C) 1992 Paul Tarau. All rights reserved.
% COMPILER: dcgs --> prolog --> binary progs --> code
% works on a clause at a time, uses no side effects

cutp(X):-name(X,"$cut").

bin_bu(('$bin_cut'(X,Cont):-true(Cont))):-cutp(X).
bin_bu(C):-  
	bu(B,_,Where),
	bu_body(Where,B,C).

bu_body(in_head,B,(B:-true(Cont))):-
	functor(B,_,N),arg(N,B,Cont).
bu_body(in_body,B,(B:-B)).

compile_mem(File):-see(File),compile_mem1(mem,File),seen.

% Mode must be mem
compile_mem1(Mode,File):-
	ttyprint(compiling(to(Mode),File,'...')),
	statistics(runtime,_),
	mcomp_file(Mode,File),
	terminate_file(mem,'$end1',1),
	!,
	statistics(runtime,[_,T]),
	ttyprint(compile_time(T)),
	abort.
compile_mem1(_,_):-
	ttyprint('compilation aborted'),
	restart,
	abort.

% modes: wam,asm,bin
compile0(Mode,[F|Fs],OutFile):-!,
  statistics(runtime,[T1,_]),
  xcompile(Mode,[F|Fs],OutFile),
  statistics(runtime,[T2,_]),
  T is T2-T1,
  write(total_compile_time(T)),nl.
compile0(Mode,File,OutFile):-xcompile(Mode,[File],OutFile).

xcompile(Mode,InFiles,OutFile):-
	tell(OutFile),
	cc_bus(Mode),
	member(InFile,InFiles),
	ttyprint(compiling(to(Mode),InFile,'...')),
	statistics(runtime,_),
	comp_file(Mode,InFile),
	statistics(runtime,[_,T]),
	ttyprint(compile_time(T)),
	fail.
xcompile(Mode,_,_):-
	terminate_file(Mode,'$end0',0),
	fail.
xcompile(_,_,_):-
	told.

terminate_file(Mode,Dummy,Level):-
	cc(Mode,Dummy),
	emit_code(Mode,[[ii(end,?,Level,Mode)]]).

survive_cleanup(F0,F):-
	name(F0,Survivor),  % Survivor is now a list on the heap
	restart,     % total cleanup of name-spaces, strings, files etc...
	name(F,Survivor),   % F is now a valid name
	see(F),seen.      % F is now linked to an internal file pointer

% compiles to memory	
mcomp_file(Mode,InFile):-
	survive_cleanup(InFile,F),
	translate_all(F,Mode).

comp_file(Mode,InFile):-
	member(Mode,[wam,asm,bin]),!,
	translate_all(InFile,Mode).

translate_all(F,Mode):-
	seeing(F0),
		see(F),
			repeat,
				read(C),
				translate(C,Mode),
			!,
		seen,
	see(F0).

translate(end_of_file,_):-!.
translate(':-'(C),Mode):-!,translate_cmd(C,Mode),fail.
translate('::-'(H,B),Mode):-!,cc_bin(Mode,(H:-B)),fail.
translate(C,Mode):-cc(Mode,C),fail.

translate_cmd([F],Mode):-!,include_file(F,Mode).
translate_cmd(compile(F),Mode):-!,include_file(F,Mode).
translate_cmd(op(X,Y,Z),wam):-!,
	op(X,Y,Z), 
	encode('operator',op(X,Y,Z),0,wam).
translate_cmd(C,_):-
  telling(F),tell(user),exec_cmd(C),told,tell(F),fail.

exec_cmd(C):-is_compiled(C),!,(C,fail;true).
exec_cmd(C):-errmes(bad_command,C).

include_file(IFile,Mode):-
	seeing(CF1),
	find_file(IFile,F),
	ttyprint(begin(including(F),in(CF1))),
	translate_all(F,Mode),
	seeing(CF2),ttyprint(end(including(F),in(CF2))).

preprocess(Clause,BinClause):-
	preprocess(Clause,_,_,BinClause).

preprocess(C,M,D,B):-
	std_expand_term(C,E),
	fact2rule(E,M),    
	mdef_to_def(M,D),
	def_to_mbin(D,B).

cc(Mode,C):-
	preprocess(C,M,D,B),
	show_steps(Mode,M,D),
	cc_bin(Mode,B).

fact2rule((:-B),(:-B)):-!.
fact2rule((H:-B),(H:-B)):-!.
fact2rule(H,(H:-true)).

% BINARY CLAUSE COMPILER

cc_bus(Mode):-Mode\==bin,bin_bu(B),cc_bin(Mode,B),fail.
cc_bus(_).

cc_bin(bin,C):-!,portray_clause(C),fail.
cc_bin(asm,C):-write('BINARY:'),nl,portray_clause(C),nl,fail.
cc_bin(Mode,C):-
	comp_clause(C,CodeC,Def,Exec),
	emit_code(Mode,[Def,CodeC,Exec]),
	!.
cc_bin(Mode,C):-
	errmes(failing_to_compile_clause(Mode),C).

emit_code(mem,C):-gen_code(mem,C).
emit_code(wam,C):-gen_code(wam,C).
emit_code(asm,C):-show_code(C).

show_steps(asm,M,D):-
  nl,M\==D,write('EXPANDED:'),nl,portray_clause(M),nl,fail
; write('DEFINITE:'),nl,portray_clause(D),nl,fail.
show_steps(_,_,_).
	
comp_clause(C,OCode,
	[ii(clause,?,F1,N1),ii(firstarg,?,G/M,LastN)],
	[ii(execute,?,F2,N2)]):-
	add_true(C,(H:-B)),
	firstarg(H,G/M),
	cc_h_b(H,F1/N1,B,F2/N2,RCode),
	max(N1,N2,MaxN),FirstN is MaxN+1,
	vars(RCode,OCode),
	functor(Dict,dict,MaxN),
	fill_info(OCode,Dict),
	collapse_args(Dict,1,MaxN),
	allocate_regs(OCode,FirstN/FirstN-[],FirstN/LastN-_).

cc_h_b(H,F1/N1,B,F2/N2,RCode):-
	cc_h(H,F1/N1,get-RCode,get-Rest), % pp(h=H),
	cc_b(B,F2/N2,put-Rest,put-[]),!. % pp(b=B)

firstarg(H,G/M):-arg(1,H,A),nonvar(A),!,functor(A,G,M).
firstarg(_,'_'/0).

cc_h(B,F/N)-->{bu(B,No,in_head)},!,
	{functor(B,F,N),arg(N,B,Cont)},
	emit(get,ii(builtin,?,No,Cont)).
cc_h(T,F/N)-->
	{functor(T,F,N),N>0},!,{functor(CT,F,N)},
	emit_head_top_term(N,T,CT).
cc_h(T,_)-->{errmes(unexpected_head_atom,T)}.

cc_b(Cont,true/1)-->{var(Cont)},!,
	emit_body_top_term(1,true(_),true(Cont)).
cc_b(true,true/0)-->!.
cc_b('$bin_cut'(_cutp,Cont),FN)-->!,
	emit(put,ii(put,_,temp(1),_cutp)),  
	cc_b(Cont,FN).
cc_b(=(A,B,Cont),FN)-->!,
	cc_t(V1=A),
	cc_t(V2=B),
	emit(put,ii(put,_,temp(0),V1)),
	emit(put,ii(get,_,temp(0),V2)), 
	cc_b(Cont,FN).
cc_b(B,FN)-->{bu(B,No,in_body)},!,
	cc_b_bu(No,B,FN).
cc_b(T,F/N)-->
	{functor(T,F,N),N>0},!,{functor(CT,F,N)},
	emit_body_top_term(N,T,CT).
cc_b(T,_)-->{errmes(unexpected_body_atom,T)}.

/*
arith_op(Op):-bu(B,arith(_,_),_),!,functor(B,Op,_).

% arith_no(No):-bu(_,arith(No,_),_).

% arith_outargs(K):-bu(_,arith(_,K),_).
*/

out_reg(0,_,0).
out_reg(1,Res,Res).

cc_b_bu(arith(No,NbOutArgs),OpArgsCont,FN)-->!,
	{ functor(OpArgsCont,Op,N1),arg(N1,OpArgsCont,Cont),
		N is N1-1, arg(N,OpArgsCont,X), out_reg(NbOutArgs,X,Res),
		I is N-NbOutArgs,functor(NewOpArgs,Op,I) % NbOutArgs = 0,1
	},
	handle_constant_res(NbOutArgs,VarRes=Res),
	emit_top_bargs(1,I,OpArgsCont,NewOpArgs),
	emit(put,ii(arith,_Type,No,VarRes)),
	cc_b(Cont,FN).
cc_b_bu(No,BodyAndCont,FN)-->
	{ functor(BodyAndCont,_,N),N1 is N-1,
		arg(N,BodyAndCont,Cont),
		arg(1,BodyAndCont,Arg)
	},
	cc_b_args(N1,Arg),
	emit(put,ii(inline,_,No,_)), % inline=>void variable
	cc_b(Cont,FN).

handle_constant_res(0,_)-->!.
handle_constant_res(1,X=C)-->{var(C)},!,{X=C}.
handle_constant_res(1,X=C)-->{atomic(C)},!,
	emit(put,ii(put,_,temp(0),C)),
	emit(put,ii(get,_,temp(0),X)).
handle_constant_res(1,X=C)-->!,
	cc_t(X=C).

% handle_constant_res(1,_=C)-->{errmes(must_be_atomic_or_var,C)}.

classif_load(X,A,_)-->{var(A)},!,{X=A}.
classif_load(X,A,constant)-->{atomic(A)},!,{X=A}.
classif_load(X,A,_)-->cc_t(X=A).
	
cc_b_args(0,_)-->[].
cc_b_args(1,Arg)-->cc_t(V=Arg),
	emit(put,ii(put,_,temp(0),V)).

emit_top_bargs(I,N,_,_) --> {I>N},!.
emit_top_bargs(I,N,T,CT) --> {I=<N,I1 is I+1},
	{arg(I,T,A),arg(I,CT,X)},
	classif_load(X,A,Type),
	emit(put,ii(load,Type,I,X)),
	emit_top_bargs(I1,N,T,CT).

emit_top_bargs(I,N,T,CT) --> {I=<N},
	{arg(I,T,A),arg(I,CT,X)},
	!,
	cc_t(X=A),
	{I1 is I+1},
	emit_top_bargs(I1,N,T,CT).

emit_head_top_term(N,T,CT) --> 
	emit_top_args(get,1,1,T,CT),
	cc_arg(1,1,CT,T),
	emit_top_args(get,2,N,T,CT),
	cc_arg(2,N,CT,T).
	
emit_body_top_term(N,T,CT) --> 
	cc_arg(1,N,CT,T),!,
	emit_top_args(put,1,N,T,CT).

emit_top_args(_,I,N,_,_) --> {I>N},!.
emit_top_args(Op,I,N,T,CT) --> {I=<N},
  {arg(I,T,A),arg(I,CT,X),classif_arg(X,A,Type)}, % must be int. if const!
  !,
  emit(Op,ii(Op,Type,arg(I),X)),
  {I1 is I+1},
  emit_top_args(Op,I1,N,T,CT).

cc_t(X=T) --> {var(T)},!,{X=T}.
cc_t(X=T) --> {atomic(T)},!,{X=T}.
cc_t(X=T) --> {functor(T,F,N)},{N>0},!,
	{functor(CT,F,N)},
	emit_term(X,F,N,T,CT).

emit_term(X,F,N,T,CT) --> emit_wam(get,X,F,N,T,CT),!,cc_arg(1,N,CT,T).
emit_term(X,F,N,T,CT) --> cc_arg(1,N,CT,T),emit_wam(put,X,F,N,T,CT).

cc_arg(I,N,_,_) -->  {I>N},!.
cc_arg(I,N,CT,T) --> {I=<N},
	{arg(I,T,A),arg(I,CT,X)},
	{I1 is I+1},!,
	cc_t(X=A),
	cc_arg(I1,N,CT,T).

emit_wam(Op,X,F,N,T,CT) --> {N>0},
	emit(Op,ii(Op,structure,F/N,X)),
	emit_args(1,N,T,CT).

emit_args(I,N,_,_) --> {I>N},!.
emit_args(I,N,T,CT) --> {I=<N},
	{arg(I,T,A),arg(I,CT,X),classif_arg(X,A,Type)},
	!,
	emit(Op,ii(UnifyOp,Type,Op,X)),
	{unify_op(Op,UnifyOp),I1 is I+1},
	emit_args(I1,N,T,CT).

unify_op(put,write).
unify_op(get,unify).

classif_arg(X,A,_):-var(A),!,X=A.
classif_arg(X,A,constant):-atomic(A),!,X=A.
classif_arg(_,_,_).

emit(Mode,E,Mode-[E|Es],Mode-Es).

max(X,Y,Z):-X>Y,!,Z=X.
max(_,Y,Y).

add_true((H:-B),(H:-B)):-!.
add_true(H,(H:-true)).


% VARIABLE OCCURRENCE CLASSIFIER

% vars(T,R) :-
% each (selected) variable V of T gives in R a term
%
%   var(NewVar,OccNo/MaxOccurrences)
%
% and T is subject to (an ugly) side effect as selected
% variables get unified to '$OCC'(s(s(...)),MaxOccurrences)

vars(T,R):-
	find_occurrences(T,R,Vars,[]),
	count_occurrences(Vars).

find_occurrences([],[])-->[].
find_occurrences([ii(Op,Type,Val,Var)|L],[ii(Op,Type,Val,Occ)|R])-->
	occurrence(Var,Occ),
	find_occurrences(L,R).

occurrence(A,A)-->{atomic(A)},!.
occurrence(V,var(NewV,1/Max))-->{var(V)},!,
	newvar(X=Max),
	{V='$OCC'(NewV,X=Max)}.
occurrence('$OCC'(OldV,X=Max),var(OldV,K/Max))-->!,
	oldvar(X=Max,K).
occurrence(Var,Occ)-->{errmes(bad_occurrence,at(var=Var,occ=Occ))}.

inc(V,K,K):-var(V),!,V=s(_).
inc(s(V),K1,K3):-K2 is K1+1,inc(V,K2,K3).

oldvar(X=_,K,Xs,Xs):-inc(X,2,K).

newvar(E,[E|Es],Es).

count_occurrences([]):-!.
count_occurrences([X=Max|Vs]):-inc(X,1,Max),count_occurrences(Vs).


% ARGUMENT REGISTER OPTIMIZER

% fills Dict and and marks still free slots in variables
% with information on liftime of arguments

fill_info(Is,Dict):-fill_all(Is,Dict,0,_).

tpoint(T2,T1,T2):-T2 is T1+1. % gets a time-point

fill_all([],_)-->[].
fill_all([I|Is],Dict)-->
	{fill_var_type(I)},
	fill_one(I,Dict),
	fill_all(Is,Dict).

% fills in liftime information using occurrence numbers

fill_var_type(ii(_,Type,_,var(_,Occ))):-var(Type),!,get_var_type(Occ,Type).
fill_var_type(_).

get_var_type(1/_,variable):-!.
get_var_type(K/Max,value):-K=<Max,!.

fill_one(ii(Op,constant,arg(An),_),Dict)-->!,tpoint(T),
	{mark_arg(Op,T,An,var(_-T/T,1/1),Dict)}.
fill_one(ii(_,constant,_,_),_)-->!,tpoint(_).
fill_one(ii(Op,_,arg(An),Xn),Dict)-->!,tpoint(T),
	{mark_arg(Op,T,An,Xn,Dict)},
	{mark_var(T,Xn)}.
fill_one(ii(_,_,_,var(Id,Occ)),_)-->!,tpoint(T),
	{mark_var(T,var(Id,Occ))}.

% marks the argument An of Dict with liftime information
mark_arg(get,From,An,Xn,Dict):-arg(An,Dict,Xn*_-From/_).
mark_arg(put,To  ,An,Xn,Dict):-arg(An,Dict,_*Xn-_/To).

% marks a variable with liftime information
mark_var(T,var(_-T/T,1/1)):-!.
mark_var(T,var(_-T/_,1/Max)):-1<Max,!.
mark_var(T,var(_-_/T,Max/Max)):-1<Max,!.
mark_var(_,var(_-_/_,_/_)).

% collapses arguments and variables, if possible
collapse_args(_,I,Max):-I>Max,!.
collapse_args(Dict,I,Max):-I=<Max,
	arg(I,Dict,X),
	collapse_them(I,X),
	I1 is I+1,
	collapse_args(Dict,I1,Max).

default(V1/V2):-set_to(0,V1),set_to(99999,V2).

set_to(Val,Val):-!.
set_to(_,_).


% checks if argument I living ALife can be collapsed with
%   input head variable H living HLife and 
%   output body variable B living BLife

collapse_them(I,var(H-HLife,_)*var(B-BLife,_)-ALife):-
	default(HLife),default(BLife),default(ALife),
	check_lifetimes(H-HLife,B-BLife,I-ALife).

check_lifetimes(I-HLife,I-BLife,I-ALife):-
	check_var_arg(HLife,ALife),
	check_var_var(HLife,BLife),
	check_var_arg(BLife,ALife),!.
check_lifetimes(I-HLife,_,I-ALife):-
	check_var_arg(HLife,ALife),!.
check_lifetimes(_,I-BLife,I-ALife):-
	check_var_arg(BLife,ALife),!.
check_lifetimes(_,_,_).

check_var_var(_/H2,B1/_):-H2=<B1,!.
check_var_var(H1/_,_/B2):-B2=<H1.

check_var_arg(X1/X2,A1/A2):-
	A1=<X1,X2=<A2.


% TEMPORARY VARIABLE ALOCATOR

allocate_regs([])-->!.
allocate_regs([I|Is])-->
	allocate1(I),
	allocate_regs(Is).

allocate1(ii(_,_,_,var(Reg-_,KMax)))-->allocate_reg(Reg,KMax),!.
allocate1(_)-->[].

allocate_reg(Reg,1/1)-->{var(Reg)},!,
	get_reg(Reg),
	free_reg(Reg).
allocate_reg(Reg,1/Max)-->{var(Reg),1<Max},!,
	get_reg(Reg).
allocate_reg(Reg,Max/Max)-->{1<Max},!,
	free_reg(Reg).

free_reg(Reg,Min/N-Regs,Min/N-[Reg|Regs]):-Reg>=Min,!.
free_reg(_,Rs,Rs).

get_reg(Reg,Min/N-[Reg|Regs],Min/N-Regs):-!.
get_reg(N,Min/N-Regs,Min/N1-Regs):-N1 is N+1.


%----------PREPROCESSOR TO BINARY LOGIC PROGRAMS-------------

% Transforms definite metaprograms to binary meta-programs
% definite meta clause -> definite clause+ some replacements

mdef_to_def((H:-B),(H:-NewB)):-repl_body(B,NewB).
mdef_to_def((:-B),(:-NewB)):-repl_body(B,NewB),!,
	NewB,  % called here and not propagated to the next step
	fail.

% replaces metavars and some builtins in clause bodies
			 
repl_body(MetaVar,call(MetaVar)):-var(MetaVar),!.
repl_body(M,ExpM):-repl_macro(M,ExpM),!.
repl_body(A,NewA):-split_op(A,L),!,strip_nil(L,NewA).
repl_body(A,A).

repl_macro('!','$bin_cut'(X)):-cutp(X).
repl_macro(var(X),Known):-nonvar(X),repl_known(var(X),Known).
repl_macro(nonvar(X),Known):-nonvar(X),repl_known(nonvar(X),Known).
repl_macro(atomic(X),Known):-nonvar(X),repl_known(atomic(X),Known).
repl_macro(float(X),Known):-nonvar(X),repl_known(float(X),Known).
repl_macro(atomic(X),Known):-nonvar(X),repl_known(atomic(X),Known).
repl_macro((A,B),NewAB):-repl_conj(A,B,NewAB).
repl_macro((A;B),NewAB):-repl_disj(A,B,NewAB).
repl_macro((A->B),(NewA->NewB)):-
	repl_body(A,NewA),
	repl_body(B,NewB).
repl_macro(compare(R,A,B),compare0(A,B,R)):-
repl_macro(A==B,compare0(A,B,=)).
repl_macro(A@<B,compare0(A,B,<)).
repl_macro(A@>B,compare0(A,B,>)).
repl_macro('=>'(K,X),lval(K1,K2,X)):-repl_lval(K,K1,K2).

repl_conj(A,B,NewAB):-nonvar(A),split_op(A,NewA),!,
	repl_body(B,NewB),
	app_body(NewA,NewB,NewAB).
repl_conj(A,B,(NewA,NewB)):-
	repl_body(A,NewA),
	repl_body(B,NewB).

repl_disj(If,C,if(NewA,NewB,NewC)):-nonvar(If),If=(A->B),!,
	repl_body(A,NewA),
	repl_body(B,NewB),
	repl_body(C,NewC).
repl_disj(A,B,or(NewA,NewB)):-
	repl_body(A,NewA),
	repl_body(B,NewB).

repl_lval('#'(K1,K2),K1,K2):-!.
repl_lval(K,K,K).

repl_known(X,true):-X,!.
repl_known(_,fail).
	
split_op(X is B,R):-split_is_rel(X,B,R).
split_op(A < B,R):-split_rel(less,A,B,R).
split_op(A > B,R):-split_rel(greater,A,B,R).
split_op(A =< B,R):-split_rel(less_eq,A,B,R).
split_op(A >= B,R):-split_rel(greater_eq,A,B,R).
split_op(A =:= B,R):-split_rel(arith_eq,A,B,R).
split_op(A =\= B,R):-split_rel(arith_dif,A,B,R).

split_is_rel(X,B,[+(B,0,X)]):-var(B),!.
split_is_rel(X,B,[+(B,0,X)]):-atomic(B),!.
split_is_rel(X,B,[+(B,0,X)]):-float(B),!.
split_is_rel(X,B,R):-split_is(X,B,R,[]).

app_body([],Bs,Bs):-!.
app_body([A|As],Bs,(A,Cs)):-app_body(As,Bs,Cs).

strip_nil([A],A):-!.
strip_nil([A|As],(A,Bs)):-strip_nil(As,Bs).

split_rel(Op,A,B,Res):-split_rel_1(Op,A,B,Res,[]).

split_rel_1(Op,A,B)-->
	split_is(X,A),
	split_is(Y,B),
	{OpXY=..[Op,X,Y]},
	emit_is(OpXY).

split_is(X,A)-->{var(A)},!,{X=A}.
split_is(X,A)-->{atomic(A)},!,{X=A}.
split_is(X,A)-->{float(A)},!,{X=A}.
split_is(R,OpAB)-->
	{OpAB=..[Op,A,B]},!,
	split_is(VA,A),
	split_is(VB,B),
	{OpArgs=..[Op,VA,VB,R]},
	emit_is(OpArgs).
split_is(R,OpA)-->
	{OpA=..[Op,A]},
	split_is(VA,A),
	{OpArgs=..[Op,VA,R]},
	emit_is(OpArgs).

emit_is(X,[X|Xs],Xs).

% converts a definite clause to a binary metaclause
%    where each metavariable Cont represents a "continuation"
%    and a goal G is represented by a clause :- G.

% def_to_mbin((:-B),(:-BC)):-!,add_cont(B,true,BC).
def_to_mbin((H:-B),(HC:-BC)) :- !,
		 termcat(H,Cont,HC), 
		 add_cont(B,Cont,BC).
def_to_mbin(H,(HCont:-true(Cont))):-
		 termcat(H,Cont,HCont).

% adds a continuation to a term

add_cont((true,Gs),C,GC):-!,add_cont(Gs,C,GC).
add_cont((fail,_),C,fail(C)):-!.
add_cont((G,Gs1),C,GC):-!,
		 add_cont(Gs1,C,Gs2),
		 termcat(G,Gs2,GC).
add_cont(G,C,GC):-termcat(G,C,GC).

% ------------------------------------------------------------------
% simple WAM-code lister

show_code(IIs):-
	write('WAM-ASSEMBLER:'),nl,
	member(Is,IIs),
	member(I,Is),
	show_or_skip(I),
	fail.
show_code(_):-nl.

show_or_skip(ii(get,variable,arg(I),var(I-_,_))):-!.
show_or_skip(ii(put,value,arg(I),var(I-_,_))):-!.
show_or_skip(ii(Op,T,X,Y)):-
	write(Op),write('_'),write(T),write(' '),write((X,Y)),nl.
	
% ------------------------------------------------------------------
% BYTE CODE GENERATOR

% eliminates redundancies and beautifies the code
beautify(arg(X),Op,Type,V,To):-!,encode_arg(X,Op,Type,V,To).
beautify(temp(X),Op,Type,V,To):-!,encode_arg(X,Op,Type,V,To).
beautify(Val,Op,Type,var(Xn-_,_),To):-!,encode2(Op,Type,Val,Xn,To).
beautify(put,write,constant,X,To):-cutp(X),!,encode2(push,cut,?,?,To).
beautify(Y,Op,X,Z,To):-encode2(Op,X,Y,Z,To).

encode_arg(I,get,variable,var(I-_,_),_):-!.
encode_arg(I,put,value,var(I-_,_),_):-!.
encode_arg(An,Op,Type,var(Xn-_,_),To):-!,encode2(Op,Type,Xn,An,To).
encode_arg(1,Op,constant,X,To):-cutp(X),!,encode2(Op,cut,?,?,To).
encode_arg(An,Op,constant,S,To):-encode2(Op,constant,S,An,To).

encode2(Op,Type,X,Y,To):-
	symcat(Op,Type,OpType),
	encode(OpType,X,Y,To).

encode(unify_variable,get,Ai,To):-        wcode(To,1,Ai,?,0).
encode(write_variable,put,Ai,To):-        wcode(To,2,Ai,?,0).

encode(unify_value,get,Ai,To):-           wcode(To,3,Ai,?,0).
encode(write_value,put,Ai,To):-           wcode(To,4,Ai,?,0).

encode(unify_constant,get,Const,To):-     wcode(To,5,0,Const,0).
encode(write_constant,put,Const,To):-     wcode(To,6,0,Const,0).

encode(get_constant,Const,Ai,To):-        wcode(To,7,Ai,Const,0).
encode(get_structure,Func/Arity,Ai,To):-  wcode(To,8,Ai,Func,Arity).

encode(put_constant,Const,Ai,To):-        wcode(To,9,Ai,Const,0).
encode(put_structure,Func/Arity,Ai,To):-  wcode(To,10,Ai,Func,Arity).

encode(get_variable,Xn,Ai,To):-           wcode(To,11,Xn,?,Ai). %move_reg
encode(put_value,Xn,Ai,To):-              wcode(To,11,Ai,?,Xn). %move_reg

encode(put_variable,Xn,Ai,To):-           wcode(To,12,Xn,?,Ai).
encode(get_value,Xn,Ai,To):-              wcode(To,13,Xn,?,Ai).

encode(push_cut,?,?,To):-                 wcode(To,14,0,?,0).
encode(put_cut,?,?,To):-                  wcode(To,15,0,?,0).
encode(get_cut,?,?,To):-                  wcode(To,16,0,?,0).

encode('execute_?',Pred,Arity,To):-       wcode(To,17,0,Pred,Arity).
% proceed ==> 18 (by emulator)

encode(load_constant,AReg,Const,To):-     wcode(To,28,AReg,Const,0).

encode(load_variable,AReg,V,To):-         wcode(To,50,AReg,?,V).

encode(load_value,AReg,V,To):-            wcode(To,29,AReg,?,V).

encode('clause_?',Pred,N,To):-n_nop(X1), X is X1+1,          
	wcode(To,X,0,Pred,N).
encode('firstarg_?',F/N,MaxRegs,To):-n_nop(X1), X is X1+2,
	wcode(To,X,MaxRegs,F,N).
encode('operator',Name,_,To):-n_nop(X1),  X is X1+3,          
	wcode(To,X,0,Name,0).

encode('end_?',Reg,Mode,To):-             wcode(To,0,Reg,Mode,0).

encode(inline_variable,No,_Arg,To):-n_inline(N),  wcode(To,N,0,?,No).

encode(arith_variable,No,Arg,To):-n_arith(N),   wcode(To,N,Arg,0,No).
encode(arith_value,No,Arg,To):-n_arith(N),      wcode(To,N,Arg,1,No).

encode('builtin_?',No,Arg,To):-n_bu(N),    wcode(To,N,Arg,?,No).

wcode(mem,Op,Reg,F,N):-add_instr(Op,Reg,F,N).
wcode(wam,Op,Reg,F,N):-put(Op),put(Reg),put(N),
	cwrite(F),
	put(0).

gen_instr(ii(Op,Type,Arg,Var),To,yes):-beautify(Arg,Op,Type,Var,To),!.
gen_instr(_,_,no).

gen_code(To,IIs):-
	member(Is,IIs),member(I,Is),gen_instr(I,To,Ok),
	Ok=no,
	!,
	fail.
gen_code(_,_).

% TOPLEVEL

main(X):-toplevel(X).

% --------------------------------------------------------------------

make_cmd(Cs,List):-make_cmd1(Cs,List,[]).

make_cmd1([])-->[].
make_cmd1([X|Xs])-->
	{listify(X,L)},
	phrase(L),
	make_cmd1(Xs).

listify([X|Xs],[X|Xs]):-!.
listify(X,L):-name(X,L).

make_appl(File):-
	compile0(wam,[File,'init.pl','lib.pl'],'newappl.bp').
 
% ?- make_executable_unix_appl('/export/home/helios/tarau/b/l37/ru','progs/hello.pl','hello').

make_executable_unix_appl(AbsolutePathToEmulator,SourceF,ExecF):-
	make_appl(SourceF),
	make_cmd([
	"(echo '#! /bin/sh '",
	"; echo 'exec ", 
	AbsolutePathToEmulator, " -q1 $* $0'",
	" ; cat newappl.bp) > ",ExecF,
	" ; chmod +x ", ExecF
	], Cmd),
	system(Cmd).
	
boot:-boot_to('wam.bp').

small_boot:-make_kernel('wam.bp',[]).

boot_to(File):-make_kernel(File,['extra.pl']).

make_kernel(File,Extras):-
	kernel_files(Kernel,Extras),
	compile0(wam,Kernel,File).

myboot :- compile0(wam,['oper.out','init.out','lib.out','dcg.out',
   'read.out','write.out','co.out','top.out','extra.out'], 'wam.out').

selfasm:-
	kernel_files(Kernel,['extra.pl']),
	compile0(asm,Kernel,'asm.tmp').

kernel_files(['oper.pl','init.pl','lib.pl','dcg.pl',
   'read.pl','write.pl','co.pl','top.pl'|Fs],Fs).

bin(Files):-compile0(bin,Files,'bin.txt').

asm:-comp_file(asm,user).

ls:-system('ls -tF').
dir:-system(dir).

t(File):-edit(textedit,File).
e(File):-edit(emacs,File).

edit(Editor,File):-
	find_file(File,F),
        make_cmd([Editor," ",F],R),
        system(R),
	compile_mem(F).

s:-statistics.

%   File   : DCG.PL
%   Author : Richard A. OKeefe
%   Updated: Tuesday July 26th, 1988.
%   Purpose: Definite Clause Grammar rule to Prolog clause translator.

/*  This file is written in the ISO 8859/1 character set.  The "Copyright"
    line contains after the right parenthesis a character which when
    transmitted was character 169, the international copyright symbol.

    Copyright (C)) 1988, Quintus Computer Systems, Inc.

    This file is distributed in the hope that it will be useful,
    but without any warrantee.  You are expressly warned that it is meant
    to serve as a model, not for direct use:  all error checking and
    reporting has been omitted, and mistakes are almost surely present.
    Permission is granted to anyone to distribute verbatim copies of
    this source code as received, in any medium, provided that the
    copyright notice, the nonwarrantee warning, and this permission
    notice are preserved.  Permission is granted to distribute modified
    versions of this source code, or of portions of it, under the above
    conditions, plus the conditions that all changed files carry
    prominent notices stating who last changed them and that the derived
    material is subject to this same permission notice.  Permission is
    granted to include this material in products for which money is
    charged, provided that the customer is given written notice that the
    code is (or is derived from) material provided by Quintus Computer
    Systems, Inc., and that the customer is given this source code on
    request.


	----------------------------------------------------------------

    Now that weve got that (adapted from the GNU copyright notice)
    out of the way, here are the technical comments.

    The predicates are all named dcg_<something>/<some arity> in order
    to keep out of the way, with the exception of phrase/2 and phrase/3
    which bear their proper names.  Only phrase/[2,3] and dcg_rule/2
    are meant to be called directly, and dcg_rule/2 is meant to be called
    from expand_term/2.  You need to keep dcg_body/4 and its dependents
    around at run time so that variables as nonterminals in DCG rule bodies
    will work correctly.

    So that Quintus have _something_ left to sell, this code has been
    rewritten from scratch with no error checking or reporting code at
    all, and a couple of places accept general grammar rule bodies where
    they are really supposed to demand lists of terminals.  However, any
    rule which is valid according to the Quintus Prolog manual will be
    translated correctly, except that this code makes no attempt to handle
    module: prefixes.  (The change is trivial.)	

    Note that dcg_rule/2 and phrase/[2,3] are steadfast.

    Minor modifications as BinProlog is not optimized for disjunction
    (a:-b;c) replaced in some places by a:-b. a:-c. by Paul Tarau.
    Using also termcat instead of structure crunching.
*/

%   dcg rule(+Grammar Rule, -Equivalent Clause)

dcg_rule(-->(Head0,Body0), Clause) :-
	dcg_head(Head0, Head, PushBack, S0, S),
	dcg_body(Body0, Body1, S0, S),
	dcg_conj(Body1, PushBack, Body),
	Clause = :-(Head,Body).


%   dcg head(+Head0, -Head, -PushBack, -S0, -S)
%   recognises both
%	NonTerminal, [PushBackList] -->
%   and
%	NonTerminal -->
%   It returns the difference pair S0\S which the body is to parse.
%   To avoid error checking, it will accept an arbitrary body in place
%   of a pushback list, but it should demand a proper list.

dcg_head((Head0,PushBack0), Head, PushBack, S0, S1) :- !,
	dcg_goal(Head0, Head, S0, S),
	dcg_body(PushBack0, PushBack, S, S1).
dcg_head(Head0, Head, true, S0, S) :-
	dcg_goal(Head0, Head, S0, S).


%   dcg goal(+Goal0, -Goal, +S0, +S)
%   adds the arguments S0, S at the end of Goal0, giving Goal.
%   It should check that Goal0 is a callable term.

dcg_goal(Goal0, Goal, S0, S) :-
   termcat(Goal0,S0,Temp),
   termcat(Temp,S,Goal).

%   dcg_body(+Body0, -Body, +S0, +S)
%   translates Body0 to Body, adding arguments as needed to parse S0\S.
%   It should complain about bodies (such as 2) which are not callable
%   terms, and about lists of terminals which are not proper lists.
%   To avoid error checking, [a|foo] is accepted as [a],foo, but it
%   really should complain.  ONLY the forms lists here should be treated;
%   other non-terminals which look like calls to built-ins could well be
%   commented on (no error reporting here) but should be expanded even
%   so.  Thus X=Y as a nonterminal is to be rewritten as =(X,Y,S0,S),
%   perhaps with a warning.  If you want the translation X=Y, use {X=Y}.

dcg_body(Var, Body, S0, S) :- var(Var), !,
	Body = phrase(Var,S0,S).
dcg_body((A0,B0), Body, S0, S) :- !,
	dcg_body(A0, A, S0, S1),
	dcg_body(B0, B, S1, S),
	dcg_conj(A, B, Body).
dcg_body((A0->B0), (A->B), S0, S) :- !,
	dcg_body(A0, A, S0, S1),
	dcg_body(B0, B, S1, S).
dcg_body((A0;B0), (A;B), S0, S) :- !,
	dcg_disj(A0, A, S0, S),
	dcg_disj(B0, B, S0, S).
dcg_body({A}, A, S, S) :- !.
dcg_body(!, !, S, S) :- !.
dcg_body([], true, S, S) :- !.
dcg_body([H0|T0], Body, S0, S) :- !,
	dcg_term(H0, H, S0, S1),
	dcg_body(T0, T, S1, S),
	dcg_conj(H, T, Body).
dcg_body(NT0, NT, S0, S) :-
	dcg_goal(NT0, NT, S0, S).


%   dcg_term(+T0, -T, +S0, +S)
%   generates code (T) which succeeds when there is a terminal T0
%   between S0 and S.  This version uses the DEC-10 Prolog predicate
%   C/3 for compatibility with DEC-10 Prolog, C Prolog, Quintus Prolog.
%   This is the only place that knows how terminals are translated, so
%   you could supply instead the definition
%	dcg_term(T0, S0=[T0|S], S0, S).
%   and reap the same benefits.  The one thing you must not do is
%   NO! dcg_term(T0, true, [T0|S], S). DONT DO THAT!

dcg_term(T0, 'C'(S0,T0,S), S0, S).


%  To see why dcg disj/4 is needed, consider the translation of
%  ( [] | [a] ).  We have to insert S1=S0 somewhere, but we do it at
%  "compile"-time if we can.

dcg_disj(Body0, Body, S0, S) :-
	dcg_body(Body0, Body1, S1, S),
        dcg_disj0(S0,S1,S,Body1,Body).

dcg_disj0(S0,S1,S,Body1,Body):-S1==S,!,dcg_conj(S1=S0, Body1, Body).
dcg_disj0(S0,S0,_,Body,Body).

%   dcg_conj(+A, +B, -C)
%   combines two conjunctions A, B, giving C.  Basically, we want to
%   ensure that there arent any excess trues kicking around (in a
%   compiled system, that shouldnt matter).  There is room for some
%   leeway here: I have chosen to flatten A completely.

dcg_conj(A, true, A) :- !.
dcg_conj(A, B, C) :-
	dcg_CONJ(A, B, C).

dcg_CONJ(true, C, C) :- !.
dcg_CONJ((A,As), C0, (A,C)) :- !,
	dcg_CONJ(As, C0, C).
dcg_CONJ(A, C, (A,C)).


%   'C'(S0, T, S)
%   is true when the terminal T "Connects" the "string positions" S0 and S.

'C'([T|S], T, S).


%   phrase(+NT0, ?S0)
%   is true when the list S0 is in the language defined by the
%   grammar rule body NT0.  E.g. phrase(([a],[b]), [a,b]).

phrase(NT0, S0) :-
	phrase(NT0, S0, []).


%   phrase(+NT0, ?S0, ?S)
%   is true when the list S0\S is in the language defined by the
%   grammar rule body NT0.  E.g. phrase(([a],[b]), [a,b|X], X).

phrase(NT0, S0, S) :-
	dcg_body(NT0, NT, T0, T),
	T0 = S0, T = S,
	NT.

try_dcg_expansion(H,B,EC):-dcg_rule('-->'(H,B),EC),!.
try_dcg_expansion(H,_,_):-errmes('dcg expansion error->',H).

portable_expand_term('-->'(H,B),EC):-!,try_dcg_expansion(H,B,EC).
portable_expand_term(C,C).
% -------------------------------------------------------------------------
%   File   : INTERP
%   Author : R.A.O'Keefe
%   Updated: 2 March 84
%   Purpose: Meta-circular interpreter for Prolog

/*  This is a genuinely meta-circular interpreter for a subset of Prolog
    containing cuts.  It relies on the fact that disjunction is transparent
    to cut just like conjunction.  If it doesn't work in your Prolog, and
    if you paid more than $100 for it, take your Prolog back to the shop
    and insist that they fix it, there are at least four different ways of
    implementing disjunction so that it works.
*/


/* R.A. O'Keefe's meta-circular interpreter */
/* slightly modified by Paul Tarau */

meta_interpreter(Body):-do_body(Body).

do_body(Body) :-
	do_body(Body, AfterCut, HadCut),
	( HadCut = yes,
		!,
		do_body(AfterCut)
	;   HadCut = no
	).


do_compiled(call(Goal)) :- !,
	nonvar(Goal),
	do_body(Goal).
do_compiled(\+(Goal)) :-
	do_body(Goal),
	!, fail.
do_compiled(\+(_)) :- !.
do_compiled(bagof(X,Y,Z)) :- !,
	bagof(X, do_body(Y), Z).
do_compiled(setof(X,Y,Z)) :- !,
	setof(X, do_body(Y), Z).
do_compiled(findall(X,Y,Z)) :- !,
	findall(X, do_body(Y), Z).
do_compiled(Goal):-Goal.

do_goal(Goal) :-
	is_dynamic(Goal),
	!,
	clause(Goal, Body),	% <--- assume anything else is interpreted
	do_body(Body, AfterCut, HadCut),
	(	HadCut = yes,
		!,
		do_body(AfterCut)
	;	HadCut = no
	).
do_goal(Goal) :-
	is_compiled(Goal), % <--- check for a compiled predicate
	!,
	do_compiled(Goal).
do_goal(Undef):-
	errmes('undefined predicate',Undef).

do_body((Goal,Body), AfterCut, HadCut):-!,
	do_conj(Goal,Body, AfterCut, HadCut).
do_body((Goal;Body), AfterCut, HadCut):-!,
	do_disj(Goal,Body, AfterCut, HadCut).
do_body(!, true, yes).
do_body(Goal, true, no) :- do_goal(Goal).

do_conj(!,AfterCut, AfterCut, yes) :- !.
do_conj(Goal,Body, AfterCut, HadCut) :- 
	do_body(Goal),
	do_body(Body, AfterCut, HadCut).

do_disj(Disj1,_, AfterCut, HadCut) :-
	do_body(Disj1, AfterCut, HadCut).
do_disj(_,Disj2, AfterCut, HadCut) :-
	do_body(Disj2, AfterCut, HadCut).

trace(Goal) :-
	tr_body(Goal, 0).

tr_goal(call(Goal), Depth) :- !,
	nonvar(Goal),
	tr_body(Goal, Depth).
tr_goal(\+(Goal), Depth) :-
	tr_body(Goal, Depth),
	!, fail.
tr_goal(\+(_), _) :- !.
tr_goal(Goal, Depth) :-
	(   tab(Depth), write('Call: '), print(Goal), nl, fail
	;   Depth1 is 1+Depth,
	    tr_call(Goal, Depth1),
	    (   tab(Depth), write('Exit: '), print(Goal), nl, fail
	    ;	true
	    ;   tab(Depth), write('Redo: '), print(Goal), nl, fail
	    )
	;   tab(Depth), write('Fail: '), print(Goal), nl, fail
	).

tr_call(bagof(X,Y,Z), Depth) :- !,	% include these 4 lines if you
	bagof(X, tr_body(Y,Depth), Z).	% really want them, but they do
tr_call(setof(X,Y,Z), Depth) :- !,	% slow things down a bit.
	setof(X, tr_body(Y,Depth), Z).
tr_call(findall(X,Y,Z), Depth) :- !,
	findall(X, tr_body(Y,Depth), Z).
tr_call(Goal, Depth) :-
	is_dynamic(Goal),!,
	clause(Goal, Body),
	tr_body(Body, Depth, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		tab(Depth), write('CUT'), nl,
		tr_body(AfterCut, Depth)
	;   HadCut = no
	).
tr_call(Goal, Depth) :-
	is_compiled(Goal), % <--- checks for compiled predicate
	!, tab(Depth),write(compiled(Goal)),nl,
	Goal.
tr_call(Undef):-
	errmes('undefined predicate',Undef).

tr_body(Body, Depth) :-
	tr_body(Body, Depth, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		tab(Depth), write('CUT'), nl,
		tr_body(AfterCut, Depth)
	;   HadCut = no
	).


tr_body((Conj1,Conj2), Depth, AfterCut, HadCut) :- !,
	tr_body(Conj1, Conj2, Depth, AfterCut, HadCut).
tr_body(!, _, true, yes) :- !.
tr_body((Disj1;_), Depth, AfterCut, HadCut) :-
	tr_body(Disj1, Depth, AfterCut, HadCut).
tr_body((_;Disj2), Depth, AfterCut, HadCut) :- !,
	tr_body(Disj2, Depth, AfterCut, HadCut).
tr_body(true, _, true, no) :- !.
tr_body(Goal, Depth, true, no) :-
	tr_goal(Goal, Depth).

tr_body(!, AfterCut, _, AfterCut, yes) :- !.
tr_body((A,B), Conj, Depth, AfterCut, HadCut) :- !,
	tr_body(A, (B,Conj), Depth, AfterCut, HadCut).
tr_body((Disj1;_), Conj, Depth, AfterCut, HadCut) :-
	tr_body(Disj1, Conj, Depth, AfterCut, HadCut).
tr_body((_;Disj2), Conj, Depth, AfterCut, HadCut) :- !,
	tr_body(Disj2, Conj, Depth, AfterCut, HadCut).
tr_body(true, Body, Depth, AfterCut, HadCut) :- !,
	tr_body(Body, Depth, AfterCut, HadCut).
tr_body(Goal, Body, Depth, AfterCut, HadCut) :-
	tr_goal(Goal, Depth),
	tr_body(Body, Depth, AfterCut, HadCut).



/* dynamic/1, assert/1 & retract/1 predicates

This is an approximation of other Prologs assert & retract predicates.
For efficiency and programming style reasons we strongly suggest
not to use them too much.

If you want maximal efficiency use bb_def/3 bb_set/3 bb_val/3 
and lval/3. They give you acces to a very fast
hashing table <key,key>--> value, the same that BinProlog
uses internally for indexing by predicate and first argument.

Lval/3 (i,i,?) introduces named global logical variables.
A friendlier syntax (with no extra cost) is

Key1#Key2=>Var  ...for lval(Key1,Key2,Var)

and Key=>Var    ...for lval(Key,Key,Var)

Beware that assert & retract are not optimized for large databases
or frequent use of an asserted predicate.

To use dynamic predicates you have to declare them with dynamic/1
otherwise asserts will simply fail.

To activate an asserted predicate you must use

	?-metacall(Goal).

instead of 

	?- Goal.

To call a dynamic predicate from compiled code, use also metacall/1. 
*/

dynamic(Ps):-make_dynamic(Ps),fail.
dynamic(_).
 
make_dynamic((P1,P2)):-!,make_dynamic(P1),make_dynamic(P2).
make_dynamic(P/N):-
	functor(T,P,N),
	check_dynamic(T,P/N),
	define_dynamic(T).

check_dynamic(T,PN):-is_dynamic(T),!,errmes('already dynamic',PN),fail.
check_dynamic(T,PN):-is_compiled(T),!,
	write('WARNING: redefining compiled predicate'(PN)),nl.
check_dynamic(_,_).

define_dynamic(Pred):-bb_def(Pred,'$first',0),bb_def(Pred,'$last',0).

add_clause(AZ,Inc,H,B):-
	val(H,AZ,V),
	val(H,V,_),
	!,
	V1 is V+Inc,
	bb_def(H,V1,(H:-B)),
	set(H,AZ,V1).
add_clause(AZ,Inc,H,B):-
	val(H,AZ,V),
	bb_def(H,V,(H:-B)).

asserta(C):-
	fact2rule(C,(H:-B)),
	add_clause('$first',-1,H,B).
	
assertz(C):-
	fact2rule(C,(H:-B)),
	add_clause('$last',1,H,B).

assert(C):-assertz(C).

retract(C0):-
	fact2rule(C0,C),C=(H:-B),
	clause0(H,B,I),
	erase0(H,I).

retractall(C0):-
	fact2rule(C0,(H:-_)),
	val(H,'$first',Min),
	val(H,'$last',Max),
	for(I,Min,Max),
	erase0(H,I),
	fail.
retractall(_).

collect_slot(H,Min):-
	val(H,'$first',Min),
	val(H,'$last',Max),
	Min<Max,!,
	Min1 is Min+1,
	set(H,'$first',Min1).
collect_slot(_,_).

instance('$ref'(H0,I),C):-val(H0,I,C0),copy_term(C0,C).

erase('$ref'(H,I)):-erase0(H,I).

erase0(H,I):-bb_rm(H,I),collect_slot(H,I).

clause(H,B):-clause0(H,B,_).

clause(H,B,'$ref'(H,I)):-clause0(H,B,I).

clause0(H,B,I):-
	val(H,'$first',Min),
	val(H,'$last',Max),
	findall(IC,ith_clause(H,IC,Min,Max),ICs),
	member(I-(H:-B),ICs).

ith_clause(G,Min-C,Min,Max):- % Min=<Max,
	val(G,Min,C).
ith_clause(G,IC,Min,Max):-
        Min<Max,
        Min1 is Min+1,
        ith_clause(G,IC,Min1,Max).

abolish(F,N):-
	functor(P,F,N),functor(C,F,N),
	retractall(C),
	fail.
abolish(F,N):-
	functor(P,F,N),
	set(P,'$first',0),
	set(P,'$last',0).

ensure_dynamic(H,_,_):-is_dynamic(H),!.
ensure_dynamic(_,P,N):-make_dynamic(P/N).

assert_it(end_of_file):-!.
assert_it(C):-
	fact2rule(C,(H:-_)),
	functor(H,P,N),
	ensure_dynamic(H,P,N),
	assertz(C),!.
assert_it(C):-errmes('unable to assert',C).


% consulting `interpreted code'

debug(F):-reconsult(F).

consult(File):-
	find_file(File,F),
        write('% using compile/1 is MUCH faster'),nl,
        consult0(F).

reconsult(File):-
	find_file(File,F0),
        write('% using compile/1 is MUCH faster'),nl,
        survive_cleanup(F0,F),
        consult0(F).

consult0(F):-
	statistics(runtime,_),
	write(reconsulting(F)),nl,
	see(F),
	repeat,
		read(C),consult_cmd(C),C=end_of_file,!,
	seen,
	statistics(runtime,[_,T]),
	write(reconsulted(F,time=T)),nl.

consult_included(F):-seeing(G),find_file(F,F0),consult0(F0),see(G).

consult_cmd(':-'([F])):-!,consult_included(F).
consult_cmd(':-'(C)):-!,metacall(C).
consult_cmd(C):-expand_term(C,E),assert_it(E).

listing(P,N):-
	functor(H,P,N),
	clause(H,B),
        portray_clause((H:-B)),
        fail
;	nl.

listing:-
	bb_list(0,Xs),
	bb_element(P/N+'$first'/0=_,Xs),
	listing(P,N),
	fail
;       nl.

% Blackboard related utilities

% bboard visualisation

bb_element(P/N+F/K=V,[P,N,F,K,V|_]).
bb_element(D,[_,_,_,_,_|L]):-bb_element(D,L).

bb_list(L):-bb_list(0,L). % lists for arity >= 0

bb:-
	statistics(bboard,X),write(bboard-X),nl,
	bb_list(L),
		bb_element(B,L),
		write(B),nl,
	fail
; nl.


% unsupported & unsafe

let(X,Y,V):-def(X,Y,V),!.
let(X,Y,V):-set(X,Y,V).

def(A,X):-def(A,A,X).

set(A,X):-set(A,A,X).

val(A,X):-val(A,A,X).

let(A,X):-let(A,A,X).

rm(A):-rm(A,A).

/* stacks */

push(Type,S,X):-val(Type,S,Xs),!,bb_set(Type,S,[X|Xs]).
push(Type,S,X):-bb_def(Type,S,[X]).

pop(Type,S,X):-val(Type,S,[A|Xs]),set(Type,S,Xs),copy_term(A,X).

stack(Type,S,Xs):-val(Type,S,Xs).

push(S,X):-push('$stack',S,X).

pop(S,X):-pop('$stack',S,X).

stack(S,X):-stack('$stack',S,X).


/*******************************************************************/

% sort, adapted from public domain code written by R.A. O'Keefe
% use merge_sort(<,_,_) if you do not want duplications eliminated
% use merge_sort(>,_,_) for descending order

sort(L1,L2):-merge_sort(<,L1,DupL),remdup(DupL,L2).

remdup([],[]).
remdup([X,Y|Xs],Ys):-compare(=,X,Y),!,remdup([X|Xs],Ys).
remdup([X|Xs],[X|Ys]):-remdup(Xs,Ys).
      
merge_sort(Rel, L,S ):-
	length(L,N),
	merge_sort1(N, Rel, L,S,[] ).

merge_sort1( 0,_,L,[],L ):-!.
merge_sort1( 1,_,[X|L],[X],L ):-!.
merge_sort1( N,Rel,L,S,R ):-	% N >= 2
	N1 is N >> 1,	N2 is N-N1,
	merge_sort1( N1,Rel,L,S1,R1),	
	merge_sort1( N2,Rel,R1,S2,R),
	merge_2( S2,Rel,S1,S ).

merge_2([],_,S,S ):-!.
merge_2([X|L1],Rel,[Y|L2],[X|L] ):-compare(Rel,X,Y),!,
	merge_2(L1,Rel,[Y|L2],L ).
merge_2(L1,Rel,[Y|L2],[Y|L] ):-
	merge_2(L2,Rel,L1,L ).

%   Keysorting.  Adapted by Mats Carlsson from R.O'Keefe's code, 
%		but uses recursion instead of an auxiliary stack.  
%		Takes care to check validity of arguments.
%   Could be speed up if there were an inline keycompare/3.

ksort(List, Sorted) :-
	keysort(List, -1, S, []), !,
	Sorted = S.
ksort(X, Y):-user_error('illegal_arguments',keysort(X,Y)).

keysort([Head|Tail], Lim, Sorted, Rest) :- !,
	nonvar(Head),
	Head = _-_,
	Qh = [Head|_],
	samkeyrun(Tail, Qh, Qh, Run, Rest0),
	keysort(Rest0, 1, Lim, Run, Sorted, Rest).
keysort(Rest, _, [], Rest).

keysort([Head|Tail], J, Lim, Run0, Sorted, Rest) :-
	J =\= Lim, !,
	nonvar(Head),
	Head = _-_,
	Qh = [Head|_],
	samkeyrun(Tail, Qh, Qh, Run1, Rest0),
	keysort(Rest0, 1, J, Run1, Run2, Rest1),
	keymerge(Run0, Run2, Run),
	K is J+J,
	keysort(Rest1, K, Lim, Run, Sorted, Rest).
keysort(Rest, _, _, Sorted, Sorted, Rest).

samkeyrun([Hd|Tail], QH, QT, Run, Rest) :-
	nonvar(Hd),
	Hd = H-_,
	QT = [Q-_|QT2], 
	Q @=< H, !,
	QT2 = [Hd|_],
	samkeyrun(Tail, QH, QT2, Run, Rest).
samkeyrun([Hd|Tail], QH, QT, Run, Rest) :-
	nonvar(Hd),
	Hd = H-_,
	QH = [Q-_|_],
	H @< Q, !,
	samkeyrun(Tail, [Hd|QH], QT, Run, Rest).
samkeyrun(Rest, Run, [_], Run, Rest).

% keymerge(+List, +List, -List).
keymerge([], L2, Out) :- !,
	Out = L2.
keymerge([H1|T1], L2, Out) :-	
	L2 = [K2-_|_],
	H1 = K1-_,
	K1 @=< K2, !,
	Out = [H1|Out1],
	keymerge(T1, L2, Out1).
keymerge(L1, [H2|L2], Out) :- !,
	Out = [H2|Out1],
	keymerge(L1, L2, Out1).
keymerge(List, _, List).

% -------------------- ---------------------

%   File   : SETOF.PL
%   Author : R.A.O'Keefe
%   Updated: 17 November 1983
%   Purpose: define setof/3, bagof/3, findall/3, and findall/4
%   Needs  : Not.Pl

% Adapted by Paul Tarau for BinProlog: uses a heap based findall/3.
% therefore some database hacking predicates have been removed.
% Updated: 19 July 1992

/*  This file defines two predicates which act like setof/3 and bagof/3.
    I have seen the code for these routines in Dec-10 and in C-Prolog,
    but I no longer recall it, and this code was independently derived
    in 1982 by me and me alone.

    Most of the complication comes from trying to cope with free variables
    in the Filter; these definitions actually enumerate all the solutions,
    then group together those with the same bindings for the free variables.
    There must be a better way of doing this.  I do not claim any virtue for
    this code other than the virtue of working.  In fact there is a subtle
    bug: if setof/bagof occurs as a data structure in the Generator it will
    be mistaken for a call, and free variables treated wrongly.  Given the
    current nature of Prolog, there is no way of telling a call from a data
    structure, and since nested calls are FAR more likely than use as a
    data structure, we just put up with the latter being wrong.  The same
    applies to negation.

    Would anyone incorporating this in their Prolog system please credit
    both me and David Warren;  he thought up the definitions, and my
    implementation may owe more to subconscious memory of his than I like
    to think.  At least this ought to put a stop to fraudulent claims to
    having bagof, by replacing them with genuine claims.

    Thanks to Dave Bowen for pointing out an amazingly obscure bug: if
    the Template was a variable and the Generator never bound it at all
    you got a very strange answer!  Now fixed, at a price.

	bagof/3,		%   Like bagof (Dec-10 manual p52)
	setof/3.		%   Like setof (Dec-10 manual p51)

:- mode
	bagof(+,+,?),
	concordant_subset(+,+,-),
	concordant_subset(+,+,-,-),
	concordant_subset(+,+,+,+,-),
	replace_key_variables(+,+,+),
	setof(+,+,?).

%   setof(Template, Generator, Set)
%   finds the Set of instances of the Template satisfying the Generator..
%   The set is in ascending order (see compare/3 for a definition of
%   this order) without duplicates, and is non-empty.  If there are
%   no solutions, setof fails.  setof may succeed more than one way,
%   binding free variables in the Generator to different values.  This
%   predicate is defined on p51 of the Dec-10 Prolog manual.
*/

setof(Template, Filter, Set) :-
	bagof(Template, Filter, Bag),
	sort(Bag, Set).


%   bagof(Template, Generator, Bag)
%   finds all the instances of the Template produced by the Generator,
%   and returns them in the Bag in they order in which they were found.
%   If the Generator contains free variables which are not bound in the
%   Template, it assumes that this is like any other Prolog question
%   and that you want bindings for those variables.  (You can tell it
%   not to bother by using existential quantifiers.)
%   bagof records three things under the key '.':
%	the end-of-bag marker	       -
%	terms with no free variables   -Term
%	terms with free variables   Key-Term
%   The key '.' was chosen on the grounds that most people are unlikely
%   to realise that you can use it at all, another good key might be ''.
%   The original data base is restored after this call, so that setof
%   and bagof can be nested.  If the Generator smashes the data base
%   you are asking for trouble and will probably get it.
%   The second clause is basically just findall, which of course works in
%   the common case when there are no free variables.

bagof(Template, Generator, Bag) :-
	free_variables(Generator, Template, [], Vars),
	Vars \== [],
	!,
	Key =.. [.|Vars],
	functor(Key, ., N),
	findall(Key-Template,Generator,Recorded),
	replace_instance(Recorded, Key, N, [], OmniumGatherum),
	keysort(OmniumGatherum, Gamut), !,
	concordant_subset(Gamut, Key, Answer),
	Bag = Answer.
bagof(Template, Generator, [B|Bag]) :-
	findall(Template,Generator,[B|Bag]).

_^Goal:-Goal.

replace_instance([], _, _, AnsBag, AnsBag) :- !.
replace_instance([NewKey-Term|Xs], Key, NVars, OldBag, NewBag) :-
		replace_key_variables(NVars, Key, NewKey), !,
		replace_instance(Xs,Key, NVars, [NewKey-Term|OldBag], NewBag).

%   There is a bug in the compiled version of arg in Dec-10 Prolog,
%   hence the rather strange code.  Only two calls on arg are needed
%   in Dec-10 interpreted Prolog or C-Prolog.

replace_key_variables(0, _, _) :- !.
replace_key_variables(N, OldKey, NewKey) :-
	arg(N, NewKey, Arg),
	nonvar(Arg), !,
	M is N-1,
	replace_key_variables(M, OldKey, NewKey).
replace_key_variables(N, OldKey, NewKey) :-
	arg(N, OldKey, OldVar),
	arg(N, NewKey, OldVar),
	M is N-1,
	replace_key_variables(M, OldKey, NewKey).

%   concordant_subset([Key-Val list], Key, [Val list]).
%   takes a list of Key-Val pairs which has been keysorted to bring
%   all the identical keys together, and enumerates each different
%   Key and the corresponding lists of values.

concordant_subset([Key-Val|Rest], Clavis, Answer) :-
	concordant_subset(Rest, Key, List, More),
	concordant_subset(More, Key, [Val|List], Clavis, Answer).


%   concordant_subset(Rest, Key, List, More)
%   strips off all the Key-Val pairs from the from of Rest,
%   putting the Val elements into List, and returning the
%   left-over pairs, if any, as More.

concordant_subset([Key-Val|Rest], Clavis, [Val|List], More) :-
	Key == Clavis,
	!,
	concordant_subset(Rest, Clavis, List, More).
concordant_subset(More, _, [], More).


%   concordant_subset/5 tries the current subset, and if that
%   doesn't work if backs up and tries the next subset.  The
%   first clause is there to save a choice point when this is
%   the last possible subset.

concordant_subset([],   Key, Subset, Key, Subset) :- !.
concordant_subset(_,    Key, Subset, Key, Subset).
concordant_subset(More, _,   _,   Clavis, Answer) :-
	concordant_subset(More, Clavis, Answer).


% ---extracted from: not.pl --------------------%

%   Author : R.A.O'Keefe
%   Updated: 17 November 1983
%   Purpose: "suspicious" negation 

%   In order to handle variables properly, we have to find all the 
%   universally quantified variables in the Generator.  All variables
%   as yet unbound are universally quantified, unless
%	a)  they occur in the template
%	b)  they are bound by X^P, setof, or bagof
%   free_variables(Generator, Template, OldList, NewList)
%   finds this set, using OldList as an accumulator.

free_variables(Term, Bound, VarList, [Term|VarList]) :-
	var(Term),
	term_is_free_of(Bound, Term),
	list_is_free_of(VarList, Term),
	!.
free_variables(Term, _, VarList, VarList) :-
	var(Term),
	!.
free_variables(Term, Bound, OldList, NewList) :-
	explicit_binding(Term, Bound, NewTerm, NewBound),
	!,
	free_variables(NewTerm, NewBound, OldList, NewList).
free_variables(Term, Bound, OldList, NewList) :-
	functor(Term, _, N),
	free_variables(N, Term, Bound, OldList, NewList).

free_variables(0,    _,     _, VarList, VarList) :- !.
free_variables(N, Term, Bound, OldList, NewList) :-
	arg(N, Term, Argument),
	free_variables(Argument, Bound, OldList, MidList),
	M is N-1, !,
	free_variables(M, Term, Bound, MidList, NewList).

%   explicit_binding checks for goals known to existentially quantify
%   one or more variables.  In particular \+ is quite common.

explicit_binding(\+ _,	       Bound, fail,	Bound      ).
explicit_binding(not(_),	     Bound, fail,	Bound	   ).
explicit_binding(Var^Goal,	   Bound, Goal,	Bound+Var).
explicit_binding(setof(Var,Goal,Set),  Bound, Goal-Set, Bound+Var).
explicit_binding(bagof(Var,Goal,Bag),  Bound, Goal-Bag, Bound+Var).

term_is_free_of(Term, Var) :-
	var(Term), !,
	Term \== Var.
term_is_free_of(Term, Var) :-
	functor(Term, _, N),
	term_is_free_of(N, Term, Var).

term_is_free_of(0, _, _) :- !.
term_is_free_of(N, Term, Var) :-
	arg(N, Term, Argument),
	term_is_free_of(Argument, Var),
	M is N-1, !,
	term_is_free_of(M, Term, Var).

list_is_free_of([], _).
list_is_free_of([Head|Tail], Var) :-
	Head \== Var,
	list_is_free_of(Tail, Var).

keysort(L,S):-ksort(L,S).


% for compatibility with various prologs

% SOUND negation -> replaced with (I think) an improved one

% REPLACED:
% not(X):-ground(X),!, \+ X.
% not(X):-user_error('should be ground',not(X)).

% I see no reason to prohibit free variables when the negation succeeds
% as no future bindings can change the logical meaning in this case.
% the case of failure is (of course) different

not(X):- \+ X,!.
not(X):-ground(X),!,fail.
not(X):-user_error('should be ground',not(X)).

term_chars(T,Cs):-nonvar(T),!,swrite(T,S),name(S,Cs).
term_chars(T,Cs):-name(S,Cs),sread(S,T).

gensym(Root,Symbol):-
        val(gensym,Root,N),!,
        N1 is N+1,
        symcat(Root,N1,Symbol),
        set(gensym,Root,N1).
gensym(Root,Symbol):-
        def(gensym,Root,1),
        symcat(Root,1,Symbol).

go:-
  DEF='defs.h',PROF='prof.h',
  n_inline(First),n_arith(Arith),n_bu(Bu),n_nop(Nop),Last is Nop-1,
  write([inline=First,arith=Arith,in_body=Bu,last=Last]),nl,
  write(making(DEF)),nl,
  tell(DEF),
  ( make_defs(First,Arith,Bu,Last)
    ; write(failing_on(DEF)),nl
  ),
  !,
  make_stats,
  !,
  told,
  write(making(PROF)),nl,
  tell(PROF),
  ( make_names
    ; write(failing_on(PROF)),nl
  ),
  !,
  told.

make_defs(First,Arith,Bu,Last0):-
  number_bu(First,Last,N-X),
  write('#define '),write(X),write(' '),write(N),nl,
  N=:=Last,
  !,
  Last=:=Last0,
  nl,write('#define INLINE '),write(First),nl,
  write('#define ARITH '),write(Arith),nl,
  write('#define BUILTIN '),write(Bu),nl,
  write('#define LAST_BUILTIN '),write(Last),nl.

make_stats:-
	nl,
	stat_dict(Name,Val),name(Name,L),
	to_upper(L,L1),
	append("STAT_",L1,L2),
	name(SName,L2),
	write('#define '),write(SName),write(' '),write(Val),nl,
	fail.
make_stats:-nl.

make_names:-
  n_inline(Max),
  write('#if TRACE>1 || PROF '),nl,
  write('char *bu_name[]={'),nl,
  number_bu(Max,Last,N-X),
  write('"'),write(X),write('",'),nl,
  N=:=Last,
  !,
  write('"LAST_BUILTIN"};'),nl,write('#endif'),nl,nl.

number_bu(First,Last,N-X):-
  findall(B,bname(B),Bs),
  length(Bs,L),Last is First+L-1,
  member_i(X,Bs,First,N).

to_upper1(X,Y):-[A]="a",[Z]="z",X>=A,X=<Z,[AA]="A",!,
  Y is AA+(X-A).  
to_upper1(X,X).

to_upper([],[]).
to_upper([X|Xs],[Y|Ys]):-
  to_upper1(X,Y),
  to_upper(Xs,Ys).

b_idiom(+,"PLUS").
b_idiom(-,"SUB").
b_idiom(*,"MUL").
b_idiom(//,"DIV").
b_idiom(/,"FDIV").
b_idiom(put,"PUT0").

b_idiom(<<,"LSHIFT").
b_idiom(>>,"RSHIFT").
b_idiom(/\,"L_AND").
b_idiom(\/,"L_OR").
b_idiom(#,"L_XOR").
b_idiom(\,"L_NEG").

to_bname(F,U):-b_idiom(F,U),!.
to_bname(F,U):-name(F,L),to_upper(L,U).

bname(Name):-
  [X]="_",
  is_builtin(F/N),name(N,Arity),
  to_bname(F,U),
  append(U,[X|Arity],List),
  name(Name,List).

nth_member(X,Xs,N):-member_i(X,Xs,1,N).

member_i(X,[X|_],N,N).
member_i(X,[_|Xs],N1,N3):-
  N2 is N1+1,
  member_i(X,Xs,N2,N3).

/*
append([],Ys,Ys).
append([A|Xs],Ys,[A|Zs]):-
        append(Xs,Ys,Zs).
*/
% ---------- ADD BUILTINS: <here> and in wam.c -----------------------

n_inline(51).                         % INLINE starts here
n_arith(X1):-n_inline(X),X1 is X+11.  % number of arith --> #change!
n_bu(X1):-n_arith(X),X1 is X+46. % number of in_body -> #change!
n_nop(X1):-n_bu(X),X1 is X+16.  % number of in_head -> #change

% #change USE FIND WITH YOUR EDITOR

bu(fail(_),0,in_body).
bu(cwrite(_,_),1,in_body).
bu(nl(_),2,in_body).
bu(var(_,_),3,in_body).
bu(nonvar(_,_),4,in_body).
bu(integer(_,_),5,in_body).
bu(atomic(_,_),6,in_body).
bu(is_compiled(_,_),7,in_body).
bu(lift_heap(_),8,in_body).
bu(seen(_),9,in_body).
bu(told(_),10,in_body).

% #change 11

bu(+(_,_,_,_),arith(0,1),in_body).
bu(-(_,_,_,_),arith(1,1),in_body).
bu(*(_,_,_,_),arith(2,1),in_body).
bu('/'(_,_,_,_),arith(3,1),in_body).
bu(mod(_,_,_,_),arith(4,1),in_body).
bu(random(_,_),arith(5,1),in_body).
bu(get0(_,_),arith(6,1),in_body).

bu(put(_,_),arith(7,0),in_body).

bu(less(_,_,_),arith(8,0),in_body).
bu(greater(_,_,_),arith(9,0),in_body).
bu(less_eq(_,_,_),arith(10,0),in_body).
bu(greater_eq(_,_,_),arith(11,0),in_body).
bu(arith_eq(_,_,_),arith(12,0),in_body).
bu(arith_dif(_,_,_),arith(13,0),in_body).

bu('<<'(_,_,_,_),arith(14,1),in_body).
bu('>>'(_,_,_,_),arith(15,1),in_body).
bu('/\\'(_,_,_,_),arith(16,1),in_body).
bu('\\/'(_,_,_,_),arith(17,1),in_body).
bu('#'(_,_,_,_),arith(18,1),in_body). % xor
bu(\(_,_,_,_),arith(19,1),in_body). % complement

bu('compare0'(_,_,_,_),arith(20,1),in_body).
bu(arg(_,_,_,_),arith(21,1),in_body).

bu(def(_,_,_,_),arith(22,0),in_body).
bu(set(_,_,_,_),arith(23,0),in_body).
bu(val(_,_,_,_),arith(24,1),in_body).
bu(rm(_,_,_),arith(25,0),in_body).

bu(symcat(_,_,_,_),arith(26,1),in_body).
bu(copy_term(_,_,_),arith(27,1),in_body).
bu(save_term(_,_,_),arith(28,1),in_body).

bu(seeing(_,_),arith(29,1),in_body).
bu(telling(_,_),arith(30,1),in_body).
bu(see_or_fail(_,_),arith(31,0),in_body).
bu(tell_or_fail(_,_),arith(32,0),in_body).

bu(add_instr(_,_,_,_,_),arith(33,0),in_body).
bu(det_append(_,_,_,_),arith(34,1),in_body).

bu(sread(_,_,_),arith(35,1),in_body).
bu(swrite(_,_,_),arith(36,1),in_body).

bu(op0(_,_,_,_),arith(37,0),in_body). 

bu(lval(_,_,_,_),arith(38,1),in_body).
bu(termcat(_,_,_,_),arith(39,1),in_body).
bu(float_fun2(_,_,_,_,_),arith(40,1),in_body).
bu(float_fun(_,_,_,_),arith(41,1),in_body).
bu(input_float(_,_,_ ,_,_),arith(42,1),in_body).
bu(write_float(_,_),arith(43,0),in_body).
bu('//'(_,_,_,_),arith(44,1),in_body).
bu(halt(_,_),arith(45,0),in_body).

% #change: 46

bu(true(_),0,in_head).
bu(call(_,_),1,in_head).

bu(findall_store_heap(_,_),2,in_head).
bu(findall_load_heap(_,_),3,in_head).

bu(functor(_,_,_,_),4,in_head).
bu(name(_,_,_),5,in_head).

bu(abort(_),6,in_head).
bu(restart(_),7,in_head).

bu(shell(_,_),8,in_head).

bu(stat0(_,_,_,_),9,in_head).

bu(list_asm(_,_,_,_),10,in_head).
bu(bboard(_,_,_),11,in_head).
bu(bb_list(_,_,_),12,in_head).
bu(bb_reset(_),13,in_head).
bu(profile(_),14,in_head).

bu(if0(_,_,_,_),15,in_head).

% #change 16

is_builtin(F/N1):-bu(T,_,_),functor(T,F,N),N1 is N-1.

stat_dict(runtime,0).
stat_dict(global_stack,1).
stat_dict(local_stack,2).
stat_dict(trail,3).
stat_dict(code,4).
stat_dict(strings,5).
stat_dict(symbols,6).
stat_dict(htable,7).
stat_dict(bboard,8).

errmes(Mes,Obj):-
	telling(F),
	tell(user),write('*** '),write(Mes),write(': '),write(Obj),nl,
	tell(F),
	fail.

user_error(Mes,Obj):-
	telling(F),
	tell(user),cwrite('>>> '),cwrite(Mes),cwrite(': '),cwrite(Obj),nl,
	tell(F),
	fail.

member(C,[C|_]).
member(C,[_|L]):- member(C,L).

append([],Ys,Ys).
append([A|Xs],Ys,[A|Zs]):-
	append(Xs,Ys,Zs).
% -----------------------------------------------------------------
% LIBRARY of basic predicates

op(Pri,Assoc,Name):-op0(Name,Assoc,Pri).

current_op(Pri,Assoc,Name):-nonvar(Name),nonvar(Assoc),!,
  val(Name,Assoc,Pri).
current_op(Pri,Assoc,Name):-
  for(I,0,10000),
  (get_op(I,Name,Assoc,Pri)->true;!,fail).

get_op(I,Name,Assoc,Pri):- 
	val(op_name,I,Name),
	val(op_assoc,I,Assoc),
	val(op_pri,I,Pri).

% true.
% call(X):-X.

call_ifdef(G,_):-is_compiled(G),!,G.
call_ifdef(_,Else):-Else.

X=X.

A->B :- A,!,B.

A->B ; C :- !,if(A,B,C).
X ; _ :-X.
_ ; Y :-Y.

or(A,_):-A.
or(_,B):-B.

if(A,B,_):-A,!,B.
if(_,_,C):-C.

(X,Y):-X,Y.

\+(X):-X,!,fail.
\+(_).

repeat.
repeat:-repeat.

findall_workhorse(X,G,_):-
	lift_heap,
	G,
	findall_store_heap(X).
findall_workhorse(_,_,Xs):-
	findall_load_heap(Xs).	

findall(X,G,Xs,End):-findall_workhorse(X,G,[End|Xs]).

findall(X,G,Xs):-findall(X,G,Xs,[]).

findall(G,Gs):-findall(G,G,Gs).

gc_read(R):-findall(X,read(X),[R]).

gc_call(G):-findall(G,G,Gs),member(G,Gs).

for(Min,Min,Max):-Min=<Max.
for(I,Min,Max):-
        Min<Max,
        Min1 is Min+1,
        for(I,Min1,Max).

numbervars('$VAR'(N0), N0, N) :- !, N is N0+1.
numbervars(X, N0, N) :- atomic(X), !, N0=N.
numbervars([X|Xs], N0, N) :- !,
        numbervars(X, N0, N1),
        numbervars(Xs, N1, N).
numbervars(X, N0, N) :-
        functor(X, _, A),
        numbervars(0, A, X, N0, N).

numbervars(A, A, _, N0, N) :- !, N0=N.
numbervars(A0, A, X, N0, N) :-
        A1 is A0+1,
        arg(A1, X, X1),
        numbervars(X1, N0, N1),
        numbervars(A1, A, X, N1, N).

ground(T):-numbervars(T,0,N),N>0,!,fail.
ground(_).

see(File):-see_or_fail(File),!.
see(File):-user_error(unable_to_see,File).

tell(File):-tell_or_fail(File),!.
tell(File):-user_error(unable_to_tell,File).

system([]):-!.
system([X|Xs]):-!,name(Command,[X|Xs]),shell(Command).
system(Command):-atomic(Command),shell(Command).

statistics(Area,[Used,Free]):-stat_dict(Area,No),stat0(Used,Free,No).

statistics:-
	statistics(Name,Data),
	cwrite(Name),cwrite(=),cwrite(Data),nl,
	fail.
statistics.	

atom(X):-integer(X),!,fail.
atom(X):-atomic(X).

float(X):-nonvar(X),X='$float'(_,_,_).

number(X):-integer(X),!.
number(X):-float(X).

compound(X):-nonvar(X), \+(atomic(X)).

=..(T,[F|Xs]):-nonvar(T),!,functor(T,F,N),term2list(1,N,T,Xs).
=..(T,[F|Xs]):-get_length(Xs,0,N),!,functor(T,F,N),term2list(1,N,T,Xs).

term2list(I,N,_,R):-I>N,!,R=[].
term2list(I,N,T,[X|Xs]):-I1 is I+1,arg(I,T,X),term2list(I1,N,T,Xs).

length(L,N):-var(N),!,get_length(L,0,N).
length(L,N):-make_length(L,0,N).

get_length([],I,I).
get_length([_|L],I0,I):-I1 is I0+1,get_length(L,I1,I).

make_length([],I,I):-!.
make_length([_|L],I0,I):-I0<I,I1 is I0+1,make_length(L,I1,I).

tab(N):-for(_,1,N),put(32),fail.
tab(_).

get(R):-repeat,get0(X),(X>32;X<0),!,R=X.

X is E:-meta_is(E,R),!,X=R.
X is E:-user_error(error_in_is,X is E).

meta_is(E,_Error):-var(E),!,errmes(variable_in_is,E).
meta_is(E,R):-atomic(E),!,R=E.
meta_is(E,R):-float(E),!,R=E.
meta_is(E,R):-functor(E,Op,2),!,
	arg(1,E,E1),arg(2,E,E2),
	meta_is(E1,X1),
	meta_is(E2,X2),
	G=..[Op,X1,X2,R],
	G,!.
meta_is(E,R):-functor(E,Op,1),
	arg(1,E,E1),
	meta_is(E1,X1),
	G=..[Op,X1,R],
	G,!.

compare(R,X,Y):-compare0(X,Y,R).

+(X,X).
-(X,N):- -(0,X,N).
\(X,N):- \(0,X,N).

A==B :- compare0(A,B,=).
A\==B :- compare0(A,B,R),'$noteq'(R).

A @< B :- compare0(A,B,<).
A @> B :- compare0(A,B,>).
A @=< B :- compare0(A,B,R),'$lesseq'(R).
A @>= B :- compare0(A,B,R),'$gteq'(R).

A < B :-meta_is(A,X), meta_is(B,Y), less(X,Y).

A > B :-meta_is(A,X), meta_is(B,Y), greater(X,Y).

A =< B :-meta_is(A,X), meta_is(B,Y), less_eq(X,Y).

A >= B :-meta_is(A,X), meta_is(B,Y), greater_eq(X,Y).

A =:= B :-meta_is(A,X), meta_is(B,Y), arith_eq(X,Y).

A =\= B :-meta_is(A,X), meta_is(B,Y), arith_dif(X,Y).

'$lesseq'(<).
'$lesseq'(=).

'$gteq'(>).
'$gteq'(=).

'$noteq'(<).
'$noteq'(>).

% BinProlog USER LEVEL naming primitives
% DO NOT rely on def/3, set/3, val/3,rm/2. Thay are unsafe
% and their implementation may change in the future...

/********* BASIC operations: safe with respect of bb_gc ***********/

% WARNING: may be moved to C, do not use their componnents directly

bb_def(A,B,X):-saved(X,S),def(A,B,S).
bb_set(A,B,X):-saved(X,S),set(A,B,S).
bb_val(A,B,CX):-val(A,B,X),copy_term(X,CX).
bb_rm(A,B):-rm(A,B).

/******************DERIVED OPERATIONS ***********************/

bb_let(A,B,X):-val(A,B,_),!,bb_set(A,B,X).
bb_let(A,B,X):-bb_def(A,B,X).

bb_def(A,X):-bb_def(A,A,X).
bb_set(A,X):-bb_set(A,A,X).
bb_val(A,X):-bb_val(A,A,X).
bb_rm(A):-bb_rm(A,A).
bb_let(A,X):-bb_let(A,A,X).

saved(X,S):-save_term(X,NewX),!,S=NewX.
saved(X,S):-bb_gc,save_term(X,NewX),!,S=NewX.
saved(_,_):-bb_fail(saved/2).
saved(_,_):-
	restart,
	abort.

% GLOBAL LOGICAL VARIABLES

'=>'(K1#K2,X):-!,lval(K1,K2,X).
'=>'(K,X):-lval(K,K,X).

/* bboard garbage collector */

bb_gc:-
	bb_list(1,B),
	copy_term(B,NewB),
	bb_reset,
	bb_put_back(NewB).

bb_put_back([]).
bb_put_back([P,N,F,K,V|Xs]):-
        functor(O,P,N),
        functor(M,F,K),
	bb_put_1(O,M,V),
	bb_put_back(Xs).

bb_put_1(O,M,V):-save_term(V,NewV),set(O,M,NewV),!,NewV=V.
bb_put_1(O,M,_):-bb_fail(O+M).

bb_fail(X):-
	statistics(bboard,[_,Z]),
	user_error('blackboard overflow, left only'(Z),culprit(X)).

is_dynamic(P):-val(P,'$first',_).

% An easy patch to read.pl for basic float input - Paul Tarau, Oct. 1993

try_float(Ds,[atom('.'),integer(box(Fraq,FDs))|Ts],I,[atom(Float)|Ts1]):-!,
	try_float_exp(Ts,Ts1,Exp),name(NFraq,FDs),
	input_float(I,NFraq,Exp,Float).
try_float(Ds,Tokens,I,[integer(box(I,Ds))|Tokens]).

try_float_exp([atom(e)|Ts],Ts1,Exp):-try_float_exp1(Ts,Ts1,Exp),!.
try_float_exp([atom(EN)|Ts],Ts,Exp):-
	name(EN,[0'e|NL]),name(Exp,NL),integer(Exp),!.
try_float_exp(Ts,Ts,0).

try_float_exp1([atom('-'),integer(box(Exp,_))|Ts],Ts,Exp1):-!,
	Exp1 is 0-Exp.
try_float_exp1([atom('+'),integer(box(Exp,_))|Ts],Ts,Exp).

float_minus(F,NegF):-NegF is 0.0-F.

pow(A,X,R):-float_fun2(pow,A,X,R).
log(A,X,R):-float_fun2(log,A,X,R).

% F(X+Y) --> R used for Y=0
exp(X,R):-float_fun(exp,X,R).
log(X,R):-float_fun(log,X,R).
sin(X,R):-float_fun(sin,X,R). 
cos(X,R):-float_fun(cos,X,R).
tan(X,R):-float_fun(tan,X,R).

% F(X)+Y --> R used for Y=0
atan(X,R):-float_fun('T',X,R).
asin(X,R):-float_fun('S',X,R).
acos(X,R):-float_fun('C',X,R).

integer(X,R):-float_fun(integer,X,R).
float(X,R):-R is 0.0+X.

:-op(1000,xfy,',').
:-op(1100,xfy,(';')).

:-op(1200,xfx,('-->')).
:-op(1200,xfx,(':-')).
:-op(1200,fx,(':-')).
:-op(700,xfx,'is').
:-op(700,xfx,'=').

:-op(500,yfx,'-').
:-op(500,fx,'-').
:-op(500,yfx,'+').
:-op(500,fx,'+').
:-op(400,yfx,'/').
:-op(400,yfx,'*').

:-op(650,xfy,'.').
:-op(700,xfx,'>=').
:-op(700,xfx,'>').
:-op(700,xfx,'=<').
:-op(700,xfx,'<').
:-op(700,xfx,(=\=)).
:-op(700,xfx,'=:=').

:-op(300,fy,'~').
:-op(200,xfy,'^').
:-op(300,xfx,(mod)).
:-op(400,yfx,'>>').
:-op(400,yfx,'<<').
:-op(400,yfx,'//').
:-op(500,yfx,'#').
:-op(500,yfx,(\/)).
:-op(500,yfx,(/\)).

:-op(700,xfx,'@>=').
:-op(700,xfx,'@=<').
:-op(700,xfx,'@>').
:-op(700,xfx,'@<').

:-op(700,xfx,(\==)).
:-op(700,xfx,'==').
:-op(700,xfx,'=..').

:-op(900,fy,(not)).
:-op(900,fy,(\+)).

:-op(1050,xfy,'->').
:-op(1150,fx,(dynamic)).

:-op(1200,xfx,('::-')).
:-op(900,yfx,(':')).
:-op(600,xfx,'=>').
:-op(600,xfx,'<=').
:-op(700,xfx,'=:').
:-op(700,xfx,':=').

:-op(1200,fx,('?-')).
std_expand_term(C,D):-expand_term(C,D).

cwrite(X):-write(X).
ttyprint(X):-telling(F),tell(user),write(X),nl,tell(F).

symcat(Op,Type,OpType):-	
	[Link]="_",
	name(Op,Pref),
	name(Type,Suf),
	append(Pref,[Link|Suf],Chars),
	!,
	name(OpType,Chars).

% appends a last argument to a term

termcat(T,C,TC):-T=..LT,append(LT,[C],LTC),!,TC=..LTC.
%   File   : READ.PL
%   Author : D.H.D.Warren + Richard O'Keefe
%   Updated: 5 July 1984 
%   Purpose: Read Prolog terms in Dec-10 syntax.
/*
    Modified by Alan Mycroft to regularise the functor modes.
    This is both easier to understand (there are no more '?'s),
    and also fixes bugs concerning the curious interaction of cut with
    the state of parameter instantiation.

    Since this file doesn't provide "metaread", it is considerably
    simplified.  The token list format has been changed somewhat, see
    the comments in the RDTOK file.

    I have added the rule X(...) -> apply(X,[...]) for Alan Mycroft.
*/

read(X):-prolog_read(X).
read_term(T,V) :- prolog_r_term(T,V).

print(X):-portable_print(X).
display(X):-telling(F),tell(user),portable_display(X),tell(F).
write(X):-portable_write(X).
writeq(X):-portable_writeq(X).
ttyprint(X):-telling(F),tell(user),cwrite(X),nl,tell(F).
ttynl:-telling(F),tell(user),nl,tell(F).
ttyput(X):-telling(F),tell(user),put(X),tell(F).

%   prolog_r_term(?Answer, ?Variables)
%   reads a term from the current input stream and unifies it with
%   Answer.  Variables is bound to a list of [Atom=Variable] pairs.

prolog_r_term(Answer, Variables) :-
	repeat,
	    read_tokens(Tokens, Variables),
	    r_and_check(Tokens,Term),
	!,
	Answer = Term.

r_and_check(Tokens,Term):-
	rt(Tokens, 1200, Term, LeftOver),
	all_read(LeftOver).
r_and_check(Tokens,_):-
	syntax_error(Tokens).

%   all_read(+Tokens)
%   checks that there are no unparsed tokens left over.

all_read([]) :- !.
all_read(S) :-
	syntax_error([operator,expected,after,expression], S).


%   expect(Token, TokensIn, TokensOut)
%   reads the next token, checking that it is the one expected, and
%   giving an error message if it is not.  It is used to look for
%   right brackets of various sorts, as they're all we can be sure of.

expect(Token, [Token|Rest], Rest) :- !.
expect(Token, S0, _) :-
	syntax_error([Token,or,operator,expected], S0).


%   I want to experiment with having the operator information held as
%   ordinary Prolog facts.  For the moment the following predicates
%   remain as interfaces to current_op. (i.e unfolded to val/3 - Paul Tarau)
%   prefixop(O -> Self, Rarg)
%   postfixop(O -> Larg, Self)
%   infixop(O -> Larg, Self, Rarg)


prefixop(Op, Prec, Prec) :-
	val(Op, fy, Prec), !.
prefixop(Op, Prec, Less) :-
	val(Op, fx, Prec),
	Less is Prec-1.

postfixop(Op, Prec, Prec) :-
	val(Op, yf, Prec), !.
postfixop(Op, Less, Prec) :-
	val(Op, xf, Prec), Less is Prec-1.

infixop(Op, Less, Prec, Less) :-
	val(Op, xfx, Prec ), !, Less is Prec-1.
infixop(Op, Less, Prec, Prec) :-
	val(Op, xfy, Prec), !, Less is Prec-1.
infixop(Op, Prec, Prec, Less) :-
	val(Op, yfx, Prec), Less is Prec-1.

ambigop(F, L1, O1, R1, L2, O2) :-
	postfixop(F, L2, O2),
	infixop(F, L1, O1, R1).

%   rt(+TokenList, +Precedence, -Term, -LeftOver)
%   parses a Token List in a context of given Precedence,
%   returning a Term and the unread Left Over tokens.

rt([], _, _, _) :- syntax_error([expression,expected], []).
rt([Token|RestTokens], Precedence, Term, LeftOver) :-
	rts(Token, RestTokens, Precedence, Term, LeftOver).

%   rts(+Token, +RestTokens, +Precedence, -Term, -LeftOver)

rts(var(Variable,_), ['('|S1], Precedence, Answer, S) :- !,
	rt(S1, 999, Arg1, S2),
	r_args(S2, RestArgs, S3), !,
	exprtl0(S3, apply(Variable,[Arg1|RestArgs]), Precedence, Answer, S).
rts(var(Variable,_), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Variable, Precedence, Answer, S).
rts(atom(-), [integer(box(Integer,_))|S1], Precedence, Answer, S) :-
	Negative is 0-Integer, !,
	exprtl0(S1, Negative, Precedence, Answer, S).
rts(atom(-), [atom(F)|S1], Precedence, Answer, S) :-
	float(F),
	call_ifdef(float_minus(F,Negative),fail),!,
	exprtl0(S1, Negative, Precedence, Answer, S).
rts(atom(Functor), ['('|S1], Precedence, Answer, S) :- !,
	rt(S1, 999, Arg1, S2),
	r_args(S2, RestArgs, S3),
	Term =.. [Functor,Arg1|RestArgs], !,
	exprtl0(S3, Term, Precedence, Answer, S).
rts(atom(Functor), S0, Precedence, Answer, S) :-
	prefixop(Functor, Prec, Right), !,
	after_prefix_op(Functor, Prec, Right, S0, Precedence, Answer, S).
rts(atom(Atom), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Atom, Precedence, Answer, S).
rts(integer(box(Integer,_)), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Integer, Precedence, Answer, S).
rts('[', [']'|S1], Precedence, Answer, S) :- !,
	exprtl0(S1, [], Precedence, Answer, S).
rts('[', S1, Precedence, Answer, S) :- !,
	rt(S1, 999, Arg1, S2),
	r_list(S2, RestArgs, S3), !,
	exprtl0(S3, [Arg1|RestArgs], Precedence, Answer, S).
rts('(', S1, Precedence, Answer, S) :- !,
	rt(S1, 1200, Term, S2),
	expect(')', S2, S3), !,
	exprtl0(S3, Term, Precedence, Answer, S).
rts('((', S1, Precedence, Answer, S) :- !,
	rt(S1, 1200, Term, S2),
	expect(')', S2, S3), !,
	exprtl0(S3, Term, Precedence, Answer, S).
rts('{', ['}'|S1], Precedence, Answer, S) :- !,
	exprtl0(S1, '{}', Precedence, Answer, S).
rts('{', S1, Precedence, Answer, S) :- !,
	rt(S1, 1200, Term, S2),
	expect('}', S2, S3), !,
	exprtl0(S3, '{}'(Term), Precedence, Answer, S).
rts(string(List), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, List, Precedence, Answer, S).
rts(Token, S0, _, _, _) :-
	syntax_error([Token,cannot,start,an,expression], S0).


%   r_args(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ')' and returns a list of terms.

r_args([','|S1], [Term|Rest], S) :- !,
	rt(S1, 999, Term, S2), !,
	r_args(S2, Rest, S).
r_args([')'|S], [], S) :- !.
r_args(S, _, _) :-
	syntax_error([',)',expected,in,arguments], S).


%   r_list(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ['|' expr(999)] ']' and returns a list 
%   of terms.

r_list([','|S1], [Term|Rest], S) :- !,
	rt(S1, 999, Term, S2), !,
	r_list(S2, Rest, S).
r_list(['|'|S1], Rest, S) :- !,
	rt(S1, 999, Rest, S2), !,
	expect(']', S2, S).
r_list([']'|S], [], S) :- !.
r_list(S, _, _) :-
	syntax_error(['|]',expected,in,list], S).


%   after_prefix_op(+Op, +Prec, +ArgPrec, +Rest, +Precedence, 
%       -Ans, -LeftOver)

after_prefix_op(Op, Oprec, _, S0, Precedence, _, _) :-
	Precedence < Oprec, !,
	syntax_error([prefix,operator,Op,in,context,with,precedence,Precedence],
	S0).
after_prefix_op(Op, Oprec, _, S0, Precedence, Answer, S) :-
	peepop(S0, S1),
	prefix_is_atom(S1, Oprec), % can't cut but would like to
	exprtl(S1, Oprec, Op, Precedence, Answer, S).
after_prefix_op(Op, Oprec, Aprec, S1, Precedence, Answer, S) :-
	rt(S1, Aprec, Arg, S2),
	Term =.. [Op,Arg], !,
	exprtl(S2, Oprec, Term, Precedence, Answer, S).


%   The next clause fixes a bug concerning "mop dop(1,2)" where
%   mop is monadic and dop dyadic with higher Prolog priority.

peepop([atom(F),'('|S1], [atom(F),'('|S1]) :- !.
peepop([atom(F)|S1], [infixop(F,L,P,R)|S1]) :- infixop(F, L, P, R).
peepop([atom(F)|S1], [postfixop(F,L,P)|S1]) :- postfixop(F, L, P).
peepop(S0, S0).


%   prefix_is_atom(+TokenList, +Precedence)
%   is true when the right context TokenList of a prefix operator
%   of result precedence Precedence forces it to be treated as an
%   atom, e.g. (- = X), p(-), [+], and so on.

prefix_is_atom([Token|_], Precedence) :- prefix_is_atom(Token, Precedence).

prefix_is_atom(infixop(_,L,_,_), P) :- L >= P.
prefix_is_atom(postfixop(_,L,_), P) :- L >= P.
prefix_is_atom(')', _).
prefix_is_atom(']', _).
prefix_is_atom('}', _).
prefix_is_atom('|', P) :- 1100 >= P.
prefix_is_atom(',', P) :- 1000 >= P.
prefix_is_atom([],  _).


%   exprtl0(+Tokens, +Term, +Prec, -Answer, -LeftOver)
%   is called by read/4 after it has read a primary (the Term).
%   It checks for following postfix or infix operators.

exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	ambigop(F, L1, O1, R1, L2, O2), !,
	(   exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, 
		Answer, S)
	;   exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, 
		Answer, S)
	).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	infixop(F, L1, O1, R1), !,
	exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, Answer, S).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	postfixop(F, L2, O2), !,
	exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, Answer, S).
exprtl0([','|S1], Term, Precedence, Answer, S) :-
	Precedence >= 1000, !,
	rt(S1, 1000, Next, S2), !,
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).
exprtl0(['|'|S1], Term, Precedence, Answer, S) :-
	Precedence >= 1100, !,
	rt(S1, 1100, Next, S2), !,
	exprtl(S2, 1100, (Term;Next), Precedence, Answer, S).
exprtl0([Thing|S1], _, _, _, _) :-
	cant_follow_expr(Thing, Culprit), !,
	syntax_error([Culprit,follows,expression], [Thing|S1]).
exprtl0(S, Term, _, Term, S).

cant_follow_expr(atom(_),	atom).
cant_follow_expr(var(_,_),	variable).
cant_follow_expr(integer(_),	integer).
cant_follow_expr(string(_),	string).
cant_follow_expr('((',		bracket).
cant_follow_expr('(',		bracket).
cant_follow_expr('[',		bracket).
cant_follow_expr('{',		bracket).

exprtl([infixop(F,L,O,R)|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= O, C =< L, !,
	rt(S1, R, Other, S2),
	Expr =.. [F,Term,Other], /*!,*/
	exprtl(S2, O, Expr, Precedence, Answer, S).
exprtl([postfixop(F,L,O)|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= O, C =< L, !,
	Expr =.. [F,Term],
	peepop(S1, S2),
	exprtl(S2, O, Expr, Precedence, Answer, S).
exprtl([','|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 1000, C < 1000, !,
	rt(S1, 1000, Next, S2), !,
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).
exprtl(['|'|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 1100, C < 1100, !,
	rt(S1, 1100, Next, S2), !,
	exprtl(S2, 1100, (Term;Next), Precedence, Answer, S).
exprtl(S, _, Term, _, Term, S).


%   This business of syntax errors is tricky.  When an error is 
%   detected, we have to write out a message.  We also have to note 
%   how far it was to the end of the input, and for this we are 
%   obliged to use the data-base.  Then we fail all the way back to 
%   read(), and that prints the input list with a marker where the 
%   error was noticed.  If subgoal_of were available in compiled code 
%   we could use that to find the input list without hacking the 
%   data base.  The really hairy thing is that the original code 
%   noted a possible error and backtracked on, so that what looked 
%   at first sight like an error sometimes turned out to be a wrong 
%   decision by the parser.  This version of the parser makes
%   fewer wrong decisions, and my goal was to get it to do no
%   backtracking at all.  This goal has not yet been met, and it 
%   will still occasionally report an error message and then decide 
%   that it is happy with the input after all.  Sorry about that.


syntax_error(Message, List) :-
	seeing(F),see(user),
	start_syntax_error(Message,List),
	see(F),
	fail.

start_syntax_error(Message,List):-
	nl, display('** SYNTAX ERROR: '),
	display_list(Message),
	length(List, Length),
	bb_let(syntax_error, length, Length),!.
start_syntax_error(Message,List).

syntax_error(List) :-
	seeing(F),see(user),
	finish_syntax_error(List),
	see(F),
	fail.

finish_syntax_error(List):-
	val(syntax_error, length, AfterError), 
	length(List, Length),
	BeforeError is Length-AfterError,
	display_list(List, BeforeError),!.
display_list([Head|Tail]) :-
	ttyput(32),
	display_token(Head), !,
	display_list(Tail).
display_list([]) :-
	ttynl.

display_list(X, 0) :-
	display(' <HERE=> '), !,
	display_list(X, 99999).
display_list([Head|Tail], BeforeError) :-
	display_token(Head),
	ttyput(32),
	Left is BeforeError-1, !,
	display_list(Tail, Left).
display_list([], _) :-
	ttynl.

display_token(atom(X))	  :- float(X),!,write_float(X).
display_token(atom(X))	  :- !,display(X).
display_token(var(_,X))	  :- !,	display(X).
display_token(integer(box(I,X))) :- !,display(I).
display_token(string(Xs))  :- !,
	det_append([0'"|Xs],[0'"],L),
	name(N,L),display(L).
display_token(X):-display(X).


% --------- rdtok.pl ----------------
%   File   : RDTOK.PL
%   Author : R.A.O'Keefe
%   Updated: 2 July 1984
%   Purpose: Tokeniser in reasonably standard Prolog.

/*  This tokeniser is meant to complement the library READ routine.
    It recognises Dec-10 Prolog with the following exceptions:

	%( is not accepted as an alternative to {

	%) is not accepted as an alternative to )

	NOLC convention is not supported (r_name could be made to 
		do it)

	,.. is not accepted as an alternative to | (hooray!)

	large integers are not read in as xwd(Top18Bits,Bottom18Bits)

	After a comma, "(" is read as '((' rather than '('.  This does 
		the parser no harm at all, and the Dec-10 tokeniser's 
		behaviour here doesn't actually buy you anything.  
		This tokeniser guarantees never to return '(' except 
		immediately after an atom, yielding '((' every
		other where.

    In particular, radix notation is EXACTLY as in Dec-10 Prolog 
    version 3.53.  Some times might be of interest.  Applied to an 
    earlier version of this file:

	this code took		    1.66 seconds
	the Dec-10 tokeniser took   1.28 seconds [DEC-10 assembler -Tim]
	A Pascal version took	    0.96 seconds

    The Dec-10 tokeniser was called via the old RDTOK interface, with
    which this file is compatible.  One reason for the difference in
    speed is the way variables are looked up: this code uses a linear
    list, while the Dec-10 tokeniser uses some sort of tree.  The 
    Pascal version is the program WLIST which lists "words" and their 
    frequencies.  It uses a hash table.  Another difference is the way 
    characters are classified: the Dec-10 tokeniser and WLIST have a 
    table which maps ASCII codes to character classes, and don't do 
    all this comparison and memberchking.  We could do that without 
    leaving standard Prolog, but what do you want from one evening's 
    work?
*/


%   read_tokens(TokenList, Dictionary)
%   returns a list of tokens.  It is needed to "prime" r_tokens/2
%   with the initial blank, and to check for end of file.  The
%   Dictionary is a list of AtomName=Variable pairs in no particular 
%   order.  The way end of file is handled is that everything else 
%   FAILS when it hits character "-1", sometimes printing a warning.  
%   It might have been an idea to return the atom 'end_of_file' 
%   instead of the same token list that you'd have got from reading 
%   "end_of_file. ", but (1) this file is for compatibility, and (b) 
%   there are good practical reasons for wanting this behaviour.

read_tokens(TokenList, Dictionary) :-
  r_toks(32, Dict, ListOfTokens),
  append(Dict, [], Dict), !, %  fill in the "hole" at the end
  Dictionary = Dict,	     %  unify explicitly so we'll read and
  TokenList = ListOfTokens.  %  then check even with filled in arguments
read_tokens([atom(end_of_file)], []). %  Eof is all that can go wrong

r_toks(-1, _, _) :- !,	     %  -1 is the end-of-file character
	fail.			     %  in every standard Prolog
r_toks(Ch, Dict, Tokens) :-
	Ch =< 32,	     	     %  ignore layout.  CR, LF, and the
	!,			     %  Dec-10 newline character (31)
	get0(NextCh),		     %  are all skipped here.
	r_toks(NextCh, Dict, Tokens).
r_toks(37, Dict, Tokens) :- !,	%  %comment
	repeat,				%  skip characters to a line
	    get0(Ch),
	    is_terminator(Ch),
	!,	%  stop when we find one
	Ch =\= -1,			%  fail on EOF
	get0(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(47, Dict, Tokens) :- !,	%  /*comment?
	get0(NextCh),
	r_solidus(NextCh, Dict, Tokens).
r_toks(33, Dict, [atom(!)|Tokens]) :- !,	%  This is a special case so
	get0(NextCh),			%  that !. reads as two tokens.
	r_after_atom(NextCh, Dict, Tokens).	%  It could be cleverer.
r_toks(40, Dict, ['(('|Tokens]) :- !,	%  NB!!!
	get0(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(41, Dict, [')'|Tokens]) :- !,
	get0(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(44, Dict, [','|Tokens]) :- !,
	get0(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(59, Dict, [atom((;))|Tokens]) :- !,	%   ; is nearly a punctuation
	get0(NextCh),			%   mark but not quite (e.g.
	r_toks(NextCh, Dict, Tokens).	%   you can :-op declare it).
r_toks(91, Dict, ['['|Tokens]) :- !,
	get0(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(93, Dict, [']'|Tokens]) :- !,
	get0(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(123, Dict, ['{'|Tokens]) :- !,
	get0(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(124, Dict, ['|'|Tokens]) :- !,
	get0(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(125, Dict, ['}'|Tokens]) :- !,
	get0(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(46, Dict, Tokens) :- !,		%  full stop
	get0(NextCh),				%  or possibly .=. &c
	r_fullstop(NextCh, Dict, Tokens).
r_toks(34, Dict, [string(S)|Tokens]) :- !,	%  "string"
	r_string(S, 34, NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(39, Dict, [atom(A)|Tokens]) :- !,	%  'atom'
	r_string(S, 39, NextCh),
	name(A, S),		%  BUG: '0' = 0 unlike Dec-10 Prolog
	r_after_atom(NextCh, Dict, Tokens).
r_toks(Ch, Dict, [var(Var,Name)|Tokens]) :- is_maj(Ch),!,
	%  have to watch out for "_"
	r_name(Ch, S, NextCh),
	(  S = "_", Name = '_'			%  anonymous variable
	;  name(Name, S),			%  construct name
	   r_lookup(Dict, Name=Var)		%  lookup/enter in dictionary
	), !,
	r_toks(NextCh, Dict, Tokens).
r_toks(Ch, Dict, Tokens) :- is_num(Ch),!,
	r_integer(Ch, I, NextCh,Digits),
	r_toks(NextCh, Dict, Tokens1),
	try_float(Digits,Tokens1,I,Tokens).
r_toks(Ch, Dict, [atom(A)|Tokens]) :- is_min(Ch),!,
	r_name(Ch, S, NextCh),
	name(A, S),
	r_after_atom(NextCh, Dict, Tokens).
r_toks(Ch, Dict, [atom(A)|Tokens]) :-	% THIS MUST BE THE LAST CLAUSE
	get0(AnotherCh),
	r_symbol(AnotherCh, Chars, NextCh),	% might read 0 chars
	name(A, [Ch|Chars]),			% so might be [Ch]
	r_after_atom(NextCh, Dict, Tokens).


%   The only difference between r_after_atom(Ch, Dict, Tokens) and
%   r_tokens/3 is what they do when Ch is "(".  r_after_atom
%   finds the token to be '(', while r_tokens finds the token to be
%   '(('.  This is how the parser can tell whether <atom> <paren> must
%   be an operator application or an ordinary function symbol 
%   application.  See the library file READ.PL for details.

r_after_atom(40, Dict, ['('|Tokens]) :- !,
	get0(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_after_atom(Ch, Dict, Tokens) :-
	r_toks(Ch, Dict, Tokens).


%   r_string(Chars, Quote, NextCh)
%   reads the body of a string delimited by Quote characters.
%   The result is a list of ASCII codes.  There are two complications.
%   If we hit the end of the file inside the string this predicate 
%   FAILS.  It does not return any special structure.  That is the 
%   only reason it can ever fail.  The other complication is that when
%   we find a Quote we have to look ahead one character in case it is 
%   doubled.  Note that if we find an end-of-file after the quote we 
%   *don't* fail, we return a normal string and the end of file 
%   character is returned as NextCh.  If we were going to accept 
%   C-like escape characters, as I think we should, this would need 
%   changing (as would the code for 0'x).  But the purpose of this 
%   module is not to present my ideal syntax but to present something 
%   which will read present-day Prolog programs.

r_string(Chars, Quote, NextCh) :-
	get0(Ch),
	r_string(Ch, Chars, Quote, NextCh).

r_string(-1, _, Quote, -1) :-
	display('! end of file in: '), ttyput(Quote),
	display(token), ttyput(Quote), ttynl,
	!, fail.
r_string(Quote, Chars, Quote, NextCh) :- !,
	get0(Ch),			% closing or doubled quote
	more_string(Ch, Quote, Chars, NextCh).
r_string(Char, [Char|Chars], Quote, NextCh) :-
	r_string(Chars, Quote, NextCh).	% ordinary character


more_string(Quote, Quote, [Quote|Chars], NextCh) :- !,
	r_string(Chars, Quote, NextCh).	% doubled quote
more_string(NextCh, _, [], NextCh).		% end



% r_solidus(Ch, Dict, Tokens)
%   checks to see whether /Ch is a /* comment or a symbol.  If the
%   former, it skips the comment.  If the latter it just calls 
%   r_symbol.  We have to take great care with /* comments to 
%   handle end of file inside a comment, which is why r_solidus/2 
%   passes back an end of file character or a (forged) blank that we 
%   can give to r_tokens.


r_solidus(42, Dict, Tokens) :- !,
	get0(Ch),
	r_solidus(Ch, NextCh),
	r_toks(NextCh, Dict, Tokens).
r_solidus(Ch, Dict, [atom(A)|Tokens]) :-
	r_symbol(Ch, Chars, NextCh),		% might read 0 chars
	name(A, [47|Chars]),
	r_toks(NextCh, Dict, Tokens).

r_solidus(-1, -1) :- !,
	display('! end_of_file in /*.. comment'), ttynl.
r_solidus(42, LastCh) :-
	get0(NextCh),
	NextCh =\= 47, !,	%  might be ^Z or * though
	r_solidus(NextCh, LastCh).
r_solidus(42, 32) :- !.	%  the / was skipped in the previous clause
r_solidus(_, LastCh) :-
	get0(NextCh),
	r_solidus(NextCh, LastCh).


%   r_name(Char, String, LastCh)
%   reads a sequence of letters, digits, and underscores, and returns
%   them as String.  The first character which cannot join this sequence
%   is returned as LastCh.

r_name(Char, [Char|Chars], LastCh) :-
	is_alpha_num(Char),!,
	get0(NextCh),
	r_name(NextCh, Chars, LastCh).
r_name(LastCh, [], LastCh).

%   r_symbol(Ch, String, NextCh)
%   reads the other kind of atom which needs no quoting: one which is
%   a string of "symbol" characters.  Note that it may accept 0
%   characters, this happens when called from r_fullstop.

r_symbol(Char, [Char|Chars], LastCh) :-
	is_spec(Char),
	get0(NextCh),
	r_symbol(NextCh, Chars, LastCh).
r_symbol(LastCh, [], LastCh).


%   r_fullstop(Char, Dict, Tokens)
%   looks at the next character after a full stop.  There are
%   three cases:
%	(a) the next character is an end of file.  We treat this
%	    as an unexpected end of file.  The reason for this is
%	    that we HAVE to handle end of file characters in this
%	    module or they are gone forever; if we failed to check
%	    for end of file here and just accepted .<EOF> like .<NL>
%	    the caller would have no way of detecting an end of file
%	    and the next call would abort.
%	(b) the next character is a layout character.  This is a
%	    clause terminator.
%	(c) the next character is anything else.  This is just an
%	    ordinary symbol and we call r_symbol to process it.

r_fullstop(-1, _, _) :- !,
	display('! end_of_file just after full_stop'), ttynl,
	fail.
r_fullstop(Ch, _, []) :-
	Ch =< 32, !.		% END OF CLAUSE
r_fullstop(Ch, Dict, [atom(A)|Tokens]) :-
	r_symbol(Ch, S, NextCh),
	name(A, [46|S]),
	r_toks(NextCh, Dict, Tokens).


%   r_integer is complicated by having to understand radix notation.
%   There are three forms of integer:
%	0 ' <any character>	- the ASCII code for that character
%	<digit> ' <digits>	- the digits, read in that base
%	<digits>		- the digits, read in base 10.
%   Note that radix 16 is not understood, because 16 is two digits,
%   and that all the decimal digits are accepted in each base (this
%   is also true of C).  So 2'89 = 25.  I can't say I care for this,
%   but it does no great harm, and that's what Dec-10 Prolog does.
%   The X =\= -1 tests are to make sure we don't miss an end of file
%   character.  The tokeniser really should be in C, not least to
%   make handling end of file characters bearable.  If we hit an end
%   of file inside an integer, r_integer will fail.

r_integer(BaseChar, IntVal, NextCh,Digits) :-
	Base is BaseChar - 48,
	get0(Ch),
	r_int(Ch, Base, IntVal, NextCh, Ds), Digits=[0'.,BaseChar|Ds].
/*
 % Paul Tarau - float like 0.0003 used to become 0.3
	( name(Name,Digits),
	  errmes(number(Name),Digits)->true
        ; true
        ).
*/

r_int(-1,_, _, _, _):-!,fail.
r_int(39, 0, IntVal, NextCh, [39,IntVal]):-!,
	get0(IntVal), IntVal =\= -1, get0(NextCh).
r_int(39, Base, IntVal, NextCh, Ds):-
	r_digits(0, Base, IntVal, NextCh, Ds),!.
r_int(Ch,Base,IntVal, NextCh, Ds):-
	r_digs(Ch, Base, 10, IntVal, NextCh ,Ds).

r_digits(SoFar, Base, Value, NextCh, Ds) :-
	get0(Ch),
	Ch =\= -1,
	r_digs(Ch, SoFar, Base, Value, NextCh, Ds).

r_digs(Digit, SoFar, Base, Value, NextCh, [Digit|Ds]) :-
	Digit >= 48, Digit =< 57,!,
	Temp is SoFar*Base, Temp1 is Temp-48, Next is Temp1+Digit,
	r_digits(Next, Base, Value, NextCh, Ds).
r_digs(LastCh, Value, _, Value, LastCh, []).

%   r_lookup is identical to memberchk except for argument order and
%   mode declaration.

r_lookup([X|_], X) :- !.
r_lookup([_|T], X) :-
	r_lookup(T, X). 

prolog_read(X):-prolog_r_term(X,_).

% added for speed: Paul Tarau, Jan 92 : to be done in C

is_alpha_num(Char):- Char >= 97, Char =< 122. % a..z
is_alpha_num(Char):- Char >= 65, Char =< 90.  % A..Z
is_alpha_num(Char):- Char >= 48, Char =< 57.   % 0..9
is_alpha_num(95).
is_alpha_num(Char):- is_latin1_min(Char).
is_alpha_num(Char):- is_latin1_maj(Char).

is_maj(Ch):- Ch >= 65, Ch =< 90.
is_maj(95).
is_maj(Ch):-is_latin1_maj(Ch).

is_min(Char):- Char >= 97, Char =< 122.
is_min(Char) :- is_latin1_min(Char).

is_num(Char):- Char >= 48, Char =< 57.

% support for latin1 - thanks to Ulrich Neumerkel

is_latin1_maj(Ch) :- Ch >= 192, Ch =< 214. 
is_latin1_maj(Ch) :- Ch >= 216, Ch =< 222. 

is_latin1_min(Ch) :- Ch >= 223, Ch =< 246.
is_latin1_min(Ch) :- Ch >= 248, Ch =< 255.

is_terminator(10).
is_terminator(13).
is_terminator(-1).

%	Char is in "#$&*+-./:<=>?@\^`~"

is_spec(35).
is_spec(36).
is_spec(38).
is_spec(42).
is_spec(43).
is_spec(45).
is_spec(46).
is_spec(47).
is_spec(58).
is_spec(60).
is_spec(61).
is_spec(62).
is_spec(63).
is_spec(64).
is_spec(92).
is_spec(94).
is_spec(96).
is_spec(126).

% neads also append/3

/*
test_read(ok):-repeat,prolog_r_term(R,S),write(R-S),nl,R=end_of_file,!.
*/

% TOPLEVEL
toplevel(bye):-
	repeat,
    		topstep,
	!.

topstep:-
	telling(O),tell(user),cwrite('?- '),
	seeing(I),see(user),
	read_term(Body,Vs),
	topinterp(Body,Vs,I,O).
	
topinterp(Goal,_,_,_):-Goal==end_of_file,!.
topinterp(Goal,Vs,I,O):-
	see(I),tell(O),
	report_answers(Vs,Goal),
	fail.

[File]:-compile(File).

compile(File):-
	find_file(File,F),
	compile_mem(F),fail.

find_file(File,NewFile):-
	seeing(CF),
	find_file1(CF,File,NewFile).

find_file1(CF,File,NewFile):-
	see_a_file(
		["","progs/","myprogs/"],File,
		[".pl",".pro",""],NewFile),
	!,
	see(CF).
find_file1(CF,File,_):-
	see(CF),
	errmes('file * *.pl *.pro not found in . progs myprogs:',File).

see_a_file(Prefs,File,Sufs,NewFile):-
	member(Suf,Sufs),
	member(Pref,Prefs),
	name(File,L),
	det_append(L,Suf,R),
	det_append(Pref,R,Fname),
	name(NewFile,Fname),
	see_or_fail(NewFile),
	seen.

report_answers([],Goal):-metacall(Goal),!,
	report_ok(yes).
report_answers([V|Vs],Goal):-
	metacall(Goal),
	telling(F),
	tell(user),
	report_one_var(V),report_top_vars(Vs),
	another_sol(Ok),
	tell(F),
        ( Ok=no->!,report_ok(yes)
	; fail
	).
report_answers(_,_):-
	report_ok(no).

report_ok(X):-cwrite(X),nl.
	
report_top_vars(Eqs):-
	member(V=E,Eqs),
	cwrite(','),nl,cwrite(V),cwrite(=),writeq(E),
	fail.
report_top_vars(_).

report_one_var(V=E):-
	cwrite(V),cwrite(=),writeq(E).

another_sol(Ok):-
	current_op(1200,fx,('?-')),!, % interactive...
	get0(A),user_action(A,Ok),nl.
another_sol(yes):-cwrite(';'),nl,nl.

user_action(10,no):-!.
user_action(59,yes):-!,get0(10).
user_action(_,Ok):-cwrite(' ; for more, <return> otherwise '),
  get0(10),get0(U),user_action(U,Ok).

interactive(yes):-op(1200,fx,('?-')).
interactive(no):-op(1199,fx,('?-')).

expand_term(C,E):-portable_expand_term(C,E).
std_expand_term(C,D):-portable_expand_term(C,D).

metacall(G):-call_ifdef(meta_interpreter(G),G).
%   File   : WRITE.PL
%   Author : Richard A. O'Keefe.
%   Updated: 22 October 1984
%   Purpose: Portable definition of write/1 and friends.

/* minor changes: Paul Tarau 1992
- uses cwrite (written in C) for simple things
- disconects unimplemented current_... predicates
- minor change by replacing (strange) l_magic with l_magic_nl
- removed code not used in BinProlog
*/

portable_display(Term) :-
	w_out(Term, display).

portable_print(Term) :-
	w_out(Term, print).

portable_write(Term) :-
	w_out(Term, write).

portable_writeq(Term) :-
	w_out(Term, writeq).

w_out(Term,Mode):-w_out(Term, Mode, 1200, punct,_),fail.
w_out(_,_).
	

%   maybe_paren(P, Prio, Char, Ci, Co)
%   writes a parenthesis if the context demands it.

maybe_paren(P, Prio, Char, _, punct) :-
	P > Prio,
	!,
	put(Char).
maybe_paren(_, _, _, C, C).

%   maybe_space(LeftContext, TypeOfToken)
%   generates spaces as needed to ensure that two successive
%   tokens won't run into each other.

maybe_space(punct, _) :- !.
maybe_space(X, X) :- !,
	put(32).
maybe_space(quote, alpha) :- !,
	put(32).
maybe_space(_, _).

%   put_string(S)
%   writes a list of character codes.

put_string([]).
put_string([H|T]) :-
	put(H),
	put_string(T).


%   put_string(S, Q)
%   writes a quoted list of character codes, where the first
%   quote has already been written.  Instances of Q in S are doubled.

put_string([], Q) :-
	put(Q).
put_string([Q|T], Q) :- !,
	put(Q), put(Q),
	put_string(T, Q).
put_string([H|T], Q) :-
	put(H),
	put_string(T, Q).



%   w_variable(V)
%   is system dependent.  This just uses whatever Prolog supplies.

w_variable(V) :-
	cwrite(V).

portray(T):-write(T).

%   w_out(Term, Style, Priority, Ci, Co)
%   writes out a Term in a given Style (display,write,writeq,print)
%   in a context of priority Priority (that is, operators with
%   greater priority have to be quoted), where the last token to be
%   written was of type Ci, and reports that the last token it wrote
%   was of type Co.

w_out(Term, _, _, Ci, alpha) :-
	var(Term),
	!,
	maybe_space(Ci, alpha),
	w_variable(Term).
w_out('$VAR'(N), Style, _, Ci, Co) :- !,
	w_VAR(N, Style, Ci, Co).
w_out(N, _, _, Ci, alpha) :-
	integer(N),
	(   N < 0, maybe_space(Ci, other)
	;   maybe_space(Ci, alpha)
	),  !,
	cwrite(N).
w_out(Term, print, _, Ci, alpha) :-
	portray(Term),
	!.
w_out(Atom, Style, Prio, _, punct) :-
	atom(Atom),
	current_op(P, _, Atom),
	P > Prio,
	!,
	put(40),
	(   Style = writeq, w_atom(Atom, Style, punct, _)
	;   cwrite(Atom)
	),  !,
	put(41).
w_out(Atom, Style, _, Ci, Co) :-
	atom(Atom),
	!,
	w_atom(Atom, Style, Ci, Co).
w_out(Term, display, _, Ci, punct) :- !,
	functor(Term, Fsymbol, Arity),
	w_atom(Fsymbol, display, Ci, _),
	w_args(0, Arity, Term, 40, display).
w_out({Term}, Style, _, _, punct) :- !,
	put(123),
	w_out(Term, Style, 1200, punct, _),
	put(125).
w_out([Head|Tail], Style, _, _, punct) :- !,
	put(91),
	w_out(Head, Style, 999, punct, _),
	w_tail(Tail, Style).
w_out((A,B), Style, Prio, Ci, Co) :- !,
	%  This clause stops writeq quoting commas.
	maybe_paren(1000, Prio, 40, Ci, C1),
	w_out(A, Style, 999, C1, _),
	put(44),
	w_out(B, Style, 1000, punct, C2),
	maybe_paren(1000, Prio, 41, C2, Co).
w_out(N, _, _, Ci, alpha) :-
	float(N),
	(   N < 0,maybe_space(Ci, other)
	;   maybe_space(Ci, alpha)
	),  !,
	write_float(N).
w_out(Term, Style, Prio, Ci, Co) :-
	functor(Term, F, N),
	w_out(N, F, Term, Style, Prio, Ci, Co).


w_out(1, F, Term, Style, Prio, Ci, Co) :-
	(   current_op(O, fx, F), P is O-1
	;   current_op(O, fy, F), P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	w_atom(F, Style, C1, C2),
	arg(1, Term, A),
	w_out(A, Style, P, C2, C3),
	maybe_paren(O, Prio, 41, C3, Co).
w_out(1, F, Term, Style, Prio, Ci, Co) :-
	(   current_op(O, xf, F), P is O-1
	;   current_op(O, yf, F), P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	arg(1, Term, A),
	w_out(A, Style, P, C1, C2),
	w_atom(F, Style, C2, C3),
	maybe_paren(O, Prio, 41, C3, Co).
w_out(2, F, Term, Style, Prio, Ci, Co) :-
	(   current_op(O, xfy, F), P is O-1, Q = O
	;   current_op(O, xfx, F), P is O-1, Q = P
	;   current_op(O, yfx, F), Q is O-1, P = O
	),  !,
	maybe_paren(O, Prio, 40, Ci, C1),
	arg(1, Term, A),
	w_out(A, Style, P, C1, C2),
	w_oper(F, O, Style, C2, C3),
	arg(2, Term, B),
	w_out(B, Style, Q, C3, C4),
	maybe_paren(O, Prio, 41, C4, Co).
w_out(N, F, Term, Style, Prio, Ci, punct) :-
	w_atom(F, Style, Ci, _),
	w_args(0, N, Term, 40, Style).


w_oper(Op, Prio, Style, Ci, Co) :-
	Prio < 700, !,
	w_atom(Op, Style, Ci, Co).
w_oper(Op, _, Style, Ci, punct) :-
	put(32),
	w_atom(Op, Style, punct, _),
	put(32).


w_VAR(N, Style, Ci, alpha) :-
	integer(N), N >= 0, !,
	maybe_space(Ci, alpha),
	Temp is N mod 26, Letter is Temp + 65, % $$$
	put(Letter),
	(   N < 26
	;   Rest is N//26, cwrite(Rest)	), !.
w_VAR(A, Style, Ci, Co) :-
	atom(A), !,
	w_atom(A, write, Ci, Co).
w_VAR(X, Style, Ci, punct) :-
	w_atom('$VAR', Style, Ci, _),
	w_args(0, 1, '$VAR'(X), 40, Style).


w_atom(('!'), _, _, punct) :- !,
	put(33).
w_atom((';'), _, _, punct) :- !,
	put(59).
w_atom([], _, _, punct) :- !,
	put(91), put(93).
w_atom('{}', _, _, punct) :- !,
	put(123), put(125).
w_atom(Atom, Style, Ci, Co) :-
	name(Atom, String),
	(   classify_name(String, Co),
	    maybe_space(Ci, Co),
	    put_string(String)
	;   Style = writeq, Co = quote,
	    maybe_space(Ci, Co),
	    put(39), put_string(String, 39)
	;   Co = alpha,
	    put_string(String)
	),  !.

%   classify_name(String, Co)
%   says whether a String is an alphabetic identifier starting
%   with a lower case letter (Co=alpha) or a string of symbol characters
%   like ++/=? (Co=other).  If it is neither of these, it fails.  That
%   means that the name needs quoting.  The special atoms ! ; [] {} are
%   handled directly in w_atom.  In a basic Prolog system with no
%   way of changing the character classes this information can be
%   calculated when an atom is created, and just looked up.  This has to
%   be as fast as you can make it.

/* Paul Tarau -> defined in read.pl: is_min, etc. */

classify_name([H|T], alpha) :-
	is_min(H),
	!,
	classify_alpha_tail(T).
classify_name([H|T], other) :-
        is_spec(H),
	classify_other_tail(T).

classify_alpha_tail([]).
classify_alpha_tail([H|T]) :-
	is_alpha_num(H),
	classify_alpha_tail(T).

classify_other_tail([]).
classify_other_tail([H|T]) :-
        is_spec(H),
	classify_other_tail(T).


%   w_args(DoneSoFar, Arity, Term, Separator, Style)
%   writes the remaining arguments of a Term with Arity arguments
%   all told in Style, given that DoneSoFar have already been written.
%   Separator is 0'( initially and later 0', .

w_args(N, N, _, _, _) :- !,
	put(41).
w_args(I, N, Term, C, Style) :-
	put(C),
	J is I+1,
	arg(J, Term, A),
	w_out(A, Style, 999, punct, _),
	w_args(J, N, Term, 44, Style).


%   w_tail(Tail, Style)
%   writes the tail of a list of a given style.

w_tail(Var, _) :-			%  |var]
	var(Var),
	!,
	put(124),
	w_variable(Var),
	put(93).
w_tail([], _) :- !,			%  ]
	put(93).
w_tail([Head|Tail], Style) :- !,	%  ,Head tail
	put(44),
	w_out(Head, Style, 999, punct, _),
	w_tail(Tail, Style).
w_tail(Other, Style) :-		%  |junk]
	put(124),
	w_out(Other, Style, 999, punct, _),
	put(93).

pp_term(T):-numbervars(T,0,_),write(T),nl,fail.
pp_term(_).

pp_clause(C):-portray_clause(C).

portray_clause(C):-pp_clause0(C),fail.
portray_clause(_).

pp_clause0(:-(Body)) :- !,
	nl,
	numbervars(Body, 0, _),
	l_clauses(Body, 0, 2, 8). % 0!='' in sicstus,sb..
pp_clause0((Pred:-Body)) :-
	numbervars(Pred+Body, 0, _),
	portable_writeq(Pred),
	l_clauses(Body, 0, 2, 8), !.
pp_clause0((Pred)) :-
	pp_clause0((Pred:-true)).


l_clauses((A,B), L, R, D) :- !,
	l_clauses(A, L, 1, D), !,
	l_clauses(B, 1, R, D).
l_clauses(true, L, 2, D) :- !,[P]=".",
	put(P), nl.
l_clauses((A;B), L, R, D) :- !,
	l_magic(fail, L, D),
	l_magic((A;B), 0, 2, D),
	l_magic_nl(R, '.').

l_clauses((A->B), L, R, D) :- !,
	l_clauses(A, L, 5, D), !,
	l_clauses(B, 5, R, D).
l_clauses(Goal, L, R, D) :-
	l_magic(Goal, L, D),
	portable_writeq(Goal),
	l_magic_nl(R,'.').

l_magic(!,    0, D) :- !,
	cwrite(' :- ').
l_magic(!,    1, D) :- !,
	cwrite(',  ').
l_magic(Goal, 0, D) :- !,
	cwrite(' :- '),
	nl, tab(D).
l_magic(Goal, 1, D) :- !, [Char]=",",
	put(Char),
	nl, tab(D).
l_magic(Goal, 3, D) :- !,
	cwrite('(   ').
l_magic(Goal, 4, D) :- !,
	cwrite(';   ').
l_magic(Goal, 5, D) :- !,
	cwrite(' ->'),
	nl, tab(D).
l_magic(Goal, Key, D) :-
	atom(Key),
	cwrite((':- ')), cwrite(Key),
	nl, tab(D).

l_magic_nl(2, C) :- !, cwrite(C),nl.
l_magic_nl(_, _).

l_magic((A;B), L, R, D) :- !,
	l_magic(A, L, 1, D), !,
	l_magic(B, 1, R, D).
l_magic(Conj,  L, R, D) :-
	E is D+8,
	M is L+3,
	l_clauses(Conj, M, 1, E),
	nl, tab(D),
	l_magic2(R, ')' ).

l_magic2(2, C) :- !, cwrite(C).
l_magic2(_, _).

