"PRS (1.0)"


"Copyright (c) 2004-2010 by the Palo Alto Research Center.  All rights
reserved. This software is made available AS IS, and PARC makes no
warranty about the software, its performance or its conformity to any
specification."

"
Author:  Danny Bobrow
%%%==========================================================================
%%%
%%% qcleanup.pl
%%%
%%% File for constructing quadri queries
%%%==========================================================================
"

skolem_origin(%%,%%,%%,%%) ==> 0.
skolem_map(%%,%%) ==> 0.
quant(%%, %%,%%,%%) ==> 0.


"These can be quoted out to show in output."
	  
"state(%%) ==> 0."

"compute top_level"

  +scopes_over(%%, %V, %%),
  -scopes_over(%%, %%, %V)
+==>
  top_level(%V).


"Patch to make group objects come out with drug-combo and 
mutation-combo as types
A patient had a regimen with norvir and epivir."
|- combo-term(drug, drug-combo, regimen-contains-drug-combo).
|- combo-term(mutation, mutation-combo, patient-has-mutation-combo).

 +qval(%Sk, qp(%%, [%%,%%])),
  qsort(%Sk, %Type),
  combo-term(%Type, %Combo, %RelNew),
  in(%QV, %QP, qp(%%Rel, [%P, %Sk]))
+==>
  qsort(%Sk, %Combo),
  in(%QV, %QP, qp(%RelNew, [%P, %Sk])).


"put together patient-has-test-at-time 
patient-has-test
test-has-value
qname
medical-test-time"


  in(%%, %%, patient-has-test(%P,%Mtest)),
  in(%%, %%, test-has-value(%Mtest,%V)),
  +qname(%Mtest, %Mtype)
==>
  in(nscope, %V, patient-has-test(%P, %Mtest, %Mtype, %V)).



" put together possible referent expressions"

  +qsort(%V, %Sort),
  qpos(%V, %N1, %N2),
  pronoun(%V, %P)  
==>
  possible_referent( %V, %Sort, %P, [%N1, %N2]).

"macro defined in qdeclarations"

@use_existential_groups.


"put together quantifier expression"
  qtype(%V,%T),
  qsort(%V,%S)
==>
  quant(%T, %V,sort, %S).

  top_level(%P),
   possible_referent(%P,%%,%%,[%%,%N])
==>
   top_level(%P, %N).




"Mark nouns that never had a quant expression computed."

  quantifier(%N, %%) 
==>
  unknown_term(%N).




"print out  all quant and rel expressions
to stdout or  /tmp/lexbase.out
   use
s-call2 system set output stdout
   to print output to stdout"

specification(%P, definite) ==> definite(%P).

"collect output"

"special case for quant"
  +quant(%T, %V, sort, %S)
==>
  lispout(quant, quant(%T, %V, sort, %S)).

"  +possible_referent(%Value, %Sort, %Anaphor, %Position)
==> 
  lispout(possible_referent, possible_referent(%Value, %Sort, %Anaphor, %Position)). "

"one arg lists"
  {member(%Fn1,[desired_answer,display, definite, input])},
   +qp(%Fn1,[%A1])
+==> 
  lispout(%Fn1, qp(%Fn1,[%A1])).

"two arg lists"

+qp(%Fn2,[%A1, %A2]),
 ({member(%Fn2,[ top_level, qval, qname, qlabel, specification, equal, exists_group])} |
{member(%Fn2,
 [
HumanAgentKillingAPerson,
HumanKillingEventPersonKilledUpperBound,
eventLocationGPE,
organizationHasMember,
organizationHasName
 ])
}) 
+==>
  lispout(%Fn2, qp(%Fn2,[%A1,%A2])).

"three arg lists"
  {member(%Fn3,[scopes_over, in])},
  +qp(%Fn3,[%A1,%A2,%A3]), 
  @neq(%A3,role(%%,%%,%%))
+==> 
    lispout(%Fn3, qp(%Fn3,[%A1,%A2,%A3])).

"Print a left paren at start of list
using backquote to quote paren and cr"


"{getp(snark_fn(%Fn))},
{call_server(file, print, $$`
, %%)},
{call_server(file,print, `( , %%)},
{call_server(file, print, %Fn, %%)},
{call_server(file, print, ` `(quote`(`
	     , %%)}
==> 0."

{call_server(file, print, $$`
, %%)},
{call_server(file,print, `( , %%)}
==> 0.

![
  {member(%Key, 
    [input, top_level, desired_answer,  quant, qval, qlabel, qname, equal, definite, qval, exists_group, 
     scopes_over, in,  display, unknown, possible_referent])},
  lispout(%Key, %V),
  @print_lisp(%V)
]!
==>
0.
  
"print out a closing right paren and carriage return."
"{call_server(file, print, `)`)`)`

	     , %%)},
{call_server(file, print, $$`
       , %%)}
 ==> 0."

{call_server(file, print, `)`

	     , %%)},
{call_server(file, print, $$`
       , %%)}
 ==> 0.

frame_temp_mod(%%,%%)
==>
0.


