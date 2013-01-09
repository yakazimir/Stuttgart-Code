"PRS (1.0)"


"Copyright (c) 2004-2010 by the Palo Alto Research Center.  All rights
reserved. This software is made available AS IS, and PARC makes no
144warranty about the software, its performance or its conformity to any
specification."

"
Author:  Danny Bobrow, Cleo Condoravdi, Kyle Richardson
%%%==========================================================================
%%%
%%% scope_rules.pl
%%%
%%% File for constructing quadri queriesdetermining scope of roles
Tentative
%%%==========================================================================
"


"==========
compute quantifier type"

"===============
create quantifier types from cardinality restrictions
Formerly in Prolog using cardQuant
Now computed in qinit
========="

"   definite(%Sk)
==>
  specification(%Sk, definite)."


"Get names first"
  +qsort(%Sk, %%Sort),
  @check_type(%Sk, named_object#n#1),
  @termName(%Sk, %Val, noun)
==>
  qname(%Sk, %Val).

"Special case for genotype test because it is introduced, and doesn't have subconcept"
  +qclass(%Sk, genotype_test)
==>
  qname(%Sk,genotype_test). 


"=========
Now get qtype, using earlier computed quantifiers"


  +qsort(%Sk,%%Sort),
  -qtype(%Sk, %%),
  quantifier(%Sk, %Quant)
==>
  qtype(%Sk, %Quant).


"------------------------
Now put modifications of quantified variables in 
restriction or nuclear scope.
Translation to domain specific terms is done in quadri.pl
 ------------------------"


"Fix  time expressions to have the right head"

"Head switching for roles with value of time period
Every patient took 12 weeks of azt."

"***changed this include synlinks"
  +synlink(prep(of), %V1, %V),
  +qsort(%V1, time_period),
  +synlink(%R, %Sk, %V1)
==>
  role(%R, %Sk, %V).

"switch of --> for"

"***changed this include synlinks"
  +synlink(prep(of), %V1, %V),
  +qsort(%V1, time_period)
==>
  role(prep(for), %V, %V1).

"***THIS IS KEPT THE SAME FROM THE OLDER SCOPE RULES, SINCE THERE IS SOMETHING SPECIAL ABOUT NUMERALS SUCH AS 24. WE MIGHT WANT TO FULLY INTEGRATE THIS WITH THE NEW SYSTEM"

"Add to time_periods their time_measure
Their current quantifier is interpreted as a measure
and they are given the standard qtype of exists"

  +qsort(%Sk, time_period),
   qtype(%Sk, %Qtype),
  @termName(%Sk, %Unit, noun),
  ((@eq(%Qtype, exists), @eq(%QT, 1))
   |( @neq(%Qtype, exists), @eq(%QT, %Qtype))) 
==>
   qtype(%Sk, exists),
   in(restriction,%Sk, time_measure(%Sk, %QT, %Unit)).


"=====================================
NEW SCOPE RULES USING SEMLINKs"



"SPECIAL RULE FOR COPULA examples
DGB: Where does copula come from? This looks like for prepositional phrase examples
"

  (+semlink(%Arg1, %Arg2, via(prep(%P)))
 | +semlink(%Arg1, %Arg2, via(tprep(%P))) ),
  +qtype(%Arg1, %%),
  +qtype(%Arg2, %%),
  +pred_role(prep(%P),%Arg1, %Arg2)
==>
  semlink(%Arg1, %Arg2, via(prep(%P), copula)),
   scopes_over(nscope, %Arg1, %Arg2),
   in(nscope, %Arg2,copula(%Arg1, %Arg2)).

"ENDING SUBINTERVAL"

  +semlink(%Headsk, %Modsk, via(ending-subinterval))
==> 
  scope_over(restriction, %Headsk, %Modsk), 
  in(restriction, %Headsk, ending-subinterval(%Headsk, %Modsk)). 


"SPECIAL RULE FOR NN_ELEMENTS" 
  +semlink(%Headsk, %Modsk, via(nn_element))
==>
   scopes_over(restriction, %Headsk, %Modsk), 
   in(restriction,%Headsk, nn_element(%Headsk, %Modsk)).

"SPECIAL RULE FOR IMPLICIT PATHs"

   +semlink(%Headsk, %Modsk, via(implicit)) 
==> 
   scopes_over(restriction, %Headsk, %Modsk), 
   in(restriction, %Modsk, implicit(%Headsk, %Modsk)).



"FOR DIRECT PP LINKAGES" 

  (+semlink(%Headsk, %Modsk, via(prep(%P))) 
| +semlink(%Headsk, %Modsk, via(tprep(%P)))),
  -pred_role(prep(%P) ,%Headsk, %Modsk)
==> 
  scopes_over(restriction, %Headsk, %Modsk), 
   in(restriction, %Modsk, qp(%P, [%Headsk, %Modsk])).  
      
"RULE FOR PP linkage with verb
after 24 weeks on a regimen the patient had 123e."

  (+semlink(%Headsk, %Modsk, via(prep(%P), %%))
  |
  (+semlink(%Headsk, %Modsk, via(tprep(%P), %%)), @eq(%P, on))),
  -pred_role(prep(%P),%Headsk, %Modsk)    
==> 
  scopes_over(restriction, %Headsk, %Modsk), 
  in(restriction, %Modsk, qp(%P, [%Headsk, %Modsk])).



"FOR VERB CO-ARGUMENT"
"DGB: Adding check for relative Clause"
  +skolem_origin(%Verbsk, %%, %Verb, verb), 
  +semlink(%Arg1, %Arg2, via(%Verbsk)), 
  -qsort(%Arg2, time_period),
  ((role(inv(%%), %Arg1,%Verbsk), @eq(%QPart,restriction ))
  |(-role(inv(%%), %Arg1,%Verbsk), @eq(%QPart, nscope)))
==>
  scopes_over(%QPart, %Arg1, %Arg2),
   in(%QPart, %Arg2,  qp(%Verb, [%Arg1, %Arg2])). 


"    +skolem_origin(%Verbsk, %%, %Verb, verb), 
  +semlink(%Arg1, %Arg2, via(%Verbsk))
==> 
  in(nscope, %Arg1, qp(%Verb, [%Arg1, %Arg2])). "


"handle qname and qlabel for scope -- both go in restriction"

 
qp(%Fn, [%Sk, %Nm]),
  {memberchk(%Fn, [qname, qlabel])}
==>
  in(restriction, %Sk, qp(%Fn, [%Sk, %Nm])).

  "negation rule
If something is marked as scopally divergent
it outscopes the negation.
  This should work for:
the regimen did not contain azt.
a patient did not take azt.
a patient who took a drug did not take azt."


  +context_lifting_relation(antiveridical,%C1,ctx(%Verb)),
  +context_relation(%C1,ctx(%Verb),%Not),
  +skolem_map(%Not, %NewNot)
==>
  negated(%Verb, %NewNot),
  qtype(%NewNot,negation),
  qsort(%NewNot,not).

"Use the not skolem as a quantifier,
and create a nested scope with the negations"

  negated(%Verb, %Not),
  scopal_divergence(%Verb, %Subj),
  -scopes_over(%%, %Subj, %%Expr)
+==>
  scopes_over(nscope,  %Subj, %Not). 

"For no patient had azt."
  negated(%Verb, %Not),
  scopal_divergence(%Verb, %Subj),
  scopes_over(%P, %Subj, %Expr)
+==>
  scopes_over(%P,  %Subj, %Not),
  scopes_over(nscope,%Not, %Expr). 
   
  negated(%Verb, %Not),
  +synlink(sb, %Verb, %Subj),
  -scopal_divergence(%Verb, %Subj)
+==>
  scopes_over(nscope,%Not, %Subj). 
   
"For no patient had azt. or the patient had no azt."
  qtype(%Var, no)",
  (synlink(sb, %%, %Var) | synlink(ob, %%, %Var))"
==>
  qtype(%Var, exists).

"Link anything linked to the verb to the subject by restriction
if it is otherwise unlinked
Ex: The patient had a high veiral load after 24 weeks on a regimen with norir.
This scopes patient over week"

 +synlink(sb, %Vsk, %Sb),
 +synlink(%%, %Vsk, %Other),
  @neq(%Sb, %Other),
 -scopes_over(%%,%%, %Other)
==>
  scopes_over(restriction, %Sb, %Other).
