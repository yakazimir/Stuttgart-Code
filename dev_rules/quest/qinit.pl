"PRS (1.0)"


"Copyright (c) 2004-2010 by the Palo Alto Research Center.  All rights
reserved. This software is made available AS IS, and PARC makes no
warranty about the software, its performance or its conformity to any
specification."

"
Author:  Danny Bobrow
%%%==========================================================================
%%%
%%% qinit.pl
%%%
%%% Initialization file  quadri
%%%==========================================================================
"
meta_info(string, %L)
==>
input(%L).

"Get rid of unneeded things from akr0 analysis"

"should be in sem_lex_cleanup"
lex_class(%%,%%) ==> 0.
vn_semantics(%%,%%) ==> 0.
sortal_restriction( %%,%%,%%) ==> 0.

"other scaffolding"
calendrical_type(%%,%%,%%) ==> 0.
context_for_real_tense(%%) ==> 0.
event(%%) ==> 0.
event_level_telicity(%%,%%) ==> 0.
event_measure_mod(%%,%%,%%) ==> 0.
lexical_level_telicity(%%,%%) ==> 0.
meta_info(%%,%%) ==> 0.
perf(%%,%%) ==> 0.
pres(%%,%%) ==> 0.
progr(%%,%%) ==> 0.
past(%%,%%)==>0.
rb_temp_frame(%%,%%)  ==> 0.
real_tense(%%,%%,%%) ==> 0.
skolem_surface_form(%%,%%) ==> 0.
temp-perspective(%%,%%)  ==> 0.
viewpoint_level_telicity( %%,%%) ==> 0.
specifier(%%,%%) ==>0.
link(%%,%%,%%) ==>0.

explicit_state_passed_in ==>0.


"Getting rid of what as a noun -- hope it works for all cases"

skolem_origin(%W,%%,what, noun),
subconcept(%W, %%),
definite(%W)
  ==>
 0.


"This reverses head and mod for 'of'. check possesives"
role(prep(of), %Mod, %Head)
==> 
role(prep(inv-of), %Head, %Mod).

"FOR NOW A BIT OF A HACK KDR"

role(prep(inv-of),%%,%%)
 ==> 
0. 

"Remove redundant prep(during)"

 (+role(tprep(%P),%V1, %V2)
 | +role(prep(%P),%V1, %V2)),
  @neq(%P, during),
 (role(prep(during),%V1, %V2) | role(tprep(during),%V1, %V2))
+==>
0.

"DGB Handle all imperatives, Give, show, find"

context_relation(t, ctx(%VerbSk), imperative)
==>
imperative(%VerbSk).

"Remove null-pro subject"
 +imperative(%VSk),
 role(sb, %VSk, %Sb),
 skolem_origin(%Sb, %%,%%,implicit),
 subconcept(%Sb, %%)
==>
  0.
"Remove indirect object e.g. find *me* X"
 +imperative(%VSk),
 role(prep(to), %VSk, %Ob),
 skolem_origin(%Ob, %%,%%,%%),
 subconcept(%Ob, %%)
==>
  0.

"Remove choice space for all attachments of prep phrases"
 +imperative(%VSk),
 (role(prep(%%), %VSk, %%Ob) | temporalRel(%%, %VSk, %%))
==>
 stop.

"Remove all null-pro objects"

skolem_origin(%%,%%,%%,implicit)
==> 0.


"eliminate any imperative with args other than direct object"

 +imperative(%VSk),
 role(%Role, %VSk, %%),
 @neq(%Role, ob)
==>
  stop.


"========
  remove last argument from nn_element
we never use it. Make it an ordinry role.

ALSO: If we have the same word treated as an adjective 
and a noun as a modifier of a head noun,
 eliminate the adjectival (subsective) role.
"

"For a word that is both an adjective and a noun, 
eliminate the choice where it is an
adjective."

  +role(subsective, %%HeadSk, %AdjSk)
==>
  <1>used_as_adj(%AdjSk).

  +used_as_adj(%ModSk),
  +skolem_origin(%ModSk, %%C, %%W, noun)
==>
  <1>eliminate_adj(%ModSk).

 +eliminate_adj(%ModSk),
 role(subsective, %%HeadSk, %ModSk)
==>
 stop.

  eliminate_adj(%%), used_as_adj(%%) ==> 0.

"Simplify nn_element role -- removing position"

  role(nn_element, %HeadSk, %ModSk, %%N),
  +skolem_origin(%ModSk, %%C, %%W, noun)
==>
  role(nn_element, %HeadSk, %ModSk).

"Create lisp readable skolems  for each AKR skolem, 
and substitute them in
role expressions, and other expressions used by Quadri"

"DGB Find all nouns -- and create a map"

((skolem_origin(%Sk, %C, %Name, noun), @eq(%P, noun))
 | skolem_origin(%Sk, %C, not, %P)),
@eq(%Sk, %Name:n(%Pos, snum(%SNum))),
@new_quadri_var(%Name, %NewSk)
+==>
skolem_origin(%NewSk,  %C, %Name, noun),
skolem_map(%Sk, %NewSk),
qpos(%NewSk, %SNum, %Pos).

"Precompute  pronouns that can be used to refer to skolems"


   +skolem_map(%Sk, %NewSk),
  (  (-role(cardinality_restriction,%Sk, %Card), {memberchk(%Card, [sg, mass])})
   | +skolem_origin(%Sk,  %%C, patient, noun)   )
+==>
   pronoun(%NewSk, it).


   +skolem_origin(%Sk,  %%C, patient, noun)   
+==>
   pronoun(%Sk, they).


   +skolem_map(%%Sk, %NewSk),
    -pronoun(%NewSk, %%)
==>
    pronoun(%NewSk, it).



"replace old skolems in roles"

+skolem_map(%Sk, %NewSk),
 role(%R, %Sk, %Val)
+==>
 role(%R, %NewSk, %Val).


+skolem_map(%Sk, %NewSk),
 role(%R, %Val,%Sk, %N)
+==>

 role(%R, %Val, %NewSk, %N).


+skolem_map(%Sk, %NewSk),
 role(%R, %Val,%Sk)
+==>
 role(%R, %Val, %NewSk).

"replace skolems in temporalRels"

+skolem_map(%Sk, %NewSk),
 temporalRel(%R, %Val,%Sk)
+==>
 temporalRel(%R, %Val, %NewSk).


+skolem_map(%Sk, %NewSk),
 temporalRel(%R,%Sk, %Val)
+==>
 temporalRel(%R, %NewSk, %Val).

"replace skolems in pred_role"

+skolem_map(%Sk, %NewSk),
 pred_role(%R, %Val,%Sk)
+==>
 pred_role(%R, %Val, %NewSk).


+skolem_map(%Sk, %NewSk),
 pred_role(%R,%Sk, %Val)
+==>
 pred_role(%R, %NewSk, %Val).

"for group objects"

+skolem_map(%Sk, %NewSk),
 group_members([%Sk,%Other],%Group)
+==>
 group_members([%NewSk,%Other],%Group).
 
+skolem_map(%Sk, %NewSk),
 group_members([%Other, %Sk],%Group)
+==>
 group_members([%Other, %NewSk],%Group).

"replace group object skolem"

+skolem_map(%Sk, %NewSk),
 group_members(%L, %Sk)
+==>
 group_members(%L, %NewSk).
 

"for subconcept"

  +skolem_map(%Sk, %NewSk),
  subconcept(%Sk, %Subs)
==>
  subconcept(%NewSk, %Subs).

   +skolem_map(%Sk, %NewSk),
  scopal_divergence(%Verb, %Sk)
==>
  scopal_divergence(%Verb, %NewSk).


+skolem_map(%Sk, %NewSk),
 definite(%Sk)
+==>
 definite(%NewSk).


"If there is an ending-subinterval and a prep during, eliminate the latter"

 role(ending-subinterval, %E, %T),
 role(prep(during), %E, %T)
==>
 equal(%E, end-time(%T)).

 role(starting-subinterval, %E, %T),
 role(prep(during), %E, %T)
==>
 same_time(%E, start-time(%T)).

 role(starting-subinterval, %E, %T),
 role(prep(during), %E, %T)
==>
 same_time(%E, start-time(%T)).

"Experiment to get rid of prep(during) for the moment.  Cleo will provide a longer term fix"
"role(prep(during), %%,%%) ==> stop."


"===============
create quantifier types from cardinality restrictions
Formerly in Prolog
========="

  +skolem_origin(%Ob, %%C, %%W, noun),
  -role(cardinality_restriction,%Ob, %%Card)
==>
  quantifier(%Ob, exists).

  role(cardinality_restriction,%Ob, %Card),
  {member(%Card, [mass, pl, sg, some(%%)])}
==>
  quantifier(%Ob, exists).

  role(cardinality_restriction,%Ob, %Card),
  {member(%Card, [all(pl), any(sg)])}
==>
  quantifier(%Ob, all).

  role(cardinality_restriction,%Ob, qp(complex_card, [%Fn, %N])),
  {member(%Fn, [more` than, greater` than, over])}
==>
  quantifier(%Ob, complex_card(greater, %N)).

  role(cardinality_restriction,%Ob, qp(complex_card, [%Fn, %N])),
  {member(%Fn, [at` least, greater` than` or` equal])}
==>
  quantifier(%Ob, complex_card(greater_eq, %N)).

  role(cardinality_restriction,%Ob, qp(complex_card, [%Fn, %N])),
  {member(%Fn, [less` than, fewer` than, under])}
==>
  quantifier(%Ob, complex_card(less,%N)).

  role(cardinality_restriction,%Ob, qp(complex_card, [%Fn, %N])),
  {member(%Fn, [at` most, less` than` or` equal])}
==>
  quantifier(%Ob, complex_card(less_eq, %N)).

  role(cardinality_restriction,%Ob, qp(complex_card, [%Fn, %N])),
  -quantifier(%Ob, %%)
==>
  quantifier(%Ob, complex_card(%Fn,%N)).

  role(cardinality_restriction,%Ob, %Card),
  -quantifier(%Ob, %%)
==>
  quantifier(%Ob, %Card).


"=======
Make subject of copula a subject of real verb"

 role(copula_subj, %Be, %Sb)
==>
 role(sb, %Be, %Sb).

"=====
DGB patch to convert  temporals found by general temporal code
into tpreps for synlinks.  We may want to change the names
e.g. startsAfterEndingOf -->after, duration--> for
Perhaps some but not all."

"table of transformations
can't we revise the rules in temporal file to include original role relation?"
|- trole_trans(startsAfterEndingOf, after).
|- trole_trans(startsBeforeStartOf, before).
|- trole_trans(duration, for).
|- trole_trans(temporallySubsumes, on). "!!!" 

  "Remove temoralRel re Now"
  temporalRel(%%, %Arg1, %Arg2),
  (@eq(%Arg1,Now) | @eq(%Arg2,Now))
==>
  0.

  +temporalRel(%Rel, %Sk, %Time),
  (trole_trans(%Rel, %Role) 
   |(-trole_trans(%Rel, %%), @eq(%Role, %Rel)))
==>
  role(prep(%Role), %Sk, %Time).
"need to add rule for when the Time is the first argument,"

"Get rid of 'what' as a noun"
+role(query, %%Sk, %QueryTerm),
skolem_origin(%QueryTerm, %%,%%,%%)
==>
0.

