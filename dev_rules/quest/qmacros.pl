"PRS (1.0)"

"Copyright (c) 2004-2011 by the Palo Alto Research Center.  All rights
reserved. This software is made available AS IS, and PARC makes no
warranty about the software, its performance or its conformity to any
specification."

"
Author:  Danny Bobrow, Cleo Condoravdi, Kyle Richardson
%%%==========================================================================
%%%
%%% qmacros.pl was fromerly part of qdeclarations.pl
%%%
%%% File for defining macros, for quadri sorts
%%%==========================================================================
"


termName(%Sk, %Name, %POS) := 
  +skolem_origin(%Sk, %%, %Name, %POS).

"local pruning"

eliminateRole ::

  +eliminate(%R, %V),
  role(%R, %%, %V)
==>
  stop;    

  eliminate(%%,%%) ==> 0.

"Add super classes ."

|- qclass_up([
" [regimen, time_period],"
 [genotype_test, medical_test],
 [viral_load, medical_test],  
 [mutation, test_result],
 [outstanding_debt, debt]
 ]).


qclass_promote ::
  qclass_up(%L),
  {member([%Qs, %Qup],%L)},
 +qsort(%X, %Qs)

+==>
  qclass(%X, %Qup).
 

"Find all direct links"
"direct_synlink ::"
direct_semlink ::
   direct_link(%Head, %Mod),
  +qclass(%Headsk, %Head), 
  +qclass(%Modsk, %Mod),
  role(%Path, %Headsk, %Modsk) 
==> 
"  synlink(%Headsk, %Modsk, via(%Path)), "
  semlink(%Headsk, %Modsk, via(%Path)),
  <1>eliminate(%Path, %Modsk);

  @eliminateRole. 

"for finding relations between sorts of objects 
 through predicates"

"Linkage to the verb -- creates common head links"
verb_synlink ::
   +skolem_origin(%Verbsk, %%, %%, verb),
   +qclass(%Modsk, %%Mod), 
    role(tprep(%Val), %Verbsk, %Modsk)
==> 
   synlink(tprep(%Val), %Verbsk, %Modsk),
   <1>eliminate(tprep(%Val), %Modsk); 

   +skolem_origin(%Verbsk, %%, %%, verb),
   +qclass(%Modsk, %%Mod),
    role(%Path, %Verbsk, %Modsk), 
    -role(tprep(%%), %Verbsk, %Modsk)
==> 
   synlink(%Path, %Verbsk, %Modsk), 
   <1>eliminate(%Path, %Modsk); 

   @eliminateRole. 


common_semlink ::
 (direct_link(%Head, %Mod)  | time_link(%Head, %Mod)),
  +qclass(%Headsk, %Head), 
  +qclass(%Modsk, %Mod),
  +synlink(%%,%Common_head,%Headsk),
  +synlink(tprep(%P),%Common_head,%Modsk),
  @neq(%Headsk,%Modsk)
==> 
  semlink(%Headsk, %Modsk, via(tprep(%P), %Common_head));

  (direct_link(%Head, %Mod)  | time_link(%Head, %Mod)),  
  +qclass(%Headsk, %Head), 
  +qclass(%Modsk, %Mod),
  +synlink(%%,%Common_head,%Headsk),
  +synlink(%%,%Common_head,%Modsk),
  @neq(%Headsk,%Modsk),
  -semlink(%Headsk, %Modsk, %%)
==> 
  semlink(%Headsk, %Modsk, via(%Common_head)). 


argument_label ::
  verb_arg_label(%Verb, %Role, %Sort, %Label),
  @termName(%Verbsk, %Verb, verb),
  +qclass(%Sortsk,%Sort),
  (+synlink(%Role, %Verbsk, %Sortsk)  
   | ( (+semlink(%Sortsk, %Modsk, %%), 
       +synlink(%%, %Verbsk, %Modsk), 
       -synlink(%Role, %Verbsk, %Sortsk))))
==>
  qlabel(%Sortsk, %Label). 


 mod_struct_direct ::
  mod_rel(%Head, %Mod, %Rel),
  +qclass(%Headsk, %Head), 
  +qclass(%Modsk, %Mod), 
  +semlink(%Headsk, %Modsk, %%),
  in(%QPart, %%, qp(%%, [%Headsk, %Modsk]))
==> 
  in(%QPart, %Modsk, qp (%Rel, [%Headsk, %Modsk])).


mod_struct_spec  ::
  mod_rel_spec(%Head, %Mod, %Path,%Rel),
  +qclass(%Headsk, %Head), 
  +qclass(%Modsk, %Mod), 
  +semlink(%Headsk, %Modsk, %Path),
  in(%QPart, %%, qp(%%, [%Headsk, %Modsk]))
==> 
  in(%QPart, %Modsk, qp (%Rel, [%Headsk, %Modsk])).
  

"Regimen ,Drug.. This handles indirect modification"
mod_struct_indirect ::
 mod_rel(%Head, %Mod, %Rel),
 +qclass(%Headsk, %Head), 
 +qclass(%Modsk, %Mod), 
 +semlink(%AnotherHead, %Headsk, %%),  
 in(%QPart, %%, qp(%%, [%AnotherHead, %Modsk])),
 -semlink(%Headsk, %Modsk, %%) 
==> 
  in(%QPart, %Modsk, qp(%Rel, [%Headsk, %Modsk])). 


"Eventuality is an expression like
patient-has-test(%P, %Test)
We want the time for this test.  Created by .
table tmod_rel specifies test_time as %Rel

We will also want to relate this time
to something else?? created by ttmod "
tmod_structure :: 
 tmod_rel(%Eventuality, %TemporalEntity, %Rel), 
 +in(%%, %%, qp(%Eventuality, [%%, %Arg2])), 
" this doesn't work for --after the regimen, the patient had a high viral load. 
no time period around
 +qclass(%%, time_period),"
 @new_quadri_var(%TemporalEntity, %TemporalEntitysk) 
==> 
 qclass(%TemporalEntitysk, %TemporalEntity),
 qsort(%TemporalEntitysk, %TemporalEntity),
 qtype(%TemporalEntitysk, exists),
 qpos(%TemporalEntitysk, 0, 0),
 in(nscope, %TemporalEntitysk, qp(%Rel, [%Arg2, %TemporalEntitysk])), 
 scopes_over(nscope, %Arg2, %TemporalEntitysk). 


"viral load after 24 weeks
prep=after
Temporal_entitysk = week_13
Arg1 = viral_load_7
time_pointsk = time of viral_load -- created by tmod "

ttmod_structure ::  
  +semlink(%Arg1, %Temporal_entitysk, via(tprep(%prep), %%)), 
  ttmod_rel(%prep, %Temporal_entity), 
 +qclass(%Temporal_entitysk, %Temporal_entity),
 +qclass(%Time_pointsk, time_point),  
  +in(%%, %%, qp(%%, [%Arg1, %Time_pointsk]))
==> 
 in(restriction, %Temporal_entitysk, qp(%prep, [%Time_pointsk, %Temporal_entitysk])),
 scopes_over(restriction, %Time_pointsk, %Temporal_entitysk). 



"A mediated_link is one that must be inserted between two known sorts
to connect them through a data structure that defines a time period.
patient drug requires regimen
patient mutation requires genotype_test

These intermediate links are created for any direct link between the two heads
Hypothesis: It is made definite, so that if one already exists
then it should be made equal.  Not clear if this is true. We need examples.
Counter example: A patient on norvir had a regimen with more than 2 drugs.

"

mediated_link ::
  link_mediator(%T1, %T2, %LinkType,%LinkSort),
  +qclass(%T1sk, %T1),
  +qclass(%T2sk, %T2),
  semlink(%T1sk,%T2sk, %Path),
  @new_quadri_var(%LinkType,%Linksk)
==>
  create_mediator(%T1sk, %T2sk, %LinkType, %Linksk, %LinkSort, %Path);

  link_mediator(%T1, %T2, %LinkType, %LinkSort),
  +qclass(%T1sk, %T1),
  +qclass(%T2sk, %T2),
  +synlink(%%P1, %V, %T1sk),
   synlink(%%P2, %V, %T2sk),
 -(+qsort(%LinkSk1, %LinkType), +synlink(%%, %V, %LinkSk1) ),
  @new_quadri_var(%LinkType,%Linksk)
==>
  create_mediator(%T1sk, %T2sk, %LinkType, %Linksk, %LinkSort, via(%V));


   +create_mediator(%T1sk, %T2sk, %LinkType, %Linksk, %%, %Path)
==>
   semlink(%T1sk, %Linksk,%Path),
   semlink(%Linksk, %T2sk, via(implicit)),
   qtype(%Linksk, exists),
   qsort(%Linksk, %LinkType),
   qclass(%Linksk, %LinkType),
   move_pred_role(%Path, %T1sk,%T2sk, %Linksk);

   move_pred_role (via(%Prep), %T1sk,%T2sk, %Linksk),
   pred_role(%Prep, %T1sk, %T2sk)
==>
    pred_role(%Prep, %T1sk, %Linksk);

    move_pred_role(%%,%%,%%,%%) ==> 0;

   @qclass_promote.

"================================
EXISTENTIAL GROUPS"



use_existential_groups ::

"Find existential groups"

 
"A non-existential quantifier has existentials under,
 or a top level existential has exitential under it"
   +qtype(%V, exists),
   +scopes_over(%%,%A,%V),
   (-qtype(%A, exists) | +top_level(%A))
+==>
   top_e(%V);

"A single existential on top"
"  +qtype(%V, exists),
  -scopes_over(%%,%%A,%V)
==>
   top_e(%V);"


  +scopes_over(%%, %V1, %V2),
  +qtype(%V2, exists)
==>
   branch(%V1, %V2);

"Initiate reachability relation, and list of reachable elements"
+top_e(%T) 
 +==> 
reachable(%T, %T), 
element_list(%T, []); 

" recurse  on reachability"
  +reachable(%X,%Y), branch(%Y, %Z) 
+*=> 
  reachable(%X,%Z);

"eliminate the top element"
reachable(%T, %T), 
+element_list(%T, [])
==>
0;

"Gather all reachable elements into a list"

reachable(%Top, %E) **  
[ element_list(%Top, %L) +==> element_list(%Top, [%E | %L]) ];

  +top_e(%T),
  element_list(%T, %L)
==>
 exists_group([%T | %L]);

  element_list(%%, %L)
==>
  exists_group(%L);

"get rid of existential groups with only a single element"

  exists_group([%%]) 
==> 0;

  exists_group(%L),
  @new_quadri_var(ex_grp, %G)
==>
  exists_group(%G, %L);
"replace all uses of a variable in exists_group with the list"

"Get rid of scopes for members of the group"
  scopes_over(%%,%V1,%V2),
  +exists_group(%%, %L),
  {member(%V1, %L)},
  {member(%V2, %L)}
==>
0;

"replace elements by group in scopes over"
  +exists_group(%G, %L),
  {member(%V, %L)},
  scopes_over(%%, %V, %V2)
+==>
  scopes_over(nscope, %G, %V2);

"there may be a problem here if the top level
element is a universal"
  +exists_group(%G, %L),
  {member(%V, %L)},
  scopes_over(%%P, %V1, %V)
+==>
  scopes_over(nscope, %V1, %G);

"Put all element expressions in any scope under list"

  +exists_group(%G, %L),
  {member(%V, %L)},
  in(%%,%V, %E)
+==>
  in(nscope,%G, %E).
