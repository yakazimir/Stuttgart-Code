"PRS (1.0)"


"Copyright (c) 2004-2010 by the Palo Alto Research Center.  All rights
reserved. This software is made available AS IS, and PARC makes no
warranty about the software, its performance or its conformity to any
specification."

"
Author:  Danny Bobrow, Kyle Richardson, Cleo Condoravdi 
(based on Quadri system)
%%%==========================================================================
%%%
%%% quest.pl
%%%
%%% File for constructing 'quest' queries (based on quadri version)
%%%==========================================================================
"
 

"grammar =  quadri." 
grammar = quest.
procedural_attachments = bio_procedures.
include(`.`./src/templates_macros).

:- set_transfer_option(prune_final_choice_space,1).
:- set_transfer_option(fixed_query_order,1).

"Includes macros for testing local relations"
include(qdeclarations).

"For show patients, drugs and regimens.
result marked with specified_answer,
equivalent to desired_answer(X) definite (X)
where the referent may be coming up"

+skolem_origin(%Sk, %%, %W, verb),
  {memberchk(%W, [show, display])},
role(ob, %Sk, %Group),
group_members(%L, %Group),
{member(%MSk, %L)},
skolem_origin(%MSk,%%,%M, %%)
+==>
  display(%M).

  +skolem_origin(%Sk, %%, show, verb),
  imperative(%Sk),
  role(ob, %Sk, %MSk),
  skolem_origin(%MSk,%%,%M, %%)
+==>
  display(%M).

include(qinit).





"Label desired answer
Do this before making links through a verb 
so that find isn't thought to be a verb when 
it is used in the imperative"
  imperative(%VSk),
  role(ob, %VSk, %Ob)
==>
  <1>eliminate_role(ob, %Ob),
  desired_answer(%Ob).

  eliminate_role(%R, %Ob),
  role(%R, %%, %Ob)
==>
  stop.

eliminate_role(%%,%%)
==>
0.

"We need to fix this up"
  role(query, %F, %What),
  query_term(%%, %What)
==>
  desired_answer(%F).





"created a separate file with the qsort information from the top of scope_rules, this is so that we can get the sort information without 
the rest of the scope information, in order to create these macros that occur before the scope_rules"

include(qsort). 

"Experiment to put in direct_links"

@direct_semlink.

"special case for without; needs to be generalizaed
  the patient had a regimen without azt."
  semlink(%R, %D, via(prep(without))),
  quantifier(%D, %%)
==>
  semlink(%R, %D, via(prep(with))),
  qtype(%D, no).

  
@verb_synlink.
@mediated_link.


"compute definite reference equalities assuming uniqueness.
Needs to be generalized"

  +definite(%A),
  +qsort(%A, %S),
  +qsort(%B, %S),
  -definite(%B)
==>
  equal(%B, %A).

@common_semlink.
@argument_label.

"Note: Scope_rules are moved up from below, and must occur before the 
Mod_struct occurs. This is obvious, since the mod_struct builds the final 
relations needed in the domain, and includes scoping information. 
"
include(scope_rules).
@mod_struct_spec.
@mod_struct_direct. "for model relations between specified etities that have direct semlinks" 
@mod_struct_indirect. "For cases of indirect modification"
@tmod_structure. "Implicit time information, relating eventualities to time_points, ect.."
@ttmod_structure. 

 include(qcleanup).





