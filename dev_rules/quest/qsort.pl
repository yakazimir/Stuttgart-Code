"PRS (1.0)"


"This gets the qsort information, as is separated from the scope_roles since this information is needed before in order to do the role mapping before the scope rules apply."

"========
Compute sort for quantifiers."

  +skolem_origin(%Sk, %%, %%, noun),
  sort_synset(%SortName, %SortWN),
  -qsort(%Sk, %%),
  @check_type(%Sk, %SortWN)
==>
  qsort(%Sk, %SortName).

  +skolem_origin(%Sk, %%, %W, noun),
  time-points(%L),
  {memberchk(%W, %L)}
==>
  qsort(%Sk, time_point).

 +skolem_origin(%Sk, %%, %W, noun),
  drug-classes(%L),
  {memberchk(%W, %L)},
  qsort(%Sk, %%)
==>
  qsort(%Sk, drug_class).


"======
If we have a group object with two elements of the same sort
then make that group object have that sort, 
give it a qsort of constant, and give the
group object a qval of the form and(member1, member2) or or(member1, member2)
  Assuming all group objects have only 2 members
**Remove choices from the choice space
which have groups containing either member. 
"
   group_members([%Memb1, %Memb2], %GroupOb),
   +qsort(%Memb1, %Sort),
   +qsort(%Memb2, %Sort),
   +subconcept(%GroupOb, and(%%,%%))
==>
   qsort(%GroupOb, %Sort),
   qtype(%GroupOb, exists),
   qval(%GroupOb, and(%Memb1, %Memb2)),
   <1>eliminate_group(%Memb1,%Memb2).

   group_members([%Memb1, %Memb2], %GroupOb),
   +qsort(%Memb1, %Sort),
   +qsort(%Memb2, %Sort),
   +subconcept(%GroupOb, or(%%,%%))
==>
   qsort(%GroupOb, %Sort),
   qtype(%GroupOb, exists),
   qval(%GroupOb, or(%Memb1, %Memb2)),
   <1>eliminate_group(%Memb1,%Memb2).


"=====
eliminate group objects that are tightly bound of the same type"

  eliminate_group(%Memb1,%Memb2),
  group_members(%L, %%GroupOb),
  ({memberchk(%Memb1, %L)} | {memberchk(%Memb2, %L)})
==>
  stop.

  eliminate_group(%%Memb1,%%Memb2) ==>0.


"For group objects, change and/or to and-<sort>, etc, and add scope"

|- group_sort(and, drug, and-drug).
|- group_sort(or, drug, or-drug).
|- group_sort(and, mutation, and-mutation).
|- group_sort(or, mutation, or-mutation).


  qval(%Group, qp(%Conj, [%Memb1, %Memb2])),
  +qsort(%Group, %Sort),
  group_sort(%Conj, %Sort, %Typed_conj)
==>
  qval(%Group,  qp(%Typed_conj, [%Memb1, %Memb2])),
  scopes_over(nscope, %Group, %Memb1),
  scopes_over(nscope, %Group, %Memb2).


"For sorted terms, if they are modified by an unsorted noun,
create a label relation.
remove the quantifier computed for this noun as modifier"

  role(nn_element, %HeadSk, %ModSk),
  +qsort(%HeadSk, %%),
  -qsort(%ModSk, %%),
  @termName(%ModSk, %Mod, %%),
  quantifier(%ModSk, %%)     
==>
  qlabel(%HeadSk, %Mod).


"Use adjectives as labels"

  role(subsective, %HeadSk, %ModSk),
  +qsort(%HeadSk, %%),
   @termName(%ModSk, %Mod, %%)
==>
  qlabel(%HeadSk, %Mod).


  +qsort(%Q, %S) ==> qclass(%Q,%S).

  +qsort(%Q, %S),
   medical_tests(%L),   
  {memberchk(%S, %L)}
==>
  qclass(%Q, medical_test).


"special cases for qclass."

  +qsort(%Q, mutation) ==> qclass(%Q, test_result).

  "DGB
Take out temporarilyto test
+time_expr(%Q, %%) ==> qclass(%Q, time_period). "

"compute definite reference equalities assuming uniqueness.
Needs to be generalized"

  +definite(%A),
  +qsort(%A, %S),
  +qsort(%B, %S),
  -definite(%B)
==>
  equal(%B, %A).

"Make prepositions into tpreps if they are followed by 
something marked with qclass as time_period
Should we include events as well as time_period"

  role(prep(%P), %Headsk, %Modsk),
  +qclass(%Modsk, time_period)
==>
   role(tprep(%P), %Headsk, %Modsk).




