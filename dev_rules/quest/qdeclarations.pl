"PRS (1.0)"

"Copyright (c) 2004-2011 by the Palo Alto Research Center.  All rights
reserved. This software is made available AS IS, and PARC makes no
warranty about the software, its performance or its conformity to any
specification."

"
Author:  Danny Bobrow, Cleo Condoravdi, Kyle Richardson 
(based largely on Quadri code, changed by Kyle)
%%%==========================================================================
%%%
%%% qdeclarations.pl
%%%
%%% File for defining macros, declarations for quest sorts
%%%==========================================================================
"

include(qmacros).

"declaration of the sorts of objects in relations" 
|- sort_synset(debt_value, named_value#n#1). 
|- sort_synset(debt,debt#n#1).
|- sort_synset(company,company#n#1). 
|- sort_synset(dso,dso#n#1). 

"links between sorts of entities that satisfy 
relations that in our domain"
|- direct_link(company, debt).
|- direct_link(debt,debt_value).
|- direct_link(company,dso).
|- direct_link(dso,debt_value). 


"specification of the relatins that particular patterns encode"
|- mod_rel(company, debt, company-has-debt). 
|- mod_rel(debt,debt_value, debt-level). 
|- mod_rel(company,dso,company-has-dso). 
|- mod_rel(dso,debt_value, dso-has-value).


|- tmod_rel(company-has-debt, time_point, time-of-debt).
|- tmod_rel(company-has-debt, money_amount, debt-has-value).

|- link_mediator(company,debt_value, debt, debt).

"Relations between temporal_entities (tt is time-time relation, i.e. relations between two different temporal entities.
for example, ttmod_rel(before, time_period), it looks into semlinks, where semlink(%Something, TIME_PERIOD, via(TPREP(AFTER))), and realizes that 'after' is relating the something to the temporal_entity 'time_period'. The macro either relates that something directly, or checks is that entity has an associated time_point and places that time_point after the time_period."

"Tpreps and time_periods"
|- ttmod_rel(after, time_period).
|- ttmod_rel(at, time_period). 
|- ttmod_rel(near, time_period). 
|- ttmod_rel(by, time_period).
|- ttmod_rel(within, time_period). 

"tpreps and time_points" 
|- ttmod_rel(after, time_point). "time_point here is 'end'"
|- ttmod_rel(at, time_point). 
|- ttmod_rel(near, time_point). 
|- ttmod_rel(by, time_point). 





