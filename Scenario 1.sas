/*Scenario 1: No true difference in case-mix, hospital effects*/
%let scenario=1;
%let n_hosp=111;/*Number of hospitals*/

/*create a dataset that will contain all of the matched cases*/
data tm.matched_cases&scenario.&sysdate.;
if _n_=1 then delete;
run;

/*create a dataset that will contain all of the template cases*/
data tm.template_cases&scenario.&sysdate.;
if _n_=1 then delete;
run;

/*create a dataset that will contain all of the solutions from template matching*/
data tm.tm_sim_rates&scenario.&sysdate.;
if _n_=1 then delete;
run;

data reg.reg_sim_rates&scenario.&sysdate.;
if _n_=1 then delete;
run;

%macro loop;
%do iteration=1 %to 1000;
/*Randomly allocate patients to pseudo hospitals to remove hospital effect.
Hospital sizes are approximately equal*/
proc surveyselect data=ipec.simulations out=simulations noprint
seed=&iteration. groups=&n_hosp.;
run;

data simulations;
set simulations;
rename groupid=pseudo_hospital;
	iteration=&iteration.;
run;

/*ROC (C-Stat) for each pseduo hospital*/
proc sort data=simulations; by pseudo_hospital; run;

proc logistic data=simulations;
by pseudo_hospital;
model mort (event='1')=pred;
roc;
ods output rocassociation=auc;
run;

data auc;
set auc;
where ROCModel="Model";
rename Area=ROC LowerArea=LowerROC UpperArea=UpperROC;
run;

data auc;
set auc (keep=pseudo_hospital ROC LowerROC UpperROC);
run;

/*Add the ROC and 95% CI to the simulations dataset. The ROC describes how well 
the predicted mortality (pred) models the outcome at each pseudo hospital*/
proc sql;
create table simulations as
select a.*, b.ROC, b.LowerROC, b.UpperRoc
from simulations a
left join auc b
on a.pseudo_hospital=b.pseudo_hospital;
quit;

proc sort data=simulations; by pseudo_hospital; run;

/*Illness severity (predicted mortality) for each pseudo hospital*/
proc means data=simulations noprint;
var pred; 
by pseudo_hospital;
output out=reg_means mean=pseudo_hosp_mean_pred std=pseudo_hosp_std_pred
median=pseudo_hosp_median_pred q1=pseudo_hosp_q1_pred q3=pseudo_hosp_q3_pred;
run;

data reg_means;
set reg_means;
drop _freq_ _type_;
run;

/*Age for each pseudo hospital*/
proc means data=simulations noprint;
var age; 
by pseudo_hospital;
output out=reg_means2 mean=pseudo_hosp_mean_age std=pseudo_hosp_std_age
median=pseudo_hosp_median_age q1=pseudo_hosp_q1_age q3=pseudo_hosp_q3_age;
run;

data reg_means2;
set reg_means2;
drop _freq_ _type_;
run;

/*Regression method analysis*/
ods output covparms=covp solutionr=randomeffect&iteration. ;
proc glimmix data=simulations;
class pseudo_hospital;
model mort=pred / noint link=logit dist=binomial solution cl ddfm=bw;
random intercept/sub=pseudo_hospital solution cl g;
run;
ods output close;

data randomeffect&iteration.;
set randomeffect&iteration.;
	reg_eventrate=exp(estimate)/(1+exp(estimate));
	reg_eventratelowerCI=exp(lower)/(1+exp(lower)); /*lower 95% CI*/
	reg_eventrateupperCI=exp(upper)/(1+exp(upper));  /*upper 95% CI*/
	iteration=&iteration.;
run;

data randomeffect&iteration.;
set randomeffect&iteration.;
	pseudo_hospital_char=left(substr(subject, 17, 3));
	pseudo_hospital=input(pseudo_hospital_char, 3.);
	drop pseudo_hospital_char;
run;

/*Join the illness severity and age measures*/
data randomeffect&iteration.;
merge randomeffect&iteration. reg_means reg_means2;
by pseudo_hospital;
run;

/*Get the median hospital event rate and the median absolute deviation(iteration-level)*/

proc univariate data=randomeffect&iteration. noprint;
var reg_eventrate;
output out=mad median=reg_medianhospital mad=reg_mad;
run;

data mad;
set mad;
call symputx("reg_medianhospital", reg_medianhospital);
call symputx("reg_mad", reg_mad);
run;

data randomeffect&iteration.;
set randomeffect&iteration.;
reg_medianhospital=&reg_medianhospital.;
reg_mad=&reg_mad.;
reg_lower_outlier=reg_medianhospital-3*reg_mad;
reg_upper_outlier=reg_medianhospital+3*reg_mad;
run;

proc sort data=randomeffect&iteration.; by reg_eventrate; run;

/*Hospital rank based on event rate. Identify the lowest quintile (hosps ranked 90-111)*/
data randomeffect&iteration.;
set randomeffect&iteration.;
	reg_hospital_rank+1;
	if reg_hospital_rank>89 then reg_lower_quintile=1;
	if reg_hospital_rank<=89 then reg_lower_quintile=0;
	if tm_hospital_rank>22 then reg_upper_quintile=0;
	if tm_hospital_rank<=22 then reg_upper_quintile=1;
run;

proc sort data=randomeffect&iteration.; by reg_hospital_rank; run;

data randomeffect&iteration.;
set randomeffect&iteration.;
	if reg_lower_outlier<=reg_eventrate<=reg_upper_outlier then reg_sigdiff=0;
	else reg_sigdiff=1;
	if reg_eventrate<reg_lower_outlier then reg_over=1;
	else reg_over=0;
	if reg_eventrate>reg_upper_outlier then reg_under=1;
	else reg_under=0;
run;

/*output the proportion of underperformers for each iteration*/
proc sql;
create table rates as
select iteration, pseudo_hospital, reg_hospital_rank, reg_lower_quintile, reg_upper_quintile,
reg_eventrate, reg_eventratelowerCI, reg_eventrateupperCI, reg_medianhospital, 
reg_mad, reg_lower_outlier, reg_upper_outlier, reg_under, reg_over, reg_sigdiff, 
mean(reg_under) as reg_underperformrate, mean(reg_over) as reg_overperformancerate,
mean(reg_sigdiff) as reg_sigdiffrate, pseudo_hosp_mean_pred, pseudo_hosp_std_pred, 
pseudo_hosp_median_pred, pseudo_hosp_q1_pred, pseudo_hosp_q3_pred
from randomeffect&iteration.;
quit;

data reg.reg_sim_rates&scenario.&sysdate.;
set reg.reg_sim_rates&scenario.&sysdate. rates;
run;

/*TEMPLATE MATCHING*/

/*create the template of size 240--Approach 1*/

proc surveyselect data=simulations noprint method = srs  sampsize = 240
   rep=500 seed=&iteration. out=sim_templatecases (rename=(replicate=template));
   id iteration id id2 anon_hospital_id site pseudo_hospital mort surgery
	pred glucose albval bili gfr bun na wbc hct pao2
	pco2 ph cx1  cx3 cx4 cx5 cx6 cx7 cx8 cx9 
	cx10 cx11 cx12 cx13 cx14 cx15 cx16 cx17 cx18 cx19 
	cx20 cx21 cx22 cx23 cx24 cx25 cx26 cx27 cx28 cx29 
	cx30 age sex white black hispanic opc nh ed 
	chf_nonhp sepsis alcohol dysrhythmia pneumonia copd
	coron_athero osteoarthros skin_infection chestpain complic_devi
	uti diabmel_w_cm complic_proc acute_ren_fail backproblem acute_mi 
	adlt_resp_fl fluid_elc_dx gi_hemorrhag;
run;

/*Calculate the means of each template and of the overall population*/
proc means data=sim_templatecases mean noprint;
var mort pred glucose albval bili gfr bun na wbc hct pao2 surgery
	pco2 ph cx1  cx3 cx4 cx5 cx6 cx7 cx8 cx9 
	cx10 cx11 cx12 cx13 cx14 cx15 cx16 cx17 cx18 cx19 
	cx20 cx21 cx22 cx23 cx24 cx25 cx26 cx27 cx28 cx29 
	cx30 age sex white black hispanic opc nh ed  
	chf_nonhp sepsis alcohol dysrhythmia pneumonia copd
	coron_athero osteoarthros skin_infection chestpain complic_devi
	uti diabmel_w_cm complic_proc acute_ren_fail backproblem acute_mi 
	adlt_resp_fl fluid_elc_dx gi_hemorrhag;
by template;
output out=means;
run;

data means;
set means;
	where _stat_="MEAN";
	drop _TYPE_--_stat_;
run;

/*Population means*/
proc means data=simulations mean noprint;
var mort pred glucose albval bili gfr bun na wbc hct pao2 surgery
	pco2 ph cx1  cx3 cx4 cx5 cx6 cx7 cx8 cx9 
	cx10 cx11 cx12 cx13 cx14 cx15 cx16 cx17 cx18 cx19 
	cx20 cx21 cx22 cx23 cx24 cx25 cx26 cx27 cx28 cx29 
	cx30 age sex white black hispanic opc nh ed 
	chf_nonhp sepsis alcohol dysrhythmia pneumonia copd
	coron_athero osteoarthros skin_infection chestpain complic_devi
	uti diabmel_w_cm complic_proc acute_ren_fail backproblem acute_mi 
	adlt_resp_fl fluid_elc_dx gi_hemorrhag ;
output out=population_means;
run;

data population_means;
set population_means;
	where _stat_="MEAN";
	drop _TYPE_--_stat_;
run;

/*Calculate the Mahalanobis Distance for each template*/
proc iml;
use means nobs nobs;
read all var {template};
read all var {mort pred glucose albval bili gfr bun na wbc hct pao2 surgery
	pco2 ph cx1  cx3 cx4 cx5 cx6 cx7 cx8 cx9 
	cx10 cx11 cx12 cx13 cx14 cx15 cx16 cx17 cx18 cx19 
	cx20 cx21 cx22 cx23 cx24 cx25 cx26 cx27 cx28 cx29 
	cx30 age sex white black hispanic opc nh ed 
	chf_nonhp sepsis alcohol dysrhythmia pneumonia copd
	coron_athero osteoarthros skin_infection chestpain complic_devi
	uti diabmel_w_cm complic_proc acute_ren_fail backproblem acute_mi 
	adlt_resp_fl fluid_elc_dx gi_hemorrhag} 
into data[c=vnames];
close;
use population_means;
read all var {mort pred glucose albval bili gfr bun na wbc hct pao2 surgery
	pco2 ph cx1  cx3 cx4 cx5 cx6 cx7 cx8 cx9 
	cx10 cx11 cx12 cx13 cx14 cx15 cx16 cx17 cx18 cx19 
	cx20 cx21 cx22 cx23 cx24 cx25 cx26 cx27 cx28 cx29 
	cx30 age sex white black hispanic opc nh ed  
	chf_nonhp sepsis alcohol dysrhythmia pneumonia copd
	coron_athero osteoarthros skin_infection chestpain complic_devi
	uti diabmel_w_cm complic_proc acute_ren_fail backproblem acute_mi 
	adlt_resp_fl fluid_elc_dx gi_hemorrhag} 
into m;
close;

center=m[:,];
level=unique(template);
templatemd=j(ncol(level),ncol(data)+2,.); 

do i=1 to ncol(level);
 templatemd[i,1]=level[i];
 idx=loc(template=level[i]);
 templatemd[i,2:ncol(data)+1]=data[idx,][:,];
end;
 
 xx=templatemd[,2:ncol(data)+1];
 cov=cov(xx);
 templatemd[,ncol(data)+2]= mahalanobis(xx,center,cov);

names={template}||vnames||{distance};
create templatemd from templatemd[c=names];
append from templatemd;
close;

quit;

/*Select the template with the smallest MD*/
proc sql;
create table templatemd as
select template, distance
from templatemd
having distance=min(distance);
quit;

/*Select the 240 template cases for the template with the smallest MD*/
proc sql;
create table sim_template as
select a.* 
from templatemd b
left join sim_templatecases a
on a.template=b.template;
quit;/*n=240 */

data sim_template;
set sim_template;
	case=1;
	iteration=&iteration.;
run;

/*Collect the template cases for all iterations*/
data tm.template_cases&scenario.&sysdate.;
set tm.template_cases&scenario.&sysdate. sim_template;
run;

/*All data with indicator for whether or not part of the template (case=1 for template)*/
proc sql;
create table templatedata as
select a.*, b.case
from simulations a
left join sim_template b
on a.id=b.id;
quit;/*n=460213*/

data templatedata;
set templatedata;
if case=. then case=0;
run;

proc sort data=templatedata; by pseudo_hospital; run;

data allmatch&iteration.;
if _n_=1 then delete;
run;


%macro match;
%do pseudo_hospital=1 %to 111;

	data templatedatahospital (compress=yes);
	set templatedata;
		if pseudo_hospital=&pseudo_hospital. or case=1 then output templatedatahospital;
	run;

	data templatedatahospital;
	set templatedatahospital;
		if case=. then case=0;
	run;

proc psmatch data=templatedatahospital region=allobs;
class case;
psmodel case(treated='1')=pred
	glucose albval bili gfr bun na wbc hct pao2 surgery
	pco2 ph cx1  cx3 cx4 cx5 cx6 cx7 cx8 cx9 
	cx10 cx11 cx12 cx13 cx14 cx15 cx16 cx17 cx18 cx19 
	cx20 cx21 cx22 cx23 cx24 cx25 cx26 cx27 cx28 cx29 
	cx30 age sex white black hispanic opc nh ed 	
	chf_nonhp sepsis alcohol dysrhythmia pneumonia copd
	coron_athero osteoarthros skin_infection chestpain complic_devi
	uti diabmel_w_cm complic_proc acute_ren_fail backproblem acute_mi 
	adlt_resp_fl fluid_elc_dx gi_hemorrhag;

match distance=lps method=optimal(k=1) caliper=.;

/*assess variable differences between the treated and control groups
for all obs in the support region*/
assess var=(pred glucose albval bili gfr bun na wbc hct pao2 surgery
pco2 ph cx1  cx3 cx4 cx5 cx6 cx7 cx8 cx9 
cx10 cx11 cx12 cx13 cx14 cx15 cx16 cx17 cx18 cx19 
cx20 cx21 cx22 cx23 cx24 cx25 cx26 cx27 cx28 cx29 
cx30 age sex white black hispanic opc nh ed 	
chf_nonhp sepsis alcohol dysrhythmia pneumonia copd
coron_athero osteoarthros skin_infection chestpain complic_devi
uti diabmel_w_cm complic_proc acute_ren_fail backproblem acute_mi 
adlt_resp_fl fluid_elc_dx gi_hemorrhag) /plots=none weight=none;

output out(obs=match)=out_match&iteration. matchid=_MatchID;
ods output matchinfo=matchinfo;
run;

data out_match&iteration.;
set out_match&iteration.;
where case=0;
run;

/*Total absolute difference in the logit of the propensity score for all matches for that hospital*/
data matchinfo (compress=yes);
set matchinfo;
total=cvalue1;
where label1="Total Absolute Difference";
run;

data matchinfo;
set matchinfo;
call symputx("total_abs_diff", total);
run;

data out_match&iteration. (compress=yes);
set out_match&iteration.;
total_abs_diff=&total_abs_diff.;
run;

data allmatch&iteration. (compress=yes);
set allmatch&iteration. out_match&iteration.;
run;

%end;
%mend;
%match;

/*Calculate the Mahalanobis Distance for each pseudo hospital*/
proc iml;
use allmatch&iteration. nobs nobs;
read all var {pseudo_hospital};
read all var {pred glucose albval bili gfr bun na wbc hct pao2 surgery
	pco2 ph cx1  cx3 cx4 cx5 cx6 cx7 cx8 cx9 
	cx10 cx11 cx12 cx13 cx14 cx15 cx16 cx17 cx18 cx19 
	cx20 cx21 cx22 cx23 cx24 cx25 cx26 cx27 cx28 cx29 
	cx30 age sex white black hispanic opc nh ed
	chf_nonhp sepsis alcohol dysrhythmia pneumonia copd
	coron_athero osteoarthros skin_infection chestpain complic_devi
	uti diabmel_w_cm complic_proc acute_ren_fail backproblem acute_mi 
	adlt_resp_fl fluid_elc_dx gi_hemorrhag} 
into data[c=vnames];
close;
use population_means;
read all var  {pred glucose albval bili gfr bun na wbc hct pao2 surgery
	pco2 ph cx1  cx3 cx4 cx5 cx6 cx7 cx8 cx9 
	cx10 cx11 cx12 cx13 cx14 cx15 cx16 cx17 cx18 cx19 
	cx20 cx21 cx22 cx23 cx24 cx25 cx26 cx27 cx28 cx29 
	cx30 age sex white black hispanic opc nh ed
	chf_nonhp sepsis alcohol dysrhythmia pneumonia copd
	coron_athero osteoarthros skin_infection chestpain complic_devi
	uti diabmel_w_cm complic_proc acute_ren_fail backproblem acute_mi 
	adlt_resp_fl fluid_elc_dx gi_hemorrhag} 
into m;
close;

center=m[:,];
level=unique(pseudo_hospital);
hospmd=j(ncol(level),ncol(data)+2,.); 

do i=1 to ncol(level);
 hospmd[i,1]=level[i];
 idx=loc(pseudo_hospital=level[i]);
 hospmd[i,2:ncol(data)+1]=data[idx,][:,];
end;
 
 xx=hospmd[,2:ncol(data)+1];
 cov=cov(xx) +I(70);/*+I(70)*/
 hospmd[,ncol(data)+2]= mahalanobis(xx,center,cov);

names={pseudo_hospital}||vnames||{distance};
create hospmd from hospmd[c=names];
append from hospmd;
close;
quit;

/*Output the MD for each hospital to see if this correlates with being an underperfomer*/
proc sql;
create table md as
select distinct(pseudo_hospital), distance as md
from hospmd;
quit;

proc sql;
create table allmatch&iteration. as
select a.*, b.md as hospital_MD
from allmatch&iteration. a 
left join md b
on a.pseudo_hospital=b.pseudo_hospital;
quit;

/*Illness severity for each pseudo hospital*/
proc means data=allmatch&iteration. noprint;
var pred; 
by pseudo_hospital;
output out=tm_means mean=pseudo_hosp_mean_pred std=pseudo_hosp_std_pred
median=pseudo_hosp_median_pred q1=pseudo_hosp_q1_pred q3=pseudo_hosp_q3_pred;
run;

data tm_means;
set tm_means;
drop _freq_ _type_;
run;

proc sql;
create table allmatch&iteration. as
select a.*, b.*
from allmatch&iteration. a
left join tm_means b
on a.pseudo_hospital=b.pseudo_hospital;
quit;

/*Determine the overall event rate across hospitals*/
proc sql;
create table overall&iteration. as
select distinct(pseudo_hospital), mean(mort) as overalleventrate
from allmatch&iteration.;
quit;

proc sql;
create table allmatch&iteration. as
select a.*, b.*
from allmatch&iteration. a
left join overall&iteration. b
on a.pseudo_hospital=b.pseudo_hospital;
quit;

/*Now determine which hospitals were significantly under or over-performing*/
proc sql;
create table eventrate&iteration. as
select distinct(pseudo_hospital), sum(mort=1) as tm_event, 
count(mort) as n, mean(mort) as tm_eventrate
from allmatch&iteration. a
group by pseudo_hospital;
quit;

proc sql;
create table allmatch&iteration. as
select a.*, b.*
from allmatch&iteration. a
left join eventrate&iteration. b
on a.pseudo_hospital=b.pseudo_hospital;
quit;

/*Add 95% CI to the eventrates*/
data allmatch&iteration.;
set allmatch&iteration.;
iteration=&iteration.;
tm_eventratelowerCI=tm_eventrate-1.96*sqrt(tm_eventrate*(1-tm_eventrate)/n);
tm_eventrateupperCI=tm_eventrate+1.96*sqrt(tm_eventrate*(1-tm_eventrate)/n);
run;

/*Calculate the median hospital event rate*/

/*Get the median hospital event rate and the median absolute deviation(iteration-level)*/

proc sort data=allmatch&iteration. nodupkey out=tm; by pseudo_hospital tm_eventrate; run;

proc univariate data=tm noprint;
var tm_eventrate;
output out=tm_mad median=tm_medianhospital mad=tm_mad;
run;

data tm_mad;
set tm_mad;
call symputx("tm_medianhospital", tm_medianhospital);
call symputx("tm_mad", tm_mad);
run;

data allmatch&iteration.;
set allmatch&iteration.;
tm_medianhospital=&tm_medianhospital.;
tm_mad=&tm_mad.;
tm_lower_outlier=tm_medianhospital-3*tm_mad;
tm_upper_outlier=tm_medianhospital+3*tm_mad;
run;

data allmatch&iteration.;
set allmatch&iteration.;
	if tm_lower_outlier<=tm_eventrate<=tm_upper_outlier then tm_sigdiff=0;
	else tm_sigdiff=1;
	if tm_eventrate<tm_lower_outlier then tm_over=1;
	else tm_over=0;
	if tm_eventrate>tm_upper_outlier then tm_under=1;
	else tm_under=0;
run;

/*Collect all of the matched cases through all iterations*/
data tm.matched_cases&scenario.&sysdate.;
set tm.matched_cases&scenario.&sysdate. allmatch&iteration.;
run;

proc sort data=allmatch&iteration. out=tm_rates&iteration. nodupkey;
by iteration pseudo_hospital; run;

proc sort data=tm_rates&iteration.; by tm_eventrate; run;

/*Hospital rank based on event rate.*/
data tm_rates&iteration.;
set tm_rates&iteration.;
	tm_hospital_rank+1;
	if tm_hospital_rank>89 then tm_lower_quintile=1;
	if tm_hospital_rank<=89 then tm_lower_quintile=0;
	if tm_hospital_rank>22 then tm_upper_quintile=0;
	if tm_hospital_rank<=22 then tm_upper_quintile=1;
run;

run;

proc sort data=tm_rates&iteration.; by tm_hospital_rank; run;

/*output the proportion of underperformers for each iteration*/
proc sql;
create table tm_rates as
select iteration, pseudo_hospital, tm_hospital_rank, tm_lower_quintile, tm_upper_quintile,
tm_eventrate, tm_eventratelowerCI, tm_eventrateupperCI, tm_medianhospital, tm_mad,
tm_lower_outlier, tm_upper_outlier, tm_under, tm_over, tm_sigdiff, hospital_MD, 
mean(tm_under) as tm_underperformrate, mean(tm_over) as tm_overperformancerate,
mean(tm_sigdiff) as tm_sigdiffrate, pseudo_hosp_mean_pred, pseudo_hosp_std_pred, 
pseudo_hosp_median_pred, pseudo_hosp_q1_pred, pseudo_hosp_q3_pred
from tm_rates&iteration.;
quit;

proc sort data=tm_rates nodupkey;
by iteration tm_hospital_rank pseudo_hospital tm_eventrate; run;

data tm.tm_sim_rates&scenario.&sysdate.;
set tm.tm_sim_rates&scenario.&sysdate. tm_rates;
run;

proc datasets;
delete tm_rates tm_rates&iteration. allmatch&iteration.;
run;
%end;
%mend;

%loop;

proc sort data=tm.tm_sim_rates&scenario.&sysdate.; by iteration tm_hospital_rank; run;
proc sort data=reg.reg_sim_rates&scenario.&sysdate.; by iteration reg_hospital_rank; run;

data tm.tm_sim_results&scenario.;
set tm.tm_sim_results&scenario. tm.tm_sim_rates&scenario.&sysdate.;
run;

data reg.reg_sim_results&scenario.;
set reg.reg_sim_results&scenario. reg.reg_sim_rates&scenario.&sysdate.;
run;
