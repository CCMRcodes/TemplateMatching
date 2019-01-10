/*Scenario 6A: No true difference in hospital effects, but differences in case-mix.
Hospital size varies.*/
%let scenario=6A;
%let n_hosp=111;

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

/*True hospital size*/
proc sql;
create table hospitals as
select distinct anon_hospital_id as hospital, count(anon_hospital_id) as hospital_size
from ipec.simulations a
group by anon_hospital_id;
quit;

/*put all values of true hospital size into macro variable name hospital_size*/
proc sql noprint;
select hospital_size into :hospital_size separated by ' '
from hospitals;
quit;
/*%put hospital_size=&hospital_size.;*/

/*Assign patients to quartiles for age*/
proc rank data=ipec.simulations groups=4 out=quartiles;
var age;
ranks quartile_age;
run;

data simulations;
set quartiles;
run;

proc datasets; delete quartiles; run;

%macro loop;

%do iteration=1 %to 1000;
proc sort data=simulations; by quartile_age; run;

/*randomly select 22 hospitals to be "younger" and 22 to be "older"*/
proc surveyselect data=hospitals out=hospitalsb seed=&iteration. n=44;
id hospital hospital_size;
run;

/*determine which half are older and which are younger*/
data hospitalsb;
set hospitalsb;
random=ranuni(&iteration.);
run;

proc sort data=hospitalsb; by random; run;

data hospitalsb;
set hospitalsb;
if _n_<= 22 then casemix="younger";
else casemix="older";
run;

/*join the younger/older assignment to the hospitals dataset*/
proc sql;
create table hospitals&iteration. as
select a.*, b.casemix
from hospitals a
left join hospitalsb b
on a.hospital=b.hospital;
quit;

/*remaining hospitals are "median"*/
data hospitals&iteration.;
set hospitals&iteration.;
if missing(casemix) then casemix="median";
run;

/*Create a hosp_id that restarts for each hospital type*/
proc sort data=hospitals&iteration.; by casemix; run;

data hospitals&iteration.;
set hospitals&iteration.;
by casemix;
if first.casemix then hosp_id=0;
hosp_id+1;
run;

/*Determine the total number of patients in the younger hospitals*/
proc means data=hospitals&iteration. sum noprint;
var hospital_size;
by casemix;
output out=casemix_sizes sum=sampsize;
run;

/*Create a macro variable for the sample size needed for younger and older hosps*/
data casemix_sizes;
set casemix_sizes;
if casemix="younger" then do;
call symputx("sampsizey", sampsize);
end;
if casemix="older" then do;
call symputx("sampsizeo", sampsize);
end;
run;

/*patients assigned to younger hospitals.
Randomly sample the number of patients needed for the younger hospitals
where 40% are in the Q0, 30% in Q1, 20% in Q2 and 10% in Q3*/

proc sort data=simulations; by quartile_age; run;

proc surveyselect data=simulations out=younger method=srs sampsize=&sampsizey.
seed=&iteration.;
id id id2;
strata quartile_age/ alloc=(0.4 0.3 0.2 0.1);
run;

/*remove patients assigned to younger hospitals from the patient pool*/
proc sql;
create table sample2 as
select a.*
from simulations a
left join younger b
on a.id=b.id and a.id2=b.id2
where b.id is null and b.id2 is null;
quit;

proc sort data=sample2; by quartile_age; run;

/*patients assigned to older hospitals
Randomly sample the number of patients needed for the older hospitals
where 10% are in the Q0, 20% in Q1, 30% in Q2 and 40% in Q3**/
proc surveyselect data=sample2 out=older method=srs sampsize=&sampsizeo.
seed=&iteration.;
id id id2 ;
strata quartile_age/ alloc=(0.1 0.2 0.3 0.4);
run;

/*remove patients assigned to older hospitals from the patient pool*/
proc sql;
create table median as
select a.*
from sample2 a
left join older b
on a.id=b.id and a.id2=b.id2
where b.id is null and b.id2 is null;
quit;

/*Assign "younger hospital" patients to one of the 22 younger hospitals*/
proc sql noprint;
select hospital_size into :younger_hosps separated by ' '
from hospitals&iteration.
where casemix="younger";
quit;

/*Assign "older hospital" patients to one of the 22 older hospitals*/
proc sql noprint;
select hospital_size into :older_hosps separated by ' '
from hospitals&iteration.
where casemix="older";
quit;

/*Assign "median hospital" patients to one of the median hospitals*/
proc sql noprint;
select hospital_size into :median_hosps separated by ' '
from hospitals&iteration.
where casemix="median";
quit;

/*Randomly allocate patients assigned to younger hospitals to pseudo hospitals*/
proc surveyselect data=younger out=younger2
seed=&iteration. groups=(&younger_hosps.);
id id id2 ;
run;

proc surveyselect data=older out=older2
seed=&iteration. groups=(&older_hosps.);
id id id2;
run;

proc surveyselect data=median out=median2
seed=&iteration. groups=(&median_hosps.);
id id id2 ;
run;

proc sql;
create table younger3 as
select a.id, a.id2, b.hospital as pseudo_hospital, b.hospital_size, b.casemix
from younger2 a
left join hospitals&iteration. b
on a.groupid=b.hosp_id
where b.casemix="younger";
quit;

proc sql;
create table older3 as
select a.id, a.id2, b.hospital as pseudo_hospital, b.hospital_size, b.casemix
from older2 a
left join hospitals&iteration. b
on a.groupid=b.hosp_id
where b.casemix="older";
quit;

proc sql;
create table median3 as
select a.id, a.id2, b.hospital as pseudo_hospital, b.hospital_size, b.casemix
from median2 a
left join hospitals&iteration. b
on a.groupid=b.hosp_id
where b.casemix="median";
quit;


data randomized;
set younger3 older3 median3
(keep=id id2 pseudo_hospital casemix);
run;

proc sort data=randomized; by id id2; run;
proc sort data=simulations; by id id2; run;

data simulations;
merge simulations randomized;
by id id2; run;

proc datasets;
delete younger older median younger2 older2 median2 younger3 older3 median3
randomized hospitalsb sample2 hospitals&iteration. casemix_sizes;
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
proc sort data=auc; by pseudo_hospital; run;
proc sort data=simulations; by pseudo_hospital; run;

/*Add the ROC and 95% CI to the simulations dataset. The ROC describes how well 
the predicted mortality (pred) models the outcome at each pseudo hospital*/
data simulations;
merge simulations auc;
by pseudo_hospital; run;

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
ods output solutionr=randomeffect&iteration. ;
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

proc datasets; delete rates randomeffect&iteration. mad reg_means reg_means2 auc; run;

/*TEMPLATE MATCHING*/

/*create the template of size 240--Approach 1*/

proc surveyselect data=simulations noprint method = srs  sampsize = 240
   rep=500 seed=&iteration. out=sim_templatecases (rename=(replicate=template));
   id id id2 anon_hospital_id site pseudo_hospital mort surgery
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
create table templatemd2 as
select template, distance
from templatemd
having distance=min(distance);
quit;

/*Select the 240 template cases for the template with the smallest MD*/
proc sql;
create table sim_template as
select a.* 
from templatemd2 b
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
select distinct(pseudo_hospital), distance as hospital_MD
from hospmd;
quit;

data md;
set md;
length pseudo_hospital 3.;
run;

proc sort data=allmatch&iteration.; by pseudo_hospital; run;
proc sort data=md; by pseudo_hospital; run;

data allmatch&iteration.;
merge allmatch&iteration. md;
by pseudo_hospital;
run;

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

data allmatch&iteration.;
merge allmatch&iteration. tm_means;
by pseudo_hospital;
run;

/*Determine the overall event rate across hospitals*/
proc sql;
create table overall&iteration. as
select distinct(pseudo_hospital), mean(mort) as overalleventrate
from allmatch&iteration.;
quit;

data allmatch&iteration.;
merge allmatch&iteration. overall&iteration.;
by pseudo_hospital;
run;

/*Now determine which hospitals were significantly under or over-performing*/
proc sql;
create table eventrate&iteration. as
select distinct(pseudo_hospital), sum(mort=1) as tm_event, 
count(mort) as n, mean(mort) as tm_eventrate
from allmatch&iteration. a
group by pseudo_hospital;
quit;

data allmatch&iteration.;
merge allmatch&iteration. eventrate&iteration.;
by pseudo_hospital;
run;

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

proc datasets; delete sim_templatecases means population_means
templatemd sim_template templatedata allmatch&iteration. templatedatahospital
out_match&iteration. matchinfo md tm_means overall&iteration. eventrate&iteration.
tm_mad tm_rates&iteration. tm_rates hospmd templatemd2 tm; run;

data simulations;
set simulations;
drop pseudo_hospital hospital_size casemix roc lowerroc upperroc;
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

/*Join the hospital size variable*/
options compress=yes;
proc sql;
create table reg.reg_sim_results&scenario. as
select a.*, b.hospital_size
from reg.reg_sim_results&scenario. a
left join hospitals b
on a.pseudo_hospital=b.hospital;
quit;

options compress=yes;
proc sql;
create table tm.tm_sim_results&scenario. as
select a.*, b.hospital_size
from tm.tm_sim_results&scenario. a
left join hospitals b
on a.pseudo_hospital=b.hospital;
quit;

proc sort data=reg.reg_sim_results&scenario. nodupkey; by iteration reg_hospital_rank; run;
proc sort data=tm.tm_sim_results&scenario. nodupkey; by iteration tm_hospital_rank; run;
