%let year=2015;

proc sql;
create table ipec.hospital_crosswalk&year. as
select distinct(site), count(site) as n 
from ipec.ipec&year._with_risklog
group by site;
quit;

/*remove sites with <960 hospitalizations*/
data ipec.hospital_crosswalk&year.;
set ipec.hospital_crosswalk&year.;
where n> 960;
run;/*n=111*/

data ipec.hospital_crosswalk&year.;
set ipec.hospital_crosswalk&year.;
random=ranuni(3298);
run;

proc sort data=ipec.hospital_crosswalk&year.;
by random; run;

data ipec.hospital_crosswalk&year.;
set ipec.hospital_crosswalk&year.;
anon_hospital_id=_n_;
run;

proc sql;
create table ipec&year. as
select a.*, b.anon_hospital_id, b.n
from ipec.hospital_crosswalk&year. b
left join ipec.ipec&year._with_risklog a
on a.site=b.site;
quit;/*n=460213*/

/*Add single-level CCS primary diagnosis variable*/
data ipec&year.;
set ipec&year.;
singlelevel_ccs=compress(unitdx, '.');
singlelevel_ccsname=compress(unitdx, '.');
format singlelevel_ccs $single. singlelevel_ccsname $singlename.;
run;

data ipec&year.;
set ipec&year.;
singlelevel_ccs=vvalue(singlelevel_ccs);
run;

/*Look at the most frequent single-level dx codes*/
proc freq data=ipec&year. order=freq;
table singlelevel_ccs;
run;

/*Create indicators for the top 20 dx codes*/
data ipec&year.;
set ipec&year.;
	if singlelevel_ccs='108' then chf_nonhp=1;
	if singlelevel_ccs='2' then sepsis=1;
	if singlelevel_ccs='660' then alcohol=1;
	if singlelevel_ccs='106' then dysrhythmia=1;
	if singlelevel_ccs='122' then pneumonia=1;
	if singlelevel_ccs='127' then copd=1;
	if singlelevel_ccs='101' then coron_athero=1;
	if singlelevel_ccs='203' then osteoarthros=1;
	if singlelevel_ccs='197' then skin_infection=1;
	if singlelevel_ccs='102' then chestpain=1;
	if singlelevel_ccs='237' then complic_devi=1;
	if singlelevel_ccs='159' then uti=1;
	if singlelevel_ccs='50' then diabmel_w_cm=1;
	if singlelevel_ccs='238' then complic_proc=1;
	if singlelevel_ccs='157' then acute_ren_fail=1;
	if singlelevel_ccs='205' then backproblem=1;
	if singlelevel_ccs='100' then acute_mi=1;
	if singlelevel_ccs='131' then adlt_resp_fl=1;
	if singlelevel_ccs='55' then fluid_elc_dx=1;
	if singlelevel_ccs='153' then gi_hemorrhag=1;
run;

/*Change missing indicators to 0*/
proc stdize data=ipec&year. out=ipec&year. reponly missing=0;
var chf_nonhp sepsis alcohol dysrhythmia pneumonia copd
coron_athero osteoarthros skin_infection chestpain complic_devi
uti diabmel_w_cm complic_proc acute_ren_fail backproblem acute_mi 
adlt_resp_fl fluid_elc_dx gi_hemorrhag;
run;

/*Calculate 30-day mortality for missing patients*/
data ipec&year.;
set ipec&year.;
if missing(DEADINPADM30) then do;
	if missing(deathdate) then mort=0;
	else if datdif(admdate,deathdate,'ACT/ACT')<=30 then mort=1;
	else if datdif(admdate,deathdate,'ACT/ACT')>30 then mort=0;
end;
run;/*n=460213*/

/*ROC (C-Stat) for each pseduo hospital*/
proc sort data=ipec&year.; by anon_hospital_id; run;

proc logistic data=ipec&year.;
by anon_hospital_id;
model mort (event='1')=pred_log;
roc;
ods output rocassociation=auc;
run;

data auc;
set auc;
where ROCModel="Model";
rename Area=ROC LowerArea=LowerROC UpperArea=UpperROC;
run;

data auc;
set auc (keep=anon_hospital_id ROC LowerROC UpperROC);
run;

/*Add the ROC and 95% CI to the ipec&year. dataset. The ROC describes how well 
the predicted mortality (pred_log) models the outcome at each  hospital*/
proc sql;
create table ipec&year. as
select a.*, b.ROC, b.LowerROC, b.UpperRoc
from ipec&year. a
left join auc b
on a.anon_hospital_id=b.anon_hospital_id;
quit;

proc sort data=ipec&year.; by anon_hospital_id; run;

/*Illness severity (predicted mortality) for each  hospital*/
proc means data=ipec&year. noprint;
var pred_log; 
by anon_hospital_id;
output out=reg_means mean=hosp_mean_pred std=hosp_std_pred
median=hosp_median_pred q1=hosp_q1_pred q3=hosp_q3_pred;
run;

data reg_means;
set reg_means;
drop _freq_ _type_;
run;

/*Age for each  hospital*/
proc means data=ipec&year. noprint;
var age; 
by anon_hospital_id;
output out=reg_means2 mean=hosp_mean_age std=hosp_std_age
median=hosp_median_age q1=hosp_q1_age q3=hosp_q3_age;
run;

data reg_means2;
set reg_means2;
drop _freq_ _type_;
run;

/*Regression method analysis*/
ods output solutionr=randomeffect ;
proc glimmix data=ipec&year.;
class anon_hospital_id;
model mort=pred_log / noint link=logit dist=binomial solution cl ddfm=bw;
random intercept/sub=anon_hospital_id solution cl g;
run;
ods output close;

data randomeffect;
set randomeffect;
	reg_eventrate=exp(estimate)/(1+exp(estimate));
	reg_eventratelowerCI=exp(lower)/(1+exp(lower)); /*lower 95% CI*/
	reg_eventrateupperCI=exp(upper)/(1+exp(upper));  /*upper 95% CI*/
run;

data randomeffect;
set randomeffect;
	hospital_char=left(substr(subject, 18, 3));
	anon_hospital_id=input(hospital_char, 3.);
	drop hospital_char;
run;

/*Join the illness severity and age measures*/
proc sql;
create table randomeffect as
select a.*, b.*, c.*
from randomeffect a
left join reg_means b
on a.anon_hospital_id=b.anon_hospital_id
left join reg_means2 c
on a.anon_hospital_id=c.anon_hospital_id;
quit;

/*Get the median hospital event rate and the median absolute deviation*/
proc univariate data=randomeffect noprint;
var reg_eventrate;
output out=mad median=reg_medianhospital mad=reg_mad;
run;

data mad;
set mad;
call symputx("reg_medianhospital", reg_medianhospital);
call symputx("reg_mad", reg_mad);
run;

data randomeffect;
set randomeffect;
reg_medianhospital=&reg_medianhospital.;
reg_mad=&reg_mad.;
reg_lower_outlier=reg_medianhospital-3*reg_mad;
reg_upper_outlier=reg_medianhospital+3*reg_mad;
run;

proc sort data=randomeffect; by reg_eventrate; run;

/*Hospital rank based on event rate.*/
data randomeffect;
set randomeffect;
	reg_hospital_rank+1;
run;

proc sort data=randomeffect; by reg_hospital_rank; run;

data randomeffect;
set randomeffect;
	if reg_lower_outlier<=reg_eventrate<=reg_upper_outlier then reg_sigdiff=0;
	else reg_sigdiff=1;
	if reg_eventrate<reg_lower_outlier then reg_over=1;
	else reg_over=0;
	if reg_eventrate>reg_upper_outlier then reg_under=1;
	else reg_under=0;
run;

/*output the proportion of underperformers*/
proc sql;
create table rates as
select anon_hospital_id, reg_hospital_rank, 
reg_eventrate, reg_eventratelowerCI, reg_eventrateupperCI, reg_medianhospital, 
reg_mad, reg_lower_outlier, reg_upper_outlier, reg_under, reg_over, reg_sigdiff, 
mean(reg_under) as reg_underperformrate, mean(reg_over) as reg_overperformancerate,
mean(reg_sigdiff) as reg_sigdiffrate, hosp_mean_pred, hosp_std_pred, 
hosp_median_pred, hosp_q1_pred, hosp_q3_pred
from randomeffect;
quit;

/*TEMPLATE MATCHING*/

/*create the template of size 240--Approach 1*/
proc surveyselect data=ipec&year. noprint method = srs  sampsize = 240
   rep=500 seed=4102018 out=sim_templatecases (rename=(replicate=template));
   id id id2 anon_hospital_id mort 
	pred_log glucose albval bili gfr bun na wbc hct pao2
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
var mort pred_log glucose albval bili gfr bun na wbc hct pao2 
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
proc means data=ipec&year. mean noprint;
var mort pred_log glucose albval bili gfr bun na wbc hct pao2 
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
read all var {mort pred_log glucose albval bili gfr bun na wbc hct pao2 
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
read all var {mort pred_log glucose albval bili gfr bun na wbc hct pao2 
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
create table template as
select template, distance
from templatemd
having distance=min(distance);
quit;


/*Select the 240 template cases for the template with the smallest MD*/
proc sql;
create table sim_template as
select a.* 
from template b
left join sim_templatecases a
on a.template=b.template;
quit;/*n=240 */

data sim_template;
set sim_template;
	case=1;
run;

/*All data with indicator for whether or not part of the template (case=1 for template)*/
proc sql;
create table templatedata as
select a.*, b.case
from ipec&year. a
left join sim_template b
on a.id=b.id;
quit;/*n=460213*/

data templatedata;
set templatedata;
if case=. then case=0;
run;

proc sort data=templatedata; by anon_hospital_id; run;

%macro match;
%do anon_hospital_id=1 %to 111;

	data templatedatahospital (compress=yes);
	set templatedata;
		if anon_hospital_id=&anon_hospital_id. or case=1 then output templatedatahospital;
	run;

	data templatedatahospital;
	set templatedatahospital;
		if case=. then case=0;
	run;

proc psmatch data=templatedatahospital region=allobs;
class case;
psmodel case(treated='1')= pred_log
	glucose albval bili gfr bun na wbc hct pao2 operative
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
assess var=(pred_log glucose albval bili gfr bun na wbc hct pao2 operative
pco2 ph cx1  cx3 cx4 cx5 cx6 cx7 cx8 cx9 
cx10 cx11 cx12 cx13 cx14 cx15 cx16 cx17 cx18 cx19 
cx20 cx21 cx22 cx23 cx24 cx25 cx26 cx27 cx28 cx29 
cx30 age sex white black hispanic opc nh ed 	
chf_nonhp sepsis alcohol dysrhythmia pneumonia copd
coron_athero osteoarthros skin_infection chestpain complic_devi
uti diabmel_w_cm complic_proc acute_ren_fail backproblem acute_mi 
adlt_resp_fl fluid_elc_dx gi_hemorrhag) /plots=none weight=none;

output out(obs=match)=out_match matchid=_MatchID;
ods output matchinfo=matchinfo;
run;

data out_match;
set out_match;
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

data out_match (compress=yes);
set out_match;
total_abs_diff=&total_abs_diff.;
run;

data allmatch (compress=yes);
set allmatch out_match;
run;

%end;
%mend;
%match;


/*How well did the match work?*/

/*Calculate the Mahalanobis Distance for each anon_hospital_id hospital*/
proc iml;
use allmatch nobs nobs;
read all var {anon_hospital_id};
read all var {pred_log glucose albval bili gfr bun na wbc hct pao2 
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
read all var  {pred_log glucose albval bili gfr bun na wbc hct pao2 
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
level=unique(anon_hospital_id);
hospmd=j(ncol(level),ncol(data)+2,.); 

do i=1 to ncol(level);
 hospmd[i,1]=level[i];
 idx=loc(anon_hospital_id=level[i]);
 hospmd[i,2:ncol(data)+1]=data[idx,][:,];
end;
 
 xx=hospmd[,2:ncol(data)+1];
 cov=cov(xx);/*+I(70)*/
 hospmd[,ncol(data)+2]= mahalanobis(xx,center,cov);

names={anon_hospital_id}||vnames||{distance};
create hospmd from hospmd[c=names];
append from hospmd;
close;
quit;

/*Output the MD for each hospital to see if this correlates with being an underperfomer*/
proc sql;
create table md as
select distinct(anon_hospital_id), distance as md
from hospmd;
quit;

proc sql;
create table matchedcases as
select a.*, b.md as hospital_MD
from allmatch a 
left join md b
on a.anon_hospital_id=b.anon_hospital_id;
quit;

/*Illness severity for each  hospital*/
proc means data=matchedcases noprint;
var pred_log; 
by anon_hospital_id;
output out=tm_means mean=hosp_mean_pred std=hosp_std_pred
median=hosp_median_pred q1=hosp_q1_pred q3=hosp_q3_pred;
run;

data tm_means;
set tm_means;
drop _freq_ _type_;
run;

proc sql;
create table matchedcases as
select a.*, b.*
from matchedcases a
left join tm_means b
on a.anon_hospital_id=b.anon_hospital_id;
quit;

/*Determine the overall event rate across hospitals*/
proc sql;
create table overall as
select distinct(anon_hospital_id), mean(mort) as overalleventrate
from matchedcases;
quit;

proc sql;
create table matchedcases as
select a.*, b.*
from matchedcases a
left join overall b
on a.anon_hospital_id=b.anon_hospital_id;
quit;

/*Now determine which hospitals were significantly under or over-performing*/
proc sql;
create table eventrate as
select distinct(anon_hospital_id), sum(mort=1) as tm_event, 
count(mort) as n, mean(mort) as tm_eventrate
from matchedcases a
group by anon_hospital_id;
quit;

data matchedcases;
set matchedcases;
drop tm_event tm_eventrate n;
run;

proc sql;
create table matchedcases as
select a.*, b.*
from matchedcases a
left join eventrate b
on a.anon_hospital_id=b.anon_hospital_id;
quit;

/*Add 95% CI to the eventrates*/
data matchedcases;
set matchedcases;
tm_eventratelowerCI=tm_eventrate-1.96*sqrt(tm_eventrate*(1-tm_eventrate)/n);
tm_eventrateupperCI=tm_eventrate+1.96*sqrt(tm_eventrate*(1-tm_eventrate)/n);
run;

/*Calculate the median hospital event rate*/

/*Get the median hospital event rate and the median absolute deviation*/

proc sort data=matchedcases nodupkey out=tm; by anon_hospital_id tm_eventrate; run;

proc univariate data=tm noprint;
var tm_eventrate;
output out=tm_mad median=tm_medianhospital mad=tm_mad;
run;

data tm_mad;
set tm_mad;
call symputx("tm_medianhospital", tm_medianhospital);
call symputx("tm_mad", tm_mad);
run;

data matchedcases;
set matchedcases;
tm_medianhospital=&tm_medianhospital.;
tm_mad=&tm_mad.;
tm_lower_outlier=tm_medianhospital-3*tm_mad;
tm_upper_outlier=tm_medianhospital+3*tm_mad;
run;

data matchedcases;
set matchedcases;
	if tm_lower_outlier<=tm_eventrate<=tm_upper_outlier then tm_sigdiff=0;
	else tm_sigdiff=1;
	if tm_eventrate<tm_lower_outlier then tm_over=1;
	else tm_over=0;
	if tm_eventrate>tm_upper_outlier then tm_under=1;
	else tm_under=0;
run;

proc sort data=matchedcases out=tm_rates nodupkey;
by  anon_hospital_id; run;

proc sort data=tm_rates; by tm_eventrate; run;

/*Hospital rank based on event rate.*/
data tm_rates;
set tm_rates;
	tm_hospital_rank+1;
run;

proc sort data=tm_rates; by tm_hospital_rank; run;

/*output the proportion of underperformers */
proc sql;
create table tm_rates as
select anon_hospital_id, tm_hospital_rank, 
tm_eventrate, tm_eventratelowerCI, tm_eventrateupperCI, tm_medianhospital, tm_mad,
tm_lower_outlier, tm_upper_outlier, tm_under, tm_over, tm_sigdiff, hospital_MD, 
mean(tm_under) as tm_underperformrate, mean(tm_over) as tm_overperformancerate,
mean(tm_sigdiff) as tm_sigdiffrate, hosp_mean_pred, hosp_std_pred, 
hosp_median_pred, hosp_q1_pred, hosp_q3_pred
from tm_rates;
quit;

proc sort data=tm_rates nodupkey;
by tm_hospital_rank anon_hospital_id tm_eventrate; run;

/*Compare template matching to regression*/

proc sql;
create table both_rates as
select a.*, b.reg_mad, b.reg_hospital_rank, b.reg_medianhospital,
b.reg_eventrate, b.reg_eventratelowerci, b.reg_eventrateupperci, b.reg_underperformrate, b.reg_under,
b.reg_over, b.reg_sigdiff
from tm_rates a
left join rates b
on a.anon_hospital_id=b.anon_hospital_id;
quit;

data both_rates;
set both_rates;
	if reg_hospital_rank<24 then reg_quint=1;
	if 24<=reg_hospital_rank<46 then reg_quint=2;
	if 46<=reg_hospital_rank<68 then reg_quint=3;
	if 68<=reg_hospital_rank<90 then reg_quint=4;
	if 90<=reg_hospital_rank then reg_quint=5;

	if tm_hospital_rank<24 then tm_quint=1;
	if 24<=tm_hospital_rank<46 then tm_quint=2;
	if 46<=tm_hospital_rank<68 then tm_quint=3;
	if 68<=tm_hospital_rank<90 then tm_quint=4;
	if 90<=tm_hospital_rank then tm_quint=5;

	if tm_quint=5 then tm_lower_quintile=1;
	if tm_quint ne 5 then tm_lower_quintile=0;
	if reg_quint=5 then reg_lower_quintile=1;
	if reg_quint ne 5 then reg_lower_quintile=0;
run;

proc sort data=both_rates; by tm_hospital_rank; run;

/*Correlation and 95% CI*/
proc corr data=both_rates outp=corr;
var reg_quint tm_quint;
run;

data corr_ci;
set corr (rename=(reg_quint=corr) drop=tm_quint _name_);
retain n;
if _type_='N' then n=corr;
if _type_='CORR' and corr ne 1;

fishersz=0.5*(log(1+corr)-log(1-corr));/*Fishers z transformation*/
sigmaz=1/sqrt(n-3);					   /*variance*/
l95=fishersz-1.96*sigmaz;
u95=fishersz+1.96*sigmaz;

l95=(exp(2*l95)-1)/(exp(2*l95)+1);		/*inverse of Fisher Z*/
u95=(exp(2*u95)-1)/(exp(2*u95)+1);		/*transformation to get CI*/
run;

proc print data=corr_ci;
run;

proc freq data=both_rates;
table tm_lower_quintile*reg_lower_quintile/out=p;
run;

data both_rates;
set both_rates;
format reg_eventrate reg_eventrateupperci reg_eventratelowerci percent7.1;
format tm_eventrate tm_eventrateupperci tm_eventratelowerci percent7.1;
run;

data both_rates;
set both_rates;
call symputx("reg_medianhospital", reg_medianhospital);
run;


/*Highlight the hospitals that are under-performers in template matching*/
data both_rates;
set both_rates;
if tm_lower_quintile=1 then do;
reg_eventrate2=reg_eventrate;
reg_eventratelowerci2=reg_eventratelowerci;
reg_eventrateupperci2=reg_eventrateupperci;
end;

if tm_quint=1 then do;
reg_eventrate3=reg_eventrate;
reg_eventratelowerci3=reg_eventratelowerci;
reg_eventrateupperci3=reg_eventrateupperci;
end;
format reg_eventrate2 reg_eventrateupperci2 reg_eventratelowerci2
reg_eventrate3 reg_eventrateupperci3 reg_eventratelowerci3 percent7.1;
run;

goptions reset=all;
ods graphics on/
imagefmt=tiff
imagename="caterpillar plot IPEC 2015 Regression TM highlighted without post-match adjustment &sysdate."
noborder;

ods listing gpath="/data/dart/2017/ord_prescott_comparing/Output";

proc sgplot data=both_rates ;
scatter x=reg_hospital_rank y=reg_eventrate/ markerattrs=(color=gray) ;
highlow x=reg_hospital_rank low=reg_eventratelowerci high=reg_eventrateupperci/ 
lineattrs=(color=black pattern=1) legendlabel="median hospitals by TM" name="median";

/*highlight the hospitals that were under-performers via TM*/
scatter x=reg_hospital_rank y=reg_eventrate2/markerattrs=(color=red symbol=circlefilled);
highlow x=reg_hospital_rank low=reg_eventratelowerci2 high=reg_eventrateupperci2/ 
lineattrs=(color=red pattern=1) legendlabel="lowest-quintile by TM" name="under";

/*highlight the hospitals that were over-performers via TM*/
scatter x=reg_hospital_rank y=reg_eventrate3/markerattrs=(symbol=circlefilled color= vilg );
highlow x=reg_hospital_rank low=reg_eventratelowerci3 high=reg_eventrateupperci3/ 
lineattrs=(color=vilg pattern=1 thickness=1.5 ) legendlabel="highest-quintile by TM" name="better";

/*Add reference lines for median and outlier limits*/
refline &reg_medianhospital. /axis=y label="median hospital rate" labelattrs=(size=12pt);
/*refline &reg_upper_outlier. /axis=y lineattrs=(pattern=2);*/
/*refline &reg_lower_outlier. /axis=y lineattrs=(pattern=2);*/

yaxis label="30-day mortality" values=(0 to .06 by .01) labelattrs=(size=12pt);
xaxis display=none;
keylegend "median" "under" "better"/valueattrs=(size=12pt) location=inside position=bottomright;
title "without post-match adjustment";
run;

goptions reset=all;
ods graphics on/
imagefmt=tiff
imagename="caterpillar plot IPEC 2015 Regression TM highlighted with post-match adjustment &sysdate."
noborder;

ods listing gpath="/data/dart/2017/ord_prescott_comparing/Output";

proc sgplot data=both_rates2 ;
scatter x=reg_hospital_rank y=reg_eventrate/ markerattrs=(color=gray) ;
highlow x=reg_hospital_rank low=reg_eventratelowerci high=reg_eventrateupperci/ 
lineattrs=(color=black pattern=1) legendlabel="median hospitals by TM" name="median";

/*highlight the hospitals that were under-performers via TM*/
scatter x=reg_hospital_rank y=reg_eventrate2/markerattrs=(color=red symbol=circlefilled);
highlow x=reg_hospital_rank low=reg_eventratelowerci2 high=reg_eventrateupperci2/ 
lineattrs=(color=red pattern=1) legendlabel="lowest-quintile by TM" name="under";

/*highlight the hospitals that were over-performers via TM*/
scatter x=reg_hospital_rank y=reg_eventrate3/markerattrs=(symbol=circlefilled color= vilg );
highlow x=reg_hospital_rank low=reg_eventratelowerci3 high=reg_eventrateupperci3/ 
lineattrs=(color=vilg pattern=1 thickness=1.5 ) legendlabel="highest-quintile by TM" name="better";

/*Add reference lines for median and outlier limits*/
refline &reg_medianhospital. /axis=y label="median hospital rate" labelattrs=(size=12pt);
/*refline &reg_upper_outlier. /axis=y lineattrs=(pattern=2);*/
/*refline &reg_lower_outlier. /axis=y lineattrs=(pattern=2);*/

yaxis label="30-day mortality" values=(0 to .06 by .01) labelattrs=(size=12pt);
xaxis display=none;
keylegend "median" "under" "better"/valueattrs=(size=12pt) location=inside position=bottomright;
title "with post-match adjustment";
run;

/*Now perform post-match adjustment*/


/*Adjusted rates*/
ods output parameterestimates=fixed ;
proc glimmix data=matchedcases empirical=classical abspconv=1e-4 method=laplace noclprint;
class anon_hospital_id _matchid;
model mort=anon_hospital_id pred_log  /  noint link=logit dist=binomial  solution cl;
random intercept/sub=_matchid solution cl  g;
nloptions maxiter=500 gconv=1e-8;
run;
ods output close;

data fixed;
set fixed;
	eventrate=exp(estimate)/(1+exp(estimate));
	eventratelowerCI=exp(lower)/(1+exp(lower)); /*lower 95% CI*/
	eventrateupperCI=exp(upper)/(1+exp(upper));  /*upper 95% CI*/
run;

proc sort data=fixed; by eventrate; run;

data fixed;
set fixed;
hospital_rank=_n_;
where effect ne "pred_log"; 
run;

/*Identify lowest quintile*/
data fixed;
set fixed;
	if hospital_rank>=89 then lower_quintile=1;
	if hospital_rank<89 then lower_quintile=0;
run;

/*Compare to regression*/

proc sql;
create table both_rates2 as
select a.hospital_rank as tm_hospital_rank_adj, a.eventrate as tm_eventrate, a.eventratelowerci as tm_eventratelowerci, a.eventrateupperci as tm_eventrateupperci,
b.reg_hospital_rank, b.reg_eventrate, b.reg_eventratelowerci, b.reg_eventrateupperci, b.reg_medianhospital
from fixed a
left join rates b
on a.anon_hospital_id=b.anon_hospital_id;
quit;

data both_rates2;
set both_rates2;
	if reg_hospital_rank<24 then reg_quint=1;
	if 24<=reg_hospital_rank<46 then reg_quint=2;
	if 46<=reg_hospital_rank<68 then reg_quint=3;
	if 68<=reg_hospital_rank<90 then reg_quint=4;
	if 90<=reg_hospital_rank then reg_quint=5;

	if tm_hospital_rank_adj<24 then tm_quint=1;
	if 24<=tm_hospital_rank_adj<46 then tm_quint=2;
	if 46<=tm_hospital_rank_adj<68 then tm_quint=3;
	if 68<=tm_hospital_rank_adj<90 then tm_quint=4;
	if 90<=tm_hospital_rank_adj then tm_quint=5;

	if tm_quint=5 then tm_lower_quintile=1;
	if tm_quint ne 5 then tm_lower_quintile=0;
	if reg_quint=5 then reg_lower_quintile=1;
	if reg_quint ne 5 then reg_lower_quintile=0;
run;

proc sort data=both_rates2; by tm_hospital_rank_adj; run;

/*Correlation and 95% CI*/
proc corr data=both_rates2 outp=corr;
var reg_quint tm_quint;
run;

data corr_ci;
set corr (rename=(reg_quint=corr) drop=tm_quint _name_);
retain n;
if _type_='N' then n=corr;
if _type_='CORR' and corr ne 1;

fishersz=0.5*(log(1+corr)-log(1-corr));/*Fishers z transformation*/
sigmaz=1/sqrt(n-3);					   /*variance*/
l95=fishersz-1.96*sigmaz;
u95=fishersz+1.96*sigmaz;

l95=(exp(2*l95)-1)/(exp(2*l95)+1);		/*inverse of Fisher Z*/
u95=(exp(2*u95)-1)/(exp(2*u95)+1);		/*transformation to get CI*/
run;

proc print data=corr_ci;
run;

proc freq data=both_rates2;
table tm_lower_quintile*reg_lower_quintile/out=p;
run;

data both_rates2;
set both_rates2;
format reg_eventrate reg_eventrateupperci reg_eventratelowerci percent7.1;
format tm_eventrate tm_eventrateupperci tm_eventratelowerci percent7.1;
run;

data both_rates2;
set both_rates2;
call symputx("reg_medianhospital", reg_medianhospital);
run;


/*Highlight the hospitals that are under-performers in template matching*/
data both_rates2;
set both_rates2;
if tm_lower_quintile=1 then do;
reg_eventrate2=reg_eventrate;
reg_eventratelowerci2=reg_eventratelowerci;
reg_eventrateupperci2=reg_eventrateupperci;
end;

if tm_quint=1 then do;
reg_eventrate3=reg_eventrate;
reg_eventratelowerci3=reg_eventratelowerci;
reg_eventrateupperci3=reg_eventrateupperci;
end;
format reg_eventrate2 reg_eventrateupperci2 reg_eventratelowerci2
reg_eventrate3 reg_eventrateupperci3 reg_eventratelowerci3 percent7.1;
run;

goptions reset=all;
ods graphics on/
imagefmt=tiff
imagename="caterpillar plot IPEC 2015 Regression TM highlighted with post-match adjustment &sysdate."
noborder;

ods listing gpath="/data/dart/2017/ord_prescott_comparing/Output";

proc sgplot data=both_rates2 ;
scatter x=reg_hospital_rank y=reg_eventrate/ markerattrs=(color=gray) ;
highlow x=reg_hospital_rank low=reg_eventratelowerci high=reg_eventrateupperci/ 
lineattrs=(color=black pattern=1) legendlabel="median hospitals by TM" name="median";

/*highlight the hospitals that were under-performers via TM*/
scatter x=reg_hospital_rank y=reg_eventrate2/markerattrs=(color=red symbol=circlefilled);
highlow x=reg_hospital_rank low=reg_eventratelowerci2 high=reg_eventrateupperci2/ 
lineattrs=(color=red pattern=1) legendlabel="lowest-quintile by TM" name="under";

/*highlight the hospitals that were over-performers via TM*/
scatter x=reg_hospital_rank y=reg_eventrate3/markerattrs=(symbol=circlefilled color= vilg );
highlow x=reg_hospital_rank low=reg_eventratelowerci3 high=reg_eventrateupperci3/ 
lineattrs=(color=vilg pattern=1 thickness=1.5 ) legendlabel="highest-quintile by TM" name="better";

/*Add reference lines for median and outlier limits*/
refline &reg_medianhospital. /axis=y label="median hospital rate" labelattrs=(size=12pt);
/*refline &reg_upper_outlier. /axis=y lineattrs=(pattern=2);*/
/*refline &reg_lower_outlier. /axis=y lineattrs=(pattern=2);*/

yaxis label="30-day mortality" values=(0 to .06 by .01) labelattrs=(size=12pt);
xaxis display=none;
keylegend "median" "under" "better"/valueattrs=(size=12pt) location=inside position=bottomright;
title "with post-match adjustment";
run;


