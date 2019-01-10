/*Scenario 8B: True difference in hospital effects, differences in hospital volume. Case mix varies by predicted mortality*/
%let scenario=8B;
%let n_hosp=111;

%macro iter;
%do iteration=1 %to 1000;

data iteration&iteration.;
set tm2.matched_cases;
where iteration=&iteration.;
run;

/*Adjusted rates*/
ods output parameterestimates=fixed&iteration. ;
proc glimmix data=iteration&iteration. empirical=classical abspconv=1e-4 method=laplace noclprint;
class pseudo_hospital _matchid;
model simulated_mort=pseudo_hospital pred  /  noint link=logit dist=binomial  solution cl;
random intercept/sub=_matchid solution cl  g;
nloptions maxiter=500 gconv=1e-8;
run;
ods output close;

data fixed&iteration.;
set fixed&iteration.;
	eventrate=exp(estimate)/(1+exp(estimate));
	eventratelowerCI=exp(lower)/(1+exp(lower)); /*lower 95% CI*/
	eventrateupperCI=exp(upper)/(1+exp(upper));  /*upper 95% CI*/
run;

proc sort data=fixed&iteration.; by eventrate; run;

data fixed&iteration.;
set fixed&iteration.;
hospital_rank=_n_;
iteration=&iteration.;
where effect ne "pred"; 
run;

/*Identify lowest quintile*/
data fixed&iteration.;
set fixed&iteration.;
	if hospital_rank>=89 then lower_quintile=1;
	if hospital_rank<89 then lower_quintile=0;
run;


/*merge the true hospital effect, casemix, hospital volume*/

proc sql;
create table fixed&iteration. as
select a.*, b.hospital_effect, b.casemix
from fixed&iteration. a
left join tm.tm_sim_results8b b
on a.iteration=b.iteration and a.pseudo_hospital=b.pseudo_hospital;
quit;

data tm2.results_Silber8b;
set tm2.results_Silber8b fixed&iteration.;
run;
%end;
%mend;
%iter;

/*here*/
proc sql;
create table both_rates2 as
select a.*, b.reg_hospital_rank, b.reg_eventrate, b.reg_eventratelowerci, b.reg_eventrateupperci, b.reg_medianhospital
from tm2.results_Silber8b a
left join reg.reg_sim_results8b b
on a.iteration=b.iteration and a.pseudo_hospital=b.pseudo_hospital;
quit;

data both_rates2;
set both_rates2;
	if reg_hospital_rank<24 then reg_quint=1;
	if 24<=reg_hospital_rank<46 then reg_quint=2;
	if 46<=reg_hospital_rank<68 then reg_quint=3;
	if 68<=reg_hospital_rank<90 then reg_quint=4;
	if 90<=reg_hospital_rank then reg_quint=5;

	if hospital_rank<24 then tm_quint=1;
	if 24<=hospital_rank<46 then tm_quint=2;
	if 46<=hospital_rank<68 then tm_quint=3;
	if 68<=hospital_rank<90 then tm_quint=4;
	if 90<=hospital_rank then tm_quint=5;

	if tm_quint=5 then tm_lower_quintile=1;
	if tm_quint ne 5 then tm_lower_quintile=0;
	if reg_quint=5 then reg_lower_quintile=1;
	if reg_quint ne 5 then reg_lower_quintile=0;
run;

proc sort data=both_rates2; by hospital_rank; run;

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


/*Assign patients to quartiles for illness severity*/
proc rank data=ipec.simulations groups=8 out=simulations;
var pred;
ranks rank;
run;

proc means data=simulations mean std median q1 q3;
var pred;
class rank;
run;

proc rank data=tm2.matched_cases groups=8 out=match;
var pred;
ranks rank;
run;

proc means data=match mean std median q1 q3;
var pred;
class rank;
run;

data true;
set  tm2.results_Silber8b;
if hospital_effect="under" then true_lower=1;
else true_lower=0;
run;

 proc freq data=true;
 table lower_quintile*true_lower;
 run;

/*Compare to old method*/
 proc freq data=tm2.results_Silber8b;
 table lower_quintile*hospital_effect;
 run;

proc sort data=tm2.results_Silber8b; by iteration hospital_rank; run;

data iteration1;
set tm2.results_silber8b;
where iteration=1;
run;

data iteration1match;
set tm2.matched_cases;
where iteration=1;
run;

data test;
set iteration1match;
where _matchid<31 and  pseudo_hospital<31;
run;

ods output cmh=cmh breslowdaytest=breslowdaytest relrisk=rr;
ods graphics on;
proc catmod data=test;
model _matchid* pseudo_hospital*simulated_mort/cmh relrisk plots=relriskplot ;
run;

proc sgplot data=tm2.results_Silber8b;
scatter x=hospital_rank y=eventrate/legendlabel="Event Rate";
highlow x=hospital_rank low=eventratelowerci high=eventrateupperci/ legendlabel="95% CI";
refline &median. /axis=y label="median hospital rate" labelattrs=(size=12pt weight=bold);
refline 89 /axis=x label="lowest quintile" labelattrs=(size=12pt weight=bold);
yaxis label="Event Rate" values=(0 to 0.12 by 0.01) labelattrs=(size=12pt weight=bold);
xaxis display=none;
keylegend/valueattrs=(size=12pt weight=bold);
run;


/*Highlight the hospitals that are true under-performers*/
data tm2.results_Silber8b;
set tm2.results_Silber8b;

if hospital_effect="under" then do;
eventrate2=eventrate;
eventratelowerci2=eventratelowerci;
eventrateupperci2=eventrateupperci;
end;

if hospital_effect="better" then do;
eventrate3=eventrate;
eventratelowerci3=eventratelowerci;
eventrateupperci3=eventrateupperci;
end;

run;

goptions reset=all;
ods graphics on/
imagefmt=tiff
imagename="caterpillar plot using Silber method TM highlighted truth"
noborder;

ods graphics/width=10in height=8in;
proc sgplot data=tm2.results_Silber8b ;
vbox eventrate/  category=hospital_rank;
refline 89 /axis=x label="lowest quintile" labelattrs=(size=12pt weight=bold);
yaxis label="Event Rate" values=(0 to 0.1 by 0.01) labelattrs=(size=12pt weight=bold);
xaxis display=none;
/*keylegend "median" "under" "better"/valueattrs=(size=12pt weight=bold);*/
run;




proc sgplot data=tm2.results_Silber8b ;
scatter x=hospital_rank y=eventrate/ markerattrs=(color=black) ;
highlow x=hospital_rank low=eventratelowerci high=eventrateupperci/ 
lineattrs=(color=black pattern=1) legendlabel="True Median" name="median";

/*highlight the hospitals that were under-performers*/
scatter x=hospital_rank y=eventrate2/markerattrs=(color=red symbol=circlefilled);
highlow x=hospital_rank low=eventratelowerci2 high=eventrateupperci2/ 
lineattrs=(color=red pattern=1) legendlabel="True Under" name="under";

/*highlight the hospitals that were over-performers*/
scatter x=hospital_rank y=eventrate3/markerattrs=(color=green symbol=circlefilled);
highlow x=hospital_rank low=eventratelowerci3 high=eventrateupperci3/ 
lineattrs=(color=green pattern=1) legendlabel="True Better" name="better";

/*Add reference lines for median*/
/*refline &median. /axis=y label="median hospital rate" labelattrs=(size=12pt weight=bold);*/
refline 89 /axis=x label="lowest quintile" labelattrs=(size=12pt weight=bold);
yaxis label="Event Rate" values=(0 to 0.12 by 0.01) labelattrs=(size=12pt weight=bold);
xaxis display=none;
keylegend "median" "under" "better"/valueattrs=(size=12pt weight=bold);
run;


/*Highlight the hospitals that are true under-performers*/
data tm2.results_Silber8b;
set tm2.results_Silber8b;
casemixeventrate=eventrate;
casemixeventratelowerci=eventratelowerci;
casemixeventrateupperci=eventrateupperci;

if casemix="more_sick" then do;
casemixeventrate2=eventrate;
casemixeventratelowerci2=eventratelowerci;
casemixeventrateupperci2=eventrateupperci;
end;

if casemix="less_sick" then do;
casemixeventrate3=eventrate;
casemixeventratelowerci3=eventratelowerci;
casemixeventrateupperci3=eventrateupperci;
end;
run;

goptions reset=all;
ods graphics on/
imagefmt=tiff
imagename="caterpillar plot using Silber method TM highlighted casemix"
noborder;

proc sgplot data=tm2.results_Silber8b ;
scatter x=hospital_rank y=casemixeventrate/ markerattrs=(color=black) ;
highlow x=hospital_rank low=casemixeventratelowerci high=casemixeventrateupperci/ 
lineattrs=(color=black pattern=1) legendlabel="Median Casemix" name="median";

/*highlight the hospitals that were more sick*/
scatter x=hospital_rank y=casemixeventrate2/markerattrs=(color=red symbol=circlefilled);
highlow x=hospital_rank low=casemixeventratelowerci2 high=casemixeventrateupperci2/ 
lineattrs=(color=red pattern=1) legendlabel="More sick" name="under";

/*highlight the hospitals that were less sick*/
scatter x=hospital_rank y=casemixeventrate3/markerattrs=(color=green symbol=circlefilled);
highlow x=hospital_rank low=casemixeventratelowerci3 high=casemixeventrateupperci3/ 
lineattrs=(color=green pattern=1) legendlabel="Less sick" name="better";

/*Add reference lines for median and outlier limits*/
refline &median. /axis=y label="median hospital rate" labelattrs=(size=12pt weight=bold);
refline 89 /axis=x label="lowest quintile" labelattrs=(size=12pt weight=bold);
yaxis label="Event Rate" values=(0 to 0.12 by 0.01) labelattrs=(size=12pt weight=bold);
xaxis display=none;
keylegend "median" "under" "better"/valueattrs=(size=12pt weight=bold);
run;
