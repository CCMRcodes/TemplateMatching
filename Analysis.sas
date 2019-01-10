/*Descriptives characteristics*/
proc means data=ipec.simulations median q1 q3;
var pred age;
run;

proc freq data=ipec.simulations;
table mort sex white black hispanic opc nh ed;
run;

/*Compare template matching to regression*/
%let scenario=1;

proc sort data=reg.reg_sim_results&scenario. nodupkey; by iteration reg_hospital_rank; run;
proc sort data=tm.tm_sim_results&scenario. nodupkey; by iteration tm_hospital_rank; run;

proc sql;
create table both_rates as
select a.*, b.reg_mad, b.reg_hospital_rank, b.reg_medianhospital,
b.reg_eventrate, b.reg_eventratelowerci, b.reg_eventrateupperci, b.reg_underperformrate, b.reg_under,
b.reg_over, b.reg_sigdiff
from tm.tm_sim_results&scenario. a
left join reg.reg_sim_results&scenario. b
on a.iteration=b.iteration and a.pseudo_hospital=b.pseudo_hospital;
quit;


/*Identify the quintile in which hospital was in*/
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
run;

/*Correlation and 95% CI*/
proc corr data=both_rates outp=corr noprint;
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

data both_rates;
set both_rates;
	if tm_hospital_rank>89 then tm_lower_quintile=1;
	if tm_hospital_rank<=89 then tm_lower_quintile=0;
	if reg_hospital_rank>89 then reg_lower_quintile=1;
	if reg_hospital_rank<=89 then reg_lower_quintile=0;
run;

proc sort data=both_rates; by iteration tm_hospital_rank; run;

proc freq data=both_rates;
table tm_lower_quintile*reg_lower_quintile/out=p;
run;

data p;
set p;
where tm_lower_quintile=1 and reg_lower_quintile=1;
p=count/22000;
lower=p-(1.96*sqrt(p*(1-p)/22000));
upper=p+(1.96*sqrt(p*(1-p)/22000));
run;

data p;
set p ;
scenario="&scenario.";
run;

data all;
set all p (keep=scenario p lower upper);
run;

proc print data=all; run;

/*Truth*/
data both_rates;
set both_rates;
if hospital_effect="under" then true_bottom=1;
else true_bottom=0;
run;

proc freq data=both_rates;
table tm_lower_quintile*true_bottom/out=p;
run;


proc freq data=both_rates;
table reg_under*tm_under/missing norow nocol nopercent;
run;

proc freq data=both_rates;
table reg_over*tm_over/missing norow nocol nopercent;
run;

proc sgplot data=both_rates;
scatter x=tm_hospital_rank y=reg_hospital_rank;
run;

data both_rates;
set both_rates;
call symputx("tm_medianhospital", tm_medianhospital);
run;

proc sgplot data=both_rates;
scatter x=tm_hospital_rank y=tm_eventrate/legendlabel="Event Rate";
highlow x=tm_hospital_rank low=tm_eventratelowerci high=tm_eventrateupperci/ legendlabel="95% CI";
refline &tm_medianhospital /axis=y label="median hospital rate" labelattrs=(size=12pt weight=bold);
yaxis label="Event Rate" values=(0 to 0.1 by 0.01) labelattrs=(size=12pt weight=bold);
xaxis display=none;
keylegend/valueattrs=(size=12pt weight=bold);
run;

data both_rates;
set both_rates;
call symputx("reg_medianhospital", reg_medianhospital);
run;

proc sgplot data=both_rates;
scatter x=reg_hospital_rank y=reg_eventrate/legendlabel="Event Rate";
highlow x=reg_hospital_rank low=reg_eventratelowerci high=reg_eventrateupperci/ legendlabel="95% CI";
refline &reg_medianhospital /axis=y label="median hospital rate" labelattrs=(size=12pt weight=bold);
yaxis label="Event Rate" values=(0 to 0.1 by 0.01) labelattrs=(size=12pt weight=bold);
xaxis display=none;
keylegend/valueattrs=(size=12pt weight=bold);
run;

