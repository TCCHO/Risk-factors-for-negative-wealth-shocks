********************************************************************************
* Risk factors for negative wealth shocks
* Construct analysis variables
* Years: 1995/96-2020
* Author: Tsai-Chin Cho
********************************************************************************
clear
clear mata
clear matrix
set maxvar 30000


global hrsdatadate 061224
global wlthdatadate 061224
global expdatadate 061224
global date 061224

//////////////////////////////////////////////////////////////////////////////
* 1) Create Initial Data by Extracting Variables from HRS Data Products 
//////////////////////////////////////////////////////////////////////////////
do 1_hrs_datamaker_$hrsdatadate.do



//////////////////////////////////////////////////////////////////////////////
* 2) Construct Intermediate Analysis Data Files 
* 2A) Create Outcome (Negative Wealth Shocks) Data File
//////////////////////////////////////////////////////////////////////////////
use ..\data\1_hrsdata_$hrsdatadate, clear
keep if iwstat_r==1

********************************************************************************
* create variables
********************************************************************************
** baseline exposure year
gen base_year= year-4  // E.g., for a negative wealth shock occurred between 2004 and 2006 (represented as year=2006 in the data), the baseline year is 2002 bc the exposures (changes in marital and health status) are defined between 2002 and 2004
gen outc_endyear=year

**==================================================================
** negative wealth shocks over two years
**==================================================================
***count N persons per couple
codebook mstat_r
recode mstat_r (1/3=2)(4/8=1)(.j .m=.), gen(ncppers)
tab mstat_r ncppers,m

**------------------------------
** total wealth
**------------------------------
***create wealth adjusted with household size and currency value
gen wealth_adj=cond(ncppers==2,atotb_h/(cpi/100)/1.5,atotb_h/(cpi/100)/1)
replace wealth_adj=. if missing(ncppers)
label var wealth_adj "Estimated total wealth (2018 USD) adjusted with household size"
****make sure the household wealth are consistent for both persons within household
bysort hhid year: egen mn_wealth_adj=mean(wealth_adj)
replace wealth_adj=mn_wealth_adj if wealth_adj!=mn_wealth_adj
drop mn_wealth_adj

***extreme wealth losses >=75%
// Note. Varaible construction idea: whether wealth in current wave is smaller than the value from prior wave that dropped by 75%
browse hhidpn year wealth_adj
gen wealth_minus75p=wealth_adj- abs(wealth_adj)*0.75
sort hhidpn year 
bysort hhidpn: gen wlthgtdrop=(wealth_adj<=wealth_minus75p[_n-1]) if !missing(wealth_adj)& !missing(wealth_adj[_n-1])& year-year[_n-1]==2
replace wlthgtdrop=0 if wealth_adj==0& wealth_adj[_n-1]==0& year-year[_n-1]==2  //5524 obs re-assigned zeros, meaning that most people have zero values in two consecutive waves
tab wlthgtdrop,m
lab var wlthgtdrop "Whether experience a negative wealth shock (wave-specific, total wealth)"


**------------------------------
** non-housing financial wealth
**------------------------------
**create wealth adjusted with household size and currency value
gen nohmwlth_adj=cond(ncppers==2,atotf_h/(cpi/100)/1.5,atotf_h/(cpi/100)/1)
replace nohmwlth_adj=. if missing(ncppers)
label var nohmwlth_adj "Estimated wealth (2018 USD) adjusted with household size"
***make sure the household wealth are consistent for both persons within household
bysort hhid year: egen mn_nohmwlth_adj=mean(nohmwlth_adj)
replace nohmwlth_adj=mn_nohmwlth_adj if nohmwlth_adj!=mn_nohmwlth_adj
drop mn_nohmwlth_adj

**extreme wealth losses >=75%
browse hhidpn year nohmwlth_adj
gen nohmwealth_minus75p=nohmwlth_adj- abs(nohmwlth_adj)*0.75
sort hhidpn year 
bysort hhidpn: gen nohmwlthgtdrop=(nohmwlth_adj<=nohmwealth_minus75p[_n-1]) if !missing(nohmwlth_adj)& !missing(nohmwlth_adj[_n-1])& year-year[_n-1]==2
replace nohmwlthgtdrop=0 if nohmwlth_adj==0& nohmwlth_adj[_n-1]==0& year-year[_n-1]==2  //5524 obs re-assigned zeros, meaning that most people have zero values in two consecutive waves
tab nohmwlthgtdrop,m
lab var nohmwlthgtdrop "Whether experience a negative wealth shock (wave-specific, non-housing financial wealth)"


unique hhidpn
unique hhidpn if year>=1996
sort hhidpn year

keep hhidpn base_year outc_endyear ncppers-nohmwlthgtdrop
keep if inrange(base_year,1992,2016)

save ..\data\2a_wlthdata_$wlthdatadate, replace


//////////////////////////////////////////////////////////////////////////////
* 2B) Create Exposure & Covariates Data File
//////////////////////////////////////////////////////////////////////////////
use ..\data\1_hrsdata_$hrsdatadate, clear
keep if iwstat_r==1

********************************************************************************
* create variables
********************************************************************************
**==================================================================
** covariates at the baseline (T)
**==================================================================
**numeric person ID
destring hhidpn, gen(id)
codebook hhidpn id

**person-level sampling weights fixed at first responded wave for longitudinal analysis
sort hhidpn year
bysort hhidpn: gen wt_fstwv=wtresp_r[1]

**baseline year
gen base_year=year
tab base_year, m

**age
codebook agey_b_r
recode agey_b_r (missing=.), gen(age_base)

**gender
codebook ragender
recode ragender (2=1)(1=0), gen(female)

**marital status
codebook mstat_r
recode mstat_r (1 2 3=1 "1.married/partnered")(4 5 6=2 "2.separated/divorced")(7=3 "3.widowed")(8=4 "4.never married")(missing=.),gen(mstat_base)
tab mstat_base mstat_r,m

**place of birth
codebook rabplace
recode rabplace (1 2 3 4 8 9=1 "1.US, non-south")(5/7=2 "2.US, south")(11=3 "3.outside US")(10 .m=.), gen(rabplace_c3)
tab rabplace rabplace_c3,m


**race/ethnicity
codebook rahispan raracem
gen race=1 if rahispan==0& raracem==1
replace race=2 if rahispan==0& raracem==2
replace race=3 if rahispan==0& raracem==3
replace race=4 if rahispan==1
tab race,m
lab def raceth 1 "1.NHW" 2 "2.NHB" 3 "3.NH Others" 4 "4.Hispanic"
lab values race raceth

**number of people in the household
codebook hhres_h
recode hhres_h (missing=.), gen(hhsize_base)
tab hhsize_base,m

**education
codebook raedyrs raeduc
tab raedyrs raeduc,m
recode raedyrs (min/11=1 "1.ls HS")(12=2 "2.HS")(13/15=3 "3.some college")(16/max=4 "4.college or above")(missing=.), gen(educyrs4)
tab raedyrs educyrs4,m

**parents' education years
codebook rameduc rafeduc
recode rameduc rafeduc (missing=.)
tab rafeduc,m
***categorical mother education years
recode rameduc (min/11=1 "1.ls HS")(12=2 "2.HS")(13/15=3 "3.some college")(16/max=4 "4.college or above")(missing=.), gen(meducyrs4)
tab rameduc meducyrs4,m
***categorical father education years
recode rafeduc (min/11=1 "1.ls HS")(12=2 "2.HS")(13/15=3 "3.some college")(16/max=4 "4.college or above")(missing=.), gen(feducyrs4)
tab rafeduc feducyrs4,m


**retirement status
codebook lbrf_r sayret_r work_r
tab sayret_r work_r,m
gen retired_age65=1 if retired==1& age_base>=65
replace retired_age65=0 if retired==0 | (retired==1& age_base<65)
tab age_base retired_age65,m
lab var retired_age65 "Retired and at least 65 years old at the baseline"


**wealth quintile
***count N persons per couple
codebook mstat_r
recode mstat_r (1/3=2)(4/8=1)(.j .m=.), gen(ncppers)
tab mstat_r ncppers,m
***create wealth adjusted with household size and currency value
gen wealth_adj=cond(ncppers==2,atotb_h/(cpi/100)/1.5,atotb_h/(cpi/100)/1)
replace wealth_adj=. if missing(ncppers)
label var wealth_adj "Estimated total wealth (2018 USD) adjusted with household size"
****make sure the household wealth are consistent for all people within household
bysort hhid year: egen mn_wealth_adj=mean(wealth_adj)
replace wealth_adj=mn_wealth_adj if wealth_adj!=mn_wealth_adj
drop mn_wealth_adj
***create wealth quintile
egen wealth_adjc5_base=xtile(wealth_adj), n(5) by(agey_b_r year) weight(wtresp_r)
recode wealth_adjc5_base (1=1) (2/5=0), gen(wealth_base_q1)
tab wealth_adjc5_base wealth_base_q1,m


**smoking status
tab smokev_r smoken_r,m
gen smoke_base=1 if smoken_r==1
replace smoke_base=2 if smoken_r==0&smokev_r==1
replace smoke_base=3 if smoken_r==0&smokev_r==0
lab def smst 1 "1.current smoker" 2 "2.former smoker" 3 "3.never smoke", modify
lab values smoke_base smst
tab smoke_base,m

**bmi
codebook bmi_r
recode bmi_r (min/18.49999=1 "1.underweight")(18.5/24.9999=2 "2.normal")(25/29.9999=3 "3.overweight")(30/max=4 "4.obese")(missing=.), gen(bmistat_base)
tab bmistat_base,m
recode bmi_r (min/24.9999=1 "1.underweight/normal")(25/29.9999=2 "2.overweight")(30/max=3 "3.obese")(missing=.), gen(bmistat_base_c3)
tab bmistat_base bmistat_base_c3,m

**doctor diagnosed health problems
codebook hibpe_r diabe_r cancre_r lunge_r hearte_r stroke_r psyche_r arthre_r conde_r
*** number of chronic conditions 
egen nhlpblm_base=rowtotal(hibpe_r diabe_r cancre_r lunge_r hearte_r stroke_r arthre_r),m
egen nhlpblm_base_miss=rowmiss(hibpe_r diabe_r cancre_r lunge_r hearte_r stroke_r arthre_r)
tab nhlpblm_base nhlpblm_base_miss,m
recode nhlpblm_base (3/7=3 "3. 3+"), gen(nhlpblm_base_cat4)
tab nhlpblm_base_cat4 nhlpblm_base,m
lab var nhlpblm_base "number of the seven chronic conditions (hibp, diab, cancr, lung, heart, strok, arthr)"

**self-reported health
codebook shlt_r
recode shlt_r (.d .r=.), gen(slfhl_base)
tab shlt_r slfhl_base,m

**CESD
// .s skip bc of proxy interview
codebook cesd_r
recode cesd_r (missing=.), gen(cesd_base)
tab cesd_r cesd_base,m
recode cesd_r (0/2=0)(3/max=1)(missing=.), gen(cesd_depress_base)
tab cesd_r cesd_depress_base,m

**cognitive status
codebook cogfunction
lab def dem 1 "1.normal" 2 "2.CIND" 3 "3.demented", modify
lab values cogfunction dem


**==================================================================
** changes in marital and health status two years after baseline 
**==================================================================
**whether becoming widowed/divorced/separated
sort hhidpn year
bysort hhidpn: gen disadvmstat=1 if inlist(mstat_base[_n+1],2,3)& mstat_base==1& year[_n+1]-year==2
bysort hhidpn: replace disadvmstat=0 if missing(disadvmstat)& !missing(mstat_base[_n+1])& !missing(mstat_base)& year[_n+1]-year==2
replace disadvmstat=.a if mstat_base!=1
lab values disadvmstat mastat
tab disadvmstat,m
lab var disadvmstat "Becoming widowed/divorced/separated two years after baseline"


**changes in self-reported health
// The health categories are numbered from 1 (excellent) to 5 (poor), so that positive values of the change in self-reported health denote deterioration
codebook slfhl_base
sort hhidpn year
bysort hhidpn: gen worseslfhl=1 if inlist(slfhl_base[_n+1],4,5)& inlist(slfhl_base,1,2,3)& year[_n+1]-year==2
bysort hhidpn: replace worseslfhl=0 if missing(worseslfhl)& !missing(slfhl_base[_n+1])& !missing(slfhl_base)& year[_n+1]-year==2
replace worseslfhl=.a if inlist(slfhl_base,4,5)
lab def badslfhl .a "fair or poor health at baseline"
lab values worseslfhl badslfhl
tab worseslfhl,m
lab var worseslfhl "Worse self-reported general health in the two years after baseline"


**changes in CESD depression status
codebook cesd_depress_base
sort hhidpn year
bysort hhidpn: gen depressed=1 if cesd_depress_base[_n+1]==1& cesd_depress_base==0& year[_n+1]-year==2
bysort hhidpn: replace depressed=0 if missing(depressed)& !missing(cesd_depress_base[_n+1])& !missing(cesd_depress_base)& year[_n+1]-year==2
replace depressed=.a if cesd_depress_base==1
lab def dep .a "depressed at baseline"
lab values depressed dep
tab depressed,m
lab var depressed "Becoming depressed in the two years after baseline"


**changes in cognitive status
codebook cogfunction
sort hhidpn year
bysort hhidpn: gen worsecog=1 if cogfunction[_n+1]>cogfunction& year[_n+1]-year==2& !missing(cogfunction[_n+1])
bysort hhidpn: replace worsecog=0 if missing(worsecog)& !missing(cogfunction[_n+1])& !missing(cogfunction)& year[_n+1]-year==2
replace worsecog=.a if inlist(cogfunction,2,3)
lab def badcog .a "CIND/demented at baseline"
lab values worsecog badcog
tab worsecog,m
tab worsecog year,m
lab var worsecog "Becoming CIND/demented in the two years after baseline"

save ..\data\2b_expdata_$expdatadate, replace


//////////////////////////////////////////////////////////////////////////////
* 3) Create Final Analysis Data- Merge the Constructed Outcome and Exposure/Covariates Data Files
//////////////////////////////////////////////////////////////////////////////
use ..\data\2a_wlthdata_$wlthdatadate, clear
merge 1:1 hhidpn base_year using ..\data\2b_expdata_$expdatadate

tab base_year if _m==3,m
keep if _m==3
drop _m


**==================================================================
** create study sample indicator
**==================================================================
gen insample=1
replace insample=. if base_year<1996
tab insample,m

** exclude sample <55 years old at baseline
replace insample=. if age_base<55
unique hhidpn if insample==1

** exclude sample who did not respond in the following survey wave, which is needed to construct exposures
sort hhidpn year
bysort hhidpn: replace insample=. if base_year<2016& year[_n+1]-year!=2
tab insample base_year,m
unique hhidpn if insample==1

** exclude sample with missing outcome
replace insample=. if missing(wlthgtdrop)
unique hhidpn if insample==1
unique hhid if insample==1
unique hhidpn if insample==1& wlthgtdrop==1
unique hhidpn if insample==1& wlthgtdrop==0
lab var insample "Indicator of study sample"


**==================================================================
** clean data
**==================================================================
unique hhidpn year if insample==1
tab base_year insample,m
order hhidpn year
sort hhidpn year


label data "HRS 1996-2020 neg wealth shock"
save ..\data\3_analysisdata_$date, replace