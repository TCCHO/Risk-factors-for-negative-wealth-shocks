********************************************************************************
* Risk factors for negative wealth shocks
* Conduct main and sensitivity analyses
* Years: 1995/96-2020
* Author: Tsai-Chin Cho
********************************************************************************
clear
clear mata
clear matrix
set maxvar 30000

global datadate 061224
global date 071924


//////////////////////////////////////////////////////////////////////////////
* A. Global controlled variable lists
//////////////////////////////////////////////////////////////////////////////
* Covariate varlists
global adj_demo c.age_base##c.age_base female i.rabplace_c3 i.race c.hhsize_base
global adj_ses i.educyrs4 i.meducyrs4 i.feducyrs4 i.wealth_adjc5_base i.retired_age65
global adj_hlth i.slfhl_base i.nhlpblm_base_cat4
global adj_lfstyl i.smoke_base i.bmistat_base_c3

* Adjustment sets for main analyses
global mainvar_disadvmstat i.year $adj_demo $adj_ses $adj_hlth $adj_lfstyl	
global mainvar_worseslfhl i.year i.mstat_base $adj_demo $adj_ses i.nhlpblm_base_cat4 $adj_lfstyl	
global mainvar_depressed i.year i.mstat_base $adj_demo $adj_ses $adj_hlth $adj_lfstyl
global mainvar_worsecog i.year i.mstat_base $adj_demo $adj_ses $adj_hlth $adj_lfstyl

* Adjustment sets for sensitivity analyses
global senvar_disadvmstat i.year $adj_demo $adj_ses $adj_hlth		
global senvar_worseslfhl i.year i.mstat_base $adj_demo $adj_ses i.nhlpblm_base_cat4	
global senvar_depressed i.year i.mstat_base $adj_demo $adj_ses $adj_hlth
global senvar_worsecog i.year i.mstat_base $adj_demo $adj_ses $adj_hlth



//////////////////////////////////////////////////////////////////////////////
* B. Main Analyses
//////////////////////////////////////////////////////////////////////////////

********************************************************************************
*Table 1. Descriptive analysis
********************************************************************************
use ..\data\3_analysisdata_$datadate, clear
svyset hhid [pweight=wtresp_r], strata(raestrat) single(certainty)

** N obs per person in the sample
sort hhidpn insample year
bysort hhidpn insample: gen Nobs=_N if _n==1
tab Nobs if insample==1
unique hhidpn if insample==1
svy: tab Nobs wlthgtdrop if insample==1, row
*** N obs per person in the sample, by negative shocks in total wealth
sort hhidpn insample wlthgtdrop year
bysort hhidpn insample wlthgtdrop: gen Nobsinsample_wlthgtdrop=_N if _n==1& insample==1
tab Nobsinsample_wlthgtdrop wlthgtdrop if insample==1


** Sample characteristics
foreach y in wlthgtdrop{
	di "`y'"
	di "======================================================="
	tab `y' if insample==1,m
	unique hhidpn if insample==1& `y'==1
	unique hhidpn if insample==1& `y'==0
	dtable age_base i.female i.mstat_base i.rabplace_c3 i.race hhsize_base i.educyrs4 i.meducyrs4 i.feducyrs4 i.wealth_adjc5_base i.retired_age65 i.nhlpblm_base_cat4 i.slfhl_base i.smoke_base i.bmistat_base_c3 if insample==1, ///
	by(`y', nototals tests) svy  ///
	nformat(%7.2f mean sd) nformat(%7.0f median q1 q3) sformat("(N=%s)" frequency)  /// 
	sample(, statistics(freq perc) place(seplabels))  ///
	continuous(age_base hhsize_base, statistics(mean sd median q1 q3) test(none)) /// 
	factor(female mstat_base rabplace_c3 race educyrs4 meducyrs4 feducyrs4 wealth_adjc5_base retired_age65 nhlpblm_base_cat4 slfhl_base smoke_base bmistat_base_c3, statistics(fvrawfreq fvpercent)) ///
export(table1.xlsx, sheet(`y',replace) modify)
}



********************************************************************************
*Table 2. Estimated IRRs from adjusted Poisson mixed effects models
*         Main adjustment
********************************************************************************
use ..\data\3_analysisdata_$datadate, clear
svyset hhid, strata(raestrat) singleunit(certainty)

foreach x in disadvmstat worseslfhl depressed worsecog{
		di "`x'"
		svy: mepoisson wlthgtdrop i.`x' ${mainvar_`x'}  if insample==1 || hhidpn: ,pweight(wtresp_r) covariance(un) irr
}



//////////////////////////////////////////////////////////////////////////////
* C. Sensitivity Analyses
//////////////////////////////////////////////////////////////////////////////

********************************************************************************
* Supplemental Table 1. Sample size by risk factors
********************************************************************************
use ..\data\3_analysisdata_$datadate, clear
svyset hhid [pweight=wtresp_r], strata(raestrat) single(certainty)

tab wlthgtdrop if insample==1,m
unique hhidpn if insample==1
		
foreach y in wlthgtdrop{
	foreach x in disadvmstat worseslfhl depressed worsecog{
		di "`y' - `x'"
		di "======================================================="
		tab `y' if insample==1& `x'!=.a& `x'!=.,m
		unique hhidpn if insample==1& `x'!=.a& `x'!=.
		}
}


foreach y in wlthgtdrop{
	foreach x in disadvmstat worseslfhl depressed worsecog{
		di "`y' - `x'"
		di "======================================================="
		tab `y' if insample==1& `x'!=.a& `x'!=.,m
		unique hhidpn if insample==1& `x'!=.a& `x'!=.& `y'==1
		unique hhidpn if insample==1& `x'!=.a& `x'!=.& `y'==0
		}
}



********************************************************************************
* Supplemental Table 3. Missing data in covariates
********************************************************************************
use ..\data\3_analysisdata_$datadate, clear
svyset hhid, strata(raestrat) singleunit(certainty)

misstable pattern age_base female mstat_base rabplace_c3 race hhsize_base educyrs4 meducyrs4 feducyrs4 wealth_adjc5_base retired_age65 nhlpblm_base_cat4 slfhl_base smoke_base bmistat_base_c3 if insample==1

mdesc age_base female mstat_base rabplace_c3 race hhsize_base educyrs4 meducyrs4 feducyrs4 wealth_adjc5_base retired_age65 nhlpblm_base_cat4 slfhl_base smoke_base bmistat_base_c3 if insample==1



********************************************************************************
* Supplemental Table 4. Included/Excluded observation characterictics 
********************************************************************************
use ..\data\3_analysisdata_$datadate, clear
svyset hhid [pweight=wtresp_r], strata(raestrat) single(certainty)

gen excludesample=1 if missing(insample)
replace excludesample=. if base_year<1996
replace excludesample=. if agey_b_r<55
tab excludesample,m

gen inc_exc_sample=1 if insample==1
replace inc_exc_sample=0 if excludesample==1
tab inc_exc_sample,m
unique hhidpn if inc_exc_sample==1
unique hhidpn if inc_exc_sample==0

dtable age_base i.female i.mstat_base i.rabplace_c3 i.race hhsize_base i.educyrs4 i.meducyrs4 i.feducyrs4 i.wealth_adjc5_base i.retired_age65 i.nhlpblm_base_cat4 i.slfhl_base i.smoke_base i.bmistat_base_c3 i.disadvmstat i.worseslfhl i.depressed i.worsecog i.wlthgtdrop i.nohmwlthgtdrop, ///
	by(inc_exc_sample, nototals tests) svy  ///
	nformat(%7.2f mean sd) nformat(%7.0f median q1 q3) sformat("(N=%s)" frequency)  /// 
	sample(, statistics(freq perc) place(seplabels))  ///
	continuous(age_base hhsize_base, statistics(mean sd median q1 q3) test(none)) /// 
	factor(female mstat_base rabplace_c3 race educyrs4 meducyrs4 feducyrs4 wealth_adjc5_base retired_age65 nhlpblm_base_cat4 slfhl_base smoke_base bmistat_base_c3 disadvmstat worseslfhl depressed worsecog wlthgtdrop nohmwlthgtdrop, statistics(fvrawfreq fvpercent)) ///
export(SupTable9.xlsx, sheet(inc_exc_sample,replace) modify)



********************************************************************************
*Supplemental Table 5. Sample characteristics by an NWS in non-housing financial wealth
********************************************************************************
use ..\data\3_analysisdata_$datadate, clear
svyset hhid [pweight=wtresp_r], strata(raestrat) single(certainty)

** N obs per person in the sample, by negative shocks in non-housing financial wealth
sort hhidpn insample nohmwlthgtdrop year
bysort hhidpn insample nohmwlthgtdrop: gen Nobsinsample_nohmwlthgtdrop=_N if _n==1& insample==1
tab Nobsinsample_nohmwlthgtdrop nohmwlthgtdrop if insample==1


** Sample characteristics
foreach y in nohmwlthgtdrop{
	di "`y'"
	di "======================================================="
	tab `y' if insample==1,m
	unique hhidpn if insample==1& `y'==1
	unique hhidpn if insample==1& `y'==0
	dtable age_base i.female i.mstat_base i.rabplace_c3 i.race hhsize_base i.educyrs4 i.meducyrs4 i.feducyrs4 i.wealth_adjc5_base i.retired_age65 i.nhlpblm_base_cat4 i.slfhl_base i.smoke_base i.bmistat_base_c3 if insample==1, ///
	by(`y', nototals tests) svy  ///
	nformat(%7.2f mean sd) nformat(%7.0f median q1 q3) sformat("(N=%s)" frequency)  /// 
	sample(, statistics(freq perc) place(seplabels))  ///
	continuous(age_base hhsize_base, statistics(mean sd median q1 q3) test(none)) /// 
	factor(female mstat_base rabplace_c3 race educyrs4 meducyrs4 feducyrs4 wealth_adjc5_base retired_age65 nhlpblm_base_cat4 slfhl_base smoke_base bmistat_base_c3, statistics(fvrawfreq fvpercent)) ///
export(suptable4.xlsx, sheet(`y',replace) modify)
}



********************************************************************************
*Supplemental Table 6. Estimated IRRs from adjusted Poisson mixed effects models
*                      Negative shocks in non-housing financial wealth
*                      Main adjustment
********************************************************************************
use ..\data\3_analysisdata_$datadate, clear
svyset hhid, strata(raestrat) singleunit(certainty)


foreach x in disadvmstat worseslfhl depressed worsecog{
		di "`x'"
		svy: mepoisson nohmwlthgtdrop i.`x' ${mainvar_`x'}  if insample==1 || hhidpn: ,pweight(wtresp_r) covariance(un) irr
}



********************************************************************************
* Supplemental Table 7. Estimated IRRs from adjusted Poisson mixed effects models
*                       Lagged outcome; NWS after a two-year lag period following risk factors 
*                       Main adjustment
********************************************************************************
use ..\data\3_analysisdata_$datadate, clear
svyset hhid, strata(raestrat) singleunit(certainty)

destring hhidpn, gen(hhidpn_num)
codebook hhidpn hhidpn_num
tsset hhidpn_num year, delta(2)
gen lag1_wlthgtdrop=L1.wlthgtdrop
browse hhidpn_num year wlthgtdrop lag1_wlthgtdrop lag2_wlthgtdrop


foreach x in disadvmstat worseslfhl depressed worsecog{
		di "`x'"
		svy: mepoisson lag1_wlthgtdrop i.`x' ${mainvar_`x'}  if insample==1 || hhidpn: ,pweight(wtresp_r) covariance(un) irr
}



********************************************************************************
* Supplemental Table 8. Estimated IRRs from adjusted Poisson mixed effects models
*                       BASE ADJUSTMENT (Main adjustment w/o smoking behavior & BMI)
********************************************************************************
use ..\data\3_analysisdata_$datadate, clear
svyset hhid, strata(raestrat) singleunit(certainty)

foreach x in disadvmstat worseslfhl depressed worsecog{
		di "`x'"
		svy: mepoisson wlthgtdrop i.`x' ${senvar_`x'} if insample==1 || hhidpn: ,pweight(wtresp_r) covariance(un) irr
}



********************************************************************************
* Supplemental Table 9. Estimated IRRs from adjusted Poisson mixed effects models
*                       Multiple imputations by chained equations (10 replicates)
********************************************************************************
use ..\data\3_analysisdata_$datadate, clear
svyset hhid, strata(raestrat) singleunit(certainty)
keep if insample==1

**==================================================================
** Conduct imputation
**==================================================================
ice wlthgtdrop year age_base female m.mstat_base m.rabplace_c3 m.race hhsize_base m.educyrs4 m.meducyrs4 m.feducyrs4 m.wealth_adjc5_base m.retired_age65 nhlpblm_base_cat4 m.slfhl_base m.smoke_base m.bmistat_base_c3, m(10) persist seed(233) saving(..\data\impdata\midata, replace)

**==================================================================
** Conduct analysis
**==================================================================
use ..\data\impdata\midata, clear
misstable pattern wlthgtdrop age_base female mstat_base rabplace_c3 race hhsize_base educyrs4 meducyrs4 feducyrs4 wealth_adjc5_base retired_age65 nhlpblm_base_cat4 slfhl_base smoke_base bmistat_base_c3 if _mj!=0

*** Multiple imputation settings
mi import ice, imputed(mstat_base rabplace_c3 race educyrs4 meducyrs4 feducyrs4 wealth_adjc5_base retired_age65 slfhl_base smoke_base bmistat_base_c3)

mi svyset hhid, strata(raestrat) singleunit(certainty)


foreach x in disadvmstat worseslfhl depressed worsecog{
		di "`x'"
		mi estimate, cmdok irr: svy: mepoisson wlthgtdrop i.`x' ${mainvar_`x'} || hhidpn: ,pweight(wtresp_r) covariance(un)
}
