***************************************************************************
***** GxE of August vs. September births - ALSPAC. 
***** Stephanie von Hinke and Pietro Biroli, March 2021
***************************************************************************

/*
ssc install ivreghdfe
*/

set more off
clear all
capture log close

if "`c(username)'" == "pbirol" {
	global dirdata  "C:/Users/pbirol/Documents/work/research/ALSPAC_GxE/Alspac"
}

else if "`c(username)'" == "ecsmvhkv" {
	global dirdata  "\\rdsfcifs.acrc.bris.ac.uk/ALSPAC_GxE/Alspac"
}

global dirdropbox  "/Users/`c(username)'/Dropbox/GEIGHEI"

global dirtables  "${dirdropbox}/projects/GxE_4practitioners/tables"
global dirfigures "${dirdropbox}/projects/GxE_4practitioners/figures"


cd "${dirdropbox}/projects/GxE_4practitioners/"

* SET UP LOG FILE
local date: display %td_CCYY-NN-DD date(c(current_date), "DMY")
local datetime = subinstr("`date'"," ","",.)+"_" +subinstr(c(current_time), ":", "-", .)
di "`datetime'"
log using "${dirdropbox}/projects/GxE_4practitioners/logfiles/PractictionersPaper_`datetime'", text replace


** SKIP Patterns (set to zero if you want to skip that section)
global dataclean      = 1
global analysis       = 1
	global IDstrategy     = 1
	global powercalc      = 1    // this takes a long while
	global rGE            = 1
	global predictive     = 1
	global funcform       = 1
	global regs           = 1
	global iv             = 1    // this takes a while
	global perm           = 1    // this takes a while

*---------------------------------------------------------------------------------*
* CLEAN THE DATA
*---------------------------------------------------------------------------------*

if ${dataclean}==1{ // cleans the data and creates the variables for the regression

*** Saving PCs into Stata format
insheet using "${dirdata}/Child/ALSPAC_pc_10_CHILDREN.csv", clear
rename 	v1 id_child 
drop 	v2
foreach i in 3 4 5 6 7 8 9 10 11 12 {
	local p = (`i'-2)
	rename v`i' c_PC`p'
}
compress
save 	"${dirdata}/Child/ALSPAC_pc_10_CHILDREN.dta", replace


*** Saving new PGSs (based on 23andme and UKB) into Stata format
foreach i in children mothers {
	foreach j in ukb 23me 23me_ukb {
		import delimited "${dirdropbox}/PGS/ALSPAC/EA/PGS_EA_alspac_ldpred_prior1_`j'_based_`i'.profile", delimiter(space, collapse) varnames(1) clear
		rename 	fid id_child 
		rename 	scoresum pgs_`i'_`j'
		compress
		save 	"${dirdata}/Child/PGS_`i'_`j'.dta", replace
	}
}

*** On ALSPAC data held at Erasmus
use 	"${dirdata}/Data_Set/Rietveld_12Sep18.dta", clear

* Merge in children's PGSs (UKB and 23me)
egen 	id_child = concat(cidB2492 qlet)
merge 	1:1 id_child using "${dirdata}/Child/ALSPAC_pc_10_CHILDREN.dta"
drop 	if _m<3
drop 	_merge
foreach j in ukb 23me 23me_ukb {
	merge 	1:1 id_child using "${dirdata}/Child/PGS_children_`j'.dta"
	drop 	if _m<3
	drop 	_merge
}

* Merge the plink-based score
merge 	1:1 cidB2492 using"${dirdata}/Child/PGS_children_alspac_plink_EA.dta" 
tab qlet birth_order, miss
drop birth_order //keep qlet, the original one
drop 	if _m<3
drop 	_merge
sum 	pgs_child_EA
gen 	pgs_children_plink = (pgs_child_EA -r(mean))/r(sd)
drop 	pgs_child_EA

* Merge with mother's PGS (UKB and 23me) and PCs
foreach j in ukb 23me 23me_ukb {
	gen 	tmp = id_child
	replace id_child = subinstr(id_child,"A","M",1)
	merge 	1:1 id_child using "${dirdata}/Child/PGS_mothers_`j'.dta"
	drop 	if _m==2 	// drop those only in using data
	replace id_child = tmp
	drop 	_merge tmp
}
gen 	id_mother = cidB2492
tostring id_mother, replace
replace id_mother = id_mother + "M"
drop 	if cidB2492 ==.
merge 	m:1 id_mother using "${dirdata}/Mother/PC10_alspac_mothers.dta"
forval i=1(1)10 {
	rename PC`i' m_PC`i'
}
drop 	_merge 
rename qlet birth_order

* Multiply 23 and me PGSs by -1 (they are reverse-coded)
pwcorr pgs*
replace pgs_children_23me = pgs_children_23me * -1
replace pgs_mothers_23me  = pgs_mothers_23me  * -1

* Month of birth
gen 	MoB = ka498
recode 	MoB (-9999=.)
label 	def MoB 1 "Jan" 2 "Feb" 3 "Mar" 4 "Apr" 5 "May" 6 "Jun" 7 "Jul" 8 "Aug" 9 "Sep" 10 "Oct" 11 "Nov" 12 "Dec"
label 	val MoB MoB
label 	var MoB "Month of birth"

* Year-Month of birth
gen 	YoB = mz024b
recode 	YoB (-9999=.)
label 	var YoB "Year of birth"
tab 	YoB, gen(YoB)
rename 	YoB1 YoB91
rename 	YoB2 YoB92
rename 	YoB3 YoB93
gen 	YMoB = ym(YoB,MoB)
format 	YMoB %tm
label 	var YMoB "Year-month of birth"

* Gender
gen 	male = kz021
recode 	male (2=0) (-9999 -1 = .)
label 	var male "Male"

* Entry assessment score
recode 	sat092b (-10 -6 -5 = .)
sum 	sat092b 
gen 	ea = (sat092b - r(mean)) / r(sd)
label 	var ea "Entry Assessment score (age 4)"

* Key stage 1 
recode 	sat190a (-10 -6 -5 = .)
sum 	sat190a
gen 	ks1 = (sat190a - r(mean)) / r(sd)
label 	var ks1 "Key stage 1 summary score (age 7)"

* Key stage 2
foreach i in k2_tote k2_totm k2_tots {
	replace `i' = " " if inlist(`i',"_NV","-10","A","B","L","T","X","Z")
}
destring k2_tote, gen(k2e)
destring k2_totm, gen(k2m)
destring k2_tots, gen(k2s)
egen 	k2 = rsum(k2e k2m k2s)
sum 	k2 if k2e<. & k2m<. & k2s<.
gen 	ks2 = (k2 - r(mean)) / r(sd) if k2e<. & k2m<. & k2s<.
label 	var ks2 "Key stage 2 summary score (age 11)"
drop 	k2e k2m k2s k2

* Key stage 3
foreach i in k3_tote k3_totm {
	replace `i' = " " if inlist(`i',"-10","A","IN","M","V")
}
destring k3_tote, gen(k3e)
destring k3_totm, gen(k3m)
egen 	k3 = rsum(k3e k3m)
sum 	k3 if k3e<. & k3m<. 
gen 	ks3 = (k3 - r(mean)) / r(sd) if k3e<. & k3m<.
label 	var ks3 "Key stage 3 summary score (age 14)"
drop 	k3e k3m k3 

* Key stage 4
replace ks4_ptstnewe = . if inlist(ks4_ptstnewe,-10,-1,0)
sum 	ks4_ptstnewe 
gen 	ks4 = (ks4_ptstnewe - r(mean)) / r(sd)
label 	var ks4 "Key stage 4 summary score (age 16)"

* IQ (WISC)
recode 	f8ws112 (-9999 -3 -2 = .)
sum 	f8ws112 
gen 	IQ = (f8ws112 - r(mean)) / r(sd)
label 	var IQ "Wechsler Intelligence Scale for Children (IQ, age 8)"

*   August vs. September births
gen 	MoBnew = MoB
recode 	MoBnew (9=0) (10=1) (11=2) (12=3) (1=4) (2=5) (3=6) (4=-5) (5=-4) (6=-3) (7=-2) (8=-1)
label 	def MoBnew 0 "Sep" 1 "Oct" 2 "Nov" 3 "Dec" 4 "Jan" 5 "Feb" 6 "Mar"        -5 "Apr" -4 "May" -3 "Jun" -2 "Jul" -1 "Aug" 
label 	val MoBnew MoBnew
label var MoBnew "Month of birth (Sept=0)"

* "Treated": born in the 6 months after September
gen treat = (MoBnew>=0) if MoBnew<.
label define treat 0 "Born before Sept" 1 "Born after Sept"
label values treat treat

tab MoB MoBnew, nol
tab MoBnew treat


**---- include and clean some additional baseline characteristics (mostly for table on baseline balance)---#
* Birth weight
recode 	kz030 (-9999 -10 -1 = .)
rename 	kz030 bw
label 	var bw "Child's birthweight (in g)"

* Smoking during pregnancy
recode 	a200 (-9999 -7 -1 = .)
gen 	smokepreg = (a200>0) if a200<.
label 	var smokepreg "Mother smoked cigarettes during pregnancy"
drop 	a200

* Marital status
recode 	a525 (-9999 -1 = .) (2 3 4 5 6 = 1) (1=0)
rename 	a525 married
label 	def a525 0 "single" 1 "married", modify
label 	val married a525
label 	var married "Mother is married"

* Mother's age at first pregnancy
recode 	b023 (-9999 -1 =.)
rename 	b023 mumagepreg
label 	var mumagepreg "Mother's age at first pregnancy"

* Maternal anxiety during pregnancy
recode 	b352a (-9999 -7 -1 = .)
rename 	b352a m_anxiety
label 	var m_anxiety "Mother's anxiety score during pregnancy"

* Maternal depression during pregnancy
recode 	b354a (-9999 -7 -1 = .)
rename 	b354a m_depression
label 	var m_depression "Mother's depression score during pregnancy"

* Mother's highest educ qualification
recode 	c645a (-9999 -1 = .)
rename 	c645a mumeduc
tab 	mumeduc, gen(mumed)
label 	var mumed1 "Mother proportion CSE"
label 	var mumed2 "Mother proportion vocational"
label 	var mumed3 "Mother proportion O-level"
label 	var mumed4 "Mother proportion A-level"
label 	var mumed5 "Mother proportion Degree"
drop 	mumeduc

* Partner's highest educ qualification
recode 	c666a (-9999 -1 = .)
rename 	c666a dadeduc
tab 	dadeduc, gen(daded)
label 	var daded1 "Partner proportion CSE"
label 	var daded2 "Partner proportion vocational"
label 	var daded3 "Partner proportion O-level"
label 	var daded4 "Partner proportion A-level"
label 	var daded5 "Partner proportion Degree"
drop 	dadeduc

* Mother's social class
recode 	c755 (-9999 -1 = .) (65=3)
rename 	c755 socclass
tab 	socclass, gen(SC)
label 	var SC1 "Mother proportion Social Class I"
label 	var SC2 "Mother proportion Social Class II"
label 	var SC3 "Mother proportion Social Class III(non-manual)"
label 	var SC4 "Mother proportion Social Class III(manual)"
label 	var SC5 "Mother proportion Social Class IV"
label 	var SC6 "Mother proportion Social Class V"
drop 	socclass


***************************************************************************
keep 	cidB2492 birth_order male ea ks1 ks2 ks3 ks4 IQ YMoB YoB YoB91 YoB92 YoB93 MoB MoBnew pgs_children_plink pgs_children_ukb pgs_children_23me pgs_children_23me_ukb pgs_mothers_ukb pgs_mothers_23me pgs_mothers_23me_ukb c_PC* m_PC* treat* mumagepreg smokepreg m_anxiety m_depression married mumed2 mumed3 mumed4 mumed5 daded2 daded3 daded4 daded5 SC2 SC3 SC4 SC5 SC6 bw 

order 	cidB2492 birth_order MoB MoBnew treat YoB YoB91 YoB92 YoB93 YMoB male ea ks* IQ pgs_* c_PC* m_PC* 

* Standardize to have mean 0 and standard deviation 1
foreach pgs of varlist pgs_* {
	qui sum `pgs'
	replace `pgs' = (`pgs'-r(mean))/(r(sd))
	gen high`pgs' = (`pgs'>0) if `pgs'<.
	tab high`pgs'
}

* Use the PGS with the higher predictive power
/* The meta-analysed score 23me_ukb is the one that always has the highest predictive power */
foreach outcome in ea ks1 ks2 ks3 ks4 {
	foreach method in plink ukb 23me 23me_ukb {
		label var pgs_children_`method' "PGS `method'"
		qui eststo `outcome'_`method': reg `outcome' pgs_children_`method' male c_PC*, robust
	} //end foreach method

esttab `outcome'_plink `outcome'_ukb `outcome'_23me `outcome'_23me_ukb, b se keep(pgs_children_*) stats(r2 N)

esttab 	`outcome'_plink `outcome'_ukb `outcome'_23me `outcome'_23me_ukb using "${dirtables}/PredictivePower_`outcome'.tex", replace ///
	frag bookt b(3) se(3) keep(pgs*) star(* 0.10 ** 0.05 *** 0.01) ///
	nomtitles stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes ///
	label
} // end foreach outcome

est drop _all


* Use meta-analysed PGS 
gen PGS     = pgs_children_23me_ukb
label var PGS "Child PGS for Educational Attainment"
gen highPGS = highpgs_children_23me_ukb 

gen PGS_mothers = pgs_mothers_23me_ukb 
label var PGS_mothers "Mother PGS for Educational Attainment"


* create all of the demeaned interactions
gen treat_PGS = treat*PGS
gen treat_highPGS = treat*highPGS

quietly sum MoB
gen MoB0 = MoB-r(mean)
quietly sum MoBnew
gen MoBnew0 = MoBnew-r(mean)

gen treat_MoB = treat*MoB0
gen treat_MoBnew = treat*MoBnew0

gen MoB_PGS = MoB0*PGS
gen MoBnew_PGS = MoBnew0*PGS

gen MoB_PGS_treat = MoB0*PGS*treat
gen MoBnew_PGS_treat = MoBnew0*PGS*treat

* sample selection based on window of n-months from the cutoff
forvalues width=2/5{
	gen window`width'mth = (MoBnew <=`width'-1 & MoBnew>(-`width'-1)) if MoBnew<.
}
tab MoB window3mth

save "${dirdata}/Data_Set/cleanALSPAC4application.dta", replace
} // end if dataclean













*---------------------------------------------------------------------------------*
* DATA ANALYSIS
*---------------------------------------------------------------------------------*
if ${analysis}==1{ // produces all of the figures and tables used in the paper (section by section)

***************************************************************************
if ${IDstrategy}==1{ // Check the validity of the identification strategy

set scheme plotplainblind

************* fig:MoB 
*------- RDD style figure (specific to this application) ----------------------------------------------------*
*** ---> checks if there are "jumps" on the outcome vars from August to Sept births (treat vs control)
*** NOTE: all sample (not only 3 months)

use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear

foreach var of varlist ea ks1 ks2 ks3 ks4 {
	gen `var'hiPGS = `var'* highPGS
	gen `var'loPGS = `var'* (1-highPGS)
}

* Collapse the data
preserve  //before collpasing

collapse (mean)  ea* ks1* ks2* ks3* ks4* ///
	 (sd)    sdea=ea sdks1=ks1 sdks2=ks2 sdks3=ks3 sdks4=ks4 ///
	 (sd)    sdeahiPGS=eahiPGS sdks1hiPGS=ks1hiPGS sdks2hiPGS=ks2hiPGS sdks3hiPGS=ks3hiPGS sdks4hiPGS=ks4hiPGS ///
	 (sd)    sdealoPGS=ealoPGS sdks1loPGS=ks1loPGS sdks2loPGS=ks2loPGS sdks3loPGS=ks3loPGS sdks4loPGS=ks4loPGS ///
	 (count) nea=ea nks1=ks1 nks2=ks2 nks3=ks3 nks4=ks4 ///
	 (count) neahiPGS=eahiPGS nks1hiPGS=ks1hiPGS nks2hiPGS=ks2hiPGS nks3hiPGS=ks3hiPGS nks4hiPGS=ks4hiPGS ///
	 (count) nealoPGS=ealoPGS nks1loPGS=ks1loPGS nks2loPGS=ks2loPGS nks3loPGS=ks3loPGS nks4loPGS=ks4loPGS ///
		 , by(MoB MoBnew)

foreach var of varlist ea* ks1* ks2* ks3* ks4* {
	generate hi`var'  = `var' + invttail(n`var'-1,0.025)*(sd`var' / sqrt(n`var'))
	generate low`var' = `var' - invttail(n`var'-1,0.025)*(sd`var' / sqrt(n`var'))
}


* Combined
twoway  (line ea  MoB, sort) ///
	(line ks1 MoB, sort) ///
	(line ks2 MoB, sort) ///
	(line ks3 MoB, sort) ///
	(line ks4 MoB, sort) ///
	(rcap hiea lowea MoB, color(gs13)) ///
	(rcap hiks1 lowks1 MoB, color(gs13)) ///
	(rcap hiks2 lowks2 MoB, color(gs13)) ///
	(rcap hiks3 lowks3 MoB, color(gs13)) ///
	(rcap hiks4 lowks4 MoB, color(gs13)) ///
	, xlabel(1(1)12, valuelabel) ///
	ytitle("Standardised score") xtitle("Month of birth") ///
	legend(on order(1 "Age 4" 2 "Age 7" 3 "Age 11" 4 "Age 14" 5 "Age 16") pos(6) row(1)) ///
	ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
	title("Test scores by month of birth") //scheme(s1mono)
graph export "${dirfigures}/MoB.png", replace


/*--- One by one (these are not used in the paper)
*create some locals for the titles inside the loop
local yea      = "Entry assessment"
local yks1     = "Key Stage 1"
local yks2     = "Key Stage 2"
local yks3     = "Key Stage 3"
local yks4     = "Key Stage 4"
local titleea  = "Standardised score on Entry Assessment test (age 4)"
local titleks1 = "Standardised score on Key Stage 1 test (age 7)"
local titleks2 = "Standardised score on Key Stage 2 test (age 11)"
local titleks3 = "Standardised score on Key Stage 3 test (age 14)"
local titleks4 = "Standardised score on Key Stage 4 test (age 16)"


foreach test in ea ks1 ks2 ks3 ks4 {
* everyone
twoway (line `test' MoB, sort) ///
	   (rcap hi`test' low`test' MoB) ///
	, xlabel(1(1)12, valuelabel) ///
	ytitle("`y`test''") xtitle("Month of birth") ///
	ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
	title("`title`test''") legend(on order(1 "Mean" 2 "95% CI") pos(6) row(1)) //scheme(s1mono)
graph export "${dirfigures}/MoB_`test'.png", replace

* High PGS vs low PGS
twoway (line `test'hiPGS MoB, sort)            (line `test'loPGS MoB, sort) ///
	   (rcap hi`test'hiPGS low`test'hiPGS MoB) (rcap hi`test'loPGS low`test'loPGS MoB) ///
	, xlabel(1(1)12, valuelabel) ///
	ytitle("`y`test''") xtitle("Month of birth") ///
	ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
	title("`title`test''") legend(on order(1 "High PGS" 2 "Low PGS" 3 "95% CI") pos(6) row(1)) //scheme(s1mono)
graph export "${dirfigures}/MoB_`test'_byPGS.png", replace
}

*/

restore //after collpasing






************* fig:kdens_treat_ea 
*------- Differences in the density of the outcome by treatment ----------------------------------------------------*
*** ---> only 3 months on each side of the cutoff

use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear
keep if window3mth == 1 // keep only 3 months of each side of the cutoff

foreach outcome in ea ks1 ks2 ks3 ks4 {
	local outcomelabel: variable label `outcome'
	local lab0 : label treat 0
	local lab1 : label treat 1
	
	* Kdensity plot of treatment over outcome
	twoway ///
		(kdensity `outcome' if treat ==0) ///, ci
		(kdensity `outcome' if treat ==1) ///, ci
		, legend(label(1 "`lab0'") label(2 "`lab1'") position(6) row(1)) ///
		  xtitle("`outcomelabel'") ytitle("Density")

		graph export "${dirfigures}/kdens_treat_`outcome'.png", replace
}









************* tab:DescrByTreated
*------- Balance of baseline characteristics by treat and control ----------------------------------------------------*
*** ---> only 3 months on each side of the cutoff

use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear
keep if window3mth == 1 // keep only 3 months of each side of the cutoff

local 	xvars "mumagepreg smokepreg m_anxiety m_depression married mumed2 mumed3 mumed4 mumed5 daded2 daded3 daded4 daded5 SC2 SC3 SC4 SC5 SC6 bw PGS PGS_mothers "

qui estpost sum `xvars' if treat==1
qui eststo 	T1
qui estpost sum `xvars' if treat==0
qui eststo 	T0
qui estpost ttest `xvars', by(treat) unequal
qui eststo 	pval

esttab 	T1 T0 pval, replace ///
	nomtitle cells("count mean(fmt(3)) p(fmt(3))") label booktabs nonum collabels(none) f noobs wide nogaps

esttab 	T1 T0 pval using "${dirtables}/DescrByTreated.tex", replace ///
	nomtitle cells("count mean(fmt(3)) p(fmt(3))") label booktabs nonum collabels(none) f noobs wide nogaps

} // end if IDstrategy

***************************************************************************
if ${powercalc}==1{ // power calculations

do ${dirdropbox}/projects/GxE_4practitioners/do/GxE_powerCalc.do

} // end if powercalc

***************************************************************************
if ${rGE}==1{ // Checks if the PGS is correlated with the treatment (it should NOT)


use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear
keep if window3mth == 1 // keep only 3 months of each side of the cutoff

local PGSlabel: variable label PGS
local PGSlabelmother: variable label PGS_mothers

* Kdensity plot of treatment over outcome
local lab0 : label treat 0
local lab1 : label treat 1


************* fig:kdens_treat_PGS
*------- Differences in the density of the PGS by treatment ----------------------------------------------------*
* Children's PGS
twoway ///
    (kdensity PGS if treat ==0) ///, ci
	(kdensity PGS if treat ==1) ///, ci
	, legend(label(1 "`lab0'") label(2 "`lab1'") row(1) position(6)) ///
	  xtitle("`PGSlabel'") ytitle("Density")

	graph export "${dirfigures}/kdens_treat_PGS.png", replace

************* fig:kdens_treat_PGS_mohter
*------- Differences in the density of maternal PGS by treatment ----------------------------------------------------*
* Mothers' PGS
twoway ///
    (kdensity PGS_mothers if treat ==0) ///, ci
	(kdensity PGS_mothers if treat ==1) ///, ci
	, legend(label(1 "`lab0'") label(2 "`lab1'") row(1) position(6)) ///
	  xtitle("`PGSlabelmother'") ytitle("Density")

	graph export "${dirfigures}/kdens_treat_PGS_mothers.png", replace


	
	
	
************* tab:corr_treat_PGS
*------- Check if PGS predicts treatment using same regression as main ----------------------------------------------------*
polychoric treat PGS
polychoric treat PGS_mothers

eststo rGE: reg treat PGS PGS_mother male YoB92 c_PC*, robust cluster(MoB), if window3mth == 1

esttab rGE using "${dirtables}/corr_treat_PGS.tex", replace ///
	frag bookt b(3) se(3) keep(PGS PGS_mothers) star(* 0.10 ** 0.05 *** 0.01) ///
	nomtitles stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes ///
	coeflabel(PGS "Child PGS" PGS_mothers "Mom PGS") 
} // end if rGE

***************************************************************************
if ${predictive}==1{ // Distribution and predictive power of the PGS
use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear
keep if window3mth == 1 // keep only 3 months of each side of the cutoff

************* fig:density_PGS_ea
*------- Predictive power of the PGS ----------------------------------------------------*
** NOTE: trimming the tails to avoid weird non-linear fit

foreach outcome in ea ks1 ks2 ks3 ks4 {
	* Bin-scatter plot of PGS over outcome + density of PGS
	local outcomelabel: variable label `outcome'
	capture drop 	PGS_bin meanP meanD tag
	xtile 	PGS_bin = PGS, nquantiles(200)
	bys 	PGS_bin: egen meanP = mean(PGS)
	bys 	PGS_bin: egen meanD = mean(`outcome')
	egen 	tag = tag(meanP meanD)

	twoway  ///
		(scatter meanD PGS if tag==1) ///
		(kdensity PGS, yaxis(2)) ///
		(lpolyci `outcome' PGS) ///
		if PGS>-3 & PGS<3 , /// XXXX CUTTING OFF THE TAILS!
		legend(off) ///
		xtitle("PGS") ytitle("`outcomelabel'") ytitle("Density", axis(2)) scheme(plotplainblind)

	graph export "${dirfigures}/density_PGS_`outcome'.png", replace
	drop 	PGS_bin meanP meanD tag
} // end of foreach outcome






************* tab:PredictivePower
*------- Predictive power of the PGS in an OLS regression --------------------------------------*
* repeated here from the cleandata section
/* The meta-analysed score 23me_ukb is the one that always has the highest predictive power */
foreach outcome in ea ks1 ks2 ks3 ks4 {
	foreach method in plink ukb 23me 23me_ukb {
		label var pgs_children_`method' "PGS `method'"
		qui eststo `outcome'_`method': reg `outcome' pgs_children_`method' male c_PC*, robust
	} //end foreach method

esttab `outcome'_plink `outcome'_ukb `outcome'_23me `outcome'_23me_ukb, b se keep(pgs_children_*) stats(r2 N)

esttab 	`outcome'_plink `outcome'_ukb `outcome'_23me `outcome'_23me_ukb using "${dirtables}/PredictivePower_`outcome'.tex", replace ///
	frag bookt b(3) se(3) keep(pgs*) star(* 0.10 ** 0.05 *** 0.01) ///
	nomtitles stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes ///
	label
} // end foreach outcome

est drop _all
} // end if predictive

***************************************************************************
if ${funcform}==1{ // Check the function form for the GxE interaction

use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear
keep if window3mth == 1 // keep only 3 months of each side of the cutoff

************* fig:PGSxTreat_ea
*------- Plot to the data to visualize the realationship between G and the outcome for treat and control -----------------------------*
** NOTE: trimming the tails to avoid weird non-linear fit

foreach outcome in ea ks1 ks2 ks3 ks4 {
	local outcomelabel: variable label `outcome'
	local lab0 : label treat 0
	local lab1 : label treat 1

	* Bin-scatter plot of PGS over outcome spearately by treatment ("raw" evidence of GxE)
	capture drop PGS_bin* 
	capture drop meanP* 
	capture drop meanD* 
	capture drop tag*
	* control
	xtile 	PGS_bin0 = PGS, nquantiles(200),        if treat == 0
	bys 	PGS_bin0: egen meanP0 = mean(PGS)       if treat == 0
	bys 	PGS_bin0: egen meanD0 = mean(`outcome') if treat == 0
	egen 	tag0 = tag(meanP0 meanD0)               if treat == 0
	* treatment
	xtile 	PGS_bin1 = PGS, nquantiles(200),        if treat == 1
	bys 	PGS_bin1: egen meanP1 = mean(PGS)       if treat == 1
	bys 	PGS_bin1: egen meanD1 = mean(`outcome') if treat == 1
	egen 	tag1 = tag(meanP1 meanD1)               if treat == 1
	
	* twoway graph
	twoway  ///
		(scatter meanD0 PGS if tag0==1) ///
		(scatter meanD1 PGS if tag1==1) ///
		(lpolyci `outcome' PGS if treat==0) ///
		(lpolyci `outcome' PGS if treat==1) ///
		if PGS>-3 & PGS<3 , /// XXXX CUTTING OFF THE TAILS!
		legend(label(1 "`lab0', bin scatter")  label(2 "`lab1', bin scatter") ///
			   label(4 "`lab0', local polynomial smooth") label(5 "`lab1', local polynomial smooth") /// 
			   label(3 "95% CI") rows(2) position(6) size(vsmall)) ///
		xtitle("PGS") ytitle("`outcomelabel'") scheme(plotplainblind)
		
	graph export "${dirfigures}/PGSxTreat_`outcome'.png", replace
	drop 	PGS_bin* meanP* meanD* tag*
} // end foreach outcome

} // end if funcform

***************************************************************************
if ${regs}==1{ // runs the regressions

use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear
keep if window3mth == 1

** Inputs: PGS, treat, and controls
pwcorr PGS pgs_children*
tab MoB treat
global Xvars       MoBnew male YoB92 c_PC* // input: variables to control for in the regression
global tokeep      treat PGS PGSxtreat treatxMoBnew PGSxMoBnew PGSxtreatxMoBnew  // variables to keep in the output



** This part could be automated
*1----------- CREATE INTERACTION terms between PGS, treat, and the demeaned controls (Keller2014 and Lin2013)
*GxE interaction
qui sum PGS
replace PGS = (PGS-r(mean)) // demean the PGS in the analytical sample (3 month window)
gen PGSxtreat = treat * PGS


*G and E iteracted with controls
global Xpgs        ""
global Xtreat      ""

foreach control of varlist $Xvars {
	qui sum `control'
	gen PGSx`control'   = PGS   * (`control'-r(mean))
	gen treatx`control' = treat * (`control'-r(mean))
	
	//automatically populate the global list of interacted controls
	global Xpgs   ${Xpgs}   PGSx`control'  
	global Xtreat ${Xtreat} treatx`control'
} //end foreach control

*GxExControls ---> only one triple interaction, with the RDD running variable, to keep things simple
qui sum MoBnew
gen PGSxtreatxMoBnew = PGSxtreat * (MoBnew -r(mean))




*2----------- REGRESSION ---------------------------*
foreach outcome in ea ks1 ks2 ks3 ks4 {
	qui eststo `outcome'_treat: reg `outcome' treat PGS                            ${Xvars} ${Xtreat}        , cluster(MoB), if window3mth == 1
	qui eststo `outcome'_gxe:   reg `outcome' treat PGS PGSxtreat PGSxtreatxMoBnew ${Xvars} ${Xtreat} ${Xpgs}, cluster(MoB), if window3mth == 1

	esttab `outcome'_treat `outcome'_gxe, b se keep(${tokeep}) order(${tokeep}) star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))

	esttab `outcome'_treat `outcome'_gxe using "${dirtables}/MoB_`outcome'.tex", replace ///
		frag bookt b(3) se(3) keep(${tokeep}) ///
		order(${tokeep}) star(* 0.10 ** 0.05 *** 0.01) ///
		nomtitles stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes ///
		coeflabel(MoB "MoB" PGSxMoBnew "MoB*PGS" PGSxtreatxMoBnew "MoB*PGS*Treated" PGS "PGS" treat "Treated" treatxMoBnew "Treated*MoB" PGSxtreat "Treated*PGS") 

} // end foreach outcome




*3----------- CLEAN OUTPUT ---------------------------*
** All of the KS key stages in one table
esttab 	ks1_treat ks1_gxe ks2_treat ks2_gxe ks3_treat ks3_gxe ks4_treat ks4_gxe, b se keep(${tokeep}) order(${tokeep}) star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))

esttab 	ks1_treat ks1_gxe ks2_treat ks2_gxe ks3_treat ks3_gxe ks4_treat ks4_gxe using "${dirtables}/MoB_ks.tex", replace ///
		frag bookt b(3) se(3) keep(${tokeep}) ///
		order(${tokeep}) star(* 0.10 ** 0.05 *** 0.01) ///
		nomtitles stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes nonumber ///
		mgroups("Key Stage 1" "Key Stage 2" "Key Stage 3" "Key Stage 4", pattern(1 0 1 0 1 0 1 0) span ///
		prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span})) ///
		coeflabel(MoB "MoB" PGSxMoBnew "MoB*PGS" PGSxtreatxMoBnew "MoB*PGS*Treated" PGS "PGS" treat "Treated" treatxMoBnew "Treated*MoB" PGSxtreat "Treated*PGS") 


** All of the outcomes in one table
esttab 	ea_treat ea_gxe ks1_treat ks1_gxe ks2_treat ks2_gxe ks3_treat ks3_gxe ks4_treat ks4_gxe, b se keep(${tokeep}) order(${tokeep}) star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))

esttab 	ea_treat ea_gxe ks1_treat ks1_gxe ks2_treat ks2_gxe ks3_treat ks3_gxe ks4_treat ks4_gxe using "${dirtables}/MoB_all_outcomes.tex", replace ///
		frag bookt b(3) se(3) keep(${tokeep}) ///
		order(${tokeep}) star(* 0.10 ** 0.05 *** 0.01) ///
		nomtitles stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes nonumber ///
		mgroups("Entry Assessment" "Key Stage 1" "Key Stage 2" "Key Stage 3" "Key Stage 4", pattern(1 0 1 0 1 0 1 0 1 0) span ///
		prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span})) ///
		coeflabel(MoB "MoB" PGSxMoBnew "MoB*PGS" PGSxtreatxMoBnew "MoB*PGS*Treated" PGS "PGS" treat "Treated" treatxMoBnew "Treated*MoB" PGSxtreat "Treated*PGS") 

} // end if regs

***************************************************************************
if ${iv}==1{ // runs the ORIV analysis
global Xvars       MoBnew male YoB92 c_PC* // input: variables to control for in the regression
global tokeep      pgs_ukbxtreat  pgs_23mextreat mainVarint  /// 
                   pgs_ukb  pgs_23me mainVar treat ///
				   pgs_ukbxMoBnew  pgs_ukbxtreatxMoBnew  /// variables to keep in the output
				   pgs_23mexMoBnew pgs_23mextreatxMoBnew ///
                   treatxMoBnew MoBnew PGSxMoBnew PGSxMoBnewxtreat ///



*---------------- Instrumenting the PGS -----------------*
*** ORIV

use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear
keep if window3mth == 1

* demean and rename the two PGS
qui sum pgs_children_ukb  
gen pgs_ukb = pgs_children_ukb -r(mean)
gen pgs_ukbxtreat= treat * pgs_ukb 

qui sum pgs_children_23me  
gen pgs_23me = pgs_children_23me -r(mean)
gen pgs_23mextreat = treat * pgs_23me 

qui sum PGS
replace PGS = PGS - r(mean)
gen PGSxtreat = treat * PGS 

*G and E iteracted with controls
global Xpgs_ukb    ""
global Xpgs_23me   ""
global Xtreat      ""

foreach control of varlist $Xvars {
	qui sum `control'
	gen PGSx`control'      = PGS      * (`control'-r(mean))
	gen pgs_ukbx`control'  = pgs_ukb  * (`control'-r(mean))
	gen pgs_23mex`control' = pgs_23me * (`control'-r(mean))
	gen treatx`control'    = treat    * (`control'-r(mean))
	
	//automatically populate the global list of interacted controls
	global Xpgs_ukb   ${Xpgs_ukb}  pgs_ukbx`control'  
	global Xpgs_23me  ${Xpgs_23me} pgs_23mex`control'  
	global Xtreat     ${Xtreat}    treatx`control'
} //end foreach control

*GxExControls ---> only one triple interaction, with the RDD running variable, to keep things simple
qui sum MoBnew
gen PGSxtreatxMoBnew      = PGSxtreat      * (MoBnew -r(mean))
gen pgs_ukbxtreatxMoBnew  = pgs_ukbxtreat * (MoBnew -r(mean))
gen pgs_23mextreatxMoBnew = pgs_23mextreat * (MoBnew -r(mean))


* Compute correlation between scores to determine the scaling factor
reg pgs_ukb pgs_23me
sca corrscores = _b[pgs_23me]
qui replace pgs_ukb = pgs_ukb/sqrt(corrscores)
qui replace pgs_23me= pgs_23me/sqrt(corrscores)

foreach outcome in ea ks1 ks2 ks3 ks4 {

	local pgs1 = "pgs_ukb"
	local pgs2 = "pgs_23me"

	* Run IV sequentially (without interactions a la Keller)
	eststo PGS_`outcome'a   : reg   `outcome' treat   PGS PGSxtreat                             ${Xvars} ${Xtreat}, cluster (MoB)
	eststo `pgs1'_`outcome'a: ivreg `outcome' treat (`pgs1' `pgs1'xtreat = `pgs2' `pgs2'xtreat) ${Xvars} ${Xtreat}, cluster (MoB)
	eststo `pgs2'_`outcome'a: ivreg `outcome' treat (`pgs2' `pgs2'xtreat = `pgs1' `pgs1'xtreat) ${Xvars} ${Xtreat}, cluster (MoB)

	* Run IV sequentially (with all interactions a la Keller, NOT instrumented)
	eststo PGS_`outcome'b   : reg   `outcome' treat   PGS PGSxtreat PGSxtreatxMoBnew            ${Xvars} ${Xtreat} ${Xpgs}, cluster (MoB)
	eststo `pgs1'_`outcome'b: ivreg `outcome' treat (`pgs1' `pgs1'xtreat = `pgs2' `pgs2'xtreat) ${Xvars} ${Xtreat} ${X`pgs1'}, cluster (MoB)
	eststo `pgs2'_`outcome'b: ivreg `outcome' treat (`pgs2' `pgs2'xtreat = `pgs1' `pgs1'xtreat) ${Xvars} ${Xtreat} ${X`pgs2'}, cluster (MoB)

	* Run IV sequentially (with all interactions a la Keller instrumented)
	eststo `pgs1'_`outcome'c: ivreg `outcome' treat (`pgs1' `pgs1'xtreat ${X`pgs1'} = `pgs2' `pgs2'xtreat ${X`pgs2'} ) ${Xvars} ${Xtreat}, cluster (MoB)
	eststo `pgs2'_`outcome'c: ivreg `outcome' treat (`pgs2' `pgs2'xtreat ${X`pgs2'} = `pgs1' `pgs1'xtreat ${X`pgs1'} ) ${Xvars} ${Xtreat}, cluster (MoB)
	
	esttab PGS_`outcome'a PGS_`outcome'b ///
	       `pgs1'_`outcome'a `pgs1'_`outcome'b `pgs1'_`outcome'c ///
	       `pgs2'_`outcome'a `pgs2'_`outcome'b `pgs2'_`outcome'c ///
		   , keep(PGSxtreat `pgs1'xtreat `pgs2'xtreat)
		   

	
	preserve
		expand 	2, generate(replicant)
		gen 	mainVar = `pgs1' if replicant == 0
		gen 	mainVarint = treat * `pgs1' if replicant==0
		replace mainVar = `pgs2' if replicant == 1
		replace mainVarint = treat * `pgs2' if replicant==1
		gen 	instrument = `pgs2' if replicant == 0
		gen 	instrumentint = treat * `pgs2' if replicant==0
		replace instrument = `pgs1' if replicant == 1
		replace instrumentint = treat * `pgs1' if replicant==1
		
		foreach control of varlist $Xvars{
			qui sum `control'
			gen     PGScombox`control' = `pgs1'*(`control'-r(mean)) if replicant==0
			replace PGScombox`control' = `pgs2'*(`control'-r(mean)) if replicant==1
			gen 	instrPGSx`control' = `pgs2'*(`control'-r(mean)) if replicant==0
			replace instrPGSx`control' = `pgs1'*(`control'-r(mean)) if replicant==1
		} // end foreach control

		gen 	PGScomboxMoBxtreat = PGScomboxMoB*treat
		gen 	instrPGSxMoBxtreat = instrPGSxMoB*treat
		
		* First stage
		reghdfe mainVar instrument treat ${Xvars} ${Xtreat}, absorb(replicant) cluster(cidB2492)
		testparm instrument
		
		* Without interactions a la Keller
		eststo oriv_`outcome'a: ivreghdfe `outcome' (mainVar mainVarint = instrument instrumentint) treat ${Xvars} ${Xtreat}, absorb(replicant, save) cluster(MoB cidB2492)
		sum __hdfe1__			// the intercept
		forvalues x=0/1 {
			qui gen constant`x' = replicant == `x'
		}
		ivreghdfe `outcome' (mainVar mainVarint = instrument instrumentint) treat ${Xvars} ${Xtreat} constant*, cluster(MoB cidB2492) 		// check intercept

		* With interactions a la Keller
		eststo oriv_`outcome'b: ivreghdfe `outcome' (mainVar mainVarint PGScomboxMoBnew PGScomboxMoBxtreat PGScomboxmale PGScomboxYoB92 PGScomboxc_PC* = instrument instrumentint instrPGSxMoBnew instrPGSxMoBxtreat instrPGSxmale instrPGSxYoB92 instrPGSxc_PC*) treat ${Xvars}, absorb(replicant, save) cluster(MoB cidB2492)
		sum __hdfe1__			// the intercept
		ivreghdfe `outcome' (mainVar mainVarint PGScomboxMoBnew PGScomboxMoBxtreat PGScomboxmale PGScomboxYoB92 PGScomboxc_PC* = instrument instrumentint instrPGSxMoBnew instrPGSxMoBxtreat instrPGSxmale instrPGSxYoB92 instrPGSxc_PC*) treat ${Xvars} constant*, cluster(MoB cidB2492) 		// check intercept
	restore

	/*esttab 	ivukb_`outcome' iv23me_`outcome' oriv_`outcome' using "${dirtables}/IV_`outcome'.tex", replace ///
		frag bookt b(3) se(3) keep(treat treat_ukb pgs_ukb treat_23me pgs_23me treat_MoB MoB) ///
		order(treat PGSxtreat treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) star(* 0.10 ** 0.05 *** 0.01) ///
		nomtitles stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes nonumber ///
		mgroups("Key Stage 1" "Key Stage 2" "Key Stage 3" "Key Stage 4", pattern(1 0 1 0 1 0 1 0) span ///
		prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span})) ///
		coeflabel(MoB "MoB" MoB_PGS "MoB*PGS" MoB_PGS_treat "MoB*PGS*Treated" PGS "PGS" treat "Treated" treat_MoB "Treated*MoB" PGSxtreat "Treated*PGS") 
		
	*/
}  // end foreach outcome

esttab 	pgs_ukb_eaa  pgs_ukb_eab  pgs_23me_eaa  pgs_23me_eab  oriv_eaa  oriv_eab , b se keep(${tokeep}) order(${tokeep}) star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))
esttab 	pgs_ukb_ks1a pgs_ukb_ks1b pgs_23me_ks1a pgs_23me_ks1b oriv_ks1a oriv_ks1b, b se keep(${tokeep}) order(${tokeep}) star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))
esttab 	pgs_ukb_ks2a pgs_ukb_ks2b pgs_23me_ks2a pgs_23me_ks2b oriv_ks2a oriv_ks2b, b se keep(${tokeep}) order(${tokeep}) star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))
esttab 	pgs_ukb_ks3a pgs_ukb_ks3b pgs_23me_ks3a pgs_23me_ks3b oriv_ks3a oriv_ks3b, b se keep(${tokeep}) order(${tokeep}) star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))
esttab 	pgs_ukb_ks4a pgs_ukb_ks4b pgs_23me_ks4a pgs_23me_ks4b oriv_ks4a oriv_ks4b, b se keep(${tokeep}) order(${tokeep}) star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))


* output
esttab oriv_eab oriv_ks1b oriv_ks2b oriv_ks3b oriv_ks4b using "${dirtables}/ORIV_all.tex" , replace ///
		frag bookt b(3) se(3) ///
		keep(treat mainVar mainVarint treatxMoBnew PGScomboxMoBnew PGScomboxMoBxtreat MoBnew) ///
		order(treat mainVar mainVarint treatxMoBnew PGScomboxMoBnew PGScomboxMoBxtreat MoBnew) ///
		star(* 0.10 ** 0.05 *** 0.01) ///
		nomtitles stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes nonumber ///
		mgroups("Entry Ass." "Key Stage 1" "Key Stage 2" "Key Stage 3" "Key Stage 4", pattern(1 1 1 1 1)) ///
		coeflabel(MoBnew "MoB" PGScomboxMoBnew "MoB*PGS" PGScomboxMoBxtreat "MoB*PGS*Treated" mainVar "PGS" treat "Treated" treatxMoBnew "Treated*MoB" mainVarint "Treated*PGS") 

} // end if iv

***************************************************************************
if ${perm} == 1 { // Permutation test as robustness checks and inference

set seed 31415

local iter 1000 //change this if you want it to go faster

use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear
keep if window3mth == 1


*----------- CREATE INTERACTION terms between PGS, treat, and the demeaned controls (Keller2014 and Lin2013)
*GxE interaction
qui sum PGS
replace PGS = (PGS-r(mean)) //mean zero in the analytical sample
gen PGSxtreat = treat * PGS

*G and E iteracted with controls
global Xvars       MoBnew male YoB92 c_PC* // input: variables to control for in the regression
global Xpgs        ""
global Xtreat      ""
 
foreach control of varlist $Xvars {
	qui sum `control'
	gen PGSx`control'   = PGS   * (`control'-r(mean))
	gen treatx`control' = treat * (`control'-r(mean))
	
	//automatically populate the global list of interacted controls
	global Xpgs   ${Xpgs}   PGSx`control'  
	global Xtreat ${Xtreat} treatx`control'
} //end foreach control

*GxExControls ---> only one triple interaction, with the RDD running variable, to keep things simple
qui sum MoBnew
gen PGSxtreatxMoBnew = PGSxtreat * (MoBnew -r(mean))







*------ Permutation test for EA
cd ${dirfigures}
capture rm permutation.dta


* true regression
reg ea  i.treat PGS treat#c.PGS   PGSxtreatxMoBnew ${Xvars} ${Xtreat} ${Xpgs}, cluster(MoB), if window3mth == 1
local beta_x  = _b[1.treat#c.PGS]
local tstat_x = _b[1.treat#c.PGS]/_se[1.treat#c.PGS]
di `beta_x'
di `tstat_x'

* permutations
permute PGS treat coef=_b[1.treat#c.PGS] se=_se[1.treat#c.PGS], saving(permutation) reps(`iter'): ///
reg ea  i.treat PGS treat#c.PGS   PGSxtreatxMoBnew ${Xvars} ${Xtreat} ${Xpgs}, cluster(MoB), if window3mth == 1

*--Plot the histogram
//preserve 
	use permutation.dta, replace

	 _pctile coef, nq(1000)
	local qin5 = r(r50)
	local qin95 = r(r950)
	local qin25 = r(r25)
	local qin975 = r(r975)
	display "`qin5'"
	display "the median is " r(r500)

	hist coef, scheme(plotplainblind) xtitle("Coefficient interaction term") legend(off) fc(none) lc(blue) xline(`qin5' `qin95' `qin25' `qin975', lp(dash)) xline(`beta_x', lc(black) )

	graph export "${dirfigures}/permutation_coef_`pgs'.png", replace



	gen tstat = coef/se
	 _pctile tstat, nq(1000)
	local qin5 = r(r50)
	local qin95 = r(r950)
	local qin25 = r(r25)
	local qin975 = r(r975)
	display "`qin5'"

	hist tstat, scheme(plotplainblind) xtitle("T-statistic interaction term") legend(off) fc(none) lc(blue) xline(`qin5' `qin95' `qin25' `qin975', lp(dash)) xline(`tstat_x', lc(black) )

	graph export "${dirfigures}/permutation_tstat_`pgs'.png", replace
//restore

capture rm permutation.dta

} // end if permutation
} // end if analysis


log close

exit
