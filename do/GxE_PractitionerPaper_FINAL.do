***************************************************************************
***** GxE of August vs. September births - ALSPAC. 
***** Stephanie von Hinke and Pietro Biroli, Apr 2020
***************************************************************************
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
global figures        = 1
	global figuresGxE = 0
	global figuresRDD = 0
global regs           = 1
global permutation    = 1


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


* Checking scores - children
insheet using "${dirdropbox}/PGS/ALSPAC/EA/PGS_EA_alspac_ldpred_prior1_ukb_based_children.csv", clear
rename 	fid id_child 
rename 	scoresum pgs_ukb
tempfile temp1
save 	`temp1', replace
insheet using "${dirdropbox}/PGS/ALSPAC/EA/PGS_EA_alspac_ldpred_prior1_23me_based_children.csv", clear
rename 	fid id_child 
rename 	scoresum pgs_23me
tempfile temp2
save 	`temp2', replace
import delimited "${dirdropbox}/PGS/ALSPAC/EA/PGS_EA_alspac_ldpred_prior1_23me_ukb_based_children.profile", delimiter(space, collapse) varnames(1) clear
rename 	fid id_child 
rename 	scoresum pgs_23me_ukb
merge 	1:1 id_child using `temp1'
drop 	_m
merge 	1:1 id_child using `temp2'
drop 	_m
merge 	1:1 id_child using "${dirdata}/Child/PGS_children_alspac_plink_EA.dta"
drop 	_merge
foreach i in pgs_23me_ukb pgs_ukb pgs_23me pgs_child_EA {
	sum 	`i'
	replace `i' = (`i'-r(mean))/r(sd)
}
replace pgs_23me = pgs_23me*-1
corr pgs_child_EA pgs_ukb pgs_23me pgs_23me_ukb 

* Checking scores - mothers
insheet using "${dirdropbox}/PGS/ALSPAC/EA/PGS_EA_alspac_ldpred_prior1_ukb_based_mothers.csv", clear
rename 	fid id_child 
rename 	scoresum pgs_ukb
tempfile temp1
save `temp1', replace
insheet using "${dirdropbox}/PGS/ALSPAC/EA/PGS_EA_alspac_ldpred_prior1_23me_based_mothers.csv", clear
rename 	fid id_child 
rename 	scoresum pgs_23me
tempfile temp2
save `temp2', replace
import delimited "${dirdropbox}/PGS/ALSPAC/EA/PGS_EA_alspac_ldpred_prior1_23me_ukb_based_mothers.profile", delimiter(space, collapse) varnames(1)clear
rename 	fid id_child 
rename 	scoresum pgs_23me_ukb
merge 1:1 id_child using `temp1'
drop _m
merge 1:1 id_child using `temp2'
drop _m
replace pgs_23me = pgs_23me*-1
corr pgs_ukb pgs_23me pgs_23me_ukb 
corr pgs*

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

* Multiply 23 and me PGSs by -1
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
recode 	MoBnew (9=1) (10=2) (11=3) (12=4) (1=5) (2=6) (3=7) (4=8) (5=9) (6=10) (7=11) (8=12)
label 	def MoBnew 1 "Sep" 2 "Oct" 3 "Nov" 4 "Dec" 5 "Jan" 6 "Feb" 7 "Mar" 8 "Apr" 9 "May" 10 "Jun" 11 "Jul" 12 "Aug" 
label 	val MoBnew MoBnew

* "Treated": born in the 6 months after September
gen treat = (MoBnew<=6) if MoBnew<.
label define treat 0 "Born before Sept" 1 "Born after Sept"
label values treat treat

***************************************************************************
keep 	cidB2492 birth_order male ea ks1 ks2 ks3 ks4 IQ YMoB YoB YoB91 YoB92 YoB93 MoB MoBnew pgs_children_ukb pgs_children_23me pgs_children_23me_ukb pgs_mothers_ukb pgs_mothers_23me pgs_mothers_23me_ukb c_PC* m_PC* treat* 
order 	cidB2492 birth_order MoB MoBnew treat YoB YoB91 YoB92 YoB93 YMoB male ea ks* IQ pgs_* c_PC* m_PC* 

* Standardize to have mean 0 and standard deviation 1
foreach i in pgs_children_ukb pgs_children_23me pgs_children_23me_ukb pgs_mothers_ukb pgs_mothers_23me pgs_mothers_23me_ukb {
	qui sum `i'
	replace `i' = (`i'-r(mean))/(r(sd))
	gen high`i' = (`i'>0) if `i'<.
	tab high`i'
}

* Use the PGS with the higher predictive power for EA
foreach i in ea ks1 ks2 ks3 ks4 {
	foreach j in ukb 23me 23me_ukb {
		qui eststo `i'_`j': reg `i' pgs_children_`j' c_PC*, robust
	}
}
esttab ea_ukb ea_23me ea_23me_ukb, b se keep(pgs_children_*) stats(r2 N)
esttab ks1_ukb ks1_23me ks1_23me_ukb, b se keep(pgs_children_*) stats(r2 N)
esttab ks2_ukb ks2_23me ks2_23me_ukb, b se keep(pgs_children_*) stats(r2 N)
esttab ks3_ukb ks3_23me ks3_23me_ukb, b se keep(pgs_children_*) stats(r2 N)
esttab ks4_ukb ks4_23me ks4_23me_ukb, b se keep(pgs_children_*) stats(r2 N)
est drop _all

* Use meta-analysed PGS 
gen PGS = pgs_children_23me_ukb
rename highpgs_children_23me_ukb highPGS

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
	gen window`width'mth = (MoBnew <=`width' | MoBnew>(12-`width')) if MoBnew<.
}

save "${dirdata}/Data_Set/cleanALSPAC4application.dta", replace
} // end if dataclean

*---------------------------------------------------------------------------------*
* FIGURES
*---------------------------------------------------------------------------------*
if ${figures}==1{ // creates some cool figures

***************************************************************************
if ${figuresRDD}==1{ // RDD-style figures

use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear
set scheme plotplainblind


foreach var of varlist ea ks1 ks2 ks3 ks4 {
	gen `var'hiPGS = `var'* highPGS
	gen `var'loPGS = `var'* (1-highPGS)
}

* Collapse the data
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


*--- One by one
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

} // end if RDD-style figures



***************************************************************************
if ${figuresGxE}==1{ // GxE figures: list of figures that *any* GxE paper should have

use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear
keep if window3mth == 1 // keep only 3 months of each side of the cutoff
set scheme plotplainblind // s1rcolor plottig plotplain uncluttered lean2 economist



***********         (1)         ***********
*------- Treatment and outcome: descriptives assuming dichotomous treatment ----------------------------------------------------*
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


***********         (2)         ***********
*------- PGS and outcome: descriptives ----------------------------------------------------*
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
}



***********         (3)         ***********
*------- Treatment and PGS: descriptives assuming dichotomous treatment ----------------------------------------------------*
label var pgs_mothers_23me_ukb "Mother PGS for educational attainment"
local PGSlabel: variable label PGS
local PGSlabelmother: variable label pgs_mothers_23me_ukb

* Kdensity plot of treatment over outcome
local lab0 : label treat 0
local lab1 : label treat 1

* Children's PGS
twoway ///
    (kdensity PGS if treat ==0) ///, ci
	(kdensity PGS if treat ==1) ///, ci
	, legend(label(1 "`lab0'") label(2 "`lab1'") row(1) position(6)) ///
	  xtitle("`PGSlabel'") ytitle("Density")

	graph export "${dirfigures}/kdens_treat_PGS.png", replace

* Mothers' PGS
twoway ///
    (kdensity pgs_mothers_23me_ukb if treat ==0) ///, ci
	(kdensity pgs_mothers_23me_ukb if treat ==1) ///, ci
	, legend(label(1 "`lab0'") label(2 "`lab1'") row(1) position(6)) ///
	  xtitle("`PGSlabelmother'") ytitle("Density")

	graph export "${dirfigures}/kdens_treat_PGS_mothers.png", replace



***********         (4)         ***********
*------- Treatment and other X's: ----------------------------------------------------*
use 	"${dirdata}/Data_Set/Rietveld_12Sep18.dta", clear

* Merge in children's PGSs (UKB and 23me)
egen 	id_child = concat(cidB2492 qlet)
merge 	1:1 id_child using "${dirdata}/Child/PGS_children_23me_ukb.dta"
keep 	if _m==3
drop 	_m

* Merge with mother's PGS (UKB and 23me) and PCs
gen 	tmp = id_child
replace id_child = subinstr(id_child,"A","M",1)
merge 	1:1 id_child using "${dirdata}/Child/PGS_mothers_23me_ukb.dta"
drop 	if _m==2 	// drop those only in using data
replace id_child = tmp
drop 	_merge tmp
rename qlet birth_order

* keep selected variables measured around birth of cohort member
keep 	cidB2492 birth_order kz021 kz030 ka498 a200 a525 b023 b352a b354a c645a c666a c686a c706a c755 pgs_children_23me_ukb pgs_mothers_23me_ukb /*pgs_child_EA */

* Month of birth
gen 	MoB = ka498
recode 	MoB (-9999=.)
label 	def MoB 1 "Jan" 2 "Feb" 3 "Mar" 4 "Apr" 5 "May" 6 "Jun" 7 "Jul" 8 "Aug" 9 "Sep" 10 "Oct" 11 "Nov" 12 "Dec"
label 	val MoB MoB
label 	var MoB "Month of birth"
drop 	ka498

* Gender
gen 	male = kz021
recode 	male (2=0) (-9999 -1 = .)
label 	var male "Male"
drop 	kz021

*   August vs. September births
gen 	MoBnew = MoB
recode 	MoBnew (9=1) (10=2) (11=3) (12=4) (1=5) (2=6) (3=7) (4=8) (5=9) (6=10) (7=11) (8=12)
label 	def MoBnew 1 "Sep" 2 "Oct" 3 "Nov" 4 "Dec" 5 "Jan" 6 "Feb" 7 "Mar" 8 "Apr" 9 "May" 10 "Jun" 11 "Jul" 12 "Aug" 
label 	val MoBnew MoBnew

* "Treated": born in the 6 months after September
gen treat = (MoBnew<=6) if MoBnew<.
label define treat 0 "Born before Sept" 1 "Born after Sept"
label values treat treat


***************************************************************************
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
rename 	pgs_children_23me_ukb PGS
label var PGS "Child PGS for Educational Attainment"
rename 	pgs_mothers_23me_ukb PGS_mothers
label var PGS_mothers "Mother PGS for Educational Attainment"

* Standardize to have mean 0 and standard deviation 1
foreach i in PGS PGS_mothers {
	qui sum `i'
	replace `i' = (`i'-r(mean))/(r(sd))
}

* sample selection based on window of n-months from the cutoff
forvalues width=2/5{
	gen window`width'mth = (MoBnew <=`width' | MoBnew>(12-`width')) if MoBnew<.
}

keep if window3mth == 1 // keep only 3 months of each side of the cutoff

***************************************************************************
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

***************************************************************************


***********         (4)         ***********
*------- PGSxTreat and outcome descriptives ----------------------------------------------------*
use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear
keep if window3mth == 1 // keep only 3 months of each side of the cutoff
set scheme plotplainblind // s1rcolor plottig plotplain uncluttered lean2 economist

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

} // end if GxE figures

} // end if figures


*---------------------------------------------------------------------------------*
* REGRESSIONS
*---------------------------------------------------------------------------------*
if ${regs}==1{ // runs the regressions

** Comparing predictive power from different PGSs
est drop _all
foreach pgs in plink ukb 23me 23me_ukb {
	use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear
	keep if window3mth == 1

	if "`pgs'"=="plink" {
		* To replicate the earlier results using PLINK-based PGS:
		drop if cidB==.
		drop 	PGS 
		merge 	1:1 cidB2492 birth_order using "${dirdata}/Child/PGS_children_alspac_plink_EA.dta"
		drop 	if _m<3
		sum 	pgs_child_EA
		gen 	PGS = (pgs_child_EA -r(mean))/r(sd)
		eststo: reg ks2 PGS c_PC*, robust
	}
	if "`pgs'"=="ukb" {
		drop 	PGS 
		gen 	PGS = pgs_children_ukb
		eststo: reg ks2 PGS c_PC*, robust
	}
	if "`pgs'"=="23me" {
		* Using LDpred-based PGS with 23&me sumstats:
		drop 	PGS 
		gen 	PGS = pgs_children_23me
		eststo: reg ks2 PGS c_PC*, robust
	}
	else if "`pgs'"=="23me_ukb" {
		* Using LDpred-based PGS with 23&me sumstats:
		drop 	PGS 
		gen 	PGS = pgs_children_23me_ukb
		eststo: reg ks2 PGS c_PC*, robust
	}
}
esttab 	, b se keep(PGS) star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes 

esttab 	using "${dirtables}/PredictivePower.tex", replace ///
	frag bookt b(3) se(3) keep(PGS) star(* 0.10 ** 0.05 *** 0.01) ///
	mgroups("PLINK" "UKB" "23\&me", pattern(1 1 1) span ///
	prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span})) ///
	nomtitles stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes ///
	coeflabel(PGS "PGS") 

foreach pgs in plink ukb 23me 23me_ukb {
	use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear
	keep if window3mth == 1

	if "`pgs'"=="plink" {
		* To replicate the earlier results using PLINK-based PGS:
		drop if cidB==.
		drop PGS treat_PGS MoB_PGS MoBnew_PGS MoB_PGS_treat MoBnew_PGS_treat 

		merge 	1:1 cidB2492 birth_order using "${dirdata}/Child/PGS_children_alspac_plink_EA.dta"
		drop 	if _m<3
		drop 	_merge
		sum 	pgs_child_EA
		gen 	PGS = (pgs_child_EA -r(mean))/r(sd)

		* create all of the demeaned interactions
		gen treat_PGS = treat*PGS

		gen MoB_PGS = MoB0*PGS
		gen MoBnew_PGS = MoBnew0*PGS

		gen MoB_PGS_treat = MoB0*PGS*treat
		gen MoBnew_PGS_treat = MoBnew0*PGS*treat
	}
	if "`pgs'"=="ukb" {
		* Using LDpred-based PGS with 23&me sumstats:
		drop PGS treat_PGS MoB_PGS MoBnew_PGS MoB_PGS_treat MoBnew_PGS_treat 
		gen PGS = pgs_children_ukb

		* create all of the demeaned interactions
		gen treat_PGS = treat*PGS

		gen MoB_PGS = MoB0*PGS
		gen MoBnew_PGS = MoBnew0*PGS

		gen MoB_PGS_treat = MoB0*PGS*treat
		gen MoBnew_PGS_treat = MoBnew0*PGS*treat
	}
	if "`pgs'"=="23me" {
		* Using LDpred-based PGS with 23&me sumstats:
		drop PGS treat_PGS MoB_PGS MoBnew_PGS MoB_PGS_treat MoBnew_PGS_treat 
		gen PGS = pgs_children_23me

		* create all of the demeaned interactions
		gen treat_PGS = treat*PGS

		gen MoB_PGS = MoB0*PGS
		gen MoBnew_PGS = MoBnew0*PGS

		gen MoB_PGS_treat = MoB0*PGS*treat
		gen MoBnew_PGS_treat = MoBnew0*PGS*treat
	}
	else if "`pgs'"=="23me_ukb" {
		* Using LDpred-based PGS with 23&me sumstats:
		drop PGS treat_PGS MoB_PGS MoBnew_PGS MoB_PGS_treat MoBnew_PGS_treat 
		gen PGS = pgs_children_23me_ukb

		* create all of the demeaned interactions
		gen treat_PGS = treat*PGS

		gen MoB_PGS = MoB0*PGS
		gen MoBnew_PGS = MoBnew0*PGS

		gen MoB_PGS_treat = MoB0*PGS*treat
		gen MoBnew_PGS_treat = MoBnew0*PGS*treat
	}

	* create the interaction terms between PGS and the controls
	foreach control of varlist male YoB92 c_PC1-c_PC10{
		gen PGSx`control' = `control'*PGS
		gen treatx`control' = `control'*treat
	}


	***************************************************************************
	* Table with only entry assessment
	qui eststo 	m3: reg ea  treat           treat_MoB MoB PGS                       male YoB92 c_PC*                                                    , robust cluster(MoB), if window3mth == 1
	qui eststo 	m4: reg ea  treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male YoB92 c_PC* PGSxmale PGSxYoB92 PGSxc_PC* treatxmale treatxYoB92, robust cluster(MoB), if window3mth == 1

	esttab 	m3 m4, b se keep(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) order(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))

	esttab 	m3 m4 using "${dirtables}/MoB_ea_`pgs'.tex", replace ///
		frag bookt b(3) se(3) keep(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) ///
		order(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) star(* 0.10 ** 0.05 *** 0.01) ///
		nomtitles stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes ///
		coeflabel(MoB "MoB" MoB_PGS "MoB*PGS" MoB_PGS_treat "MoB*PGS*Treated" PGS "PGS" treat "Treated" treat_MoB "Treated*MoB" treat_PGS "Treated*PGS") 

	* Other KS's
	qui eststo 	k1: reg ks1 treat treat_MoB MoB PGS male YoB92 c_PC*, robust cluster(MoB), if window3mth == 1
	qui eststo 	k2: reg ks1 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male YoB92 c_PC* PGSxmale PGSxYoB92 PGSxc_PC* treatxmale treatxYoB92, robust cluster(MoB), if window3mth == 1
	qui eststo 	k3: reg ks2 treat treat_MoB MoB PGS male YoB92 c_PC*, robust cluster(MoB), if window3mth == 1
	qui eststo 	k4: reg ks2 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male YoB92 c_PC* PGSxmale PGSxYoB92 PGSxc_PC* treatxmale treatxYoB92, robust cluster(MoB), if window3mth == 1
	qui eststo 	k5: reg ks3 treat treat_MoB MoB PGS male YoB92 c_PC*, robust cluster(MoB), if window3mth == 1
	qui eststo 	k6: reg ks3 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male YoB92 c_PC* PGSxmale PGSxYoB92 PGSxc_PC* treatxmale treatxYoB92, robust cluster(MoB), if window3mth == 1
	qui eststo 	k7: reg ks4 treat treat_MoB MoB PGS male YoB92 c_PC*, robust cluster(MoB), if window3mth == 1
	qui eststo 	k8: reg ks4 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male YoB92 c_PC* PGSxmale PGSxYoB92 PGSxc_PC* treatxmale treatxYoB92, robust cluster(MoB), if window3mth == 1

	esttab 	k1 k2 k3 k4 k5 k6 k7 k8 , b se keep(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) order(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))

	esttab 	k1 k2 k3 k4 k5 k6 k7 k8 using "${dirtables}/MoB_ks_`pgs'.tex", replace ///
		frag bookt b(3) se(3) keep(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) ///
		order(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) star(* 0.10 ** 0.05 *** 0.01) ///
		nomtitles stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes nonumber ///
		mgroups("Key Stage 1" "Key Stage 2" "Key Stage 3" "Key Stage 4", pattern(1 0 1 0 1 0 1 0) span ///
		prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span})) ///
		coeflabel(MoB "MoB" MoB_PGS "MoB*PGS" MoB_PGS_treat "MoB*PGS*Treated" PGS "PGS" treat "Treated" treat_MoB "Treated*MoB" treat_PGS "Treated*PGS") 
}


*** ORIV
global xvars "MoB treat_MoB male YoB92 c_PC* treatxmale treatxYoB92" 
global order "order(treat treat_pgs_ukb pgs_ukb treat_pgs_23me pgs_23me mainVar mainVarint treat_MoB MoB pgs_ukbxMoB pgs_23mexMoB PGSxMoB MoB_pgs_ukb_treat MoB_pgs_23me_treat PGSxMoBxtreat) "
global keep  "keep(treat treat_pgs_ukb pgs_ukb treat_pgs_23me pgs_23me mainVar mainVarint treat_MoB MoB pgs_ukbxMoB pgs_23mexMoB PGSxMoB MoB_pgs_ukb_treat MoB_pgs_23me_treat PGSxMoBxtreat) "

use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear
keep if window3mth == 1

* rename variables
rename 	pgs_children_ukb  pgs_ukb 
rename 	pgs_children_23me pgs_23me

* Create interactions
gen treat_pgs_ukb=treat*pgs_ukb
gen treat_pgs_23me=treat*pgs_23me

foreach control of varlist male YoB92 c_PC1-c_PC10 MoB {
	gen treatx`control' = `control'*treat
	gen pgs_ukbx`control' = `control'*pgs_ukb
	gen pgs_23mex`control' = `control'*pgs_23me
}

gen MoB_pgs_ukb_treat  = MoB*treat_pgs_ukb
gen MoB_pgs_23me_treat = MoB*treat_pgs_23me

* Compute correlation between scores to determine the scaling factor
reg pgs_ukb pgs_23me
sca corrscores = _b[pgs_23me]
qui replace pgs_ukb = pgs_ukb/sqrt(corrscores)
qui replace pgs_23me= pgs_23me/sqrt(corrscores)

foreach outcome in ea ks1 ks2 ks3 ks4 {

	local pgs1 = "pgs_ukb"
	local pgs2 = "pgs_23me"

	* Run IV sequentially (without interactions a la Keller)
	eststo `pgs1'_`outcome'a: ivreg `outcome' treat (`pgs1' treat_`pgs1' = `pgs2' treat_`pgs2') $xvars, cluster (MoB)
	eststo `pgs2'_`outcome'a: ivreg `outcome' treat (`pgs2' treat_`pgs2' = `pgs1' treat_`pgs1') $xvars, cluster (MoB)

	* Run IV sequentially (with all interactions a la Keller instrumented)
	eststo `pgs1'_`outcome'b: ivreg `outcome' treat (`pgs1' treat_`pgs1' `pgs1'xMoB MoB_`pgs1'_treat `pgs1'xmale `pgs1'xYoB92 `pgs1'xc_PC* = `pgs2' treat_`pgs2' `pgs2'xMoB MoB_`pgs2'_treat `pgs2'xmale `pgs2'xYoB92 `pgs2'xc_PC*) $xvars, cluster (MoB)
	eststo `pgs2'_`outcome'b: ivreg `outcome' treat (`pgs2' treat_`pgs2' `pgs2'xMoB MoB_`pgs2'_treat `pgs2'xmale `pgs2'xYoB92 `pgs2'xc_PC* = `pgs1' treat_`pgs1' `pgs1'xMoB MoB_`pgs1'_treat `pgs1'xmale `pgs1'xYoB92 `pgs1'xc_PC*) $xvars, cluster (MoB)

	preserve							// STILL NEED TO ADD INTERACTIONS A LA KELLER - IT MAKES A BIG DIFFERENCE IN STANDARD IV ABOVE
		expand 	2, generate(replicant)
		gen 	mainVar = `pgs1' if replicant == 0
		gen 	mainVarint = treat * `pgs1' if replicant==0
		replace mainVar = `pgs2' if replicant == 1
		replace mainVarint = treat * `pgs2' if replicant==1
		gen 	instrument = `pgs2' if replicant == 0
		gen 	instrumentint = treat * `pgs2' if replicant==0
		replace instrument = `pgs1' if replicant == 1
		replace instrumentint = treat * `pgs1' if replicant==1
		gen 	PGSxMoB = `pgs1'*MoB if replicant==0
		replace PGSxMoB = `pgs2'*MoB if replicant==1
		gen 	instrPGSxMoB = `pgs2'*MoB if replicant==0
		replace instrPGSxMoB = `pgs1'*MoB if replicant==1
		gen 	PGSxMoBxtreat = PGSxMoB*treat
		gen 	instrPGSxMoBxtreat = instrPGSxMoB*treat
		gen 	PGSxmale = `pgs1'*male if replicant==0
		replace PGSxmale = `pgs2'*male if replicant==1
		gen 	instrPGSxmale = `pgs2'*male if replicant==0
		replace instrPGSxmale = `pgs1'*male if replicant==1
		gen 	PGSxYoB92 = `pgs1'*YoB92 if replicant==0
		replace PGSxYoB92 = `pgs2'*YoB92 if replicant==1
		gen 	instrPGSxYoB92 = `pgs2'*YoB92 if replicant==0
		replace instrPGSxYoB92 = `pgs1'*YoB92 if replicant==1
		forval i=1(1)10 {
			gen 	PGSxPC`i' = `pgs1'*c_PC`i' if replicant==0
			replace PGSxPC`i' = `pgs2'*c_PC`i' if replicant==1
			gen 	instrPGSxPC`i' = `pgs2'*c_PC`i' if replicant==0
			replace instrPGSxPC`i' = `pgs1'*c_PC`i' if replicant==1
		}
		
		* First stage
		reghdfe mainVar instrument $xvars, absorb(replicant) cluster(cidB2492)
		testparm instrument
		
		* Without interactions a la Keller
		eststo oriv_`outcome'a: ivreghdfe `outcome' (mainVar mainVarint = instrument instrumentint) treat $xvars, absorb(replicant, save) cluster(MoB cidB2492)
		sum __hdfe1__			// the intercept
		forvalues x=0/1 {
			qui gen constant`x' = replicant == `x'
		}
		ivreghdfe `outcome' (mainVar mainVarint = instrument instrumentint) treat $xvars constant*, cluster(MoB cidB2492) 		// check intercept

		* Withinteractions a la Keller
		eststo oriv_`outcome'b: ivreghdfe `outcome' (mainVar mainVarint PGSxMoB PGSxMoBxtreat PGSxmale PGSxYoB92 PGSxPC* = instrument instrumentint instrPGSxMoB instrPGSxMoBxtreat instrPGSxmale instrPGSxYoB92 instrPGSxPC*) treat $xvars, absorb(replicant, save) cluster(MoB cidB2492)
		sum __hdfe1__			// the intercept
		ivreghdfe `outcome' (mainVar mainVarint PGSxMoB PGSxMoBxtreat PGSxmale PGSxYoB92 PGSxPC* = instrument instrumentint instrPGSxMoB instrPGSxMoBxtreat instrPGSxmale instrPGSxYoB92 instrPGSxPC*) treat $xvars constant*, cluster(MoB cidB2492) 		// check intercept
	restore

	*esttab 	ivukb_`outcome' iv23me_`outcome' oriv_`outcome' using "${dirtables}/IV_`outcome'.tex", replace ///
		frag bookt b(3) se(3) keep(treat treat_ukb pgs_ukb treat_23me pgs_23me treat_MoB MoB) ///
		order(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) star(* 0.10 ** 0.05 *** 0.01) ///
		nomtitles stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes nonumber ///
		mgroups("Key Stage 1" "Key Stage 2" "Key Stage 3" "Key Stage 4", pattern(1 0 1 0 1 0 1 0) span ///
		prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span})) ///
		coeflabel(MoB "MoB" MoB_PGS "MoB*PGS" MoB_PGS_treat "MoB*PGS*Treated" PGS "PGS" treat "Treated" treat_MoB "Treated*MoB" treat_PGS "Treated*PGS") 
}

esttab 	pgs_ukb_eaa  pgs_ukb_eab  pgs_23me_eaa  pgs_23me_eab  oriv_eaa  oriv_eab , b se $keep $order star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))
esttab 	pgs_ukb_ks1a pgs_ukb_ks1b pgs_23me_ks1a pgs_23me_ks1b oriv_ks1a oriv_ks1b, b se $keep $order star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))
esttab 	pgs_ukb_ks2a pgs_ukb_ks2b pgs_23me_ks2a pgs_23me_ks2b oriv_ks2a oriv_ks2b, b se $keep $order star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))
esttab 	pgs_ukb_ks3a pgs_ukb_ks3b pgs_23me_ks3a pgs_23me_ks3b oriv_ks3a oriv_ks3b, b se $keep $order star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))
esttab 	pgs_ukb_ks4a pgs_ukb_ks4b pgs_23me_ks4a pgs_23me_ks4b oriv_ks4a oriv_ks4b, b se $keep $order star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))




***************************************************************************
* Continuous MoB
eststo 	m1: reg ea  treat treat_MoB MoB PGS male YoB92 c_PC*, robust cluster(MoB)
eststo 	m2: reg ks1 treat treat_MoB MoB PGS male YoB92 c_PC*, robust cluster(MoB)
eststo 	m3: reg ks2 treat treat_MoB MoB PGS male YoB92 c_PC*, robust cluster(MoB)
eststo 	m4: reg ks3 treat treat_MoB MoB PGS male YoB92 c_PC*, robust cluster(MoB)
eststo 	m5: reg ks4 treat treat_MoB MoB PGS male YoB92 c_PC*, robust cluster(MoB) 

esttab 	m1 m2 m3 m4 m5, b se keep(treat treat_MoB MoB PGS) star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(0))

esttab 	m1 m2 m3 m4 m5 using "${dirtables}/MoB_OLS2.tex", replace ///
	frag bookt b(3) se(3) keep(treat treat_MoB MoB PGS) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Age 4" "Age 7" "Age 11" "Age 14" "Age 16") ///
	stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes coeflabel(MoB "MoB" PGS "PGS" treat "Treated" treat_MoB "Treated*MoB") 

set scheme plotplainblind

***************************************************************************
* Continuous MoB
eststo 	m1: reg ea  treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male YoB92 c_PC* PGSxmale PGSxYoB92 PGSxc_PC* treatxmale treatxYoB92, robust cluster(MoB)
eststo 	m2: reg ks1 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male YoB92 c_PC* PGSxmale PGSxYoB92 PGSxc_PC* treatxmale treatxYoB92, robust cluster(MoB)
eststo 	m3: reg ks2 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male YoB92 c_PC* PGSxmale PGSxYoB92 PGSxc_PC* treatxmale treatxYoB92, robust cluster(MoB)
eststo 	m4: reg ks3 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male YoB92 c_PC* PGSxmale PGSxYoB92 PGSxc_PC* treatxmale treatxYoB92, robust cluster(MoB)
eststo 	m5: reg ks4 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male YoB92 c_PC* PGSxmale PGSxYoB92 PGSxc_PC* treatxmale treatxYoB92, robust cluster(MoB) 

esttab 	m1 m2 m3 m4 m5, b se keep(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(0))

esttab 	m1 m2 m3 m4 m5 using "${dirtables}/MoB_GxE2.tex", replace ///
	frag bookt b(3) se(3) keep(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Age 4" "Age 7" "Age 11" "Age 14" "Age 16") ///
	stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes coeflabel(MoB "MoB" MoB_PGS "MoB*PGS" MoB_PGS_treat "MoB*PGS*Treated" PGS "PGS" treat "Treated" treat_MoB "Treated*MoB" treat_PGS "Treated*PGS") 

*------ Excluding the observations "far" from the cutoff
forvalues width=2/5{

	* Continuous MoB
	eststo 	m1: reg ea  treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male YoB92 c_PC* PGSxmale PGSxYoB92 PGSxc_PC* treatxmale treatxYoB92, robust cluster(MoB), if window`width'mth == 1
	eststo 	m2: reg ks1 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male YoB92 c_PC* PGSxmale PGSxYoB92 PGSxc_PC* treatxmale treatxYoB92, robust cluster(MoB), if window`width'mth == 1
	eststo 	m3: reg ks2 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male YoB92 c_PC* PGSxmale PGSxYoB92 PGSxc_PC* treatxmale treatxYoB92, robust cluster(MoB), if window`width'mth == 1
	eststo 	m4: reg ks3 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male YoB92 c_PC* PGSxmale PGSxYoB92 PGSxc_PC* treatxmale treatxYoB92, robust cluster(MoB), if window`width'mth == 1
	eststo 	m5: reg ks4 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male YoB92 c_PC* PGSxmale PGSxYoB92 PGSxc_PC* treatxmale treatxYoB92, robust cluster(MoB), if window`width'mth == 1

	esttab 	m1 m2 m3 m4 m5, b se keep(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(0))

	esttab 	m1 m2 m3 m4 m5 using "${dirtables}/MoB_GxE2_window`width'mth.tex", replace ///
		frag bookt b(3) se(3) keep(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) star(* 0.10 ** 0.05 *** 0.01) ///
		mtitles("Age 4" "Age 7" "Age 11" "Age 14" "Age 16") ///
		stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes coeflabel(MoB "MoB" MoB_PGS "MoB*PGS" MoB_PGS_treat "MoB*PGS*Treated" PGS "PGS" treat "Treated" treat_MoB "Treated*MoB" treat_PGS "Treated*PGS") 
	} // end forvalues width

	* RDrobust
	foreach test in ea ks1 ks2 ks3 ks4{
		rdrobust `test' MoB if highPGS==1, c(9) covs(PGS male c_PC*)
		rdrobust `test' MoB if highPGS==0, c(9) covs(PGS male c_PC*)
	}

} // end if regs




if ${permutation} == 1 {

set seed 31415
foreach pgs in plink ukb 23me 23me_ukb {
	use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear

	if "`pgs'"=="plink" {
		* To replicate the earlier results using PLINK-based PGS:
		drop if cidB==.
		drop PGS treat_PGS MoB_PGS MoBnew_PGS MoB_PGS_treat MoBnew_PGS_treat 

		merge 	1:1 cidB2492 birth_order using "${dirdata}/Child/PGS_children_alspac_plink_EA.dta"
		drop 	if _m<3
		drop 	_merge
		sum 	pgs_child_EA
		gen 	PGS = (pgs_child_EA -r(mean))/r(sd)

		* create all of the demeaned interactions
		gen treat_PGS = treat*PGS

		gen MoB_PGS = MoB0*PGS
		gen MoBnew_PGS = MoBnew0*PGS

		gen MoB_PGS_treat = MoB0*PGS*treat
		gen MoBnew_PGS_treat = MoBnew0*PGS*treat
	}
	if "`pgs'"=="ukb" {
		* Using LDpred-based PGS with 23&me sumstats:
		drop PGS treat_PGS MoB_PGS MoBnew_PGS MoB_PGS_treat MoBnew_PGS_treat 
		gen PGS = pgs_children_ukb

		* create all of the demeaned interactions
		gen treat_PGS = treat*PGS

		gen MoB_PGS = MoB0*PGS
		gen MoBnew_PGS = MoBnew0*PGS

		gen MoB_PGS_treat = MoB0*PGS*treat
		gen MoBnew_PGS_treat = MoBnew0*PGS*treat
	}
	if "`pgs'"=="23me" {
		* Using LDpred-based PGS with 23&me sumstats:
		drop PGS treat_PGS MoB_PGS MoBnew_PGS MoB_PGS_treat MoBnew_PGS_treat 
		gen PGS = pgs_children_23me

		* create all of the demeaned interactions
		gen treat_PGS = treat*PGS

		gen MoB_PGS = MoB0*PGS
		gen MoBnew_PGS = MoBnew0*PGS

		gen MoB_PGS_treat = MoB0*PGS*treat
		gen MoBnew_PGS_treat = MoBnew0*PGS*treat
	}
	else if "`pgs'"=="23me_ukb" {
		* Using LDpred-based PGS with 23&me sumstats:
		drop PGS treat_PGS MoB_PGS MoBnew_PGS MoB_PGS_treat MoBnew_PGS_treat 
		gen PGS = pgs_children_23me_ukb

		* create all of the demeaned interactions
		gen treat_PGS = treat*PGS

		gen MoB_PGS = MoB0*PGS
		gen MoBnew_PGS = MoBnew0*PGS

		gen MoB_PGS_treat = MoB0*PGS*treat
		gen MoBnew_PGS_treat = MoBnew0*PGS*treat
	}
	
	* create the interaction terms between PGS and the controls
	foreach control of varlist male YoB92 c_PC1-c_PC10{
		gen PGSx`control' = `control'*PGS
		gen treatx`control' = `control'*treat
	}

	*------ Permutation test for EA
	cd ${dirfigures}
	capture rm permutation.dta

	global controlsX       treat_MoB MoB_PGS MoB_PGS_treat MoB male YoB92 c_PC* PGSxmale PGSxYoB92 PGSxc_PC* treatxmale treatxYoB92

local pgs 23me_ukb
	* true regressions
	reg ea i.treat PGS treat#c.PGS $controlsX , cluster(MoB), if window3mth == 1
	local beta_x  = _b[1.treat#c.PGS]
	local tstat_x = _b[1.treat#c.PGS]/_se[1.treat#c.PGS]
	di `beta_x'
	di `tstat_x'

	* permutations
	local iter 1000
	permute PGS treat coef=_b[1.treat#c.PGS] se=_se[1.treat#c.PGS], saving(permutation) reps(`iter'): ///
	reg ea i.treat PGS treat#c.PGS $controlsX , cluster(MoB), if window3mth == 1


	*--Plot the histogram
	preserve 
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
	restore
}

*--
} // end if permutation




log close











exit


/***************************************************************************
***** Identifying week of birth from data on 
* Date questionnaire completed
* Age in weeks when questionnaire completed
* Month of birth
***************************************************************************
use "\\rdsfcifs.acrc.bris.ac.uk\RDSF_EFIM\shared\cohgroup\alspacrawdata\alspacchild_1.dta", clear
keep 	cid084a qlet ka320 ka321 ka322 ka325 ka326 
rename 	ka320 Q_day
rename 	ka321 Q_month
rename 	ka322 Q_year
rename 	ka325 Q_age
rename 	ka326 MoB

replace Q_year = Q_year + 1900
gen 	date = mdy(Q_month,Q_day,Q_year)
gen 	WoB = date - (Q_age*7)
format 	date WoB %td

* Check mismatches
gen 	month = month(WoB)
tab 	month MoB

* Few mismatches - ensure WoB takes same value as MoB (former is derived by us from data available; latter is provided by ALSPAC)
replace WoB = WoB-6 if month-MoB==1 | (month==1 & MoB==12)

* Check mismatches again
drop 	month
gen 	month = month(WoB)
assert 	month == MoB if month<. & MoB<.
drop 	month

* Create weeks with week 1 being the w/c 1 September 
gen 	week = 0 if (WoB>=td("01sep1991") & WoB<=td("07sep1991")) | (WoB>=td("01sep1992") & WoB<=td("07sep1992"))

* 1/9/1991 = 11566; 1/9/1992 = 11932
local wkstart90 = 11208 	//  8-9-1990
local wkend90   = 11214		// 14-9-1990
local wkstart91 = 11573 	//  8-9-1991
local wkend91   = 11579		// 14-9-1991
local wkstart92 = 11939 	//  8-9-1992
local wkend92   = 11945		// 14-9-1992
local wk        = 1
while `wk'<=51 {
	replace week    = `wk' if (WoB>=`wkstart90' & WoB<=`wkend90') | (WoB>=`wkstart91' & WoB<=`wkend91') | (WoB>=`wkstart92' & WoB<=`wkend92')
	local wkstart90 = `wkstart90'+7
	local wkend90   = `wkend90'+7
	local wkstart91 = `wkstart91'+7
	local wkend91   = `wkend91'+7
	local wkstart92 = `wkstart92'+7
	local wkend92   = `wkend92'+7
	local wk        = `wk'+1
}

* Add last 1-2 days of the 'year' (i.e. 30/31 Aug) to week 52 
replace week = 51 if WoB==td(30aug1991) | WoB==td(31aug1991) | WoB==td(30aug1992) | WoB==td(31aug1992)


* Check graphs of educational attainment by week
merge 	1:1 cid084a qlet using "C:\Users\ecsmvhkv\OneDrive - University of Bristol\MyFiles-Migrated\Projects\PrenatalMentalHealth\Stata\data02.dta", keepusing(ea_avg k1avg_alt k2avg_alt k3avg_alt k4avg_alt)
drop 	if _m<3
drop 	_m


preserve

	* Collapse the data
	collapse ea_avg k1avg_alt k2avg_alt k3avg_alt k4avg_alt, by(week)

	twoway  (line ea_avg    week, sort) ///
		(line k1avg_alt week, sort) ///
		(line k2avg_alt week, sort) ///
		(line k3avg_alt week, sort) ///
		(line k4avg_alt week, sort) ///
		if ea_avg>-0.5 & k1avg_alt>-0.5 & k2avg_alt>-0.5 & k3avg_alt>-0.5 & k4avg_alt>-0.5 ///
		, xlabel(0(10)51, valuelabel) ///
		ytitle("Standardised score") xtitle("Week of birth (relative to September)") ///
		legend(on order(1 "Age 4" 2 "Age 7" 3 "Age 11" 4 "Age 14" 5 "Age 16") pos(6) row(1)) ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Test scores by week of birth") scheme(s1mono)
	graph export "${dirfigures}/WoB.png", replace

	twoway (line ea_avg week, sort), xlabel(0(10)50, valuelabel) ///
		ytitle("Entry assessment") xtitle("Week of birth") ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Score on Entry Assessment test (age 4/5)") scheme(s1mono)
	graph export "${dirfigures}/WoB_ea.png", replace
	
	twoway (line k1avg_alt week, sort) if k1avg_alt>-0.5, xlabel(0(10)50, valuelabel) ///
		ytitle("Key Stage 1") xtitle("Week of birth") ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Score on Key Stage 1 test (age 7)") scheme(s1mono)
	graph export "${dirfigures}/WoB_ks1.png", replace

	twoway (line k2avg_alt week, sort) if k2avg_alt>-0.5, xlabel(0(10)50, valuelabel) ///
		ytitle("Key Stage 2") xtitle("Week of birth") ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Score on Key Stage 2 test (age 11)") scheme(s1mono)
	graph export "${dirfigures}/WoB_ks2.png", replace

	twoway (line k3avg_alt week, sort) if k3avg_alt>-0.5, xlabel(0(10)50, valuelabel) ///
		ytitle("Key Stage 3") xtitle("Week of birth") ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Score on Key Stage 3 test (age 14)") scheme(s1mono)
	graph export "${dirfigures}/WoB_ks3.png", replace

	twoway (line k4avg_alt week, sort) if k4avg_alt>-0.5, xlabel(0(10)50, valuelabel) ///
		ytitle("Key Stage 4") xtitle("Week of birth") ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Score on Key Stage 4 test (age 16)") scheme(s1mono)
	graph export "${dirfigures}/WoB_ks4.png", replace
	

restore

preserve

	gen 	wk = week-40 if week>=39 & week<.
	replace wk = week+12 if week<=12
	
	* Collapse the data
	collapse ea_avg k1avg_alt k2avg_alt k3avg_alt k4avg_alt, by(wk)

	twoway  (line ea_avg    wk, sort) ///
		(line k1avg_alt wk, sort) ///
		(line k2avg_alt wk, sort) ///
		(line k3avg_alt wk, sort) ///
		(line k4avg_alt wk, sort) ///
		if ea_avg>-0.5 & k1avg_alt>-0.5 & k2avg_alt>-0.5 & k3avg_alt>-0.5 & k4avg_alt>-0.5 ///
		, xlabel(0(4)24, valuelabel) ///
		ytitle("Standardised score") xtitle("Week of birth (September = week 12)") ///
		legend(on order(1 "Age 4" 2 "Age 7" 3 "Age 11" 4 "Age 14" 5 "Age 16") pos(6) row(1)) ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Test scores by week of birth") scheme(s1mono)
	graph export "${dirfigures}/WoB.png", replace

	twoway (line ea_avg wk, sort), xlabel(0(4)24, valuelabel) ///
		ytitle("Entry assessment") xtitle("Week of birth") ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Score on Entry Assessment test (age 4/5)") scheme(s1mono)
	graph export "${dirfigures}/WoB_ea.png", replace
	
	twoway (line k1avg_alt wk, sort) if k1avg_alt>-0.5, xlabel(0(4)24, valuelabel) ///
		ytitle("Key Stage 1") xtitle("Week of birth") ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Score on Key Stage 1 test (age 7)") scheme(s1mono)
	graph export "${dirfigures}/WoB_ks1.png", replace

	twoway (line k2avg_alt wk, sort) if k2avg_alt>-0.5, xlabel(0(4)24, valuelabel) ///
		ytitle("Key Stage 2") xtitle("Week of birth") ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Score on Key Stage 2 test (age 11)") scheme(s1mono)
	graph export "${dirfigures}/WoB_ks2.png", replace

	twoway (line k3avg_alt wk, sort) if k3avg_alt>-0.5, xlabel(0(4)24, valuelabel) ///
		ytitle("Key Stage 3") xtitle("Week of birth") ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Score on Key Stage 3 test (age 14)") scheme(s1mono)
	graph export "${dirfigures}/WoB_ks3.png", replace

	twoway (line k4avg_alt wk, sort) if k4avg_alt>-0.5, xlabel(0(4)24, valuelabel) ///
		ytitle("Key Stage 4") xtitle("Week of birth") ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Score on Key Stage 4 test (age 16)") scheme(s1mono)
	graph export "${dirfigures}/WoB_ks4.png", replace
	

restore



hist coef, addplot(pci 0 0.16 4 0.16) xtitle("Coefficient interaction term") legend(off)
hist tstat, addplot(pci 0 2 0.4 2) xtitle("t-statistic interaction term") legend(off)
