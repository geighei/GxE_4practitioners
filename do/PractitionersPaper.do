***************************************************************************
***** Check August vs. September births - ALSPAC. 
***** Stephanie von Hinke, Feb 2020
***************************************************************************
set more off
clear all
capture log close


if "`c(username)'" == "pbirol" {
	global dir "C:/Users/pbirol/Documents/work/ALSPAC_GxE/Alspac"
}

else{
	global dir "\\rdsfcifs.acrc.bris.ac.uk/ALSPAC_GxE/Alspac"
}

global dirtables  "${dir}/tables"
global dirfigures "${dir}/figures"


cd "${dir}"

	* SET UP LOG FILE
local date: display %td_CCYY-NN-DD date(c(current_date), "DMY")
local datetime = subinstr("`date'"," ","",.)+"_" +subinstr(c(current_time), ":", "-", .)
di "`datetime'"
log using "${dir}/logfiles/PractictionersPaper_`datetime'.smcl", replace

*** On ALSPAC data held at Erasmus
use 	"${dir}/Data_Set/Rietveld_12Sep18.dta", clear


* Month of birth
gen 	MoB = ka498
recode 	MoB (-9999=.)
label 	def MoB 1 "Jan" 2 "Feb" 3 "Mar" 4 "Apr" 5 "May" 6 "Jun" 7 "Jul" 8 "Aug" 9 "Sep" 10 "Oct" 11 "Nov" 12 "Dec"
label 	val MoB MoB
label 	var MoB "Month of birth"


* Gender
gen 	male = kz021
recode 	male (2=0) (-9999 -1 = .)
label 	var male "Male"


* Entry assessment score
recode 	sat092b (-10 -6 -5 = .)
sum 	sat092b 
gen 	ea = (sat092b - r(mean)) / r(sd)
label 	var ea "Entry Assessment score"


* Key stage 1 
recode 	sat190a (-10 -6 -5 = .)
sum 	sat190a
gen 	ks1 = (sat190a - r(mean)) / r(sd)
label 	var ks1 "Key stage 1 summary score"


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
label 	var ks2 "Key stage 2 summary score"
drop 	k2e k2m k2s k2


* Key stage 3 			// We're missing the science score
foreach i in k3_tote k3_totm {
	replace `i' = " " if inlist(`i',"-10","A","IN","M","V")
}
destring k3_tote, gen(k3e)
destring k3_totm, gen(k3m)
egen 	k3 = rsum(k3e k3m)
sum 	k3 if k3e<. & k3m<. 
gen 	ks3 = (k3 - r(mean)) / r(sd) if k3e<. & k3m<.
label 	var ks3 "Key stage 3 summary score"
drop 	k3e k3m k3 


* Key stage 4
replace ks4_ptstnewe = . if inlist(`i',-10,-1,0)
sum 	ks4_ptstnewe 
gen 	ks4 = (ks4_ptstnewe - r(mean)) / r(sd)
label 	var ks4 "Key stage 4 summary score"


* IQ (WISC)
recode 	f8ws112 (-9999 -3 -2 = .)
sum 	f8ws112 
gen 	IQ = (f8ws112 - r(mean)) / r(sd)
label 	var IQ "IQ (WISC) score, F8"


*   August vs. September births
gen 	MoBnew = MoB
recode 	MoBnew (9=1) (10=2) (11=3) (12=4) (1=5) (2=6) (3=7) (4=8) (5=9) (6=10) (7=11) (8=12)
label 	def MoBnew 1 "Sep" 2 "Oct" 3 "Nov" 4 "Dec" 5 "Jan" 6 "Feb" 7 "Mar" 8 "Apr" 9 "May" 10 "Jun" 11 "Jul" 12 "Aug" 
label 	val MoBnew MoBnew



***************************************************************************
keep 	cidB2492 qlet male ea ks1 ks2 ks3 ks4 IQ MoB MoBnew

preserve

	* Collapse the data
	collapse ea ks1 ks2 ks3 ks4, by(MoB)

	twoway  (line ea    MoB, sort) ///
		(line ks1 MoB, sort) ///
		(line ks2 MoB, sort) ///
		(line ks3 MoB, sort) ///
		(line ks4 MoB, sort) ///
		, xlabel(1(1)12, valuelabel) ///
		ytitle("Standardised score") xtitle("Month of birth") ///
		legend(on order(1 "Age 4" 2 "Age 7" 3 "Age 11" 4 "Age 14" 5 "Age 16") pos(6) row(1)) ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Test scores by month of birth") scheme(s1mono)
	graph export "${dirfigures}/MoB.png", replace

	twoway (line ea MoB, sort), xlabel(1(1)12, valuelabel) ///
		ytitle("Entry assessment") xtitle("Month of birth") ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Score on Entry Assessment test (age 4/5)") scheme(s1mono)
	graph export "${dirfigures}/MoB_EA.png", replace

	twoway (line ks1 MoB, sort) if ks1>-0.5, xlabel(1(1)12, valuelabel) ///
		ytitle("Key Stage 1") xtitle("Month of birth") ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Score on Key Stage 1 test (age 7)") scheme(s1mono)
	graph export "${dirfigures}/MoB_KS1.png", replace

	twoway (line ks2 MoB, sort) if ks2>-0.5, xlabel(1(1)12, valuelabel) ///
		ytitle("Neuroticism score") xtitle("Month of birth") ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Score on Key Stage 2 test (age 11)") scheme(s1mono)
	graph export "${dirfigures}/MoB_KS2.png", replace

	twoway (line ks3 MoB, sort) if ks3>-0.5, xlabel(1(1)12, valuelabel) ///
		ytitle("Digits remembered correctly") xtitle("Month of birth") ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Score on Key Stage 3 test (age 14)") scheme(s1mono)
	graph export "${dirfigures}/MoB_KS3.png", replace

	twoway (line ks4 MoB, sort) if ks4>-0.5, xlabel(1(1)12, valuelabel) ///
		ytitle("Prospective memory result") xtitle("Month of birth") ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Score on Key Stage 4 test (age 16)") scheme(s1mono)
	graph export "${dirfigures}/MoB_KS4.png", replace

restore

* Some simple regressions  	// currently missing cohort members' (continuous) age when they sat the exam, need to request 
*foreach i in ea age_ks1 age_ks2 age_ks3 age_ks4 {
*	gen `i'x = `i'^2
*}

eststo 	m1: reg ea  ib8.MoB /*ea_age  ea_agex */ male, robust cluster(MoB)
eststo 	m2: reg ks1 ib8.MoB /*age_ks1 age_ks1x*/ male, robust cluster(MoB)
eststo 	m3: reg ks2 ib8.MoB /*age_ks2 age_ks2x*/ male, robust cluster(MoB)
eststo 	m4: reg ks3 ib8.MoB /*age_ks3 age_ks3x*/ male, robust cluster(MoB)
eststo 	m5: reg ks4 ib8.MoB                      male, robust cluster(MoB) 

esttab 	m1 m2 m3 m4 m5, b se keep(*.MoB) star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(0))

esttab 	m1 m2 m3 m4 m5 using "${dirtables}/MoB_OLS.tex", replace ///
	frag bookt b(2) se(2) keep(*.MoB) drop(8.MoB) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Age 4" "Age 7" "Age 11" "Age 14" "Age 16") ///
	stats(N, label("Observations") fmt(0)) nonotes coeflabel(1.MoB "Jan" 2.MoB "Feb" 3.MoB "Mar" 4.MoB "Apr" 5.MoB "May" 6.MoB "Jun" 7.MoB "Jul" 9.MoB "Sep" 10.MoB "Oct" 11.MoB "Nov" 12.MoB "Dec" ) 

log close



