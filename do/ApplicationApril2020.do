***************************************************************************
***** Check August vs. September births - ALSPAC. 
***** Stephanie von Hinke, Apr 2020
***************************************************************************
set more off
clear all
capture log close


if "`c(username)'" == "pbirol" {
	global dir  "C:/Users/pbirol/Documents/work/ALSPAC_GxE/Alspac"
	global dirx "C:/Users/pbirol/Documents/work/ALSPAC_GxE/Alspac"
}

else if "`c(username)'" == "ecsmvhkv" {
	global dir  "\\rdsfcifs.acrc.bris.ac.uk/ALSPAC_GxE/Alspac"
	global dirx "\\\\\\rdsfcifs.acrc.bris.ac.uk/ALSPAC_GxE/Alspac"
}

global dirtables  "${dirx}/tables"
global dirfigures "${dir}/figures"


cd "${dir}"

	* SET UP LOG FILE
local date: display %td_CCYY-NN-DD date(c(current_date), "DMY")
local datetime = subinstr("`date'"," ","",.)+"_" +subinstr(c(current_time), ":", "-", .)
di "`datetime'"
log using "${dir}/logfiles/PractictionersPaper_`datetime'.smcl", replace



*** Saving PCs into Stata format
insheet using "${dir}/Child/ALSPAC_pc_10_CHILDREN.csv", clear
rename 	v1 id_child 
drop 	v2
foreach i in 3 4 5 6 7 8 9 10 11 12 {
	local p = (`i'-2)
	rename v`i' PC`p'
}
compress
save 	"${dir}/Child/ALSPAC_pc_10_CHILDREN.dta", replace



*** On ALSPAC data held at Erasmus
use 	"${dir}/Data_Set/Rietveld_12Sep18.dta", clear
rename 	qlet birth_order
merge 	1:1 cidB2492 birth_order using "${dir}/Child/PGS_children_alspac_plink_EA.dta"
drop 	if _m<3
drop 	_merge
merge 	1:1 id_child using "${dir}/Child/ALSPAC_pc_10_CHILDREN.dta"
drop 	if _m<3
drop 	_merge


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


* Key stage 3
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
keep 	cidB2492 birth_order male ea ks1 ks2 ks3 ks4 IQ MoB MoBnew pgs_child_EA PC*
rename 	pgs_child_EA PGS

* Standardize to have mean 0 and standard deviation 1
sum 	PGS
replace PGS = (PGS-r(mean))/(r(sd))
sum 	PGS

preserve

	* Collapse the data
	collapse ea ks1 ks2 ks3 ks4, by(MoB MoBnew)

	twoway  (line ea  MoBnew, sort) ///
		(line ks1 MoBnew, sort) ///
		(line ks2 MoBnew, sort) ///
		(line ks3 MoBnew, sort) ///
		(line ks4 MoBnew, sort) ///
		, xlabel(1(1)12, valuelabel) ///
		ytitle("Standardised score") xtitle("Month of birth") ///
		legend(on order(1 "Age 4" 2 "Age 7" 3 "Age 11" 4 "Age 14" 5 "Age 16") pos(6) row(1)) ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Test scores by month of birth") scheme(s1mono)
	graph export "${dirfigures}/MoBnew.png", replace
	
	twoway  (line ea  MoB, sort) ///
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
		ylabel(-1(.5)1) yscale(range(-1 1)) yline(0) ///
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

* Some simple regressions

* No GxE interactions
eststo 	m1: reg ea  ib9.MoB PGS male PC*, robust cluster(MoB)
eststo 	m2: reg ks1 ib9.MoB PGS male PC*, robust cluster(MoB)
eststo 	m3: reg ks2 ib9.MoB PGS male PC*, robust cluster(MoB)
eststo 	m4: reg ks3 ib9.MoB PGS male PC*, robust cluster(MoB)
eststo 	m5: reg ks4 ib9.MoB PGS male PC*, robust cluster(MoB) 

esttab 	m1 m2 m3 m4 m5, b se keep(*.MoB PGS) star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(0))

*esttab 	m1 m2 m3 m4 m5 using "${dirtables}/MoB_OLS.tex", replace ///
	frag bookt b(3) se(3) keep(*.MoB PGS) drop(9.MoB) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Age 4" "Age 7" "Age 11" "Age 14" "Age 16") ///
	stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes coeflabel(1.MoB "Jan" 2.MoB "Feb" 3.MoB "Mar" 4.MoB "Apr" 5.MoB "May" 6.MoB "Jun" 7.MoB "Jul" 8.MoB "Aug" 9.MoB "Sep" 10.MoB "Oct" 11.MoB "Nov" 12.MoB "Dec" PGS "PGS") 


eststo 	m1: reg ea  ib1.MoBnew PGS male PC*, robust cluster(MoB)
eststo 	m2: reg ks1 ib1.MoBnew PGS male PC*, robust cluster(MoB)
eststo 	m3: reg ks2 ib1.MoBnew PGS male PC*, robust cluster(MoB)
eststo 	m4: reg ks3 ib1.MoBnew PGS male PC*, robust cluster(MoB)
eststo 	m5: reg ks4 ib1.MoBnew PGS male PC*, robust cluster(MoB) 

esttab 	m1 m2 m3 m4 m5, b se keep(*.MoBnew PGS) star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(0))

esttab 	m1 m2 m3 m4 m5 using "${dirtables}/MoB_OLS.tex", replace ///
	frag bookt b(3) se(3) keep(*.MoBnew PGS) drop(1.MoBnew) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Age 4" "Age 7" "Age 11" "Age 14" "Age 16") ///
	stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes coeflabel(1.MoBnew "Sep" 2.MoBnew "Oct" 3.MoBnew "Nov" 4.MoBnew "Dec" 5.MoBnew "Jan" 6.MoBnew "Feb" 7.MoBnew "Mar" 8.MoBnew "Apr" 9.MoBnew "May" 10.MoBnew "Jun" 11.MoBnew "Jul" 12.MoBnew "Aug" PGS "PGS") 



* Continuous MoB
eststo 	m1: reg ea  MoBnew PGS male PC*, robust cluster(MoB)
eststo 	m2: reg ks1 MoBnew PGS male PC*, robust cluster(MoB)
eststo 	m3: reg ks2 MoBnew PGS male PC*, robust cluster(MoB)
eststo 	m4: reg ks3 MoBnew PGS male PC*, robust cluster(MoB)
eststo 	m5: reg ks4 MoBnew PGS male PC*, robust cluster(MoB) 

esttab 	m1 m2 m3 m4 m5, b se keep(MoBnew PGS) star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(0))

esttab 	m1 m2 m3 m4 m5 using "${dirtables}/MoB_OLS2.tex", replace ///
	frag bookt b(3) se(3) keep(MoBnew PGS) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Age 4" "Age 7" "Age 11" "Age 14" "Age 16") ///
	stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes coeflabel(MoBnew "MoB" PGS "PGS") 



***************************************************************************
* With GxE interactions
eststo 	m1: reg ea  ib9.MoB#c.PGS ib9.MoB PGS male PC*, robust cluster(MoB)
eststo 	m2: reg ks1 ib9.MoB#c.PGS ib9.MoB PGS male PC*, robust cluster(MoB)
eststo 	m3: reg ks2 ib9.MoB#c.PGS ib9.MoB PGS male PC*, robust cluster(MoB)
eststo 	m4: reg ks3 ib9.MoB#c.PGS ib9.MoB PGS male PC*, robust cluster(MoB)
eststo 	m5: reg ks4 ib9.MoB#c.PGS ib9.MoB PGS male PC*, robust cluster(MoB) 

esttab 	m1 m2 m3 m4 m5, b se keep(*.MoB#c.PGS *.MoB PGS) star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(0))

*esttab 	m1 m2 m3 m4 m5 using "${dirtables}/MoB_GxE.tex", replace ///
	frag bookt b(3) se(3) keep(*.MoB#c.PGS *.MoB PGS) drop(9.MoB 9.MoB#c.PGS) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Age 4" "Age 7" "Age 11" "Age 14" "Age 16") ///
	stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes coeflabel(1.MoB "Jan" 2.MoB "Feb" 3.MoB "Mar" 4.MoB "Apr" 5.MoB "May" 6.MoB "Jun" 7.MoB "Jul" 9.MoB "Sep" 10.MoB "Oct" 11.MoB "Nov" 12.MoB "Dec" PGS "PGS" 1.MoB#c.PGS "Jan*PGS" 2.MoB#c.PGS "Feb*PGS" 3.MoB#c.PGS "Mar*PGS" 4.MoB#c.PGS "Apr*PGS" 5.MoB#c.PGS "May*PGS" 6.MoB#c.PGS "Jun*PGS" 7.MoB#c.PGS "Jul*PGS" 9.MoB#c.PGS "Sep*PGS" 10.MoB#c.PGS "Oct*PGS" 11.MoB#c.PGS "Nov*PGS" 12.MoB#c.PGS "Dec*PGS" ) 


eststo 	m1: reg ea  ib1.MoBnew#c.PGS ib1.MoBnew PGS male PC*, robust cluster(MoB)
eststo 	m2: reg ks1 ib1.MoBnew#c.PGS ib1.MoBnew PGS male PC*, robust cluster(MoB)
eststo 	m3: reg ks2 ib1.MoBnew#c.PGS ib1.MoBnew PGS male PC*, robust cluster(MoB)
eststo 	m4: reg ks3 ib1.MoBnew#c.PGS ib1.MoBnew PGS male PC*, robust cluster(MoB)
eststo 	m5: reg ks4 ib1.MoBnew#c.PGS ib1.MoBnew PGS male PC*, robust cluster(MoB) 

esttab 	m1 m2 m3 m4 m5, b se keep(*.MoBnew#c.PGS *.MoBnew PGS) star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(0))

esttab 	m1 m2 m3 m4 m5 using "${dirtables}/MoB_GxE.tex", replace ///
	frag bookt b(3) se(3) keep(*.MoBnew#c.PGS *MoBnew PGS) drop(1.MoBnew 1.MoBnew#c.PGS) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Age 4" "Age 7" "Age 11" "Age 14" "Age 16") ///
	stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes coeflabel(1.MoBnew "Sep" 2.MoBnew "Oct" 3.MoBnew "Nov" 4.MoBnew "Dec" 5.MoBnew "Jan" 6.MoBnew "Feb" 7.MoBnew "Mar" 8.MoBnew "Apr" 9.MoBnew "May" 10.MoBnew "Jun" 11.MoBnew "Jul" 12.MoBnew "Aug" 1.MoBnew#c.PGS "Sep*PGS" 2.MoBnew#c.PGS "Oct*PGS" 3.MoBnew#c.PGS "Nov*PGS" 4.MoBnew#c.PGS "Dec*PGS" 5.MoBnew#c.PGS "Jan*PGS" 6.MoBnew#c.PGS "Feb*PGS" 7.MoBnew#c.PGS "Mar*PGS" 8.MoBnew#c.PGS "Apr*PGS" 9.MoBnew#c.PGS "May*PGS" 10.MoBnew#c.PGS "Jun*PGS" 11.MoBnew#c.PGS "Jul*PGS" 12.MoBnew#c.PGS "Aug*PGS" PGS "PGS") 


* Continuous MoB
eststo 	m1: reg ea  c.MoBnew#c.PGS MoBnew PGS male PC*, robust cluster(MoB)
eststo 	m2: reg ks1 c.MoBnew#c.PGS MoBnew PGS male PC*, robust cluster(MoB)
eststo 	m3: reg ks2 c.MoBnew#c.PGS MoBnew PGS male PC*, robust cluster(MoB)
eststo 	m4: reg ks3 c.MoBnew#c.PGS MoBnew PGS male PC*, robust cluster(MoB)
eststo 	m5: reg ks4 c.MoBnew#c.PGS MoBnew PGS male PC*, robust cluster(MoB) 

esttab 	m1 m2 m3 m4 m5, b se keep(c.MoBnew#c.PGS MoBnew PGS) star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(0))

esttab 	m1 m2 m3 m4 m5 using "${dirtables}/MoB_GxE2.tex", replace ///
	frag bookt b(3) se(3) keep(c.MoBnew#c.PGS MoBnew PGS) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Age 4" "Age 7" "Age 11" "Age 14" "Age 16") ///
	stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes coeflabel(MoBnew "MoB" c.MoBnew#c.PGS "MoB*PGS" PGS "PGS") 


log close














exit










