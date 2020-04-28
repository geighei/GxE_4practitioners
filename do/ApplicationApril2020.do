***************************************************************************
***** Check August vs. September births - ALSPAC. 
***** Stephanie von Hinke, Apr 2020
***************************************************************************
set more off
clear all
capture log close

/*
ssc install blindschemes, replace all
ssc install rdrobust, replace all
*/

if "`c(username)'" == "pbirol" {
	global dirdata  "C:/Users/pbirol/Documents/work/ALSPAC_GxE/Alspac"
}

else if "`c(username)'" == "ecsmvhkv" {
	global dirdata  "\\rdsfcifs.acrc.bris.ac.uk/ALSPAC_GxE/Alspac"
}

global dirdropbox  "/Users/`c(username)'/Dropbox/GEIGHEI/projects/GxE_4practitioners"

global dirtables  "${dirdropbox}/tables"
global dirfigures "${dirdropbox}/figures"


cd "${dirdropbox}/"

	* SET UP LOG FILE
local date: display %td_CCYY-NN-DD date(c(current_date), "DMY")
local datetime = subinstr("`date'"," ","",.)+"_" +subinstr(c(current_time), ":", "-", .)
di "`datetime'"
log using "${dirdropbox}/logfiles/PractictionersPaper_`datetime'", text replace




** SKIP Patterns (set to zero if you want to skip that section)
global dataclean = 1
global figures   = 0
global regs      = 1 


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
	rename v`i' PC`p'
}
compress
save 	"${dirdata}/Child/ALSPAC_pc_10_CHILDREN.dta", replace



*** On ALSPAC data held at Erasmus
use 	"${dirdata}/Data_Set/Rietveld_12Sep18.dta", clear
rename 	qlet birth_order
merge 	1:1 cidB2492 birth_order using "${dirdata}/Child/PGS_children_alspac_plink_EA.dta"
drop 	if _m<3
drop 	_merge
merge 	1:1 id_child using "${dirdata}/Child/ALSPAC_pc_10_CHILDREN.dta"
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

* "Treated": born in the 6 months after September
gen treat = (MoBnew<=6) if MoBnew<.

***************************************************************************
keep 	cidB2492 birth_order male ea ks1 ks2 ks3 ks4 IQ MoB MoBnew pgs_child_EA PC* treat*
rename 	pgs_child_EA PGS

* Standardize to have mean 0 and standard deviation 1
sum 	PGS
replace PGS = (PGS-r(mean))/(r(sd))
sum 	PGS

gen highPGS = (PGS>0) if PGS<.
tab highPGS

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

save "${dirdata}/Data_Set/cleanALSPAC4application.dta", replace
} // end if dataclean

*---------------------------------------------------------------------------------*
* FIGURES
*---------------------------------------------------------------------------------*
if ${figures}==1{ // creates some cool figures
use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear

foreach var of varlist ea ks1 ks2 ks3 ks4 IQ {
	gen `var'hiPGS = `var'* highPGS
	gen `var'loPGS = `var'* (1-highPGS)
}

* RD plot figures
* overall
foreach test in ea ks1 ks2 ks3 ks4{
	rdplot   `test' MoB, c(9) //graph_option(legend(pos(6)))  covs(PGS male PC*)
	graph export "${dirfigures}/rdplot_`test'.png", replace
}

* by high and low PGS
foreach test in ea ks1 ks2 ks3 ks4{
	rdplot   `test' MoB if highPGS==1, c(9) graph_options(title("`test' High PGS")) //graph_options(legend(pos(6)))  covs(PGS male PC*)
	graph export "${dirfigures}/rdplot_`test'_highPGS.png", replace

	rdplot   `test' MoB if highPGS==0, c(9) graph_options(title("`test' Low PGS")) //graph_options(legend(pos(6)))  covs(PGS male PC*)
	graph export "${dirfigures}/rdplot_`test'_lowPGS.png", replace
}


//preserve
	* Collapse the data
	collapse (mean)  ea* ks1* ks2* ks3* ks4* IQ* ///
	         (sd)    sdea=ea sdks1=ks1 sdks2=ks2 sdks3=ks3 sdks4=ks4 sdIQ=IQ ///
	         (sd)    sdeahiPGS=eahiPGS sdks1hiPGS=ks1hiPGS sdks2hiPGS=ks2hiPGS sdks3hiPGS=ks3hiPGS sdks4hiPGS=ks4hiPGS sdIQhiPGS=IQhiPGS ///
	         (sd)    sdealoPGS=ealoPGS sdks1loPGS=ks1loPGS sdks2loPGS=ks2loPGS sdks3loPGS=ks3loPGS sdks4loPGS=ks4loPGS sdIQloPGS=IQloPGS ///
	         (count) nea=ea nks1=ks1 nks2=ks2 nks3=ks3 nks4=ks4 nIQ=IQ ///
	         (count) neahiPGS=eahiPGS nks1hiPGS=ks1hiPGS nks2hiPGS=ks2hiPGS nks3hiPGS=ks3hiPGS nks4hiPGS=ks4hiPGS nIQhiPGS=IQhiPGS ///
	         (count) nealoPGS=ealoPGS nks1loPGS=ks1loPGS nks2loPGS=ks2loPGS nks3loPGS=ks3loPGS nks4loPGS=ks4loPGS nIQloPGS=IQloPGS ///
			 , by(MoB MoBnew)
	
	foreach var of varlist ea* ks1* ks2* ks3* ks4* IQ* {
		generate hi`var'  = `var' + invttail(n`var'-1,0.025)*(sd`var' / sqrt(n`var'))
		generate low`var' = `var' - invttail(n`var'-1,0.025)*(sd`var' / sqrt(n`var'))
	}
	
	set scheme plotplainblind
	
	* Combined
	twoway  (line ea  MoBnew, sort) ///
		(line ks1 MoBnew, sort) ///
		(line ks2 MoBnew, sort) ///
		(line ks3 MoBnew, sort) ///
		(line ks4 MoBnew, sort) ///
		///(line IQ MoBnew, sort) ///
		(rcap hiea lowea MoBnew, color(gs13)) ///
		(rcap hiks1 lowks1 MoBnew, color(gs13)) ///
		(rcap hiks2 lowks2 MoBnew, color(gs13)) ///
		(rcap hiks3 lowks3 MoBnew, color(gs13)) ///
		(rcap hiks4 lowks4 MoBnew, color(gs13)) ///
		///(rcap hiIQ lowIQ MoBnew, color(gs13)) ///
		, xlabel(1(1)12, valuelabel) ///
		ytitle("Standardised score") xtitle("Month of birth") ///
		legend(on order(1 "Age 4" 2 "Age 7" 3 "Age 11" 4 "Age 14" 5 "Age 16") pos(6) row(1)) ///6 "IQ"
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Test scores by month of birth") //scheme(s1mono)
	graph export "${dirfigures}/MoBnew.png", replace
	
	twoway  (line ea  MoB, sort) ///
		(line ks1 MoB, sort) ///
		(line ks2 MoB, sort) ///
		(line ks3 MoB, sort) ///
		(line ks4 MoB, sort) ///
		///(line IQ MoB, sort) ///
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
	local yIQ      = "IQ Score"
	local titleea  = "Score on Entry Assessment test (age 4/5)"
	local titleks1 = "Score on Key Stage 1 test (age 7)"
	local titleks2 = "Score on Key Stage 2 test (age 11)"
	local titleks3 = "Score on Key Stage 3 test (age 14)"
	local titleks4 = "Score on Key Stage 4 test (age 16)"
	local titleIQ  = "Wechsler Intelligence Scale for Children (age 8)"
	
	foreach test in ea ks1 ks2 ks3 ks4 IQ {
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
	
//restore
} // end if figures


*---------------------------------------------------------------------------------*
* REGRESSIONS
*---------------------------------------------------------------------------------*
if ${regs}==1{ // runs the regressions
use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear

* Some simple regressions
* create the interaction terms between PGS and the controls
foreach control of varlist male PC1-PC10{
	gen PGSx`control' = `control'*PGS
}

***************************************************************************
***** No GxE interactions
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
eststo 	m1: reg ea  treat treat_MoB MoB PGS male PC*, robust cluster(MoB)
eststo 	m2: reg ks1 treat treat_MoB MoB PGS male PC*, robust cluster(MoB)
eststo 	m3: reg ks2 treat treat_MoB MoB PGS male PC*, robust cluster(MoB)
eststo 	m4: reg ks3 treat treat_MoB MoB PGS male PC*, robust cluster(MoB)
eststo 	m5: reg ks4 treat treat_MoB MoB PGS male PC*, robust cluster(MoB) 

esttab 	m1 m2 m3 m4 m5, b se keep(treat treat_MoB MoB PGS) star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(0))

esttab 	m1 m2 m3 m4 m5 using "${dirtables}/MoB_OLS2.tex", replace ///
	frag bookt b(3) se(3) keep(treat treat_MoB MoB PGS) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Age 4" "Age 7" "Age 11" "Age 14" "Age 16") ///
	stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes coeflabel(MoB "MoB" PGS "PGS" treat "Treated" treat_MoB "Treated*MoB") 

* rdrobust doesn't work unless I put covariates (I think because MoB is not actually continuous) but rdplot does
set scheme s1mono

foreach test in ea ks1 ks2 ks3 ks4{
	rdrobust `test' MoB, c(9) covs(PGS male PC*)
}

***************************************************************************
**** With GxE interactions
eststo 	m1: reg ea  ib9.MoB#c.PGS ib9.MoB PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB)
eststo 	m2: reg ks1 ib9.MoB#c.PGS ib9.MoB PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB)
eststo 	m3: reg ks2 ib9.MoB#c.PGS ib9.MoB PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB)
eststo 	m4: reg ks3 ib9.MoB#c.PGS ib9.MoB PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB)
eststo 	m5: reg ks4 ib9.MoB#c.PGS ib9.MoB PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB) 

esttab 	m1 m2 m3 m4 m5, b se keep(*.MoB#c.PGS *.MoB PGS) star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(0))

*esttab 	m1 m2 m3 m4 m5 using "${dirtables}/MoB_GxE.tex", replace ///
	frag bookt b(3) se(3) keep(*.MoB#c.PGS *.MoB PGS) drop(9.MoB 9.MoB#c.PGS) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Age 4" "Age 7" "Age 11" "Age 14" "Age 16") ///
	stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes coeflabel(1.MoB "Jan" 2.MoB "Feb" 3.MoB "Mar" 4.MoB "Apr" 5.MoB "May" 6.MoB "Jun" 7.MoB "Jul" 9.MoB "Sep" 10.MoB "Oct" 11.MoB "Nov" 12.MoB "Dec" PGS "PGS" 1.MoB#c.PGS "Jan*PGS" 2.MoB#c.PGS "Feb*PGS" 3.MoB#c.PGS "Mar*PGS" 4.MoB#c.PGS "Apr*PGS" 5.MoB#c.PGS "May*PGS" 6.MoB#c.PGS "Jun*PGS" 7.MoB#c.PGS "Jul*PGS" 9.MoB#c.PGS "Sep*PGS" 10.MoB#c.PGS "Oct*PGS" 11.MoB#c.PGS "Nov*PGS" 12.MoB#c.PGS "Dec*PGS" ) 


eststo 	m1: reg ea  ib1.MoBnew#c.PGS ib1.MoBnew PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB)
eststo 	m2: reg ks1 ib1.MoBnew#c.PGS ib1.MoBnew PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB)
eststo 	m3: reg ks2 ib1.MoBnew#c.PGS ib1.MoBnew PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB)
eststo 	m4: reg ks3 ib1.MoBnew#c.PGS ib1.MoBnew PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB)
eststo 	m5: reg ks4 ib1.MoBnew#c.PGS ib1.MoBnew PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB) 

esttab 	m1 m2 m3 m4 m5, b se keep(*.MoBnew#c.PGS *.MoBnew PGS) star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(0))

esttab 	m1 m2 m3 m4 m5 using "${dirtables}/MoB_GxE.tex", replace ///
	frag bookt b(3) se(3) keep(*.MoBnew#c.PGS *MoBnew PGS) drop(1.MoBnew 1.MoBnew#c.PGS) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Age 4" "Age 7" "Age 11" "Age 14" "Age 16") ///
	stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes coeflabel(1.MoBnew "Sep" 2.MoBnew "Oct" 3.MoBnew "Nov" 4.MoBnew "Dec" 5.MoBnew "Jan" 6.MoBnew "Feb" 7.MoBnew "Mar" 8.MoBnew "Apr" 9.MoBnew "May" 10.MoBnew "Jun" 11.MoBnew "Jul" 12.MoBnew "Aug" 1.MoBnew#c.PGS "Sep*PGS" 2.MoBnew#c.PGS "Oct*PGS" 3.MoBnew#c.PGS "Nov*PGS" 4.MoBnew#c.PGS "Dec*PGS" 5.MoBnew#c.PGS "Jan*PGS" 6.MoBnew#c.PGS "Feb*PGS" 7.MoBnew#c.PGS "Mar*PGS" 8.MoBnew#c.PGS "Apr*PGS" 9.MoBnew#c.PGS "May*PGS" 10.MoBnew#c.PGS "Jun*PGS" 11.MoBnew#c.PGS "Jul*PGS" 12.MoBnew#c.PGS "Aug*PGS" PGS "PGS") 

	
* Continuous MoB
eststo 	m1: reg ea  treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB)
eststo 	m2: reg ks1 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB)
eststo 	m3: reg ks2 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB)
eststo 	m4: reg ks3 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB)
eststo 	m5: reg ks4 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB) 

esttab 	m1 m2 m3 m4 m5, b se keep(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(0))

esttab 	m1 m2 m3 m4 m5 using "${dirtables}/MoB_GxE2.tex", replace ///
	frag bookt b(3) se(3) keep(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Age 4" "Age 7" "Age 11" "Age 14" "Age 16") ///
	stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes coeflabel(MoB "MoB" MoB_PGS "MoB*PGS" MoB_PGS_treat "MoB*PGS*Treated" PGS "PGS" treat "Treated" treat_MoB "Treated*MoB" treat_PGS "Treated*PGS") 

*------ Excluding the observations "far" from the cutoff
forvalues width=2/5{
capture drop window*
gen window`width' = (MoBnew <=`width' | MoBnew>(12-`width')) if MoBnew<.

* Continuous MoB
eststo 	m1: reg ea  treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB), if window`width' == 1
eststo 	m2: reg ks1 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB), if window`width' == 1
eststo 	m3: reg ks2 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB), if window`width' == 1
eststo 	m4: reg ks3 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB), if window`width' == 1
eststo 	m5: reg ks4 treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS male PC* PGSxmale PGSxPC*, robust cluster(MoB), if window`width' == 1

esttab 	m1 m2 m3 m4 m5, b se keep(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) star(* 0.10 ** 0.05 *** 0.01) stats(N, fmt(0))

esttab 	m1 m2 m3 m4 m5 using "${dirtables}/MoB_GxE2_window`width'.tex", replace ///
	frag bookt b(3) se(3) keep(treat treat_PGS treat_MoB MoB_PGS MoB_PGS_treat MoB PGS) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("Age 4" "Age 7" "Age 11" "Age 14" "Age 16") ///
	stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes coeflabel(MoB "MoB" MoB_PGS "MoB*PGS" MoB_PGS_treat "MoB*PGS*Treated" PGS "PGS" treat "Treated" treat_MoB "Treated*MoB" treat_PGS "Treated*PGS") 
} // end forvalues width

* RDrobust
foreach test in ea ks1 ks2 ks3 ks4{
	rdrobust `test' MoB if highPGS==1, c(9) covs(PGS male PC*)
	rdrobust `test' MoB if highPGS==0, c(9) covs(PGS male PC*)
}
	
} // end if regs

/* remove temporary dataset
rm "${dirdata}/Data_Set/cleanALSPAC4application.dta"
*/

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

