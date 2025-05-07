***************************************************************************
***** GxE of August vs. September births - ALSPAC. 
***************************************************************************

/* Description

This do file provides a step-by-step replication of the empirical application
in the paper "The Economics and Econometrics of Gene–Environment Interplay" ReStud 2025

The Figure, Table, and page numbers refer to the pre-print of November 22, 2024

The are two main steps:
1. Import and cleans the ALSPAC data and merges it with the PGIs
2. Run the GxE analysis


input: "${dirdata}/2_cleanData/cleanALSPAC_GxE.dta", clear
output: Tables and Figures

NOTE: Polygenic Score (PGS) and Polygenic Index (PGI) are used interchangeably in the code
in the paper we only use PGI
*/


/* commands to install
net describe polychoric, from(http://staskolenikov.net/stata)
net install polychoric
ssc install estout 
cap ado uninstall reghdfe
net install reghdfe, from("https://raw.githubusercontent.com/sergiocorreia/reghdfe/master/src/")
ssc install coefplot
*/





set more off
clear all
capture log close

/* these globals were already set in 0_runall
	global dirdata    "/Users/`c(username)'/Documents/data/alspac/GxE_metrics_replication_data"  // this is not publicly available
	global dirpackage "/Users/`c(username)'/Downloads/GxE_metrics_replication_package"		// change this to the folder where you downloaded the replication package
*/

global dircode    "${dirpackage}/0_code/"
global dirtables  "${dirpackage}/3_output/tables"
global dirfigures "${dirpackage}/3_output/figures"


********************************************************************************
cd "${dircode}"
log using 2_GxE_analysis.log, replace


/* Clear the output folders
rm ${dirtables} 
rm ${dirfigures} 

mkdir ${dirtables} 
mkdir ${dirfigures} 
*/




********************************************************************************
**#                            MAIN PAPER                                    #**
********************************************************************************








*------------------------------------------------------------------------------*
* Figure 2: Standardized test scores at different ages by month of birth.
*------------------------------------------------------------------------------*
use "${dirdata}/2_cleanData/cleanALSPAC_GxE.dta", clear

preserve  
	collapse (mean)  ea ks1 ks2 ks3 ks4 ///
		 (sd)    sdea=ea sdks1=ks1 sdks2=ks2 sdks3=ks3 sdks4=ks4 ///
		 (count) nea=ea nks1=ks1 nks2=ks2 nks3=ks3 nks4=ks4 ///
			 , by(MoB MoBnew)

	foreach var of varlist ea ks1 ks2 ks3 ks4 {
		generate hi`var'  = `var' + invttail(n`var'-1,0.025)*(sd`var' / sqrt(n`var'))
		generate low`var' = `var' - invttail(n`var'-1,0.025)*(sd`var' / sqrt(n`var'))
	}

	* Combined
	twoway  (line ea  MoB, sort lp(solid) lc(gs0)) ///
		(line ks1 MoB, sort lp(dash_dot) lc(gs0)) ///
		(line ks2 MoB, sort lp(dash) lc(gs5)) ///
		(line ks3 MoB, sort lp(longdash_dot) lc(gs5)) ///
		(line ks4 MoB, sort lp(shortdash) lc(gs4)) ///
		(rcap hiea lowea MoB, color(gs13)) ///
		(rcap hiks1 lowks1 MoB, color(gs13)) ///
		(rcap hiks2 lowks2 MoB, color(gs13)) ///
		(rcap hiks3 lowks3 MoB, color(gs13)) ///
		(rcap hiks4 lowks4 MoB, color(gs13)) ///
		, xlabel(1(1)12, valuelabel) ///
		ytitle("Standardized score") xtitle("Month of birth") ///
		legend(on order(1 "Age 4" 2 "Age 7" 3 "Age 11" 4 "Age 14" 5 "Age 16") pos(6) row(1) symxsize(0.2cm)) ///
		ylabel(-.5(.25).5) yscale(range(-0.5 0.5)) yline(0) ///
		title("Test scores by month of birth") scheme(s1mono)
	graph export "${dirfigures}/MoB.png", replace

restore 



*------------------------------------------------------------------------------*
* Table 2: Descriptive statistics of child and family characteristics by treatment status
*------------------------------------------------------------------------------*
	use "${dirdata}/2_cleanData/cleanALSPAC_GxE.dta", clear
	keep if window3mth == 1 // keep only 3 months of each side of the cutoff

	local 	xvars "mumagepreg smokepreg m_anxiety m_depression married mumed2 mumed3 mumed4 mumed5 daded2 daded3 daded4 daded5 SC2 SC3 SC4 SC5 SC6 bw PGI PGI_mother PGI_father "

	qui estpost sum `xvars' if treat==1
	qui eststo 	T1
	qui estpost sum `xvars' if treat==0
	qui eststo 	T0
	qui estpost ttest `xvars', by(treat) unequal
	qui eststo 	pval

	esttab 	T1 T0 pval, replace ///
		nomtitle cells("count(fmt(%6.0fc)) mean(fmt(3)) p(fmt(3))") label ///
		booktabs nonum collabels(none) f noobs wide nogaps

	esttab 	T1 T0 pval using "${dirtables}/DescrByTreated.tex", replace ///
		nomtitle cells("count(fmt(%6.0fc)) mean(fmt(3)) p(fmt(3))") label ///
		booktabs nonum collabels(none) f noobs wide nogap

*------------------------------------------------------------------------------*
* Check if PGI predicts treatment
	/* Section 4.3 page 28*/
	polychoric treat PGI
	polychoric treat PGI_mothers
	polychoric treat PGI_fathers



*------------------------------------------------------------------------------*
* Table 3: OLS estimates of the effect of the PGI for EA on test scores at different ages
*------------------------------------------------------------------------------*
	use "${dirdata}/2_cleanData/cleanALSPAC_GxE.dta", clear
	keep if window3mth == 1 // keep only 3 months of each side of the cutoff

	*------- Predictive power of the PGI in an OLS regression -----------------*
	* With and without parental genotypes
	foreach outcome in ea ks1 ks2 ks3 ks4 {
		foreach method in 23me_ukb {
			* Without parental genotypes
			gen PGI_ch = pgs_children_`method'
			qui eststo `outcome'_`method'_a: reg `outcome' PGI_ch male c_PC*, robust
			gen sample=1 if e(sample)
			* With parental genotypes and missing PGIs replaced with 0 and missing dummy
			gen PGI_m = pgs_mothers_`method'
			gen PGI_f = pgs_fathers_`method'
			gen motherdummy = cond(PGI_m==. & sample==1,1,0)
			gen fatherdummy = cond(PGI_f==. & sample==1,1,0)
			replace PGI_m = 0 if PGI_m ==. & sample==1
			replace PGI_f = 0 if PGI_f ==. & sample==1
			qui eststo `outcome'_`method'_c: reg `outcome' PGI_ch PGI_m PGI_f male motherdummy fatherdummy c_PC*, robust
			qui eststo `outcome'_`method'_d: reg `outcome' PGI_m PGI_f male motherdummy fatherdummy c_PC*, robust
			drop sample PGI_ch PGI_m PGI_f motherdummy fatherdummy
		} 
	* Without parental genotypes (a), with parental genotypes (c), and without child genotype (d)
	esttab `outcome'_23me_ukb_a `outcome'_23me_ukb_c `outcome'_23me_ukb_d, b se keep(PGI_ch PGI_m PGI_f motherdummy fatherdummy) stats(r2 N)
	}

	* Table 3								
	esttab 	ea_23me_ukb_a ks1_23me_ukb_a ks2_23me_ukb_a ks3_23me_ukb_a ks4_23me_ukb_a, b se keep(PGI_*) stats(r2 N)
	esttab 	ea_23me_ukb_a ks1_23me_ukb_a ks2_23me_ukb_a ks3_23me_ukb_a ks4_23me_ukb_a using "${dirtables}/PredictivePower_23me_ukb.tex", replace ///
		frag bookt b(3) se(3) keep(PGI_*) star(* 0.10 ** 0.05 *** 0.01) ///
		mgroups("Entry Assessment" "Key Stage 1" "Key Stage 2" "Key Stage 3" "Key Stage 4", pattern(1 1 1 1 1 ) span ///
		prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span})) ///
		mtitles("Age 4-5" "Age 6-7" "Age 10-11" "Age 13-14" "Age 15-16") nonumber ///
		noobs stats(r2 N, label("R$^2$" "Observations") fmt(3 0)) nonotes ///
		coeflabel(PGI_ch "PGI Child") refcat(PGI_ch "\textbf{Panel A: Not controlling for parental PGIs}", nolabel)
	esttab 	ea_23me_ukb_c ks1_23me_ukb_c ks2_23me_ukb_c ks3_23me_ukb_c ks4_23me_ukb_c using "${dirtables}/PredictivePower_23me_ukb.tex", append ///
		frag bookt b(3) se(3) keep(PGI_ch PGI_m PGI_f) star(* 0.10 ** 0.05 *** 0.01) ///
		nomtitles stats(r2 N, label("R$^2$" "Observations") fmt(3 0)) nonotes nonumber ///
		coeflabel(PGI_ch "PGI Child" PGI_m "PGI Mother" PGI_f "PGI Father") ///
		refcat(PGI_ch "\textbf{Panel B: Controlling for the parental PGIs}", nolabel)


	*------------------------------------------------------------------------------*
	* Appendix Table G.2: OLS estimates of the effect of the parental PGIs for EA on test scores at different ages.
	// included here instead of the appendix to avoid running the loop again
	*------------------------------------------------------------------------------*
	esttab 	ea_23me_ukb_d ks1_23me_ukb_d ks2_23me_ukb_d ks3_23me_ukb_d ks4_23me_ukb_d, b se keep(PGI_*) stats(r2 N)
	esttab 	ea_23me_ukb_d ks1_23me_ukb_d ks2_23me_ukb_d ks3_23me_ukb_d ks4_23me_ukb_d using "${dirtables}/PredictivePower_23me_ukb_app.tex", replace ///
		frag bookt b(3) se(3) keep(PGI_*) star(* 0.10 ** 0.05 *** 0.01) ///
		nomtitles stats(r2 N, label("R$^2$" "Observations") fmt(3 0)) nonotes nonumber ///
		mgroups("Entry Assessment" "Key Stage 1" "Key Stage 2" "Key Stage 3" "Key Stage 4", pattern(1 1 1 1 1 ) span ///
		prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span})) ///
		coeflabel(PGI_m "PGI Mother" PGI_f "PGI Father") 

	est drop _all



*------------------------------------------------------------------------------*
* Figure 3: The relation between PGI Child and the Entry Assessment (age 4-5) test score in the treatment and control group
*------------------------------------------------------------------------------*

use "${dirdata}/2_cleanData/cleanALSPAC_GxE.dta", clear
keep if window3mth == 1 // keep only 3 months of each side of the cutoff

/*
* Functional Form
* Plot to the data to visualize the realationship between G and the outcome for treat and control 
* The polygenic index (PGI) distribution is trimmed to be between -3 and +3 to avoid nonlinear overfitting of outliers
* The loop can be done for all the key stages, 
but in the paper we only have it for the entry assessment score so we only produce that figure here
*/
		
foreach outcome in ea { //ks1 ks2 ks3 ks4 
	local outcomelabel: variable label `outcome'
	local lab0 : label treat 0
	local lab1 : label treat 1

	* Bin-scatter plot of PGI over outcome spearately by treatment ("raw" evidence of GxE)
	capture drop PGI_bin* 
	capture drop meanP* 
	capture drop meanD* 
	capture drop tag*
	* control
	xtile 	PGI_bin0 = PGI, nquantiles(200),        if treat == 0
	bys 	PGI_bin0: egen meanP0 = mean(PGI)       if treat == 0
	bys 	PGI_bin0: egen meanD0 = mean(`outcome') if treat == 0
	egen 	tag0 = tag(meanP0 meanD0)               if treat == 0
	* treatment
	xtile 	PGI_bin1 = PGI, nquantiles(200),        if treat == 1
	bys 	PGI_bin1: egen meanP1 = mean(PGI)       if treat == 1
	bys 	PGI_bin1: egen meanD1 = mean(`outcome') if treat == 1
	egen 	tag1 = tag(meanP1 meanD1)               if treat == 1
		
	* twoway graph
	twoway  ///
		(lpolyci `outcome' PGI if treat==0, acolor(gs15) clcolor(gs9)) /// 		local polynomial
		(lpolyci `outcome' PGI if treat==1, acolor(gs15) clcolor(gs0)) ///
		(scatter meanD0 PGI if tag0==1, msize(vsmall) mcolor(gs9)) ///			scatter
		(scatter meanD1 PGI if tag1==1, msize(vsmall) mcolor(gs0)) ///
		if (PGI>-3 & PGI<3) & (meanD1>-3), /// cutting off the tails
		legend(label(2 "Control")  label(4 "Treated") ///
			   label(5 "Control, scatter") label(6 "Treated, scatter") /// 
			   rows(1) position(6) order(4 2)) ///
		xtitle("PGI Child") ytitle("`outcomelabel'") scheme(s1mono) xsc(r(-3 3)) xlabel(-3(1)3)
			
	graph export "${dirfigures}/PGIxTreat_`outcome'.png", replace
	drop 	PGI_bin* meanP* meanD* tag*
} // end foreach outcome



*------------------------------------------------------------------------------*
* Table 4, 5 and Appendix Figure G.2
*------------------------------------------------------------------------------*
	use "${dirdata}/2_cleanData/cleanALSPAC_GxE.dta", clear
	keep if window3mth == 1

	** Inputs: PGI, treat, and controls
	tab MoB treat
	global Xvars       MoBnew male YoB92 c_PC* // input: variables to control for in the regression
	global tokeep      treat PGI PGIxtreat MoBnew treatxMoBnew PGIxMoBnew PGIxtreatxMoBnew  // variables to keep in the output

	** demean child, mother, and father PGI
	qui sum PGI
	replace PGI = (PGI-r(mean))/r(sd) // demean the PGI in the analytical sample (3 month window)
	gen PGIxtreat = treat * PGI
	
	qui sum PGI_mothers
	replace PGI_mothers = (PGI_mothers-r(mean))/r(sd)
	qui sum PGI_fathers
	replace PGI_fathers = (PGI_fathers-r(mean))/r(sd)
	
	* Missing parental PGI: create the parental PGI imputed to 0 if missing
	gen motherdummy = cond(PGI_mothers==. ,1,0)
	gen fatherdummy = cond(PGI_fathers==. ,1,0)
	replace PGI_mothers= 0 if motherdummy == 1
	replace PGI_fathers= 0 if fatherdummy == 1

	*G and E iteracted with controls
	global Xpgi        ""
	global Xtreat      ""

	foreach control of varlist $Xvars {
		qui sum `control'
		gen PGIx`control'   = PGI   * (`control'-r(mean))
		gen treatx`control' = treat * (`control'-r(mean))
		
		//automatically populate the global list of interacted controls
		global Xpgi   ${Xpgi}   PGIx`control'
		global Xtreat ${Xtreat} treatx`control'
	} 

	*GxExControls ---> only one triple interaction, with the RDD running variable, to keep things simple
	qui sum MoBnew
	gen PGIxtreatxMoBnew = PGIxtreat * (MoBnew -r(mean))
	

	*****************************************
	*** Regressions without parental PGIs ***
	*****************************************
	foreach outcome in ea ks1 ks2 ks3 ks4 {
		qui eststo `outcome'_treat_a: reg `outcome' treat PGI                            ${Xvars} ${Xtreat}        , cluster(MoB), if window3mth == 1
		qui eststo `outcome'_gxe_a:   reg `outcome' treat PGI PGIxtreat PGIxtreatxMoBnew ${Xvars} ${Xtreat} ${Xpgi}, cluster(MoB), if window3mth == 1

	********************************************************************************
	*** Regressions WITH parental PGIs, missing dummies and parental PGIxTreated ***
	********************************************************************************
	foreach control of varlist PGI_mothers PGI_fathers motherdummy fatherdummy {
		qui sum `control'
		gen PGIx`control'   = PGI   * (`control'-r(mean))
		gen treatx`control' = treat * (`control'-r(mean))
	} 
		
	* Interactions parental PGI and treated: TreatxPGI_mothers and TreatxPGI_fathers 
	rename treatxPGI_mothers PGImumTreat 
	rename treatxPGI_fathers PGIdadTreat 
	rename PGIxPGI_mothers PGImumchild 
	rename PGIxPGI_fathers PGIdadchild 

	qui eststo `outcome'_gxe_e:   reg `outcome' treat PGI PGIxtreat PGIxtreatxMoBnew PGI_mothers PGI_fathers motherdummy fatherdummy treatxmotherdummy treatxfatherdummy PGIxmotherdummy PGIxfatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild ${Xvars} ${Xtreat} ${Xpgi}, cluster(MoB), if window3mth == 1 & e(sample)

	esttab `outcome'_treat_a `outcome'_gxe_a `outcome'_gxe_e, b se ///
		keep(${tokeep} PGI_mothers PGI_fathers motherdummy fatherdummy treatxmotherdummy treatxfatherdummy PGIxmotherdummy PGIxfatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild) ///
		order(${tokeep} PGI_mothers PGI_fathers motherdummy fatherdummy treatxmotherdummy treatxfatherdummy PGIxmotherdummy PGIxfatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild PGImumchild PGIdadchild) ///
		star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))
 
	drop PGIxmotherdummy PGIxfatherdummy treatxmotherdummy treatxfatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild
	} 
	
	*------------------------------------------------------------------------------*
	* Table 4: OLS estimates of the main and interaction effects of being old-for-grade (Treated) and the EA PGI on children's entry assessment (age 4) test scores, with and without controls for parental PGIs
	*------------------------------------------------------------------------------*
	esttab ea_treat_a ea_gxe_a ea_gxe_e using "${dirtables}/MoB_ea.tex", replace ///
		frag bookt b(3) se(3) keep(${tokeep} PGI_mothers PGI_fathers PGImumTreat PGIdadTreat PGImumchild PGIdadchild ) ///
		order(${tokeep} PGI_mothers PGI_fathers PGImumTreat PGIdadTreat PGImumchild PGIdadchild ) star(* 0.10 ** 0.05 *** 0.01) ///
		nomtitles stats(r2 N, label("R$^2$" "Observations") fmt(3 0)) nonotes ///
		coeflabel(MoBnew "MoB" PGIxMoBnew "MoB $\times$ PGI Child" PGIxtreatxMoBnew "MoB $\times$ PGI Child $\times$ Treated" PGI "PGI Child" treat "Treated" treatxMoBnew "Treated $\times$ MoB" ///
		PGIxtreat "Treated $\times$ PGI Child" PGI_mothers "PGI Mother" PGI_fathers "PGI Father" PGImumTreat "PGI Mother $\times$ Treated" PGIdadTreat "PGI Father $\times$ Treated" PGImumchild "PGI Mother $\times$ PGI Child" PGIdadchild "PGI Father $\times$ PGI Child" ) 


	*------------------------------------------------------------------------------*
	* Table 5: OLS estimates of the main and interaction effects of being old-for-grade (Treated) and the EA PGI on children's Key Stage test scores
	*------------------------------------------------------------------------------*
	esttab ks1_gxe_a ks1_gxe_e ks2_gxe_a ks2_gxe_e ks3_gxe_a ks3_gxe_e ks4_gxe_a ks4_gxe_e using "${dirtables}/MoB_other.tex", replace ///
		frag bookt b(3) se(3) keep(${tokeep} PGI_mothers PGI_fathers PGImumTreat PGIdadTreat PGImumchild PGIdadchild ) ///
		order(${tokeep} PGI_mothers PGI_fathers PGImumTreat PGIdadTreat PGImumchild PGIdadchild ) star(* 0.10 ** 0.05 *** 0.01) ///
		nomtitles stats(r2 N, label("R2" "Observations") fmt(3 0)) nonotes ///
		mgroups("KS1 (age 7)" "KS2 (age 11)" "KS3 (age 14)" "KS4 (age 16)", pattern(1 0 1 0 1 0 1 0) span ///
		prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span})) ///
		coeflabel(MoBnew "MoB" PGIxMoBnew "MoB $\times$ PGI Child" PGIxtreatxMoBnew "MoB $\times$ PGI Child $\times$ Treated" PGI "PGI Child" treat "Treated" treatxMoBnew "Treated $\times$ MoB" ///
		PGIxtreat "Treated $\times$ PGI Child" PGI_mothers "PGI Mother" PGI_fathers "PGI Father" PGImumTreat "PGI Mother $\times$ Treated" PGIdadTreat "PGI Father $\times$ Treated" PGImumchild "PGI Mother $\times$ PGI Child" PGIdadchild "PGI Father $\times$ PGI Child" ) 


		
	*------------------------------------------------------------------------------*
	* Appendix Figure G.2: The G × E coefficients for the Entry Assessment and Key Stage 1–4 test scores
	*------------------------------------------------------------------------------*
		coefplot  (ea_gxe_e , aseq("Entry Assessment")  mcolor(gs0) lcolor(gs0)) ///
				  (ks1_gxe_e, aseq("Key Stage 1") mcolor(gs0) lcolor(gs0)) ///
		          (ks2_gxe_e, aseq("Key Stage 2") mcolor(gs0) lcolor(gs0)) ///
				  (ks3_gxe_e, aseq("Key Stage 3") mcolor(gs0) lcolor(gs0)) ///
				  (ks4_gxe_e, aseq("Key Stage 4") mcolor(gs0) lcolor(gs0)) ///
		, keep(PGIxtreat) ///
		msymbol(D) xline(0) byopts(xrescale) levels(95 90) ciopts(lcolor(gs0 gs0) recast(. rcap mlcolor(gs0))) legend(order(1 "95% c.i." 2 "90% c.i." )) /// 
		aseq swapnames
		graph export "${dirfigures}/coefplot_gxe.png", replace

		
	est drop _all


*---------------------------------------------------------------------------------*
*Table 6: OLS estimates of the main and interaction effects of being old-for-grade (Treated) 
* and the EA PGI on children's Key Stage test scores, controlling for Entry Assessment.
*---------------------------------------------------------------------------------*
use "${dirdata}/2_cleanData/cleanALSPAC_GxE.dta", clear
keep if window3mth == 1

** Inputs: PGI, treat, and controls
tab MoB treat
global Xvars       MoBnew male YoB92 c_PC* // input: variables to control for in the regression
global tokeep      treat PGI PGIxtreat MoBnew treatxMoBnew PGIxMoBnew PGIxtreatxMoBnew  // variables to keep in the output

** demean child, mother, and father PGI
qui sum PGI
replace PGI = (PGI-r(mean))/r(sd) // demean the PGI in the analytical sample (3 month window)
gen PGIxtreat = treat * PGI

qui sum PGI_mothers
replace PGI_mothers = (PGI_mothers-r(mean))/r(sd)
qui sum PGI_fathers
replace PGI_fathers = (PGI_fathers-r(mean))/r(sd)

* Missing parental PGI: create the parental PGI imputed to 0 if missing
gen motherdummy = cond(PGI_mothers==. ,1,0)
gen fatherdummy = cond(PGI_fathers==. ,1,0)
replace PGI_mothers= 0 if motherdummy == 1
replace PGI_fathers= 0 if fatherdummy == 1

*G and E iteracted with controls
global Xpgi        ""
global Xtreat      ""

foreach control of varlist $Xvars {
	qui sum `control'
	gen PGIx`control'   = PGI   * (`control'-r(mean))
	gen treatx`control' = treat * (`control'-r(mean))
	
	//automatically populate the global list of interacted controls
	global Xpgi   ${Xpgi}   PGIx`control'  
	global Xtreat ${Xtreat} treatx`control'
} 

*GxExControls ---> only one triple interaction, with the RDD running variable, to keep things simple
qui sum MoBnew
gen PGIxtreatxMoBnew = PGIxtreat * (MoBnew -r(mean))

* parental PGIs
foreach control of varlist PGI_mothers PGI_fathers motherdummy fatherdummy {
	qui sum `control'
	gen PGIx`control'   = PGI   * (`control'-r(mean))
	gen treatx`control' = treat * (`control'-r(mean))
} 
	
* Interactions parental PGI and treated: TreatxPGI_mothers and TreatxPGI_fathers 
rename treatxPGI_mothers PGImumTreat 
rename treatxPGI_fathers PGIdadTreat 
rename PGIxPGI_mothers PGImumchild 
rename PGIxPGI_fathers PGIdadchild 

* entry assessment
qui sum ea
qui gen PGIxea = PGI*(ea - r(mean))

* entry assessment sample
foreach outcome in ks1 ks2 ks3 ks4 {
	qui capture drop ea_sample
	qui reg `outcome' treat PGI ea ${Xvars} ${Xtreat} , cluster(MoB), if window3mth == 1
	qui gen ea_sample = e(sample)
} 

*** Regressions WITH parental PGIs, missing dummies and parental PGIxTreated ***
foreach outcome in ks1 ks2 ks3 ks4 {
	qui reg `outcome' treat PGI ${Xvars} ${Xtreat} , cluster(MoB), if window3mth == 1
	
	qui eststo `outcome'_gxe_e1:   reg `outcome' treat PGI PGIxtreat          PGIxtreatxMoBnew PGI_mothers PGI_fathers motherdummy fatherdummy treatxmotherdummy treatxfatherdummy PGIxmotherdummy PGIxfatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild ${Xvars} ${Xtreat} ${Xpgi}, cluster(MoB), if window3mth == 1 
	qui eststo `outcome'_gxe_e2:   reg `outcome' treat PGI PGIxtreat          PGIxtreatxMoBnew PGI_mothers PGI_fathers motherdummy fatherdummy treatxmotherdummy treatxfatherdummy PGIxmotherdummy PGIxfatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild ${Xvars} ${Xtreat} ${Xpgi}, cluster(MoB), if window3mth == 1 & ea_sample == 1
	qui eststo `outcome'_gxe_med: reg `outcome' treat PGI PGIxtreat ea PGIxea PGIxtreatxMoBnew PGI_mothers PGI_fathers motherdummy fatherdummy treatxmotherdummy treatxfatherdummy PGIxmotherdummy PGIxfatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild ${Xvars} ${Xtreat} ${Xpgi}, cluster(MoB), if window3mth == 1 & ea_sample == 1

	esttab `outcome'_gxe_e1 `outcome'_gxe_e2 `outcome'_gxe_med, b se keep(${tokeep} ea PGIxea PGI_mothers PGI_fathers motherdummy fatherdummy treatxmotherdummy treatxfatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild) order(${tokeep} ea PGIxea PGI_mothers PGI_fathers motherdummy fatherdummy treatxmotherdummy treatxfatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild PGImumchild PGIdadchild) star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))
	
} 

* Save on dirtables (Table 6)
esttab ks1_gxe_e1 ks1_gxe_e2 ks1_gxe_med ///
	   ks2_gxe_e1 ks2_gxe_e2 ks2_gxe_med ///
	   ks3_gxe_e1 ks3_gxe_e2 ks3_gxe_med ///
	   ks4_gxe_e1 ks4_gxe_e2 ks4_gxe_med using "${dirtables}/mechan_ea.tex", ///
	replace frag bookt b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) /// 
	keep(treat PGI PGIxtreat ea PGIxea) ///
	order(treat PGI PGIxtreat ea PGIxea) ///
	nomtitles stats(r2 N, label("R$^2$" "Observations") fmt(3 0)) nonotes ///
	mgroups("KS1 (age 7)" "KS2 (age 11)" "KS3 (age 14)" "KS4 (age 16)", pattern(1 0 0 1 0 0 1 0 0 1 0 0)  span ///
	prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span})) ///
	coeflabel(MoBnew "MoB" PGIxMoBnew "MoB $\times$ PGI Child" PGIxtreatxMoBnew "MoB $\times$ PGI Child $\times$ Treated" PGI "PGI Child" treat "Treated" treatxMoBnew "Treated $\times$ MoB" ///
	PGIxtreat "Treated $\times$ PGI" PGI_mothers "PGI Mother" PGI_fathers "PGI Father" PGImumTreat "PGI Mother $\times$ Treated" PGIdadTreat "PGI Father $\times$ Treated" PGImumchild "PGI Mother $\times$ PGI Child" PGIdadchild "PGI Father $\times$ PGI Child" ea "Entry assessment (age 4)" PGIxea "Ent. Ass. $\times$ PGI Child") 


	est drop _all
		







********************************************************************************
**#                           APPENDIX                                       #**
********************************************************************************













/*-----------------------------------------------------------------------------*
* POWER CALCULATIONS (Appendix Section E.2, Figure E.1, appendix E)
* Figure E.1
*-----------------------------------------------------------------------------*/
do "${dircode}/9_GxE_powerCalc.do" //NOTE: for speed, now replications are 10; change to 1000


/*-----------------------------------------------------------------------------*
Table F.1: OLS estimates of the main and interaction effects of being old-for-grade (Treated) 
and the EA PGI on children's test scores, allowing for non-linearities (quadratic) in the PGI effect.
*-----------------------------------------------------------------------------*/
use "${dirdata}/2_cleanData/cleanALSPAC_GxE.dta", clear
keep if window3mth == 1

** Inputs: PGI, treat, and controls
tab MoB treat
global Xvars       MoBnew male YoB92 c_PC* // input: variables to control for in the regression
global tokeep      treat PGI PGIxtreat MoBnew treatxMoBnew PGIxMoBnew PGIxtreatxMoBnew  // variables to keep in the output

** This part could be automated, create interaction terms between PGI, treat, and the demeaned controls (Keller2014 and Lin2013)
qui sum PGI
replace PGI = (PGI-r(mean)) // demean the PGI in the analytical sample (3 month window)
gen PGI2 = PGI^2
gen PGIxtreat = treat * PGI
gen PGI2xtreat = treat * PGI2

qui sum PGI_mothers
replace PGI_mothers = (PGI_mothers-r(mean))/r(sd)
qui sum PGI_fathers
replace PGI_fathers = (PGI_fathers-r(mean))/r(sd)

*G and E iteracted with controls
global Xpgi2       ""
global Xtreat      ""

foreach control of varlist $Xvars {
	qui sum `control'
	gen PGIx`control'   = PGI   * (`control'-r(mean))
	gen PGI2x`control'  = PGI2  * (`control'-r(mean))
	gen treatx`control' = treat * (`control'-r(mean))
	
	//automatically populate the global list of interacted controls
	global Xpgi2  ${Xpgi2}   PGIx`control' PGI2x`control' 
	global Xtreat ${Xtreat} treatx`control'
} 

*GxExControls ---> only one triple interaction, with the RDD running variable, to keep things simple
qui sum MoBnew
gen PGIxtreatxMoBnew = PGIxtreat * (MoBnew -r(mean))
gen PGI2xtreatxMoBnew = PGI2xtreat * (MoBnew -r(mean))

*** Regressions WITH parental PGIs, missing dummies & interactions ***
global tokeep2 "treat PGI PGI2 PGIxtreat MoBnew treatxMoBnew PGIxMoBnew PGI2xMoBnew PGIxtreatxMoBnew PGI2xtreatxMoBnew"
foreach outcome in ea ks1 ks2 ks3 ks4 {
	qui reg `outcome' treat PGI ${Xvars} ${Xtreat}        , cluster(MoB), if window3mth == 1

	gen motherdummy = cond(PGI_mothers==. & e(sample),1,0)
	gen fatherdummy = cond(PGI_fathers==. & e(sample),1,0)
	replace PGI_mothers= 0 if motherdummy == 1 & e(sample)
	replace PGI_fathers= 0 if fatherdummy == 1 & e(sample)
	gen treatXmotherdummy = treat * motherdummy
	gen treatXfatherdummy = treat * fatherdummy
	
foreach control of varlist PGI_mothers PGI_fathers motherdummy fatherdummy {
	qui sum `control'
	gen PGIx`control'   = PGI   * (`control'-r(mean))
	gen treatx`control' = treat * (`control'-r(mean))
} 
	
* Interactions parental PGI and treated: TreatxPGI_mothers and TreatxPGI_fathers 
rename treatxPGI_mothers PGImumTreat 
rename treatxPGI_fathers PGIdadTreat 
rename PGIxPGI_mothers PGImumchild 
rename PGIxPGI_fathers PGIdadchild 

qui eststo `outcome'_gxe_e:   reg `outcome' treat PGI PGI2 PGIxtreat PGIxtreatxMoBnew PGI2xtreatxMoBnew PGI_mothers PGI_fathers motherdummy fatherdummy treatxmotherdummy treatxfatherdummy PGIxmotherdummy PGIxfatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild ${Xvars} ${Xtreat} ${Xpgi2}, cluster(MoB), if window3mth == 1 & e(sample)

esttab `outcome'_gxe_e, b se keep(${tokeep2} PGI_mothers PGI_fathers PGImumTreat PGIdadTreat PGImumchild PGIdadchild) order(${tokeep2} PGI_mothers PGI_fathers PGImumTreat PGIdadTreat PGImumchild PGIdadchild) star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))

drop motherdummy fatherdummy treatXmotherdummy treatXfatherdummy PGIxmotherdummy PGIxfatherdummy treatxmotherdummy treatxfatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild
} 

* Only with parental PGIs and interactions	
esttab ea_gxe_e ks1_gxe_e ks2_gxe_e ks3_gxe_e ks4_gxe_e using "${dirtables}/MoB_G2b.tex", replace ///
	frag bookt b(3) se(3) keep(${tokeep2} PGI_mothers PGI_fathers PGImumTreat PGIdadTreat PGImumchild PGIdadchild ) ///
	order(${tokeep2} PGI_mothers PGI_fathers PGImumTreat PGIdadTreat PGImumchild PGIdadchild ) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("EA" "KS1" "KS2" "KS3" "KS4" "EA" "KS1" "KS2" "KS3" "KS4") nonumber ///
	stats(r2 N, label("R$^2$" "Observations") fmt(3 0)) nonotes ///
	coeflabel(MoBnew "MoB" PGIxMoBnew "MoB $\times$ PGI Child" PGI2xMoBnew "MoB $\times$ PGI Child$^2$" PGIxtreatxMoBnew "MoB $\times$ PGI Child $\times$ Treated" PGI2xtreatxMoBnew "MoB $\times$ PGI Child$^2$ $\times$ Treated" PGI "PGI Child" PGI2 "PGI Child$^2$" treat "Treated" treatxMoBnew "Treated $\times$ MoB" ///
	PGIxtreat "Treated $\times$ PGI Child" PGI_mothers "PGI Mother" PGI_fathers "PGI Father" PGImumTreat "PGI Mother $\times$ Treated" PGIdadTreat "PGI Father $\times$ Treated" PGImumchild "PGI Mother $\times$ PGI Child" PGIdadchild "PGI Father $\times$ PGI Child")





/*-----------------------------------------------------------------------------*
Table F.2: OLS estimates of the main and interaction effects of being old-for-grade (Treated) and the EA
PGI on children's test scores, exploring sensitivity to different bandwidths.
*-----------------------------------------------------------------------------*/

foreach j in 2 4 {
	use "${dirdata}/2_cleanData/cleanALSPAC_GxE.dta", clear
	keep if window`j'mth == 1

	** Inputs: PGI, treat, and controls
	tab MoB treat
	global Xvars       MoBnew male YoB92 c_PC* // input: variables to control for in the regression
	global tokeep      treat PGI PGIxtreat MoBnew treatxMoBnew PGIxMoBnew PGIxtreatxMoBnew  // variables to keep in the output

	** demean child, mother, and father PGI
	qui sum PGI
	replace PGI = (PGI-r(mean))/r(sd)
	gen PGIxtreat = treat * PGI

	qui sum PGI_mothers
	replace PGI_mothers = (PGI_mothers-r(mean))/r(sd)
	qui sum PGI_fathers
	replace PGI_fathers = (PGI_fathers-r(mean))/r(sd)

	* Missing parental PGI: create the parental PGI imputed to 0 if missing
	gen motherdummy = cond(PGI_mothers==. ,1,0)
	gen fatherdummy = cond(PGI_fathers==. ,1,0)
	replace PGI_mothers= 0 if motherdummy == 1
	replace PGI_fathers= 0 if fatherdummy == 1

	*G and E iteracted with controls
	global Xpgi        ""
	global Xtreat      ""

	foreach control of varlist $Xvars {
		qui sum `control'
		gen PGIx`control'   = PGI   * (`control'-r(mean))
		gen treatx`control' = treat * (`control'-r(mean))
		
		//automatically populate the global list of interacted controls
		global Xpgi   ${Xpgi}   PGIx`control'  
		global Xtreat ${Xtreat} treatx`control'
	} 

	foreach control of varlist PGI_mothers PGI_fathers motherdummy fatherdummy {
		qui sum `control'
		gen PGIx`control'   = PGI   * (`control'-r(mean))
		gen treatx`control' = treat * (`control'-r(mean))
	} 

	* Interactions parental PGI and treated: TreatxPGI_mothers and TreatxPGI_fathers 
	rename treatxPGI_mothers PGImumTreat 
	rename treatxPGI_fathers PGIdadTreat 
	rename PGIxPGI_mothers PGImumchild 
	rename PGIxPGI_fathers PGIdadchild 

	*GxExControls ---> only one triple interaction, with the RDD running variable, to keep things simple
	qui sum MoBnew
	gen PGIxtreatxMoBnew = PGIxtreat * (MoBnew -r(mean))

	foreach outcome in ea ks1 ks2 ks3 ks4 {
		qui eststo `outcome'_gxe_e`j':   reg `outcome' treat PGI PGIxtreat PGIxtreatxMoBnew PGI_mothers PGI_fathers motherdummy fatherdummy treatxmotherdummy treatxfatherdummy PGIxmotherdummy PGIxfatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild ${Xvars} ${Xtreat} ${Xpgi}, cluster(MoB)
	}
	drop motherdummy fatherdummy treatxmotherdummy treatxfatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild 
}


esttab ea_gxe_e2 ks1_gxe_e2 ks2_gxe_e2 ks3_gxe_e2 ks4_gxe_e2, b se ///
	keep(${tokeep} PGI_mothers PGI_fathers motherdummy fatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild) ///
	order(${tokeep} PGI_mothers PGI_fathers motherdummy fatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild) star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))
esttab ea_gxe_e4 ks1_gxe_e4 ks2_gxe_e4 ks3_gxe_e4 ks4_gxe_e4, b se ///
	keep(${tokeep} PGI_mothers PGI_fathers motherdummy fatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild) ///
	order(${tokeep} PGI_mothers PGI_fathers motherdummy fatherdummy PGImumTreat PGIdadTreat PGImumchild PGIdadchild) star(* 0.10 ** 0.05 *** 0.01) stats(r2 N, fmt(3 0))

* Save on dirtables 
esttab ea_gxe_e2 ks1_gxe_e2 ks2_gxe_e2 ks3_gxe_e2 ks4_gxe_e2 ea_gxe_e4 ks1_gxe_e4 ks2_gxe_e4 ks3_gxe_e4 ks4_gxe_e4 using "${dirtables}/MoB_bandwidth.tex", replace ///
	frag bookt b(3) se(3) keep(${tokeep} PGI_mothers PGI_fathers PGImumTreat PGIdadTreat PGImumchild PGIdadchild ) ///
	order(${tokeep} PGI_mothers PGI_fathers PGImumTreat PGIdadTreat PGImumchild PGIdadchild ) star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("EA" "KS1" "KS2" "KS3" "KS4" "EA" "KS1" "KS2" "KS3" "KS4") nonumber ///
	stats(r2 N, label("R$^2$" "Observations") fmt(3 0)) nonotes ///
	mgroups("Bandwidth 2 months" "Bandwidth 4 months", pattern(1 0 0 0 0 1 0 0 0 0) span ///
	prefix(\multicolumn{@span}{c}{) suffix(}) erepeat(\cmidrule(lr){@span})) ///
	coeflabel(MoBnew "MoB" PGIxMoBnew "MoB $\times$ PGI Child" PGIxtreatxMoBnew "MoB $\times$ PGI Child $\times$ Treated" PGI "PGI Child" treat "Treated" treatxMoBnew "Treated $\times$ MoB" ///
	PGIxtreat "Treated $\times$ PGI Child" PGI_mothers "PGI Mother" PGI_fathers "PGI Father" PGImumTreat "PGI Mother $\times$ Treated" PGIdadTreat "PGI Father $\times$ Treated" PGImumchild "PGI Mother $\times$ PGI Child" PGIdadchild "PGI Father $\times$ PGI Child" ) 

	est drop _all



/*-----------------------------------------------------------------------------*
Figure G.1: Densities of children's, mothers' and fathers' EA PGI by treatment status.
*-----------------------------------------------------------------------------*/
use "${dirdata}/2_cleanData/cleanALSPAC_GxE.dta", clear
keep if window3mth == 1 // keep only 3 months of each side of the cutoff

local PGIlabel: variable label PGI
local PGIlabelmother: variable label PGI_mother
local PGIlabelfather: variable label PGI_father

* Kdensity plot of treatment over outcome
local lab0 : label treat 0
local lab1 : label treat 1


*------- Differences in the density of the PGI by treatment -------------------*
* Children's PGI
twoway ///
	(kdensity PGI if treat ==0, lp(solid)) ///, ci
	(kdensity PGI if treat ==1, lp(dash)) ///, ci
	, legend(label(1 "`lab0'") label(2 "`lab1'") row(1) position(6)) ///
	  xtitle("`PGIlabel'") ytitle("Density") scheme(s1mono)

	graph export "${dirfigures}/kdens_treat_PGI.png", replace

* Mothers' PGI
twoway ///
	(kdensity PGI_mother if treat ==0, lp(solid)) ///, ci
	(kdensity PGI_mother if treat ==1, lp(dash)) ///, ci
	, legend(label(1 "`lab0'") label(2 "`lab1'") row(1) position(6)) ///
	  xtitle("`PGIlabelmother'") ytitle("Density") scheme(s1mono)

	graph export "${dirfigures}/kdens_treat_PGI_mothers.png", replace

* Fathers' PGI
twoway ///
	(kdensity PGI_father if treat ==0, lp(solid)) ///, ci
	(kdensity PGI_father if treat ==1, lp(dash)) ///, ci
	, legend(label(1 "`lab0'") label(2 "`lab1'") row(1) position(6)) ///
	  xtitle("`PGIlabelfather'") ytitle("Density") scheme(s1mono)

	graph export "${dirfigures}/kdens_treat_PGI_fathers.png", replace



/*-----------------------------------------------------------------------------*
Figure G.2: The G × E coefficients for the Entry Assessment and Key Stage 1–4 test scores.
*-----------------------------------------------------------------------------*/
//--> see above, below the code for Table 4 and 5


/*-----------------------------------------------------------------------------*
Table G.2: OLS estimates of the effect of the parental PGIs for EA on test scores at different ages.
*-----------------------------------------------------------------------------*/
//--> see above, below the code for Table 3

capture log close
