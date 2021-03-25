***************************************************************************
***** TRYING TO UNDERSTAND WTF IS HAPPENING WITH THE INTERACTION WITH MOB
***** Pietro Biroli, March 2021
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
use "${dirdata}/Data_Set/cleanALSPAC4application.dta", clear
keep if window3mth == 1





** Inputs: PGS, treat, and controls
pwcorr PGS pgs_children*
tab MoB treat
global Xvars       MoBnew male YoB92 c_PC* // input: variables to control for in the regression

capture drop MoBnew

gen MoBnew = MoB-9

*1----------- CREATE INTERACTION terms between PGS, treat, and the demeaned controls (Keller2014 and Lin2013)
*GxE interaction
qui sum PGS
replace PGS = (PGS-r(mean))
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






eststo clear
*------- Regressions
*1 BASELINE: what I think shoudl go in the paper ---> everything is demeaned *on our sample*
eststo base:   reg ea treat PGS PGSxtreat PGSxtreatxMoBnew ${Xvars} ${Xtreat} ${Xpgs}, cluster(MoB), if window3mth == 1

*2 "normal" interaction with MoB
eststo MoB:   reg ea i.treat##c.PGS##c.MoB    ${Xvars} ${Xtreat} ${Xpgs}, cluster(MoB), if window3mth == 1

*3 "normal" interaction with MoBnew
eststo MoBnew:   reg ea i.treat##c.PGS##c.MoBnew    ${Xvars} ${Xtreat} ${Xpgs}, cluster(MoB), if window3mth == 1

*4 what we had before ---> demeaned, but on the whole sample, not on the one with the 3-month-window 
eststo old:   reg ea treat treat_PGS PGS treat_MoB MoB MoB_PGS_treat  ${Xvars} ${Xtreat} ${Xpgs}, cluster(MoB), if window3mth == 1

esttab, keep(PGSxtreat treat_PGS 1.treat#c.PGS)

*---------- Sample splitting: it gives a different sign of the interaction depending on whether we split by PGS or by treat.
*Splitting by high-low PGS 
eststo highPGS:   reg ea treat ${Xvars} ${Xtreat}, cluster(MoB), if window3mth == 1 & highPGS ==1
eststo lowPGS:    reg ea treat ${Xvars} ${Xtreat}, cluster(MoB), if window3mth == 1 & highPGS ==0

*Splitting by treat-control
eststo treat:   reg ea PGS ${Xvars}, cluster(MoB), if window3mth == 1 & treat==1
eststo control:    reg ea PGS ${Xvars}, cluster(MoB), if window3mth == 1 & treat==0

esttab highPGS lowPGS treat control, keep(treat PGS)
