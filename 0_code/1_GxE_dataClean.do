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


Data:
in: 
		1. ALSPAC phenotypes 
				"${dirdata}/1_rawData/Rietveld_12Sep18.dta"
		2. Children's principal components of the genomewide matrix
				"${dirdata}/1_rawData/ALSPAC_pc_10_CHILDREN.dta"
		3. PGI child
				"${dirdata}/1_rawData/PGS_children_23me_ukb.dta"
		4. PGI mother
				"${dirdata}/1_rawData/PGS_mothers_23me_ukb.dta"
		5. PGI father 
				"${dirdata}/1_rawData/PGS_fathers_23me_ukb.dta"


out: "${dirdata}/2_cleanData/cleanALSPAC_GxE"

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




********************************************************************************
cd "${dircode}"
log using 1_GxE_dataClean.log, replace

//rm "${dirdata}/2_cleanData/ALSPAC_GxE"



*------------------------------------------------------------------------------*
* CLEAN THE DATA
*------------------------------------------------------------------------------*

	****************************************************************************
	**# 					MERGE PHENOTYPIC DATA 							****
	**** 					  WITH GENETIC DATA 							****
	****************************************************************************

	use 	"${dirdata}/1_rawData/Rietveld_12Sep18.dta", clear


	********************************
	*** Merge with genetic data
	* Merge in children's PCs
	egen 	id_child = concat(cidB2492 qlet)
	merge 	1:1 id_child using "${dirdata}/1_rawData/ALSPAC_pc_10_CHILDREN.dta"
	drop 	if _m<3
	drop 	_merge

	* Merge in children's meta-analysed PGI 
	merge 	1:1 id_child using "${dirdata}/1_rawData/PGS_children_23me_ukb.dta"
	drop 	if _m<3
	drop 	_merge

	* Merge in mothers' meta-analysed PGI
	gen 	tmp = id_child
	replace id_child = subinstr(id_child,"A","M",1)
	merge 	1:1 id_child using "${dirdata}/1_rawData/PGS_mothers_23me_ukb.dta"
	drop 	if _m==2 	// drop those only in using data
	replace id_child = tmp
	drop 	_merge tmp

	* Merge in meta-analysed (imputed) paternal PGI
	merge 	m:1 cidB2492 using "${dirdata}/1_rawData/PGS_fathers_23me_ukb.dta"
	drop _m

	* Standardize all PGI to have mean 0 and standard deviation 1 in this sample
	foreach PGI of varlist pgs_* {
		qui sum `PGI'
		replace `PGI' = (`PGI'-r(mean))/(r(sd))
	}

	********************************
	*** Phenotypic data
	* Month of birth
	gen 	MoB = ka498
	recode 	MoB (-9999=.)
	label 	def MoB 1 "Jan" 2 "Feb" 3 "Mar" 4 "Apr" 5 "May" 6 "Jun" 7 "Jul" 8 "Aug" 9 "Sep" 10 "Oct" 11 "Nov" 12 "Dec"
	label 	val MoB MoB
	label 	var MoB "Month of birth (MoB)"

	* Year-Month of birth
	gen 	YoB = mz024b
	recode 	YoB (-9999=.)
	label 	var YoB "Year of birth"
	tab 	YoB, gen(YoB)
	rename 	YoB1 YoB91
	rename 	YoB2 YoB92
	rename 	YoB3 YoB93
	label var YoB92 "Indicator for born in 1992"
	gen 	YMoB = ym(YoB,MoB)
	format 	YMoB %tm
	label 	var YMoB "Year-month of birth"
	*

	* Gender
	gen 	male = kz021
	recode 	male (2=0) (-9999 -1 = .)
	label 	var male "Male"

	* Entry assessment score
	recode 	sat092b (-10 -6 -5 = .)
	sum 	sat092b 
	//standardized to be mean 0 std 1 in the sample
	gen 	ea = (sat092b - r(mean)) / r(sd)  
	label 	var ea "Entry Assessment score (age 4)"

	* Key stage 1 
	recode 	sat190a (-10 -6 -5 = .)
	sum 	sat190a
	//standardized to be mean 0 std 1 in the sample
	gen 	ks1 = (sat190a - r(mean)) / r(sd)
	label 	var ks1 "Key stage 1 summary score (age 7)"

	* Key stage 2
	foreach varname in k2_tote k2_totm k2_tots {
		//missings
		replace `varname' = " " if inlist(`varname',"_NV","-10","A","B","L","T","X","Z")
	}
	destring k2_tote, gen(k2e)
	destring k2_totm, gen(k2m)
	destring k2_tots, gen(k2s)
	egen 	k2 = rsum(k2e k2m k2s) if k2e<. & k2m<. & k2s<.
	sum 	k2 if k2e<. & k2m<. & k2s<.
	//standardized to be mean 0 std 1 in the sample
	gen 	ks2 = (k2 - r(mean)) / r(sd) if k2e<. & k2m<. & k2s<.
	label 	var ks2 "Key stage 2 summary score (age 11)"
	drop 	k2e k2m k2s k2

	* Key stage 3
	foreach varname in k3_tote k3_totm {
		replace `varname' = " " if inlist(`varname',"-10","A","IN","M","V")
	}
	destring k3_tote, gen(k3e)
	destring k3_totm, gen(k3m)
	egen 	k3 = rsum(k3e k3m) if k3e<. & k3m<.
	sum 	k3 if k3e<. & k3m<. 
	//standardized to be mean 0 std 1 in the sample
	gen 	ks3 = (k3 - r(mean)) / r(sd) if k3e<. & k3m<.
	label 	var ks3 "Key stage 3 summary score (age 14)"
	drop 	k3e k3m k3 

	* Key stage 4
	replace ks4_ptstnewe = . if inlist(ks4_ptstnewe,-10,-1,0)
	sum 	ks4_ptstnewe 
	//standardized to be mean 0 std 1 in the sample
	gen 	ks4 = (ks4_ptstnewe - r(mean)) / r(sd)
	label 	var ks4 "Key stage 4 summary score (age 16)"
	
	drop ks4_hgmath

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
	label 	var bw "Child's birthweight (in Kg)"

	* Smoking during pregnancy
	recode 	a200 (-9999 -7 -1 = .)
	gen 	smokepreg = (a200>0) if a200<.
	label 	var smokepreg "Mother smoked cigarettes during pregnancy*"
	drop 	a200

	* Marital status
	recode 	a525 (-9999 -1 = .) (2 3 4 5 6 = 1) (1=0)
	rename 	a525 married
	label 	def a525 0 "single" 1 "married", modify
	label 	val married a525
	label 	var married "Mother's marital status*"

	* Mother's age at first pregnancy
	recode 	b023 (-9999 -1 =.)
	rename 	b023 mumagepreg
	label 	var mumagepreg "Mother's age at first pregnancy (years)"

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
	label 	var mumed1 "Mother has CSE*"
	label 	var mumed2 "Mother has vocational training*"
	label 	var mumed3 "Mother has O-levels*"
	label 	var mumed4 "Mother has A-levels*"
	label 	var mumed5 "Mother has a university degree*"
	drop 	mumeduc

	* Partner's highest educ qualification
	recode 	c666a (-9999 -1 = .)
	rename 	c666a dadeduc
	tab 	dadeduc, gen(daded)
	label 	var daded1 "Father has CSE*"
	label 	var daded2 "Father has vocational*"
	label 	var daded3 "Father has O-levels*"
	label 	var daded4 "Father has A-levels*"
	label 	var daded5 "Father has university degree*"
	drop 	dadeduc

	* Mother's social class
	recode 	c755 (-9999 -1 = .) (65=3)
	rename 	c755 socclass
	tab 	socclass, gen(SC)
	label 	var SC1 "Mother in Social Class I*"
	label 	var SC2 "Mother in Social Class II*"
	label 	var SC3 "Mother in Social Class III (non-manual)*"
	label 	var SC4 "Mother in Social Class III (manual)*"
	label 	var SC5 "Mother in Social Class IV*"
	label 	var SC6 "Mother in Social Class V*"
	drop 	socclass



* rename PGI for simplicity
gen PGI     = pgs_children_23me_ukb		// meta-analysed original children's PGI to maximise sample 
label var PGI "PGI Child"

gen PGI_mothers = pgs_mothers_23me_ukb 	// meta-analysed original mothers' PGI 
label var PGI_mothers "PGI Mother"

gen PGI_fathers = pgs_fathers_23me_ukb 	// meta-analysed imputed paternal PGI (i.e., trio sample)
label var PGI_fathers "PGI Father"


* Create sample windows of n-months from the cutoff
forvalues width=2/5{
	gen window`width'mth = (MoBnew <=`width'-1 & MoBnew>(-`width'-1)) if MoBnew<.
}
tab MoB window3mth

save "${dirdata}/2_cleanData/cleanALSPAC_GxE", replace

log close
