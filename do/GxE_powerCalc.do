*DO POWER CALCULATIONS FOR EFFECT SIZE & SAMPLE SIZE AT THE SAME TIME
*KEVIN THOM WROTE IT IN MATLAB AND HANS VAN KIPPERSLUIS ADAPTED IT TO STATA. 
*18 MAY 2020
*25 OCT 2020 PIETRO CHANGED IT FOR THE EMPIRICAL APPLICATION (PUTTING A BINARY ENVIRONMENT)

capture clear all
set more off
set seed 100
set matsize 11000
set scheme plotplainblind


* SET YOUR DIRECTORY
*global dirdropbox  "/Users/`c(username)'/Dropbox/GEIGHEI/projects/GxE_4practitioners"
global dirdropbox  "/mnt/data/Dropbox/GEIGHEI/projects/GxE_4practitioners" //for pietro's work computer

cd ${dirdropbox}

*SET NUMBER OF REPLICATIONS
local repl = 1000

*SET SAMPLE SIZES: there will be one line in the graph for each sample size
*here I have 4 different regressions whose power I want to test, and we know the sample size for each, so I set it to be equal
mat samplesizes = (1000 \ 3500 \ 3500 \ 3000 \ 3500)


**------------ SET PARAMETER VALUES
* Estimated effect size of G, E, and GxE
scalar beta_0 = 0.00     // constant: our outcomes are mean 0 and std 1
scalar beta_G = 0.259    // effect of G: taken from Allegrini et al. 2019 https://www.nature.com/articles/s41380-019-0394-4
mat    beta_E = (0.90 \ 0.60 \ 0.35 \ 0.20 \ 0.13)     // effect of E: taken from Crawford et al. (2010); one for each key-stage; has to be the same lenght as samplesizes
*scalar beta_E = 0.60     // effect of E

*SET EFFECT SIZES INTERACTION TERM: these are the grid-points that will be plotted in the graph
mat beta_int = J(13,1,.)
local iter=0
forvalues grid=0(0.025)0.3{
	local iter=`iter'+1
 	mat beta_int[`iter',1] = `grid'
}

* Distrubtion of the G and E variables
scalar sd_G = 1.00    // standard dev of the genes, equal to 1 for PGS
*scalar sd_E = 0.5    // standard dev of the environemnt, for continuous E
scalar share_E = 0.5  // share of treated, for binary E (in our case, we have a balanced sample around the Aug/Sept cutoff)
scalar sd_error = 1.00 // standard dev of the error term, in our case the outcome is variance 1.


*
*
*
*
*
*
*
*    DO NOT CHANGE FROM HERE DOWN 
*
*
*
*
*
*
*





**---------- DO REPLICATIONS AND COMPUTE NUMBER OF TIMES INTERACTION COEFFICIENT IS SIGNIFICANT (POWER)
local ss    = rowsof(samplesizes)
local betas = rowsof(beta_int)
local rows  = `betas'
mat pvalues = J(`rows',`repl',0)
mat sig     = J(`rows',`repl',0)
mat siglr   = J(`rows',`repl',0)
mat power   = J(`rows',1,0)

forvalues kSample = 1/`ss' {
	di "Looping over kSample `kSample'"
    forvalues jGrid = 1/`betas' {
		di "Looping over jGrid `jGrid'"
		scalar nsig = 0
		forvalues iRepl = 1/`repl' {
			*di "Looping over iRepl `iRepl'"
			capture clear
			local N = samplesizes[`kSample',1]
			qui set obs `N'
			
			*gen E = rnormal(0,sd_E)      // use this for continuous E
			gen E = (runiform()>share_E)  // use this for dichotomous E; equal to 1 if random uniform distribution is bigger than sahre of treated
			gen G = rnormal(0,sd_G)
			local beta_GxE  = beta_int[`jGrid',1]
			*gen Y = beta_0 + beta_G * G + beta_E * E + `beta_GxE' * (G*E) + rnormal(0,sd_error)   // use this one if there's only one beta_E
			local beta_Enum = beta_E[`kSample',1]   //we have one different beta_E for each sample/outcome, if not simply use the scalar beta_E
			gen Y = beta_0 + beta_G * G + `beta_Enum' * E + `beta_GxE' * (G*E) + rnormal(0,sd_error)
		
			*COMPUTE P-VALUE FOR MODEL WITH ROBUST STANDARD ERRORS
			qui reg Y G E c.G#c.E, robust
			mat temp = r(table)
			mat pvalues[`jGrid',`iRepl'] = temp[4,3]
			mat sig[`jGrid',`iRepl'] = (pvalues[`jGrid',`iRepl'] < 0.05)
		
			*COMPUTE LIKELIHOOD RATIO TEST FOR FULL VS REDUCED MODEL WITHOUT ROBUST STANDARD ERRORS
			qui reg Y G E c.G#c.E
			estimates store full
			qui reg Y G E
			estimates store reduced
			qui lrtest full reduced
			mat siglr[`jGrid',`iRepl'] = (r(p)<0.05)
			}
		mata : st_matrix("power`kSample'", rowsum(st_matrix("sig")/`repl'))
		mata: st_matrix("powerlr`kSample'", rowsum(st_matrix("siglr")/`repl'))
	}
	mat repl`kSample' = J(`betas',1,`repl')
	mat overall_power`kSample' = (beta_int, power`kSample', powerlr`kSample', repl`kSample')
	matrix colnames overall_power`kSample' = beta power`kSample' powerlr`kSample' replications
}

*STORE RESULTS IN A MATRIX AND PLOT THE RESULTS
forvalues kSample = 1/`ss' {
	svmat overall_power`kSample', names(matcol)
    local ss`kSample' = samplesizes[`kSample',1]
}

*I ONLY DO GRAPHS FOR THE T-TEST, NOT THE LR TEST
if `ss'==1 {
twoway (line overall_power1power1 overall_power1beta) ///
       , ytitle("Power") xtitle("Coefficient of the GxE  interaction term") ///
         note("Note: Number of replications `repl'") ///
         legend( pos(6) lab(1 "Sample size `ss1'"))
}
if `ss'==2 {
twoway (line overall_power1power1 overall_power1beta) ///
       (line overall_power2power2 overall_power2beta) ///
       , ytitle("Power") xtitle("Coefficient of the GxE  interaction term") ///
         note("Note: Number of replications `repl'") ///
         legend( pos(6) lab(1 "Sample size `ss1'") lab(2 "Sample size `ss2'"))
}
if `ss'==3 {
twoway (line overall_power1power1 overall_power1beta) ///
       (line overall_power2power2 overall_power2beta) ///
       (line overall_power3power3 overall_power3beta) ///
       , ytitle("Power") xtitle("Coefficient of the GxE  interaction term") ///
         note("Note: Number of replications `repl'") ///
         legend( pos(6) lab(1 "Sample size `ss1'") lab(2 "Sample size `ss2'") lab(3 "Sample size `ss3'"))

graph export ${dirdropbox}/figures/power.pdf, replace

}
if `ss'==4 {
twoway (line overall_power1power1 overall_power1beta) ///
       (line overall_power2power2 overall_power2beta) ///
       (line overall_power3power3 overall_power3beta) ///
       (line overall_power4power4 overall_power4beta) ///
       , ytitle("Power") xtitle("Coefficient of the GxE  interaction term") ///
         note("Note: Number of replications `repl'") ///
         legend( pos(6) lab(1 "Sample size `ss1'") lab(2 "Sample size `ss2'") lab(3 "Sample size `ss3'") lab(4 "Sample size `ss4'"))
}


/* AD HOC FOR THE PAPER */
if `ss'==5 {
twoway (line overall_power1power1 overall_power1beta) ///
       (line overall_power2power2 overall_power2beta) ///
       (line overall_power3power3 overall_power3beta) ///
       (line overall_power4power4 overall_power4beta) ///
       (line overall_power5power5 overall_power5beta) ///
       , ytitle("Power") xtitle("Coefficient of the GxE  interaction term") ///
         note("Note: Number of replications `repl'") ///
         legend( pos(6) lab(1 "Entry Assessment (Age 4)") lab(2 "Key Stage 1 (Age 7)") lab(3 "Key Stage 2 (Age 11)") lab(4 "Key Stage 3 (Age 14)") lab(5 "Key Stage 4 (Age 16)"))
}


graph export ${dirdropbox}/figures/power.pdf, replace
	


