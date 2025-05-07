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

Each do file provides more information on what files are needed for input/output
*/


set more off
clear all
capture log close


/* commands to install */
program main
    * *** Add required packages from SSC to this list ***
    local ssc_packages "estout coefplot"
    * *** Add required packages from SSC to this list ***

    if !missing("`ssc_packages'") {
        foreach pkg in "`ssc_packages'" {
        * install using ssc, but avoid re-installing if already present
            capture which `pkg'
            if _rc == 111 {                 
               dis "Installing `pkg'"
               quietly ssc install `pkg', replace
               }
        }
    }

    * Install packages using net, but avoid re-installing if already present
    capture which polychoric
       if _rc == 111 {
        quietly net from "http://staskolenikov.net/stata"
        quietly cap ado uninstall polychoric
        quietly net install polychoric
       }

    capture which reghdfe
       if _rc == 111 {
        quietly net from "https://raw.githubusercontent.com/sergiocorreia/reghdfe/master/src/"
        quietly cap ado uninstall reghdfe
        quietly net install reghdfe
       }
	   
end
*






	global dirdata    "/Users/`c(username)'/Documents/data/alspac/GxE_metrics_replication_data"  // this is not publicly available
	global dirpackage "/Users/`c(username)'/Downloads/GxE_metrics_replication_package"		// change this to the folder where you downloaded the replication package


global dircode    "${dirpackage}/0_code/"
global dirtables  "${dirpackage}/3_output/tables"
global dirfigures "${dirpackage}/3_output/figures"







***************************************************************************

do "${dircode}/1_GxE_dataClean.do"

do "${dircode}/2_GxE_analysis.do"

***************************************************************************