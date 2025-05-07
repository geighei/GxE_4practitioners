# Replication Package for: "The Economics and Econometrics of Gene–Environment Interplay"
## March 4th, 2025

This replication package accompanies:

Biroli, P., Galama, T., von Hinke, S., van Kippersluis, H., Rietveld, C. A., & Thom, K. (forthcoming). "The Economics and Econometrics of Gene–Environment Interplay." Review of Economic Studies.

## Authors

- Pietro Biroli (University of Bologna)
- Titus Galama (University of Southern California and VU University Amsterdam)
- Stephanie von Hinke (University of Bristol; Institute for Fiscal Studies)
- Hans van Kippersluis (Erasmus University Rotterdam)
- Cornelius A. Rietveld (Erasmus University Rotterdam)
- Kevin Thom (University of Wisconsin)

# Data Availability and Provenance Statements

### Statement about rights
The authors of the manuscript have legitimate access to and permission to use the data used in this manuscript.

### Summary of availability
- Some data **cannot be made** publicly available.

### Details on each data source
- The study uses data from the Avon Longitudinal Study of Parents and Children (ALSPAC). Due to confidentiality agreements, access to ALSPAC data must be requested directly from the data providers (University of Bristol). Details on data access can be found at: [https://www.bristol.ac.uk/alspac/](https://www.bristol.ac.uk/alspac/).
- Genetic data used in this study, including polygenic indices, were obtained under controlled access agreements with the ALSPAC and cannot be shared directly. The summary statistics used to construct the polygenic indices were constructed by Dr. Rita Dias Pererira; a more detail description of the coding procedures to generate the polygenic indices and to impute the parental genomes can be found in Dias Pereira and van Kippersluis (2025).

# Computational Requirements

## Software Requirements

- The replication package contains one program to install all dependencies and describes the necessary directory structure ( `0_code/0_runall.do`)

- Stata (code was last run with version 18)
	- estout (version 3.31  26apr2022)
	- coefplot (version 1.8.6  22feb2023)
	- reghdfe (version 6.12.4 12sep2023)
	- polychoric (v.1.5, by Stas Kolenikov)

## Controlled Randomness
- Random seed is set at line 5 of program `0_code/9_GxE_powerCalc.do` (needed only for the power calculations in the Appendix)

## Memory, Runtime, Storage Requirements
Approximate time needed to reproduce the **main analyses** on a standard (2024) desktop machine:
- <5 minutes

Approximate time needed to reproduce the **power-calcations** with 1000 iterations on a standard (2024) desktop machine:
- 10-60 minutes

Approximate storage space needed:
- 25 MB - 250 MB

### Details
The code was last run on a 4-core Intel-based laptop with Windows 11 Enterprise (Version 24H2, OS build 26100.3775), featuring an 11th Gen Intel(R) Core(TM) i7-1165G7 @ 2.80GHz processor and 16 GB RAM.

# Description of Programs/Code

This replication package consists of Stata do-files that reproduce the empirical analyses presented in the manuscript. The folder structure is as follows:

### Folder Structure
- `0_code/` - Contains all Stata do-files.
- `1_rawdata/` - Stores the original datasets used in the study.
- `2_cleanData/` - Contains processed and cleaned datasets ready for analysis.
- `3_output/figures/` - Stores figures generated from the analysis.
- `3_output/tables/` - Stores tables generated from the analysis.

### Code Execution
- `0_code/0_runall.do` - Main script that executes the entire replication package.
- `0_code/1_GxE_dataClean.do` - Cleans and prepares the raw data for analysis.
- `0_code/2_GxE_analysis.do` - Conducts the main empirical analysis.
- `0_code/9_GxE_powerCalc.do` - Performs power calculations.

# Instructions to Replicators

To reproduce the results, check that the folder structure is correct, open Stata, set the working directory to the root folder of the replication package, and run:
```
do 0_code/0_runall.do
```

# List of Tables and Figures

The provided code reproduces:
- All tables and figures in the paper.

| Figure/Table #     | Program                   | Output file                                       |
|--------------------|---------------------------|---------------------------------------------------|
| Table 2            | 0_code/2_GxE_analysis.do  | 3_output/tables/DescrByTreated.tex                |
| Table 3            | 0_code/2_GxE_analysis.do  | 3_output/tables/PredictivePower_23me_ukb.tex      |
| Table 4            | 0_code/2_GxE_analysis.do  | 3_output/tables/MoB_ea.tex                        |
| Table 5            | 0_code/2_GxE_analysis.do  | 3_output/tables/MoB_other.tex                     |
| Table 6            | 0_code/2_GxE_analysis.do  | 3_output/tables/mechan_ea.tex                     |
| Figure 2           | 0_code/2_GxE_analysis.do  | 3_output/figures/MoB.png                          |
| Figure 3           | 0_code/2_GxE_analysis.do  | 3_output/figures/PGIxTreat_ea.png                 |
|--------------------|---------------------------|---------------------------------------------------|
|Appendix Table F.1  | 0_code/2_GxE_analysis.do  | 3_output/tables/PredictivePower_23me_ukb_app.tex  |
|Appendix Table F.2  | 0_code/2_GxE_analysis.do  | 3_output/tables/MoB_bandwidth.tex                 |
|Appendix Table G.2  | 0_code/2_GxE_analysis.do  | 3_output/tables/MoB_G2.tex                        |
|Appendix Figure E.1 | 0_code/9_GxE_powerCalc.do | 3_output/figures/power-png                        |
|Appendix Figure G.1 | 0_code/2_GxE_analysis.do  | 3_output/figures/kdens_treat_PGI.png              |
|                    |                           | 3_output/figures/kdens_treat_PGI_mothers.png      |
|                    |                           | 3_output/figures/kdens_treat_PGI_fathers.png      |
|Appendix Figure G.2 | 0_code/2_GxE_analysis.do  | 3_output/figures/coefplot_gxe.png                 |

Note: to fully replicate the power calculations, change the replications to 1000

# License for Code

The code is licensed under a Creative Commons Attribution-NonCommercial 4.0 International License (CC BY-NC 4.0). 

# References

- Biroli, P., Galama, T., von Hinke, S., van Kippersluis, H., Rietveld, C. A., & Thom, K. (forthcoming). "The Economics and Econometrics of Gene–Environment Interplay." [Journal Name]. DOI: [Insert DOI here].
- ALSPAC: Boyd, A., et al. (2013). "Cohort Profile: The 'Children of the 90s'--the index offspring of the Avon Longitudinal Study of Parents and Children." *International Journal of Epidemiology*.
- Rita Dias Pereira, Hans van Kippersluis. (2025) "Intergenerational education persistence: Using Molecular genetic data to quantify the role of genetic transmission" *Mimeo*

For further details or assistance in replication, please contact pietrobiroli@gmail.com
