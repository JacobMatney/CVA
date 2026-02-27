# Cerebrovascular-Researchers-Toolbox

---
Title: "Cerebrovascular Researcher's ToolBox"
Author: "Jacob Matney"
Date: "2025-09-02"
---

# Introduction

The calculation of critical closing pressure (CrCP) and resistance area product (RAP) has the capability to enhance our understanding of cerebrovascular hemodynamics by estimating vascular activity using a two-component model rather than the traditional single-component model of cerebrovascular conductance/resistnace indices. In doing so, researchers can *estimate* the activity of cerebral vessels at various branching of the vascular tree, as well as *estimate* metabolic and myogenic-mediated activity of the brain's vasculature in response to stress, respectively. However, the quantification of these variables requires high data quality, considerable data processing, and careful calculation, of which accumulates to a high burden on researchers when compared to traditional single-component models. Because of this, progress towards utilizing stronger, more physiologically interpretable analyses of vascular activity rather than the traditional convenient calculations has been slow.

This shiny app provided here within is based on mathematical calculations set forth by [R.B Panerai](https://linkinghub.elsevier.com/retrieve/pii/S1350453303000274) and colleagues since 2003. In particular, this application calculates CrCP and RAP utilizing the 2-point mean method as described by [Panerai et al. (2011).](https://iopscience.iop.org/article/10.1088/0967-3334/32/4/007) Calculations within the subcomponent analysis of autoregulation follows those set forth by [Beishon et al. (2018)](http://doi.wiley.com/10.14814/phy2.13803). Additionally, this program calculates estimated cerebral perfusion pressure (CPP) by adjusting mean arterial pressure for the effects of the hydrostatic column (CPP = MAP - (0.7355 * probe distance (cm)), as well as calculates cerebral pulsatility index for the inputed data. 

# Setting Up R and R Studio

To run this application on your system you must install R and R studio, both of which are freely available and open source.

After installation, launch R studio and install the "shiny" package utilizing `install.packages('shiny')`. This allows for you to launch the application directly from the GitHub repository.

The application has other R packages within itself and should automatically install once accessed, however if there is errors with these installs, the following are required so that you may install manually, if needed:

1.  shiny,
2.  shinydashboard
3.  shinythemes
4.  shinyBS
5.  tidyverse
6.  ggplot2
7.  lme4
8.  readxl
9.  spiro
10. signal
11. zoo,
12. writexl
13. MESS,
14. dplyr
15. bslib
16. plotly
17. shinyjs

All of these packages can be manually installed using `install.packages('<package name>')`

# Running the Application

After installation of R and rshiny, the application can be run without the need of direct download. In the console, enter the `shiny::runGitHub("CRToolbox", "JacobMatney", ref = "main"")`.

The application should then open on your default browser. By utilizing this method, you are ensuring that you are always using the most up-to-date code available.

## Preparing Your Data

Within this repository there is a excel template entitled "CrCP and RAP Template". You can download this file selecting the file within GitHub and then select the "download raw file" button located on the right of the screen.

![](https://github.com/JacobMatney/CRToolbox/blob/main/IMGs/Download_Template.PNG?raw=true)
<br>

To run your own data, you will need to generate and prepate your own data onto this template. Data should be exported as a raw data, where mean arterial pressure data should be placed under "reBAP", middle cerebral artery velocity data should be placed under "LMCAv", End-tidal CO2 volume should be placed under "PETCO2"", and the comments that denote each R-wave is placed under "Beat_Num". You do not need to place any data within the "Sample" column. Examples of filled template can be seen under the GitHub repository.

![](https://github.com/JacobMatney/CRToolbox/blob/main/IMGs/Filled_Template.PNG?raw=true)
<br>

Place baseline values within the "Cond-\_Baseline" and stimulus values within the "Cond-\_Stim". You can place both baseline and stimulus data for 2 conditions in the same file (CondA = Condition A, CondB = CondB). You will be able to select which condition you analyze within the application.

![](https://github.com/JacobMatney/CRToolbox/blob/main/IMGs/Conditions_Template.PNG?raw=true)
<br>

## Working Within the Application

To begin analyzing data, select your file by clicking on the "Browse" button. This will automatically begin the analysis of your data with the default settings provided within the app. Note: Analysis will begin with condition A; if you wish to perform condition B, select Condition B under "select condition". 

For the calculation of absolute cerebrovascular variables (MCAv, CrCP, RAP, CVCi, CPP, CPI, and MAP), select the appropriate settings using the settings under "CrCP and RAP Analyzer". Height correction reflects the distance (cm) above the heart to the TCD probe. If participant was in supine position, this value should be 0. Baseline Steady State reflects the number of seconds from the end of baseline that you wish to be averaged to reflect steady state. Similarly, Stimulus steady state reflects the number of seconds from the end of the stimulus stage that you wish to be averaged to reflect steady state. 

Settings for the subcomponent analysis of autoregulation can be found under "Subcomponent Analyzer" section to the left of the screen. Using the initial value average setting, select the amount of time in seconds from the end of baseline you wish to average to produce inital values of velocity, RAP, MAP, and CrCP. This may or may not be the same averaging duration as the absolute values. Moreso, to determine the steady state contribution of each subcomponent during stimulation, select the number of seconds from the end of the stimulus stage you wish to average to produce steady state values. 

For both absolute and subcomponent values, normalized mean square error (NMSE) values were calculated to check the fit of the models. For absolute calculations, if estimations resulted in a NMSE less than 0.1, the averaged values will appear green and are trusted to be a good fit. If values are above this threshold, data will appear red and could reflect poor data quality and lack of fit. Similarily, subcomponent results will also show green with good fit (MSNE less than 1.5), and red for poor fit (MNSE great than 1.5). 

Each time a setting is changed, a recalculation of the appropriate measures will occur. 

## Exporting Results

You can export the current results within the analyzer by naming the file you wish to save under the "Export" tab. Once a name has been given, an export button will appear and allow you to download an excel file containing calculated dataframes, averaged results, and MSNE score. 

# Conclusion

This application provides an easy to use tool to reliably calculated CrCP and RAP, as well as perform subcompoenent analysis of autoregulation. Although the amount of time each stimulus may be recorded, and the needed time for steady state hemodynamics may differ, this application tries to minimize these issues. If, when using this code, you stumble across a critical error, please provide an example of the error that can be reproduced and fixed.

# Referencing
