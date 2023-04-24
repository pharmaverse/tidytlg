#' ADSL data created from subsetting the CDISC ADSL with 15 subjects (5 subjects in each arm)
#'
#' @source CDISC SDTM/ADAM Pilot Project.
#'
#' @format A data frame with 15 rows and 49 variables:
#' \describe{
#'  \item{STUDYID}{Study Identifier}
#'  \item{USUBJID}{Unique Subject Identifier}
#'  \item{SUBJID}{Subject Identifier for the Study}
#'  \item{SITEID}{Study Site Identifier}
#'  \item{SITEGR1}{Pooled Site Group 1}
#'  \item{ARM}{Description of Planned Arm}
#'  \item{TRT01P}{Planned Treatment for Period 01}
#'  \item{TRT01PN}{Planned Treatment for Period 01 (N)}
#'  \item{TRT01A}{Actual Treatment for Period 01}
#'  \item{TRT01AN}{Actual Treatment for Period 01 (N)}
#'  \item{TRTSDT}{Date of First Exposure to Treatment}
#'  \item{TRTEDT}{Date of Last Exposure to Treatment}
#'  \item{TRTDUR}{Duration of Treatment (days)}
#'  \item{AVGDD}{Avg Daily Dose (as planned)}
#'  \item{CUMDOSE}{Cumulative Dose (as planned)}
#'  \item{AGE}{Age}
#'  \item{AGEGR1}{Pooled Age Group 1}
#'  \item{AGEGR1N}{Pooled Age Group 1 (N)}
#'  \item{AGEU}{Age Units}
#'  \item{RACE}{Race}
#'  \item{RACEN}{Race (N)}
#'  \item{SEX}{Sex}
#'  \item{ETHNIC}{Ethnicity}
#'  \item{SAFFL}{Safety Population Flag}
#'  \item{ITTFL}{Intent-To-Treat Population Flag}
#'  \item{EFFFL}{Efficacy Population Flag}
#'  \item{COMP8FL}{Completers of Week 8 Population Flag}
#'  \item{COMP16FL}{Completers of Week 16 Population Flag}
#'  \item{COMP24FL}{Completers of Week 24 Population Flag}
#'  \item{DISCONFL}{Did the Subject Discontinue the Study?}
#'  \item{DSRAEFL}{Discontinued due to AE?}
#'  \item{DTHFL}{Subject Died?}
#'  \item{BMIBL}{Baseline BMI (kg/m^2)}
#'  \item{BMIBLGR1}{Pooled Baseline BMI Group 1}
#'  \item{HEIGHTBL}{Baseline Height (cm)}
#'  \item{WEIGHTBL}{Baseline Weight (kg)}
#'  \item{EDUCLVL}{Years of Education}
#'  \item{DISONSDT}{Date of Onset of Disease}
#'  \item{DURDIS}{Duration of Disease (Months)}
#'  \item{DURDSGR1}{Pooled Disease Duration Group 1}
#'  \item{VISIT1DT}{Date of Visit 1}
#'  \item{RFSTDTC}{Subject Reference Start Date/Time}
#'  \item{RFENDTC}{Subject Reference End Date/Time}
#'  \item{VISNUMEN}{End of Trt Visit (Vis 12 or Early Term.)}
#'  \item{RFENDT}{Date of Discontinuation/Completion}
#'  \item{DCDECOD}{Standardized Disposition Term}
#'  \item{EOSSTT}{End of Study Status}
#'  \item{DCREASCD}{Reason for Discontinuation}
#'  \item{MMSETOT}{MMSE Total}
#'
#' }
#' @keywords datasets CDISC adsl
#' @name cdisc adsl
"cdisc_adsl"

#'  ADAE data created from subsetting the CDISC ADAE dataset
#'
#' @source CDISC SDTM/ADAM Pilot Project.
#'
#' @format A data frame with 84 rows and 55 variables:
#' \describe{
#'  \item{STUDYID}{Study Identifier}
#'  \item{SITEID}{Study Site Identifier}
#'  \item{USUBJID}{Unique Subject Identifier}
#'  \item{SUBJID}{Subject Identifier for the Study}
#'  \item{TRTA}{Actual Treatment}
#'  \item{TRTAN}{Actual Treatment (N)}
#'  \item{AGE}{Age}
#'  \item{AGEGR1}{Pooled Age Group 1}
#'  \item{AGEGR1N}{Pooled Age Group 1 (N)}
#'  \item{RACE}{Race}
#'  \item{RACEN}{Race (N)}
#'  \item{SEX}{Sex}
#'  \item{SAFFL}{Safety Population Flag}
#'  \item{TRTSDT}{Date of First Exposure to Treatment}
#'  \item{TRTEDT}{Date of Last Exposure to Treatment}
#'  \item{ASTDT}{Analysis Start Date}
#'  \item{ASTDTF}{Analysis Start Date Imputation Flag}
#'  \item{ASTDY}{Analysis Start Relative Day}
#'  \item{AENDT}{Analysis End Date}
#'  \item{AENDY}{Analysis End Relative Day}
#'  \item{ADURN}{AE Duration (N)}
#'  \item{ADURU}{AE Duration Units}
#'  \item{AETERM}{Reported Term for the Adverse Event}
#'  \item{AELLT}{Lowest Level Term}
#'  \item{AELLTCD}{Lowest Level Term Code}
#'  \item{AEDECOD}{Dictionary-Derived Term}
#'  \item{AEPTCD}{Preferred Term Code}
#'  \item{AEHLT}{High Level Term}
#'  \item{AEHLTCD}{High Level Term Code}
#'  \item{AEHLGT}{High Level Group Term}
#'  \item{AEHLGTCD}{High Level Group Term Code}
#'  \item{AEBODSYS}{Body System or Organ Class}
#'  \item{AESOC}{Primary System Organ Class}
#'  \item{AESOCCD}{Primary System Organ Class Code}
#'  \item{AESEV}{Severity/Intensity}
#'  \item{AESER}{Serious Event}
#'  \item{AESCAN}{Involves Cancer}
#'  \item{AESCONG}{Congenital Anomaly or Birth Defect}
#'  \item{AESDISAB}{Persist or Signif Disability/Incapacity}
#'  \item{AESDTH}{Results in Death}
#'  \item{AESHOSP}{Requires or Prolongs Hospitalization}
#'  \item{AESLIFE}{Is Life Threatening}
#'  \item{AESOD}{Occurred with Overdose}
#'  \item{AEREL}{Causality}
#'  \item{AEACN}{Action Taken with Study Treatment}
#'  \item{AEOUT}{Outcome of Adverse Event}
#'  \item{AESEQ}{Sequence Number}
#'  \item{TRTEMFL}{Treatment Emergent Analysis Flag}
#'  \item{AOCCFL}{1st Occurrence of Any AE Flag}
#'  \item{AOCCSFL}{1st Occurrence of SOC Flag}
#'  \item{AOCCPFL}{1st Occurrence of Preferred Term Flag}
#'  \item{AOCC02FL}{1st Occurrence 02 Flag for Serious}
#'  \item{AOCC03FL}{1st Occurrence 03 Flag for Serious SOC}
#'  \item{AOCC04FL}{1st Occurrence 04 Flag for Serious PT}
#'  \item{CQ01NAM}{Customized Query 01 Name}
#'  \item{AOCC01FL}{1st Occurrence 01 Flag for CQ01}
#' }
#' @keywords datasets CDISC adae
#' @name cdisc adae
"cdisc_adae"

#' ADLB data created from subsetting the CDISC ADLB dataset
#'
#' @source CDISC SDTM/ADAM Pilot Project.
#'
#' @format A data frame with 2154 rows and 46 variables:
#' \describe{
#'  \item{STUDYID}{Study Identifier}
#'  \item{SUBJID}{Subject Identifier for the Study}
#'  \item{USUBJID}{Unique Subject Identifier}
#'  \item{TRTA}{Actual Treatment}
#'  \item{TRTAN}{Actual Treatment (N)}
#'  \item{TRTSDT}{Date of First Exposure to Treatment}
#'  \item{TRTEDT}{Date of Last Exposure to Treatment}
#'  \item{AGE}{Age}
#'  \item{AGEGR1}{Pooled Age Group 1}
#'  \item{AGEGR1N}{Pooled Age Group 1 (N)}
#'  \item{RACE}{Race}
#'  \item{RACEN}{Race (N)}
#'  \item{SEX}{Sex}
#'  \item{COMP24FL}{Completers of Week 24 Population Flag}
#'  \item{DSRAEFL}{Discontinued due to AE?}
#'  \item{SAFFL}{Safety Population Flag}
#'  \item{AVISIT}{Analysis Visit}
#'  \item{AVISITN}{Analysis Visit (N)}
#'  \item{ADY}{Analysis Relative Day}
#'  \item{ADT}{Analysis Date}
#'  \item{VISIT}{Visit Name}
#'  \item{VISITNUM}{Visit Number}
#'  \item{PARAM}{Parameter}
#'  \item{PARAMCD}{Parameter Code}
#'  \item{PARAMN}{Parameter (N)}
#'  \item{PARCAT1}{Parameter Category 1}
#'  \item{AVAL}{Analysis Value}
#'  \item{BASE}{Baseline Value}
#'  \item{CHG}{Change from Baseline}
#'  \item{A1LO}{Analysis Range 1 Lower Limit}
#'  \item{A1HI}{Analysis Range 1 Upper Limit}
#'  \item{R2A1LO}{Ratio to Analysis Range 1 Lower Limit}
#'  \item{R2A1HI}{Ratio to Analysis Range 1 Upper Limit}
#'  \item{BR2A1LO}{Base Ratio to Analysis Range 1 Lower Lim}
#'  \item{BR2A1HI}{Base Ratio to Analysis Range 1 Upper Lim}
#'  \item{ANL01FL}{Analysis 01 - Special Interest Flag}
#'  \item{ALBTRVAL}{Amount Threshold Range}
#'  \item{ANRIND}{Analysis Reference Range Indicator}
#'  \item{BNRIND}{Baseline Reference Range Indicator}
#'  \item{ABLFL}{Baseline Record Flag}
#'  \item{AENTMTFL}{Last value in treatment visit}
#'  \item{LBSEQ}{Sequence Number}
#'  \item{LBNRIND}{Reference Range Indicator}
#'  \item{LBSTRESN}{Numeric Result/Finding in Standard Units}
#' }
#' @keywords datasets CDISC adlb
#' @name cdisc adlb
"cdisc_adlb"


#' ADVS data created from subsetting the CDISC ADVS dataset
#'
#' @source CDISC SDTM/ADAM Pilot Project.
#'
#' @format A data frame with 1938 rows and 35 variables:
#' \describe{
#'  \item{STUDYID}{Study Identifier}
#'  \item{SITEID}{Study Site Identifier}
#'  \item{USUBJID}{Unique Subject Identifier}
#'  \item{AGE}{Age}
#'  \item{AGEGR1}{Pooled Age Group 1}
#'  \item{AGEGR1N}{Pooled Age Group 1 (N)}
#'  \item{RACE}{Race}
#'  \item{RACEN}{Race (N)}
#'  \item{SEX}{Sex}
#'  \item{SAFFL}{Safety Population Flag}
#'  \item{TRTSDT}{Date of First Exposure to Treatment}
#'  \item{TRTEDT}{Date of Last Exposure to Treatment}
#'  \item{TRTP}{Planned  Treatment}
#'  \item{TRTPN}{Planned  Treatment (N)}
#'  \item{TRTA}{Actual Treatment}
#'  \item{TRTAN}{Actual Treatment (N)}
#'  \item{PARAMCD}{Parameter Code}
#'  \item{PARAM}{Parameter}
#'  \item{PARAMN}{Parameter (N)}
#'  \item{ADT}{Analysis Date}
#'  \item{ADY}{Analysis Relative Day}
#'  \item{ATPTN}{Analysis Timepoint (N)}
#'  \item{ATPT}{Analysis Timepoint}
#'  \item{AVISIT}{Analysis Visit}
#'  \item{AVISITN}{Analysis Visit (N)}
#'  \item{AVAL}{Analysis Value}
#'  \item{BASE}{Baseline Value}
#'  \item{BASETYPE}{Baseline Value}
#'  \item{CHG}{Change from Baseline}
#'  \item{PCHG}{Percent Change from Baseline}
#'  \item{VISITNUM}{Visit Number}
#'  \item{VISIT}{Visit Name}
#'  \item{VSSEQ}{Sequence Number}
#'  \item{ANL01FL}{Analysis 01 - Special Interest Flag}
#'  \item{ABLFL}{Baseline Record Flag}
#' }
#' @keywords datasets CDISC advs
#' @name cdisc advs
"cdisc_advs"


#' Metadata describing the data, functions and arguments needed to produce your
#' results.
#'
#' @format A data frame with one row per function call and 16 variables:
#' \describe{
#'   \item{func}{name of the function you wish to call}
#'   \item{df}{data frame to pass to the function call}
#'   \item{subset}{filter df records, this is passed directly to filter,
#'   ex. "AESER == 'Y'"}
#'   \item{rowvar}{variable being summarized that will pass to the function
#'   call}
#'   \item{rowtext}{row label text to display in the table}
#'   \item{row_header}{header text to display above row summary}
#'   \item{statlist}{list of statistics in the analysis, see individual
#'   functions for what is available per function (eg. "N, n (x.x)")}
#'   \item{colvar}{variable used to determine the columns of the table}
#'   \item{decimal}{decimal precision}
#'   \item{rowbyvar}{repeat rowvar summary by this variable/s, comma separated
#'   for multiple (eg. "ETHNIC, AGEGR1")}
#'   \item{tablebyvar}{repeat the entire table summary by this variable/s,
#'   comma separated for multiple (eg. "ETHNIC, AGEGR1")}
#'   \item{denom_df}{used to set denominators if df does not contain
#'   all required records}
#' }
#' @name table_metadata
"table_metadata"


#' Metadata describing table column layouts
#'
#' This is used by tlgsetup to prepare you input data to support
#' the desired column layout.
#'
#' @format A data frame with one row per column for each table type
#' and 6 variables:
#' \describe{
#'   \item{tbltype}{identifier used to group a table column layout}
#'   \item{coldef}{distinct variable values used, typically numeric
#'   and typically a treatment/main effect variable, think TRT01PN}
#'   \item{decode}{decode of coldef that will display as a column header
#'   in the table}
#'   \item{span1}{spanning header to display across multiple columns}
#'   \item{span2}{spanning header to display across multiple columns,
#'   second level}
#'   \item{span3}{spanning header to display across multiple columns,
#'   third level}
#' }
#'
#' @name column_metadata
"column_metadata"
