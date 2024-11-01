# [TODO] MMVbase 1.1.0

* Added functions
* Removed functions
  - [TODO] dichotomicSearch - this function is used in several scripts in MMVworkflows and MMVmodules. The following steps need 
  to be taken: 
     1. Which of the scripts or workflows using this function are not useful and can be deleted?
     2. See if the function calls can be replaced by calls to stats::uniroot - not a trivial task. 
     3. If all uses of the function can be replaced by stats::uniroot calls, and this is quick to do, do this 
     and remove the dichotomicSearch function.
  
  # MMVbase 1.0.0

* Initial CRAN submission.
* Added functions
  - Formulas derived from the EMAX model
    - formula_EC50toEClevel	- Convert EC50 to EC for a desired effect level in the EMAX model
    - formula_EMAXEffect - Calculate the Effect of an EMAX model for a given concentration x
    - formula_EMAXmodelParsToMIC - MIC in plasma according to an EMAX PD model
    - formula_PRRtoEMAX, formula_EMAXtoPRR - Determine EMAX value from PRR and vice versa accounting for parasite growth
    - formula_HepExtraction - calculate hepatic extraction ratio from hepatic blood flow and heptic clearance
  - Formulas derived from hepatic clearance models
    - formula_CLhPlToCLintu - Convert hepatic plasma clearance to unbound intrinsic clearance using the Well Stirred model
    - formula_CLintuToCLhPl - Convert unbound intrinsic clearance to hepatic plasma clearance using the Well Stirred model
    - formula_CLhToCLintu - Convert hepatic blood clearance to unbound intrinsic clearance using the Well Stirred or Parallel Tube model
    - formula_CLintuToCLh - Convert unbound intrinsic clearance to hepatic blood clearance using the Well Stirred or Parallel Tube model
    - formula_CLhToCLintu_Parallel - Convert hepatic blood clearance to unbound intrinsic clearance using the Parallel Tube model
    - formula_CLhToCLintu_WellStirred - Convert hepatic blood clearance to unbound intrinsic clearance using the Well Stirred model
    - formula_CLintuToCLh_Parallel - Convert unbound intrinsic clearance to hepatic blood clearance using the Parallel Tube model
    - formula_CLintuToCLh_WellStirred - Convert unbound intrinsic clearance to hepatic blood clearance using the Well Stirred model
  - Other functions
    - dichotomicSearch - migrated from MMVmalaria, because this function is used in several scripts in MMVworkflows and MMVmalaria/modules. 
    - dribble	Create data.frames rowwize.
    - get_NumericConstants and get_NumericConstantsAsList - these functions were migrated from `MMVhuPKpred` package.
    - formula_convertConc	Formula converting concentrations between matrices and free and total. This function is planned to replace `MMVmalaria::formula_convertConc()`.
    - GeometricMean	Geometric mean of a positive numeric vector. This function is planned to replace `MMVmalaria::geoMeanMMV()`.
    - GeometricSD	Geometric standard deviation of a positive numeric vector.
    - get_FromTable	Extract a predicted parameter or return a default value.
    - IsInClosedInterval	Check if value is in the closed interval '[low, high]'.
    - IsInLeftOpenInterval	Check if value is in the left-open interval '(low, high]'.
    - IsInOpenInterval	Check if value is in the open interval '(low, high)'.
    - IsInRightOpenInterval	Check if value is in the right-open interval '[low, high)'.
    - CreateOrAppendToReportTable	Generate a ReportTable or add rows to an existing one.
    - ConvertReportTableToList	Convert a reportTable to a list of data.frames corresponding to non-NA entries in the Style column.
* Written unit tests for all functions. Current line coverage is 83 %.
* Added a `NEWS.md` file to track changes to the package.
* Added a `README.md` file with a brief description of the package and a link to the vignette.
* Added a vignette to the package, describing the package purpose, the adopted naming conventions and the currently available functions.
* Added a `DESCRIPTION` file with the package metadata.
* Added a `NAMESPACE` file with the package exports.
* Added a `R` folder with the package functions.
* Added a `man` folder with the package documentation generated using roxygen2.
* Added a `tests` folder with the package unit tests.
* Passed CRAN check on IQdesktop (Linux) 2023.12 running R 4.3.1.
* [TODO BEFORE MAKING PUBLIC ON GITHUB ON CRAN] Add a `LICENSE` file with the package exports.
* [TODO BEFORE MAKING PUBLIC ON GITHUB ON CRAN] Add a `CITATION` with a bib text for citing this package. 
