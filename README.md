
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MMVbase

<!-- badges: start -->
<!-- badges: end -->

This R package contains general functions, originally written for
malaria pharmacometric modeling and simulation activities, which can be
useful in a broader set of use cases. The package is part of the
MMVverse, a collection of open source R packages developed by MMV. This
document summarizes conventions the authors have adopted for naming
functions in the package and provides an overview of the available
functions. You can find detailed function description and examples in
the standard manual page of the function, which is accessible from the R
console using \`?<function-name>.

# Table of Contents

- [Installation](#installation)
- [Naming conventions](#naming-conventions)
- [Function overview](#function-overview)
  - [Graphical functions](#graphical-functions)
  - [Data functions](#data-functions)
  - [File functions](#file-functions)
  - [Numeric constants](#numeric-constants)
  - [Mathematical functions](#mathematical-functions)
  - [Statistical functions](#statistical-functions)
  - [Reporting functions](#reporting-functions)
  - [Translational functions](#translational-functions)
  - [Formulas derived from the Emax parasitemia killing
    model](#formulas-derived-from-the-emax-parasitemia-killing-model)
  - [Algorithm functions](#algorithm-functions)
  - [Other utility functions](#other-utility-functions)

# Installation

You can install the development version of MMVbase from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MMVverse/MMVbase", build_vignettes = TRUE)
```

# Naming conventions

New functions in the MMVbase package follow
UpperCamelCase_WithUnderscores notation. Underscores symbols are used
rarely, mostly to improve readability when the function name contains
accronymes or model parameter names such as `CL` and `PL`. For example,
we prefer the function name CalculateCL_FromInVitro, because it clearly
separates the parameter name `CL` from the word `From`, whereas the
function name `CalculateCLFromInVitro` is less clear. We prefer function
names starting with a verb with a capital first letter, such as
`Calculate`, `Predict`, to help distinguishing function names from
variable names and R keywords such as `if` and `for`. Yet, there are
exceptions to these naming rules, in particular:

- `utilities` replacing functions from 3$^{\text{rd}}$ party packages or
  other useful functions. There is no specific convention adopted for
  naming such functions, but there names should be simple and easy to
  remember, e.g. `dichotomicSearch`.
- `get_` functions used to extract values from tables,
  e.g. `get_FromTable`, or to get lists or data.frames of constants, e.g
  `get_NumericConstants` and `get_NumericConstantsAsList`.
- `formula_` functions, which indicate that the function implements a
  (simple) mathematical expression or a set of related expressions,
  e.g. `formula_convertConc`. `formula_` functions do not necessarily
  follow any notation rule, because many such functions have been
  migrated from other (older) MMV packages and have kept their original
  names.
- `predict_` functions, which indicate algorithms to predict parameter
  values. Such functions usually are more complex than `formula_`
  functions and return a report table with multiple rows, although this
  is not a strict rule.

The prefixes `get_`, `formula_`, and `predict_` are indicative of how
these functions should be used in scripts: `get_` and `formula_`
functions are supposed to execute quickly and therefore can be called
repeatedly within for loops or with the same arguments, as long as this
helps the clarity of the code. `predict_` functions are more complex and
usually slower to run, hence, they should be called only once with a
given set of arguments and the returned object (usually a report table)
should be used in the rest of the script.

# Function overview

## Graphical functions

| Function                         | Description                                                                   |
|----------------------------------|-------------------------------------------------------------------------------|
| `MMVcolors`                      | A vector of 5000 colors for MMVbase to be used as standard colors in graphics |
| `MMVggplot`                      | ggplot functionality implementing MMV style                                   |
| `MMVtheme`                       | MMVbase theme for ggplot graphics                                             |
| `transform_IQRggplotToMMVggplot` | Transform IQR ggplot object to MMV ggplot object                              |
| `adjust_ggplotTheme`             | Adjust ggplot Theme                                                           |

## Data functions

| Function                         | Description                                                                                     |
|----------------------------------|-------------------------------------------------------------------------------------------------|
| `aux_createUSUBJID`              | Create USUBJID                                                                                  |
| `get_IXGDFtoRemove`              | Get IXGDF indices of observations below LLOQ to remove                                          |
| `IQRtableToDataFrame`            | Convert saved IQR table back to data.frame                                                      |
| `Check_MissingDatabyNAME`        | Check_MissingDatabyNAME                                                                         |
| `check_dataGeneralMMV`           | Checks if CENTER and CENTERNAME is well defined within `dataGeneral`                            |
| `swapName_MMVnameToName`         | Swaps from MMV name for compound for nick name of interest                                      |
| `swapName_NameToMMVname`         | Swaps from nick name of interest for compound for MMV name                                      |
| `transform_dataFrame_WideToLong` | Transform Data Frame from Wide to Long Format                                                   |
| `transform_dataFrame_LongToWide` | Transform Data Frame from Long to Wide Format                                                   |
| `convert_Unit`                   | Convert Unit                                                                                    |
| `summaryByTrial`                 | Summarize given outcome variable(s) for a single trial                                          |
| `summaryAcrossTrials`            | Summarize confidence intervals for provided metrics across trials                               |
| `summarize_PIandCIgeneric`       | Generic function to estimate prediction intervals (PI) or confidence intervals (CI) of a metric |

## File functions

| Function                | Description                                                           |
|-------------------------|-----------------------------------------------------------------------|
| `thisFile`              | Get the executing script file name                                    |
| `is.fileMMV`            | Checks if filename is an existing file and not a directory            |
| `file.copyMMV`          | Copy a file with creating the destination folder if it does not exist |
| `get_fileNameExtension` | Get the extension of a filename                                       |
| `load_RDataAsList`      | Load RData into a list                                                |
| `aux_CommonSubPath`     | Common Sub-Path                                                       |
| `saveMMV`               | Save R objects                                                        |

## Numeric constants

| Function                     | Description                                                                |
|------------------------------|----------------------------------------------------------------------------|
| `get_NumericConstantsAsList` | Get numeric constants as a named list                                      |
| `get_NumericConstants`       | Get numeric constants such as growth rates, body weights, and cell numbers |

## Mathematical functions

| Function                 | Description                                                                      |
|--------------------------|----------------------------------------------------------------------------------|
| `IsInClosedInterval`     | Check if value is in the closed interval                                         |
| `IsInOpenInterval`       | Check if value is in the open interval                                           |
| `IsInLeftOpenInterval`   | Check if value is in the left open interval                                      |
| `IsInRightOpenInterval`  | Check if value is in the right open interval                                     |
| `rectintMMV`             | Calculate the integral of a function using the Riemann sum method                |
| `logLinTrapzMMV`         | log-Linear Trapez Integration Method                                             |
| `trapzMMV`               | Trapezoidal Integration                                                          |
| `simpsonMMV`             | Simpson’s Rule Integration                                                       |
| `newtonRaphson.Function` | Find the root of of y=f(x) within an interval                                    |
| `newtonRaphson.Vector`   | Newton-Raphson Method for Finding Roots in a Vector                              |
| `find_MinMMV`            | Find minimum of a continuous function (specified by x,y values) by interpolation |

We recommend using interval functions in R scripts and other functions
implementations instead of coding, e.g. `x>=low && x<=high`. This
motivates the modelers to carefully choose the type of interval check
that is needed for the use case, as well to remember the choice made and
to make the same choice consistently across different use cases if such
type of consistency would be meaningful.

## Statistical functions

| Function            | Description                                               |
|---------------------|-----------------------------------------------------------|
| `GeometricMean`     | Geometric mean of a positive numeric vector               |
| `GeometricSD`       | Geometric standard deviation of a positive numeric vector |
| `clopperPearsonMMV` | Clopper-Pearson Test                                      |

## Reporting functions

Functions currently available are:

| Function                      | Description                                                                                        |
|-------------------------------|----------------------------------------------------------------------------------------------------|
| `CreateOrAppendToReportTable` | Generate a ReportTable or add rows to an existing one                                              |
| `ConvertReportTableToList`    | Convert a reportTable to a list of data.frames corresponding to non-NA entries in the Style column |
| `get_FromTable`               | Extract a value for a parameter or a constant from a data.frame such as a report table             |
| `PrintReportTableAsRmd`       | Print a report table in rmarkdown format                                                           |

Report tables provide a common way for `Calculate` and `Predict`
functions to return more than just a numeric value, but a table
(data.frame) with rows corresponding to output values. There are
multiple benefits to using such report tables, in particular when the
prediction goes in more than one step and can have several logical
branches:

- returned values can be numeric or character
- report tables can contain multiple returned values on separate rows or
  as value columns on the same row.
- report tables can contain the units of the returned values
- report tables can contain text titles, subtitles and comments
  providing information about how the calculation or prediction was
  done.
- \[TODO\] report tables can be exported to different table formats,
  such as Excel sheets, or xtable.

## Translational functions

The translational family of functions provides various formulas for
converting concentrations between different matrices, estimating hepatic
clearance, and calculating hepatic extraction ratios. These functions
are essential for pharmacokinetic modeling and simulations.

The following functions are currently available:

| Function                          | Description                                                                                                                           |
|-----------------------------------|---------------------------------------------------------------------------------------------------------------------------------------|
| `formula_convertConc`             | Formula converting concentrations between matrices and free and total                                                                 |
| `formula_CLintuToCLhPl`           | Estimate hepatic plasma CL, based on intrinsic unbound hepatic clearance, hepatic blood flow, fraction unbound in plasma and BP-ratio |
| `formula_CLhPlToCLintu`           | Estimate intrinsic unbound hepatic clearance from final hepatic plasma clearance                                                      |
| `formula_CLhToCLintu`             | Convert hepatic blood clearance to intrinsic clearance (unbound)                                                                      |
| `formula_CLintuToCLh`             | Convert unbound intrinsic clearance to hepatic blood clearance                                                                        |
| `formula_CLintuToCLh_Parallel`    | Calculate hepatic blood clearance (CLh) from unbound intrinsic clearance (CLintu) or vice versa using the Parallel-Tube model         |
| `formula_CLhToCLintu_Parallel`    | Calculate hepatic blood clearance (CLh) from unbound intrinsic clearance (CLintu) or vice versa using the Parallel-Tube model         |
| `formula_CLintuToCLh_WellStirred` | Calculate hepatic blood clearance (CLh) from unbound intrinsic clearance (CLintu) vice versa using the Well-Stirred model             |
| `formula_CLhToCLintu_WellStirred` | Calculate hepatic blood clearance (CLh) from unbound intrinsic clearance (CLintu) vice versa using the Well-Stirred model             |
| `formula_HepExtraction`           | Calculate hepatic extraction ratio from hepatic blood clearance and hepatic blood flow                                                |

## Formulas derived from the Emax parasitemia killing model model

The EMAX model is a pharmacodynamic model used to describe the effect of
a drug as a sigmoidal function of drug concentration. Here’s a summary
of the functions:

| Function                     | Description                                                                             |
|------------------------------|-----------------------------------------------------------------------------------------|
| `formula_EC50toMPC90`        | Convert EC50 to EC for a desired effect level in the EMAX model                         |
| `formula_EC50toEClevel`      | Convert EC50 to EC for a desired effect level in the EMAX model                         |
| `formula_EMAXEffect`         | Calculate the Effect of an EMAX model for a given concentration x                       |
| `formula_EMAXmodelParsToMIC` | Minimum parasitemia growth inhibitory concentration (MIC) according to an EMAX PD model |
| `formula_PRRtoEMAX`          | Determine EMAX value from PRR or PRR value from EMAX accounting for parasite growth     |
| `formula_EMAXtoPRR`          | Determine EMAX value from PRR or PRR value from EMAX accounting for parasite growth     |

Each function has specific input parameters related to the EMAX model,
such as EC50 (the concentration of a drug that gives half-maximal
response), hill (the Hill coefficient), and level (the level of the
maximum effect). The functions perform checks to ensure that all
arguments are numeric and of the correct values. The results are
returned as numeric values or vectors.

## Algorithm functions

Functions currently available are:

- `dichotomicSearch` - this function was migrated from the MMVmalaria
  package.

## Other utility functions

| Function                     | Description                                   |
|------------------------------|-----------------------------------------------|
| `get_ActivityPath`           | Get the directory path of an activity         |
| `remove_duplicates`          | Reduce character to the non-repeating entries |
| `aux_addNamesToPercentiles`  | Add names to percentiles vector               |
| `aux_constructCIpercentiles` | Construct CI percentiles from CI level        |
| `aux_removeEscapeChar`       | Removal of escape characters                  |
| `collapseMMV`                | Customizable Collapse Function                |
