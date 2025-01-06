#' Summarize given outcome variable(s) for a single trial
#'
#' Summarize given outcome variable(s) for a single trial using a choice of metrics
#' (mean, percentile or geomean). If no metric is provided, percentile is used by default
#' for numeric variable, while mean is used for logical variable.
#'
#' @param dataInput data.table containing outcome variable(s) and selected other variables for each individual.
#' @param varCOL List containing metrics (percentiles, mean and/or geomean) to be calculated for each outcome variable - i.e. \code{list(Cmax=c("Percentile","Mean"),APR="Mean")}.
#' @param percentiles Percentiles of outcome variable(s) to be calculated if appropriate (Default: \code{c(5,50,90)}).
#' @param usubJIDCOL Name(s) of column(s) of `dataInput` uniquely describing individuals (Default: \code{c("IndivID","USUBJID")}).
#' @param trialCOL Name(s) of column(s) of `dataInput` uniquely describing trial (Default: \code{c("ScenID","ExpID","DoseID","Dose","nbrDoses","TrialID","TIME")}).
#'
#' @return data table containing values of outcome metrics for each trial
#'
#' @export
#' @author Sam Jones (MMV), Mohammed H. Cherkaoui (MMV)
#' @family Summarize Functions
summaryByTrial <- function(dataInput,
                           varCOL,
                           percentiles = c(5,50,95),
                           usubjidCOL  = c("IndivID","USUBJID"),
                           trialCOL    = c("ScenID","ExpID","DoseID","Dose","nbrDoses","TrialID","TIME")){
  
  #---------------------------------------------------#
  # STEP 1: Run some checks and initialization ----
  #---------------------------------------------------#
  
  # Which metrics are accepted for calculation by this code:
  #   NOTE: Adding additional metrics will require additional coding
  accepted_metrics <- c("mean","geomean","percentiles")
  
  # Add/Adjust name to percentiles (Predictive percentiles):
  percentiles <- aux_addNamesToPercentiles(percentiles)
  
  # Generate colKey to use:
  colKey <- intersect(c(trialCOL,usubjidCOL),
                      names(dataInput))
  
  # Check if dataInput is a data.table:
  if(!data.table::is.data.table(dataInput)){
    data.table::setDT(dataInput, key = colKey)
  }
  
  # VarCOL checks:
  #   If varCOL is not provided
  if(is.null(varCOL)) {
    stop("varCOL is not provided - it must be provided in as a list with outcome measures as list names i.e: list(PRR=c('Percentiles','Mean')), please adjust")
  }
  #   If varCOL is provided but without correct structure, generate error
  if(is.null(names(varCOL))){
    varCOL <- sapply(varCOL, function(x){
      out <- "percentiles"
      names(out) <- x
      out
    },USE.NAMES = FALSE)
    # stop("varCOL must be provided as a list with outcome measures as list names i.e: list(PRR=c('Percentiles','Mean')), please adjust")
  }
  # If varCOL is provided, but not in the data:
  #   NOTE: This is calling names as varCOL is a list...
  idx_ToKeep <- (names(varCOL) %in% names(dataInput))
  if(!any(idx_ToKeep)){
    stop("None of the variables in 'varCOL' are present in dataInput: Please adjust.")
  }
  
  # Force varCOL to a list:
  varCOL <- as.list(varCOL[idx_ToKeep])
  
  # Create measure.vars:
  measure.vars <- names(varCOL)
  
  # Make sure that only mean is being used for logical variables:
  class.vars   <- sapply(measure.vars, function(x){class(dataInput[[x]])})
  logical.vars <- measure.vars[class.vars=="logical"]
  if(any(tolower(varCOL[logical.vars])!="mean")){
    varCOL[logical.vars] <- "mean"
    warning("Metrics other than arithmetic mean cannot be calculated for logical outcome variables: It was automatically adjusted.")
  }
  
  # Ensure that only mean, geomean and percentiles are chosen in varCOL
  metrics_diff <- lapply(varCOL, function(x){
    out <- setdiff(tolower(x), accepted_metrics)
    out
  })
  if(any(lapply(metrics_diff,length)>0)){
    stop("Metric provided in varCOL that cannot be calculated. Accepted metrics are 'mean','geomean','percentiles' ")
  }
  
  
  #---------------------------------------------------#
  # STEP 2: Identify what to apply for each variable in varCOL ----
  #---------------------------------------------------#
  idx_mean        <- sapply(varCOL, function(x){"mean"        %in% tolower(x)})
  idx_geomean     <- sapply(varCOL, function(x){"geomean"     %in% tolower(x)})
  idx_percentiles <- sapply(varCOL, function(x){"percentiles" %in% tolower(x)})
  
  
  #---------------------------------------------------#
  # STEP 3: Produce long format data tables to operate on ----
  #---------------------------------------------------#
  
  # Add variables and values to data-frame if tagged to calculate mean:
  if(any(idx_mean)){
    data.byIndiv.mean <- data.table::melt.data.table(dataInput,
                                                     id.vars       = colKey,
                                                     measure.vars  = measure.vars[idx_mean],
                                                     variable.name = "Variable",
                                                     value.name    = "Value")
  }else{
    data.byIndiv.mean <- NULL
  }
  # Add variables and values to data-frame if tagged to calculate geometric mean
  if(any(idx_geomean)){
    data.byIndiv.geomean <- data.table::melt.data.table(dataInput,
                                                        id.vars       = colKey,
                                                        measure.vars  = measure.vars[idx_geomean],
                                                        variable.name = "Variable",
                                                        value.name    = "Value")
  }else{
    data.byIndiv.geomean <- NULL
  }
  # Add variables and values to data-frame if tagged to calculate percentiles
  if(any(idx_percentiles)){
    data.byIndiv.percentiles <- data.table::melt.data.table(dataInput,
                                                            id.vars       = colKey,
                                                            measure.vars  = measure.vars[idx_percentiles],
                                                            variable.name = "Variable",
                                                            value.name    = "Value")
  }else{
    data.byIndiv.percentiles <- NULL
  }
  
  
  #---------------------------------------------------#
  # STEP 4: Operate on data tables to obtain outcome metrics described in varCOL ----
  #---------------------------------------------------#
  
  # Query: What's better here - re-using if(any(idx...)) or using if(length(data.by.Indiv... >0)) ?
  # Either way the intent is the same - process calculations for specific metrics if idx_ objects are TRUE for any of varCOL.
  
  # Mean:
  if(any(idx_mean)){
    colKey <- intersect(trialCOL,
                        names(data.byIndiv.mean))
    data.byTrial.mean <- data.byIndiv.mean[,{
      list(Metric = "Mean",
           Value  = mean(Value,na.rm=TRUE))
    },
    by = c(colKey,"Variable")]
  }else{
    data.byTrial.mean<-NULL
  }
  
  # Geometric Mean
  if(any(idx_geomean)){
    colKey <- intersect(trialCOL,
                        names(data.byIndiv.geomean))
    data.byTrial.geomean <- data.byIndiv.geomean[,{
      list(Metric = "Geomean",
           Value  = geomean(Value,na.rm=TRUE))
    },
    by = c(colKey,"Variable")]
  }else{
    data.byTrial.geomean<-NULL
  }
  
  # Percentiles
  if(any(idx_percentiles)){
    colKey <- intersect(trialCOL,
                        names(data.byIndiv.percentiles))
    data.byTrial.percentiles <- data.byIndiv.percentiles[,{
      list(Metric =  names(percentiles),
           Value  =  quantile(Value, probs = percentiles/100, na.rm = TRUE))
    },
    by = c(colKey,"Variable")]
  }else{
    data.byTrial.percentiles<-NULL
  }
  
  # Combine data tables of all metrics
  data.byTrial <- data.table::rbindlist(list(data.byTrial.mean,
                                             data.byTrial.geomean,
                                             data.byTrial.percentiles))
  
  # Output:
  data.byTrial
}


#' Summarize confidence intervals for provided metrics across trials.
#'
#' @param dataInput data.table containing summarized outcome variable(s) summarized by trials - produced by `summaryByTrial` or similar format.
#' @param CIlevel Numeric containing the confidence interval in percent to be estimated (Default: 90)
#' @param metricCOL name of column of `dataInput` containing names of metrics on which CI is being estimated (Default: \code{"Metric"})
#' @param variableCOL name of column of `dataInput` containing names of variables (Default: \code{"Variable"})
#' @param valueCOL name of column of `dataInput` containing values of metrics for specific variables (Default: \code{"Value"})
#' @param scenCOL Name(s) of column(s) of `dataInput` uniquely describing scenario (Default: \code{c("ScenID","ExpID","DoseID","Dose","nbrDoses","TIME")})
#'
#' @return data table containing confidence intervals for selected outcome variable(s) and summarized metric(s)
#'
#' @export
#' @author Sam Jones (MMV), Mohammed H. Cherkaoui (MMV)
#' @family Summarize Functions
summaryAcrossTrials <- function(dataInput,
                                CIlevel     = 90,
                                metricCOL   = "Metric",
                                variableCOL = "Variable",
                                valueCOL    = "Value",
                                scenCOL     = c("ScenID","ExpID","DoseID","Dose","nbrDoses","TIME")){
  
  #---------------------------------------------------#
  # STEP 1: Run some checks and initialization ----
  #---------------------------------------------------#
  
  # Define quantile of CI:
  CI.percentile <- aux_constructCIpercentiles(CIlevel)
  
  # Check if data is data.table:
  if(!data.table::is.data.table(dataInput)){
    colKey <- intersect(c(scenCOL,variableCOL,metricCOL,valueCOL),
                        names(dataInput))
    data.table::setDT(dataInput, key = colKey)
  }
  
  
  #---------------------------------------------------#
  # STEP 2: Calculate CI for metrics of variables ----
  #---------------------------------------------------#
  
  # Get CI:
  colKey <- intersect(c(scenCOL,variableCOL,metricCOL),
                      names(dataInput))
  data <- dataInput[,{
    Variable.Q        <- quantile(get(valueCOL), probs = CI.percentile/100, na.rm = TRUE)
    Variable.Q        <- as.list(Variable.Q)
    names(Variable.Q) <- names(CI.percentile)
    Variable.Q
  },
  by = colKey]
  
  # Add CI level:
  data$`CI Level` <- CIlevel
  
  # Output:
  data
}


#' Generic function to estimate PI and CI
#'
#' Generic function to estimate the confidence interval of a metric (mean, percentiles, and/or geoman)
#' of various given variables, when multiple trials have been simulated. It calls `summaryByTrial`
#' and `summmaryAcrossTrials` in a row. If no metric is provided, percentile is used by default
#' for numeric variable, while mean is used for logical variable.
#'
#' @param dataInput data.table containing outcome variable(s) and selected other variables for each individual.
#' @param varCOL List containing metrics (percentiles, mean and/or geomean) to be calculated for each outcome variable - i.e. \code{list(Cmax=c("Percentile","Mean"),APR="Mean")}.
#' @param percentiles Percentiles of outcome variable(s) to be calculated if appropriate (Default: \code{c(5,50,90)}).
#' @param CIlevel Numeric containing the confidence interval in percent to be estimated (Default: 90)
#' @param usubJIDCOL Name(s) of column(s) of `dataInput` uniquely describing individuals (Default: \code{c("IndivID","USUBJID")}).
#' @param trialCOL Name(s) of column(s) of `dataInput` uniquely describing trial (Default: \code{c("ScenID","ExpID","DoseID","Dose","nbrDoses","TrialID","TIME")}).
#' @param scenCOL Name(s) of column(s) of `dataInput` uniquely describing scenario (Default: \code{c("ScenID","ExpID","DoseID","Dose","nbrDoses","TIME")})
#'
#' @return
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV)
#' @family Summarize Functions
summarize_PIandCIgeneric <- function(dataInput,
                                     varCOL,
                                     percentiles = c(5,50,90),
                                     CIlevel     = 90,
                                     usubjidCOL  = c("IndivID","USUBJID"),
                                     trialCOL    = c("ScenID","ExpID","DoseID","Dose","nbrDoses","TrialID","TIME"),
                                     scenCOL     = c("ScenID","ExpID","DoseID","Dose","nbrDoses","TIME")){
  
  # Summarize 'dataInput' by Trial:
  data.byTrial <- summaryByTrial(dataInput   = dataInput,
                                 percentiles = percentiles,
                                 varCOL      = varCOL,
                                 usubjidCOL  = usubjidCOL,
                                 trialCOL    = unique(c(scenCOL,trialCOL)))
  
  # Summarize 'data.byTrial' Across Trials:
  data <- summaryAcrossTrials(dataInput = data.byTrial,
                              CIlevel   = CIlevel,
                              scenCOL   = scenCOL)
  
  # Output:
  data
}
