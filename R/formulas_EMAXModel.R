#' @rdname formula_EC50toEClevel
#' @export 
formula_EC50toMPC90 <- function(EC50, hill) {

  formula_EC50toEClevel(EC50, hill, 0.9)
}

#' Convert EC50 to EC for a desired effect level in the EMAX model
#' 
#' * formula_EC50toEClevel: Calculate the ECx corresponding to a given level of the maximum effect.
#' * formula_EC50toMPC90: Calculate the MPC90 from EC50 and Hill according to the EMAX model. MPC90 
#'   is the concentration at which 90% of the maximum effect is reached. 
#' 
#' @param EC50 numeric indicating concentration.
#' @param hill numeric indicating hill parameter value.
#' @param level a numeric in the interval \code{(0, 1)}
#'
#' @return formula_EC50toEClevel returns a numeric - the ECx corresponding to \code{level*100}% of the maximum effect. 
#' formula_EC50toMPC90 returns the MPC90 corresponding to EC50 and hill.
#' 
#' @details The formula is derived from the EMAX model: EClevel = EC50*(level/(1-level))^(1/hill).
#' The function supports some or all of the input values being vectors.
#' 
#' @export
#'
#' @author Anne Kuemmel (IntiQuan), Venelin Mitov (IntiQuan)
#' 
#' @importFrom stats na.omit
#' 
#' @rdname formula_EC50toEClevel
#' 
#' @family EMAX
#' 
#' @examples
#' formula_EC50toEClevel(23, 3, 0.5)
#' formula_EC50toEClevel(23, 10, 0.9)
#' formula_EC50toEClevel(23, 1, 0.9)
#' @md
formula_EC50toEClevel <- function(EC50, hill, level) {
  if (!(is.numeric(EC50) && is.numeric(hill)) ||
       isTRUE(any(na.omit(EC50) <= 0) || any(na.omit(hill) <= 0) || any(na.omit(level) < 0) || any(na.omit(level) >= 1))) {
    stop(
      "formula_EC50toEClevel: Invalid parameter values. EC50, hill, and level",
      " must satisfy EC50 > 0, hill > 0, and level in the interval [0, 1)."
    )
  }
  
  EC50*(level/(1-level))^(1/hill)
}


#' Calculate the Effect of an EMAX model for a given concentration x ####
#'
#' @param x numeric non-negative vector of concentration at which to calculate the effect.
#' @param EC50 a positive numeric indicating the EC50 parameter value0
#' @param hill a positive numeric indicating the hill parameter value
#'
#' @return a numeric vector of the same length as x with values in the interval [0,1).
#' @export
#'
#' @author Anne Kuemmel (IntiQuan), Venelin Mitov (IntiQuan)
#' 
#' @family EMAX
#' 
#' @examples
#' curve(formula_EMAXEffect(x, 23, 10), .1, 50)
formula_EMAXEffect <- function(x, EC50, hill) {
  if (!(is.numeric(x) && is.numeric(EC50) && is.numeric(hill) && length(EC50) == 1 && length(hill) == 1) ||
    isTRUE(any(na.omit(x) < 0) || na.omit(EC50) <= 0 || na.omit(hill) <= 0)) {
    stop(
      "formula_EMAXEffect: Invalid parameter values. x, EC50, and hill must be numeric;",
      "EC50 and hill must be positive and x must be non-negative."
    )
  }
  x^hill / (x^hill + EC50^hill)
}

#' Minimum parasitemia growth inhibitory concentration (MIC) according to an EMAX PD model
#'
#' Determine MIC from EMAX PD model (sigmoidal emax model). MIC corresponds to
#' concentration at which the killing compensates growth. If EMAX <= GR, Inf is returned.
#'
#' @param GR positive numeric indicating the parasite growth rate (1/hours).
#' @param EMAX positive numeric indicating the maximum killing effect (1/hours).
#' @param EC50 positive numeric indicating the concentration at which half of EMAX is reached.
#' @param hill positive numeric indicating the hill coefficient.
#'
#' @details
#' Not to confuse Minimum Inhibitory Concentration (MIC) with MICrosomes,
#' MIC will be noted MinIC.
#'
#' @return numeric, MIC value. If EMAX <= GR, Inf is returned.
#' @author Anne Kuemmel (IntiQuan), Venelin Mitov (IntiQuan)
#' 
#' @family EMAX
#' 
#' @export
formula_EMAXmodelParsToMIC = function(GR, EMAX, EC50, hill) {
  if (!(is.numeric(GR) && is.numeric(EMAX) && is.numeric(EC50) && is.numeric(hill)) ||
    isTRUE(any(na.omit(GR) <= 0) || any(na.omit(EMAX) <= 0) || any(na.omit(EC50) <= 0) || any(na.omit(hill) <= 0))) {
    stop(
      "formula_EMAXmodelParsToMIC: Invalid parameter values. GR, EMAX, EC50, and hill must be numeric;",
      "GR, EMAX, EC50, and hill must be positive."
    )
  }
  ifelse(EMAX > GR,
    (GR / (EMAX - GR))^(1 / hill) * EC50,
    Inf
  )
}


#' Determine EMAX value from PRR or PRR value from EMAX accounting for parasite growth
#'
#' Emax is the maximum killing effect. It is assumed that the PRR
#' was obtained over a period at which the drug achieved maximum
#' effect. The observed PRR is the difference of parasite growth
#' and killing by the drug. The derivation follows the EMAX model:
#' EMAX = PRR*log(10)/timePRR + GR.
#'
#' @param GR Growth rate coefficient (1/hours)
#' @param PRR Parasite reduction ratio in log10 units
#' @param EMAX Maximum killing effect (1/hours).
#' @param timePRR Hours over which PRR was assessed (default: 48 hours)
#'
#' @return formula_PRRtoEMAX numeric - EMAX value (1/hours);
#' formula_EMAXtoPRR returns numeric - PRR value (log10 units).
#'
#' @author Anne Kuemmel (IntiQuan), Venelin Mitov (IntiQuan)
#'
#' @examples
#' formula_PRRtoEMAX(0.1, 2)
#' formula_EMAXtoPRR(0.1, 0.195)
#' 
#' @rdname formula_PRRtoEMAX
#' 
#' @family EMAX
#' 
#' @export
formula_PRRtoEMAX <- function(GR, PRR, timePRR = 48) {
  if(!(is.numeric(GR) && is.numeric(PRR) && is.numeric(timePRR)) ||
     isTRUE(any(na.omit(GR) <= 0) || any(na.omit(PRR) <= 0) || any(na.omit(timePRR) <= 0))) {
    stop(
      "formula_PRRtoEMAX: Invalid parameter values. GR, PRR, and timePRR must be positive numeric;"
    )
  }
  PRR*log(10)/timePRR + GR
}

#' @rdname formula_PRRtoEMAX
#' 
#' @family EMAX
#' 
#' @export
formula_EMAXtoPRR <- function(GR, EMAX, timePRR = 48) {
  if(!(is.numeric(GR) && is.numeric(EMAX) && is.numeric(timePRR)) ||
     isTRUE(any(na.omit(GR) <= 0) || any(na.omit(EMAX) <= 0) || any(na.omit(timePRR) <= 0))) {
    stop(
      "formula_EMAXtoPRR: Invalid parameter values. GR, EMAX, and timePRR must be positive numeric;"
    )
  }
  (EMAX-GR)*timePRR / log(10)
}