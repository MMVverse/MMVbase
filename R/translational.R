
#' Formula converting concentrations between matrices and free and total
#'
#' @param Cin input concentration (numeric)
#' @param from source matrix (character), must be one of "blood", "plasma", "unbound", or "medium"
#' @param to target matrix (character), must be one of "blood", "plasma", "unbound", or "medium"
#' @param Rbp_from blood to plasma ratio for the source matrix (numeric), must be greater than 0
#' @param PPB_from plasma protein binding for the source matrix (numeric), must be a value between 0 and 1 (inclusive).
#' @param Rbp_to blood to plasma ratio for the target matrix (numeric), must be greater than 0
#' @param PPB_to plasma protein binding for the target matrix (numeric), must be a value between 0 and 1 (inclusive).
#'
#' @return numeric, converted concentration
#' @author Anne Kuemmel (IntiQuan), Venelin Mitov (IntiQuan)
#' @examples
#' # Example 1: Convert concentration from blood to plasma
#' # formula_convertConc converts concentration from blood to unbound then to plasma 
#' Cin <- 10
#' Rbp_from <- 2
#' PPB_from <- 0.1
#' Rbp_to <- 0.8
#' PPB_to <- 0.2
#' 
#' expected_unbound <- Cin * (1 - PPB_from) / Rbp_from
#' result_unbound <- formula_convertConc(Cin, from = "blood", to = "unbound", 
#'                                       Rbp_from, PPB_from, Rbp_to, PPB_to)
#' 
#' print(c(result_unbound, expected_unbound))
#' 
#' expected_plasma <- result_unbound / (1 - PPB_to)
#' result_plasma <- formula_convertConc(result_unbound, from = "unbound", to = "plasma", 
#'                                      Rbp_from, PPB_from, Rbp_to, PPB_to)
#' 
#' print(c(result_plasma, expected_plasma))
#'   
#' expected_plasma <- Cin * (1 - PPB_from) / (1 - PPB_to) / Rbp_from
#' result_plasma <- formula_convertConc(Cin, from = "blood", to = "plasma", 
#'                                      Rbp_from, PPB_from, Rbp_to, PPB_to)
#' 
#' print(c(result_plasma, expected_plasma))
#' @export
#' @family Translational
formula_convertConc <- function(Cin,
                                from = c("blood", "plasma", "unbound", "medium")[1],
                                to   = c("blood", "plasma", "unbound", "medium")[2],
                                Rbp_from = 1,
                                PPB_from = 0,
                                Rbp_to   = 1,
                                PPB_to   = 0) {
   
  fu_from <- 1 - PPB_from
  fu_to <- 1 - PPB_to
  
  matrices <- c("blood", "plasma", "unbound", "medium")

  # Check if from and to are valid options
  if (!(from %in% matrices)) {
    stop("Invalid value for 'from'. Must be one of ", toString(matrices))
  }
  if (!(to %in% matrices)) {
    stop("Invalid value for 'to'. Must be one of ", toString(matrices))
  }

  # Input checks -------------------------------------------------------------
  
  # . Check if PPB is between 0 and 1
  if (!IsInClosedInterval(PPB_from, 0, 1)) {
    stop("Invalid value for 'PPB_from'. Must be between 0 and 1")
  }

  if (!IsInClosedInterval(PPB_to, 0, 1)) {
    stop("Invalid value for 'PPB_to'. Must be between 0 and 1")
  }

  # PPB_to not allowed to be 1 when to is everything but unbound
  if (to != "unbound" && PPB_to == 1) {
    stop("Invalid value for 'PPB_to'. When to is not 'unbound' PPB_to cannot be 1")
  }
  # Rbp_from not allowed to be 0 when from is blood
  if (from == "blood" && Rbp_from == 0) {
    stop("Invalid value for 'Rbp_from'. When from is 'blood' Rbp_from cannot be 0")
  } 

  # Conversion ---------------------------------------------------------------

  # . 1 - convert to free concentration -------------------------------------
  if (from %in% c("plasma","medium")) {
    Cfree <- Cin * fu_from
  }
  if (from == "blood") {
    Cfree <- Cin * fu_from / Rbp_from
  }
  if (from == "unbound") {
    Cfree <- Cin
  }
  
  # . 2 - convert from free concentration to desired concentration ----------
  if (to %in% c("plasma","medium")) {
    Cout <- Cfree / fu_to
  }
  if (to == "blood") {
    Cout <- Cfree / fu_to * Rbp_to
  }
  if (to == "unbound") {
    Cout <- Cfree
  }
  
  return(Cout)
  
}

#' Estimate hepatic plasma CL, based on intrinsic unbound hepatic clearance, hepatic blood flow,
#' fraction unbound in plasma and BP-ratio
#'
#' @param CLintu intrinsic unbound hepatic clearance (mL/min/kg).
#' @param Qh hepatic blood flow (mL/min/kg).
#' @param Fu fraction unbound in plasma (.).
#' @param BP BP-ratio (.).
#'
#' @details Currently using the well-stirred model. 
#'
#' @return Estimated hepatic plasma clearance (mL/min/kg).
#' @author Venelin Mitov (IntiQuan)
#' @family Translational
#' @export
formula_CLintuToCLhPl <- function(CLintu, Qh, Fu, BP) {
  # Check that all arguments are numeric of equal length
  stopifnot(is.numeric(CLintu) && is.numeric(Qh) && is.numeric(Fu) && is.numeric(BP) &&
    length(unique(c(length(CLintu), length(Qh), length(Fu), length(BP)))) == 1 &&
    identical(names(CLintu), names(Qh)) &&
    identical(names(CLintu), names(Fu)) &&
    identical(names(CLintu), names(BP)))

  res <- sapply(seq_along(CLintu), function(i) {
    # formula_CLintuToCLh express CLh in blood
    BP[i] * formula_CLintuToCLh(CLintu = CLintu[i], Qh = Qh[i], fub = Fu[i] / BP[i], Model = "Well-Stirred")
  })
  names(res) <- names(CLintu)
  res
}

#' Estimate intrinsic unbound hepatic clearance from final hepatic plasma clearance
#'
#' @param CLh numeric vector indicating hepatic plasma CL (mL/min/kg).
#' @param Qh numeric vector indicating hepatic blood flow (mL/min/kg).
#' @param Fu numeric vector indicating fraction unbound in plasma (.).
#' @param BP numeric vector indicating BP-ratio (.), used to convert plasma to blood matrix for
#' CLh and Fu.
#'
#' @details Currently using the well-stirred model. If arguments are vectors
#' they should be the same length as CLh, otherwise, an error is thrown.
#'
#' @return Estimated intrinsic unbound hepatic clearance (mL/min/kg) or an error
#' if the arguments don't correspond.
#'
#' @seealso formula_CLhToCLintu
#'
#' @author Venelin Mitov (IntiQuan)
#' @family Translational
#' @export
formula_CLhPlToCLintu <- function(CLh, Qh, Fu, BP) {
  # Check that all arguments are numeric of equal length
  stopifnot(is.numeric(CLh) && is.numeric(Qh) && is.numeric(Fu) && is.numeric(BP) &&
    length(unique(c(length(CLh), length(Qh), length(Fu), length(BP)))) == 1 &&
    identical(names(CLh), names(Qh)) &&
    identical(names(CLh), names(Fu)) &&
    identical(names(CLh), names(BP)))

  res <- sapply(seq_along(CLh), function(i) {
    formula_CLhToCLintu(CLh = CLh[i] / BP[i], Qh = Qh[i], fub = Fu[i] / BP[i], Model = "Well-Stirred")
  })

  names(res) <- names(CLh)
  res
}

#' Convert hepatic blood clearance to intrinsic clearance (unbound)
#'
#' This function converts hepatic blood clearance (CLh) to intrinsic clearance (CLintu) using different models.
#'
#' @param CLh Numeric vector - hepatic clearance (mL/min, L/hr or L/hr/kg).
#' @param Qh numeric vector - hepatic blood flow (mL/min, L/hr or L/hr/kg); vector length, 
#'   names and units should be consistent with CLh.
#' @param fub numeric vector - fraction unbound in Blood (.). Vector length, names and units should be 
#'   consistent with CLh.
#' @param Model Character string indicating the model to use for conversion (
#'   possible options: "Well-Stirred", "Parallel").
#'
#' @return numeric vector of the same length as CLh - intrinsic clearance (unbound) (mL/min, L/hr or L/hr/kg).
#'
#' @details
#' The function converts hepatic clearance (CLh) to intrinsic clearance (CLintu) using different models.
#' The available models are "Well-Stirred" and "Parallel".
#' If the model is "Well-Stirred", the function uses the formula_CLhToCLintu_WellStirred() function.
#' If the model is "Parallel", the function uses the formula_CLhToCLintu_Parallel() function.
#' If the model is not one of the available options, an error is thrown.
#'
#' @examples
#' # Convert hepatic clearance to intrinsic clearance using the Well-Stirred model
#' formula_CLhToCLintu(CLh = 100, Qh = 200, fub = 0.5, Model = "Well-Stirred")
#'
#' # Convert hepatic clearance to intrinsic clearance using the Parallel model
#' formula_CLhToCLintu(CLh = 100, Qh = 200, fub = 0.5, Model = "Parallel")
#' 
#' @seealso \code{\link{formula_CLhToCLintu_WellStirred}}, \code{\link{formula_CLhToCLintu_Parallel}}
#' 
#' @author Mohammed H. Cherkaoui (MMV), Venelin Mitov (IntiQuan)
#' @family Translational
#' @export
formula_CLhToCLintu <- function(CLh, Qh, fub, Model = "Well-Stirred") {
  if (toupper(Model) == "WELL-STIRRED" || toupper(Model) == "WELLSTIRRED") {
    CLintu <- formula_CLhToCLintu_WellStirred(CLh, Qh, fub)
  } else if (toupper(Model) == "PARALLEL") {
    CLintu <- formula_CLhToCLintu_Parallel(CLh, Qh, fub)
  } else {
    stop("'Model' should be equal to 'Well-Stirred' or 'Parallel'")
  }

  CLintu
}

#' Convert unbound intrinsic clearance to hepatic blood clearance
#'
#' This function converts the unbound intrinsic clearance (CLintu) to hepatic
#' clearance (CLh) using either the Well-Stirred or Parallel model.
#'
#' @param CLintu numeric vector - unbound intrinsic clearance (mL/min, L/hr or L/hr/kg).
#' @param Qh Hepatic blood flow (mL/min, L/hr or L/hr/kg) - Should be consistent with CLintu
#' @param fub Fraction unbound in Blood (.)
#' @param Model Model to use for conversion. Options are "Well-Stirred" or "Parallel".
#'
#' @return Hepatic  blood clearance (CLh) in the same units as CLintu and Qh.
#'
#' @examples
#' # Example 1: Using the Well-Stirred model
#' formula_CLintuToCLh(CLintu = 10, Qh = 100, fub = 0.2, Model = "Well-Stirred")
#'
#' # Example 2: Using the Parallel model
#' formula_CLintuToCLh(CLintu = 10, Qh = 100, fub = 0.2, Model = "Parallel")
#'
#' @seealso \code{\link{formula_CLintuToCLh_WellStirred}}, \code{\link{formula_CLintuToCLh_Parallel}}
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV), Venelin Mitov (IntiQuan)
#' @family Translational
formula_CLintuToCLh <- function(CLintu, Qh, fub, Model = "Well-Stirred") {
  if (toupper(Model) == "WELL-STIRRED" | toupper(Model) == "WELLSTIRRED") {
    CLh <- formula_CLintuToCLh_WellStirred(CLintu, Qh, fub)
  } else if (toupper(Model) == "PARALLEL") {
    CLh <- formula_CLintuToCLh_Parallel(CLintu, Qh, fub)
  } else {
    stop("'Model' should be equal to 'Well-Stirred' or 'Parallel")
  }

  # Output:
  CLh
}

#' Calculate hepatic blood clearance (CLh) from unbound intrinsic clearance (CLintu) or vice versa using the Parallel-Tube model.
#'
#' The Parallel-Tube model assumes that the liver is composed
#' of multiple parallel tubes, each with the same flow rate and drug concentration. It takes into
#' account the blood flow rate through the liver (Qh), the fraction of drug unbound in the blood (fub),
#' and the unbound intrinsic clearance (CLintu).
#' The equation used to calculate CLh from CLintu is
#' CLh = Qh * (1 - exp(-fub * CLintu / Qh)).
#' The equation used to calculate CLintu from CLh is
#' CLintu = -Qh / fub * log(1 - CLh / Qh).
#'
#' @param CLintu Numeric, unbound intrinsic clearance (mL/min, L/hr or L/hr/kg).
#' @param CLh Numeric, hepatic blood clearance (mL/min, L/hr or L/hr/kg).
#' @param Qh Numeric, blood flow rate through the liver (mL/min, L/hr or L/hr/kg).
#' The unit should be consistent with CLintu and/or CLh.
#' @param fub Numeric, fraction of drug unbound in the blood.
#'
#' @return numeric, for formula_CLintuToCLh_Parallel - the hepatic blood clearance
#' CLh (mL/min, L/hr or L/hr/kg), for formula_CLhToCLintu_Parallel - the
#' unbound intrinsic clearance CLintu (mL/min, L/hr or L/hr/kg).
#'
#' @references
#' 1. Pang KS, Rowland M. Hepatic clearance of drugs. I. Theoretical considerations of a "well-stirred"
#' model and a "parallel tube" model. Influence of hepatic blood flow, plasma and blood cell binding,
#' and the hepatocellular enzymatic activity on hepatic drug clearance.
#' J Pharmacokinet Biopharm. 1977;5(6):625-653. doi.org/10.1007/BF01059688
#'
#' @examples
#' CLintu <- 10 # mL/min
#' Qh <- 1000 # mL/min
#' fub <- 0.5
#' CLh <- formula_CLintuToCLh_Parallel(CLintu, Qh, fub)
#' CLh
#' CLintu <- formula_CLhToCLintu_Parallel(CLh, Qh, fub)
#' CLintu
#' 
#' @rdname formula_CLintuToCLh_Parallel
#' @export
#' @author Mohammed H. Cherkaoui (MMV), Venelin Mitov (IntiQuan)
#' @family Translational
formula_CLintuToCLh_Parallel <- function(CLintu, Qh, fub) {
  # Clearance Hepatic:
  CLh <- Qh * (1 - exp(-fub * CLintu / Qh))

  # Output:
  CLh
}

#' @rdname formula_CLintuToCLh_Parallel
#' @export
#' @family Hepatic CL
formula_CLhToCLintu_Parallel <- function(CLh, # Hepatic blood clearance (mL/min, L/hr or L/hr/kg)
                                         Qh, # Heptic blood flow (mL/min, L/hr or L/hr/kg - Should be consitent with CLint)
                                         fub # Fraction unbound in blood (.)
) {
  # Unbound Intrinsic Clearance:
  CLintu <- -Qh / fub * log(1 - CLh / Qh)

  # Output:
  CLintu
}

#' Calculate hepatic blood clearance (CLh) from unbound intrinsic clearance (CLintu) vice versa using the Well-Stirred model
#'
#' The Well-Stirred model assumes that the liver is a
#' well-mixed organ and that drug elimination occurs primarily in the liver.
#' It takes into account the blood flow rate through the liver (Qh) and the
#' fraction of drug unbound in the blood (fub).
#' formula_CLintuToCLh_WellStirred
#' calculates hepatic clearance (CLh) from unbound intrinsic clearance (CLintu)
#' using the equation:
#' CLh = Qh * fub * CLintu / (Qh + fub * CLintu).
#' formula_CLhToCLintu_WellStirred
#' calculates unbound intrinsic clearance (CLintu) from hepatic clearance (CLh)
#' using the equation:
#' CLintu = 1 / fub * Qh * CLh / (Qh - CLh).
#'
#' @param CLh Numeric, hepatic Clearance (mL/min, L/hr or L/hr/kg).
#' @param CLintu Numeric, unbound Intrinsic Clearance (mL/min, L/hr or L/hr/kg).
#' @param Qh Numeric, blood flow rate through the liver (mL/min, L/hr or L/hr/kg).
#' The unit should be consistent with CLint.
#' @param fub Numeric, fraction of drug unbound in the blood.
#'
#' @return The function formula_CLintuToCLh_WellStirred returns CLh -
#' Hepatic clearance (mL/min, L/hr or L/hr/kg). The function
#' formula_CLhToCLintu_WellStirred returns CLintu - unbound intrinsic clearance
#' (mL/min, L/hr or L/hr/kg).
#'
#' @references
#' 1. Pang KS, Rowland M. Hepatic clearance of drugs. I. Theoretical considerations of a "well-stirred"
#' model and a "parallel tube" model. Influence of hepatic blood flow, plasma and blood cell binding,
#' and the hepatocellular enzymatic activity on hepatic drug clearance.
#' J Pharmacokinet Biopharm. 1977;5(6):625-653. doi.org/10.1007/BF01059688
#'
#' @examples
#' CLintu <- 10 # mL/min
#' Qh <- 1000 # mL/min
#' fub <- 0.5
#' CLh <- formula_CLintuToCLh_WellStirred(CLintu, Qh, fub)
#' CLh
#' CLintu <- formula_CLhToCLintu_WellStirred(CLh, Qh, fub)
#' CLintu
#'
#' @rdname formula_CLintuToCLh_WellStirred
#' 
#' @seealso formula_CLintuToCLh_Parallel
#' 
#' @export
#' @author Catalina Barcelo (MMV), Mohammed H. Cherkaoui (MMV), Venelin Mitov (IntiQuan)
#' @family Translational
formula_CLintuToCLh_WellStirred <- function(CLintu, Qh, fub) {
  # Clearance Hepatic:
  CLh <- Qh * fub * CLintu / (Qh + fub * CLintu)

  # Output:
  CLh
}

#' @rdname formula_CLintuToCLh_WellStirred
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV), Venelin Mitov (IntiQuan)
#' @family Translational
formula_CLhToCLintu_WellStirred <- function(CLh, Qh, fub) {
  # Unbound Intrinsic Clearance:
  CLintu <- 1/fub*Qh*CLh/(Qh-CLh)

  # Output:
  CLintu
}



#' Calculate hepatic extraction ratio from hepatic blood clearance and hepatic blood flow
#'
#' @param CLh numeric - hepatic clearance.
#' @param Qh numeric - hepatic blood flow.
#'
#' @return numeric - hepatic extraction ratio.
#'
#' @details The hepatic extraction ratio is equal to CLh/Qh.
#' @examples
#' # hepatic clearance [mL/min/kg]
#' CLh_mLminkg <- 23
#' # Blood to plasma ratio
#' BPratio <- 2
#' # Hepatic blood flow [mL/min/kg]
#' Qh_mLminkg <- get_NumericConstantsAsList()$QhPatient_mLPerMinPerkg
#' 
#' Qh_mLminkg
#'
#' # Hepatic extraction ratio
#' Eh <- formula_HepExtraction(CLh = CLh_mLminkg / BPratio, Qh = Qh_mLminkg)
#' # Hepatic bioavailability
#' Fh <- 1 - Eh
#'
#' @export
#' @author Anne Kuemmel (IntiQuan), Venelin Mitov (IntiQuan)
#' @family Translational
formula_HepExtraction <- function(CLh, Qh) {
  Eh <- CLh / Qh
  Eh
}
