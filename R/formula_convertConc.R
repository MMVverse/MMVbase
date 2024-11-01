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
