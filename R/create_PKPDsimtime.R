
#' Generate Simulation Times
#'
#' Calculate time points using given time steps between specifed time point. This is useful
#' to optimize time step for ploting. For example with PK profile the time step is often needed
#' to be small during absorption and large during the elimination phase.
#'
#' Calculate timepoints from 0 to Tend separated by regular time steps sepcified in dt. Several time steps can be specified: they will be applied
#' for the respective time intervals specified in the vector Tswitch.
#'
#' @param Tend Final time
#' @param dt Time steps (Defaul: \code{c(  0.25,  1,  2, 6)}).
#' @param Tswitch Time intervals over which time steps are used. It should be a vector of similar length as \code{dt} (Default: \code{c(0,  12, 24, 48)}).
#'
#' @return Numeric vector with the calcultaed time point
#'
#' @examples
#' create_PKPDsimtime(Tend=1,dt=c(0.1,0.5),Tswitch=c(0,0.4))
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org})
#' @family PKPD simulation
create_PKPDsimtime <- function(Tend,
                               dt      = c(  0.25,  1,  2, 6),
                               Tswitch = c(0,  12, 24, 48)){

  # Control that Tend is larger than first value of Tswitch
  if (Tend<=Tswitch[1]){
    stop("First value in 'Tswitch' is larger than 'Tend'. Please adjust 'Tswitch'.")
  }

  # Control that dt and Tswitch have same length:
  if (length(dt)!=length(Tswitch)){
    stop("'dt' and 'Tswitch' have different length. Please adjust.")
  }

  # Generate simtim:
  #   Loop over dt/Tswitch
  simtime <- c()
  k       <- 1
  while (Tend>Tswitch[k+1] & k<length(dt)){
    simtime_k <- seq(Tswitch[k], Tswitch[k+1], by=dt[k])
    simtime   <- c(simtime, simtime_k)
    k         <- k+1
  }
  #   Add last segment
  simtime_k <- seq(Tswitch[k], Tend, by=dt[k])
  simtime   <- unique(c(simtime, simtime_k, Tend))

  # Output:
  simtime
}
