
#' Get Activity Path
#'
#' Get activity path to be used in graphs. This allows to keep track of where the
#' figures are located and which script was used to generate them.
#'
#' @param ActivityPath If the user want to force the activity path to plot. (Default: \code{NULL}).
#'
#' @return A character vector with the activity path
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org}), 
#'   Venelin Mitov (IntiQuan, \email{venelin.mitov@@intiquan.com})
#' 
#' @family Activity
get_ActivityPath <- function(ActivityPath = NULL) {

  # Define Activity Path for caption:
  if (is.null(ActivityPath)) {
    # ActivityPath if on PiNK
    if(file.exists("../tags.RData")){
      load("../tags.RData")
      ActivityPath <- file.path(gsub("/sites/department/ModellingTeam/","",tags$itemPath),
                                tags$itemName)

    # ActivityPath if on S:/M&S
    }else{
      # Get Current Working Directory:
      WorkDir       <- getwd()
      WorkDir_Split <- strsplit(WorkDir, "/", fixed = TRUE)[[1]]
      n_str         <- length(WorkDir_Split)

      if (n_str>5 && WorkDir_Split[n_str-4]!="Projects" && WorkDir_Split[n_str-5]=="Projects_Discovery"){

        # Define Serie:
        SerieName  <- WorkDir_Split[n_str-4]

        # Define Project Name:
        ProjectName <- WorkDir_Split[n_str-2]

        # Define ActivityName if NULL:
        ActivityName  <- WorkDir_Split[n_str-1]

        # Define Activity Path:
        ActivityPath <- file.path(SerieName,"Work", ProjectName,  ActivityName)

      }else if(n_str>4 && WorkDir_Split[n_str-4]=="Projects"){
        # Define Project Name:
        ProjectName <- WorkDir_Split[n_str-3]

        # Define ActivityName if NULL:
        ActivityName  <- WorkDir_Split[n_str-1]

        # Define Activity Path:
        ActivityPath <- file.path(ProjectName, "Work", ActivityName)
      } else{
        warning("The current activity is not in the project folder and 'ActivityPath' is set to NULL, therefore, it is not automatically generated\nYou can manually enter a value for 'ActivityPath'")
        ActivityPath <- "NULL"
      }
    }
  }

  # Output:
  ActivityPath
}

