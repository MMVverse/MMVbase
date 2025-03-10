#' A vector of 5000 colors for MMVbase to be used as standard colors in graphics.
#' @details
#' Only the first 9 colors are unique, they are repeated up to the length of
#' 5000.
#' 
#' @author Venelin Mitov (IntiQuan, \email{venelin.mitov@@intiquan.com})
#' 
#' @export
#' @family Graphical
MMVcolors <- rep_len(c("#000000", "#C5000B", "#0084D1",
                       "#579D1C", "#FF950E", "#4B1F6F",
                       "#1B9E77", "#D95F02", "#7570B3"), length.out = 5000)

#' ggplot functionality implementing MMV style
#'
#' ggplot function similar to `ggplot2::ggplot` implementing MMV style.
#'
#' @param ...          Typical ggplot input arguments
#' @param style        Style to choose between \code{"MMV"}, \code{"MMVvpc"} or \code{"IQR"} (Default: \code{"MMV"}).
#' @param ActivityPath Path of the current activity (Default: \code{NULL})
#' @param Caption      Caption to add to the plot (Default: \code{NULL})
#'
#' @return A ggplot object with MMV standards.
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org}), 
#'   Venelin Mitov (IntiQuan, \email{venelin.mitov@@intiquan.com})
#' 
#' @family Graphical
MMVggplot <- function(...,
                      style = c("MMV", "MMVvpc","IQR")[1],
                      ActivityPath = NULL,
                      Caption      = NULL) {

  ggplot_internal <- function(..., fontsize = 12) {
    p__ <- ggplot(...) + MMVtheme(base_size = fontsize)
    return(p__)
  }
  
  #-----------------------------------------------------------------------------#
  # STEP 1: Use Style of Interest ----
  #-----------------------------------------------------------------------------#
  
  # MMV Style
  if (grepl("MMV", toupper(style))) {
    gr <- ggplot_internal(...) +
      scale_color_manual(values = MMVcolors[2:length(MMVcolors)]) +
      scale_fill_manual(values = MMVcolors[2:length(MMVcolors)])
  } else if (toupper(style)=="IQR") {
    # IQR Style:
    gr <- ggplot_internal(...)
  } else {
    # Normal ggplot:
    gr <- ggplot(...)
  }


  #-----------------------------------------------------------------------------#
  # STEP 2: Adjust Activity Path ----
  #-----------------------------------------------------------------------------#
  if (is.null(ActivityPath) || ActivityPath!="") {
    ActivityPath <- paste0("Activity: ",get_ActivityPath(ActivityPath))
  }


  #-----------------------------------------------------------------------------#
  # STEP 3: Caption ----
  #-----------------------------------------------------------------------------#

  # Adjust Caption:
  if(is.character(Caption)) {
    Caption <- paste0(ActivityPath, "\n", Caption)
  }else {
    Caption <- ActivityPath
  }

  # Add Caption
  gr <- gr + labs(caption = Caption)


  #-----------------------------------------------------------------------------#
  # STEP 4: Adjust theme ----
  #-----------------------------------------------------------------------------#
  gr <- adjust_ggplotTheme(gr    = gr,
                           style = style)


  #-----------------------------------------------------------------------------#
  # STEP 5: Output ----
  #-----------------------------------------------------------------------------#
  return(gr)
}

#' MMVbase theme for ggplot graphics
#' 
#' The standard gg theme is based on theme_bw.
#'
#' @param base_size	numeric, font-size (default:12).
#' @param base_family	character, font-name (default: "").
#' 
#' 
#' @return a ggplot2 theme object. 
#' @author Venelin Mitov (venelin.mitov@@intiquan.com)
#' 
#' @importFrom ggplot2 theme_bw element_line element_rect rel unit
#' @export
#' @family Graphical
MMVtheme <- function (base_size = 12, base_family = "") {
  colors <- list(medium = c(gray = "#737373", red = "#F15A60", 
                            green = "#7AC36A", blue = "#5A9BD4", orange = "#FAA75B", 
                            purple = "#9E67AB", maroon = "#CE7058", magenta = "#D77FB4"), 
                 dark = c(black = "#010202", red = "#EE2E2F", green = "#008C48", 
                          blue = "#185AA9", orange = "#F47D23", purple = "#662C91", 
                          maroon = "#A21D21", magenta = "#B43894"), light = c(gray = "#CCCCCC", 
                                                                              red = "#F2AFAD", green = "#D9E4AA", blue = "#B8D2EC", 
                                                                              orange = "#F3D1B0", purple = "#D5B2D4", maroon = "#DDB9A9", 
                                                                              magenta = "#EBC0DA"))
  gray <- colors$medium["gray"]
  black <- colors$dark["black"]
  theme_bw(base_size = base_size, base_family = base_family) + 
    theme(line = element_line(colour = "black"), rect = element_rect(fill = "white", 
                                                                     colour = NA), text = element_text(colour = "black"), 
          axis.text = element_text(size = rel(1), colour = "black"), 
          axis.text.x = element_text(margin = unit(c(4, 4, 
                                                     0, 4), "mm")), axis.text.y = element_text(margin = unit(c(4, 
                                                                                                               4, 4, 0), "mm")), axis.ticks = element_line(colour = "black"), 
          axis.ticks.length = unit(-2, "mm"), legend.key = element_rect(colour = NA), 
          panel.border = element_rect(colour = "black"), strip.background = element_rect(fill = "white", 
                                                                                         colour = NA), strip.text = element_text(size = rel(1)))
}

#' Transform IQR ggplot object to MMV ggplot object
#'
#' When a ggplot object is generated using `IQRtools` functions, this function
#' allows to adjust the ggplot object to be compliant with MMV standards;
#' It will add the path of the activity and adjust the theme if specified.
#'
#' @param IQRggplot ggplot object generated with `IQRtools` functions.
#' @param style Style to choose between \code{"MMV"} or \code{"MMVvpc"} 
#' (Default: \code{"MMV"}).
#' @param ActivityPath Path of the current activity (Default: \code{NULL})
#' @param Caption Caption to add to the plot (Default: \code{NULL})
#'
#' @return A ggplot object compliant with MMV standars.
#'
#' @export
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org}), 
#'   Venelin Mitov (IntiQuan, \email{venelin.mitov@@intiquan.com})
#' 
#' @family Graphical
transform_IQRggplotToMMVggplot <- function(IQRggplot,
                                           style        = c("MMV", "MMVvpc")[1],
                                           ActivityPath = NULL,
                                           Caption      = NULL) {

  #-----------------------------------------------------------------------------#
  # STEP 1: Use Style of Interest ----
  #-----------------------------------------------------------------------------#

  # Quick Check:
  if(is.null(style)) {
    style <- "original"
  }

  # MMV Style
  if (grepl("MMV", toupper(style))) {
    base::suppressMessages(MMVggplot <- IQRggplot +
                           scale_color_manual(values = MMVcolors[2:length(MMVcolors)]))

  # Any other style:
  } else {
    MMVggplot <- IQRggplot
  }


  #-----------------------------------------------------------------------------#
  # STEP 2: Adjust Activity Path ----
  #-----------------------------------------------------------------------------#
  if (is.null(ActivityPath) || ActivityPath!="") {
    ActivityPath <- paste0("Activity: ",get_ActivityPath(ActivityPath))
  }


  #-----------------------------------------------------------------------------#
  # STEP 3: Caption ----
  #-----------------------------------------------------------------------------#

  # Adjust Caption:
  if(is.character(Caption)) {
    Caption <- paste0(ActivityPath, "\n", Caption)
  }else{
    Caption <- ActivityPath
  }

  # Add Caption
  MMVggplot <- MMVggplot + labs(caption = Caption)


  #-----------------------------------------------------------------------------#
  # STEP 4: Adjust theme ----
  #-----------------------------------------------------------------------------#
  MMVggplot <- adjust_ggplotTheme(gr    = MMVggplot,
                                  style = style)


  #-----------------------------------------------------------------------------#
  # STEP 5: Output ----
  #-----------------------------------------------------------------------------#
  MMVggplot
}



#' Adjust ggplot Theme
#'
#' Adjust the theme of the ggplot object `gr` to various style (e.g. to MMV style).
#'
#' @param gr ggplot object to adjust theme.
#' @param style Style to choose between \code{"MMV"} or \code{"MMVvpc"} (Default: \code{"MMV"}).
#'
#' @return `gr` with adjusted theme
#'
#' @author Mohammed H. Cherkaoui (MMV, \email{cherkaouim@@mmv.org}), 
#'   Venelin Mitov (IntiQuan, \email{venelin.mitov@@intiquan.com})
#' 
#' @family Graphical
#' 
#' @importFrom ggplot2 theme element_text scale_color_manual scale_fill_manual ggplot labs 
adjust_ggplotTheme <- function(gr,
                               style = c("MMV", "MMVvpc")[1]) {

  # Quick Check:
  if(is.null(style)) {
    style <- "original"
  }

  # MMV Style
  if (grepl("MMV", toupper(style))) {
    # Adjust theme for Sub-style:
    #   VPC
    if (grepl("VPC", toupper(style))) {
      gr <- gr + theme(axis.title    = element_text(face="bold"),
                       plot.title    = element_text(size=16),
                       plot.subtitle = element_text(size=14),
                       legend.text   = element_text(size=10),
                       legend.title  = element_text(size=12),
                       axis.text.x   = element_text(size=8 ),
                       axis.text.y   = element_text(size=8 ),
                       axis.title.x  = element_text(size=10),
                       axis.title.y  = element_text(size=10),
                       strip.text    = element_text(size=10),
                       plot.caption  = element_text(size=8, hjust=0))

     
    } else {
      #   Any Other:
      gr <- gr + theme(axis.title    = element_text(face="bold"),
                       plot.title    = element_text(size=18),
                       plot.subtitle = element_text(size=16),
                       legend.text   = element_text(size=12),
                       legend.title  = element_text(size=14),
                       axis.text.x   = element_text(size=14),
                       axis.text.y   = element_text(size=14),
                       axis.title.x  = element_text(size=14),
                       axis.title.y  = element_text(size=14),
                       strip.text    = element_text(size=12),
                       plot.caption  = element_text(size=10, hjust=0))
    }

    # Any other style:
  } else {
    gr <- gr
  }

  # Output:
  gr
}



