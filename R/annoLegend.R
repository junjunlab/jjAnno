#' @title annoLegend
#' @name annoLegend
#' @author Junjun Lao
#' @description This function is used to add legend annotations in plot.
#' @param object This function is used to add segment annotations in plot.
#' @param relPos The relative position of legend.  Default(c(0.9,0.9)).
#' @param xPosition The x axis coordinate for the legend. Default(NULL).
#' @param yPosition The x axis coordinate for the legend. Default(NULL).
#' @param labels The legend text labels. Default(NULL).
#' @param pch Legend shape. Default(NULL).
#' @param ncol Legend columns to show. Default(NULL).
#' @param col Legend colors. Default(NULL).
#' @param fill Legend fill colors. Default(NULL).
#' @param do.lines Whether to show lines. Default(FALSE).
#' @param lines.first Whether to show lines first. Default(FALSE).
#' @param textSize Legend text size. Default(NULL).
#' @param fontfamily Legend text fontfamily. Default(NULL).
#' @param fontface Legend text fontface. Default(NULL).
#'
#' @param vgap Vertical space between the legend entries. Default(1).
#' @param hgap Horizontal space between the legend entries. Default(1).
#'
#' @return Return a ggplot object.
#' @export
#'
#' @examples
#' # ===============================
#' # test function
#'
#' # load data
#' data(p)
#'
#' # add legend
#' annoLegend(object = p,
#'            labels = paste('legend ',1:5),
#'            pch = 21,
#'            col = 'black',
#'            fill = useMyCol('paired',5),
#'            textSize = 15)
#'
#' # change pos
#' annoLegend(object = p,
#'            relPos = c(0.2,0.9),
#'            labels = paste('legend ',1:5),
#'            pch = 21,
#'            col = 'black',
#'            fill = useMyCol('paired',5),
#'            textSize = 15)

# define function
annoLegend <- function(object = NULL,
                       relPos = c(0.9,0.9),
                       xPosition = NULL,
                       yPosition = NULL,
                       labels = NULL,
                       vgap = 1,
                       hgap = 1,
                       pch = NULL,
                       ncol = 1,
                       col = NULL,
                       fill = NULL,
                       do.lines = FALSE,
                       lines.first = FALSE,
                       textSize = NULL,
                       fontfamily = NULL,
                       fontface = NULL){
  # ============================================================================
  # get data
  data <- object$data

  # get mapping variables
  aes_x <- ggiraphExtra::getMapping(object$mapping,"x")
  aes_y <- ggiraphExtra::getMapping(object$mapping,"y")

  # test variable type
  data_x <- data[,c(aes_x)]
  data_y <- data[,c(aes_y)]

  # ============================================================================
  # calculate legend relative pos
  if(is.null(xPosition) | is.null(yPosition)){
    if(is.numeric(data_x)){
      legX <- relPos[1]*max(data_x)
    }else{
      legX <- relPos[1]*length(data_x)
    }
    # Y
    if(is.numeric(data_y)){
      legY <- relPos[2]*max(data_y)
    }else{
      legY <- relPos[2]*length(data_y)
    }
  }else{
    legX <- xPosition
    legY <- yPosition
  }
  # ============================================================================
  p <- object +
    ggplot2::annotation_custom(grob = grid::legendGrob(labels = labels,
                                                       pch = pch,
                                                       ncol = ncol,
                                                       do.lines = do.lines,
                                                       lines.first = lines.first,
                                                       vgap = vgap,
                                                       hgap = hgap,
                                                       gp = grid::gpar(col = col,
                                                                       fill = fill,
                                                                       fontsize = textSize,
                                                                       fontfamily = fontfamily,
                                                                       fontface = fontface)),
                               xmin = ggplot2::unit(legX,'native'),xmax = ggplot2::unit(legX,'native'),
                               ymin = ggplot2::unit(legY,'native'),ymax = ggplot2::unit(legY,'native'))

  # ============================================================================
  # print
  print(p)
}
