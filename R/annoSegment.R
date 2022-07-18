#' @title annoSegment
#' @name annoSegment
#' @author Junjun Lao
#' @description This function is used to add segment annotations in plot.
#' @param object Your ggplot list. Default(NULL).
#' @param relSideDist The relative distance ratio to the y axis range. Default(0.1).
#' @param annoPos The position for the annotation to be added. Default("top").
#' @param xPosition The x axis coordinate for the segment. Default(NULL).
#' @param yPosition The y axis coordinate for the segment. Default(NULL).
#' @param pCol The segment colors. Default(NULL).
#' @param segWidth The relative segment width. Default(1).
#' @param lty The segment line type. Default(NULL).
#' @param lwd The segment line width. Default(NULL).
#' @param alpha The segment color alpha. Default(NULL).
#' @param lineend The segment line end. Default("square").
#' @param annoManual Whether annotate by yourself by supplying with x and y coordinates. Default(FALSE).
#' @param mArrow Whether add segment arrow. Default(FALSE).
#' @param addBranch Whether add segment branch. Default(FALSE).
#' @param bArrow Whether add branch arrow. Default(FALSE).
#' @param branDirection The branch direction. Default(1).
#' @param branRelSegLen The branch relative length to the segment. Default(0.3).
#' @param addText Whether add text label on segment. Default(FALSE).
#' @param textCol The text colors. Default(NULL).
#' @param textSize The text size. Default(NULL).
#' @param fontfamily The text fontfamily. Default(NULL).
#' @param fontface The text fontface. Default(NULL).
#' @param textLabel The text textLabel. Default(NULL).
#' @param textRot The text angle. Default(NULL).
#' @param textHVjust The text distance from the segment. Default(0.2).
#' @param hjust The text hjust. Default(NULL).
#' @param vjust The text vjust. Default(NULL).
#'
#' @return Return a ggplot object.
#' @export
#'
#' @examples
#'# ===============================
#'# test function
#'
#' # load data
#' data(p)
#' data(pdot)
#'
#'# default plot
#'annoSegment(object = p,
#'            annoPos = 'top',
#'            xPosition = c(1:10))
#'
#'# adjust rectWidth
#'annoSegment(object = p,
#'            annoPos = 'top',
#'            xPosition = c(1:10),
#'            segWidth = 0.8)
#'
#'# add branch
#'annoSegment(object = pdot,
#'            annoPos = 'top',
#'            annoManual = TRUE,
#'            xPosition = list(c(1,3,4,7,9,11,12,15,17,19,20),
#'                             c(2,3,6,8,10,11,14,16,18,19,21)),
#'            yPosition = 9,
#'            segWidth = 0.8,
#'            pCol = rep('black',11),
#'            addBranch = TRUE,
#'            branDirection = -1,
#'            lwd = 3)

# define function
annoSegment <- function(object = NULL,
                        relSideDist = 0.1,
                        annoPos = 'top',
                        xPosition = NULL,
                        yPosition = NULL,
                        pCol = NULL,
                        segWidth = 1,
                        lty = NULL,
                        lwd = 10,
                        alpha = NULL,
                        lineend = 'square',
                        annoManual = FALSE,
                        mArrow = NULL,
                        addBranch = FALSE,
                        bArrow = NULL,
                        branDirection = 1,
                        branRelSegLen = 0.3,
                        addText = FALSE,
                        textCol = NULL,
                        textSize = NULL,
                        fontfamily = NULL,
                        fontface = NULL,
                        textLabel = NULL,
                        textRot = 0,
                        textHVjust = 0.2,
                        hjust = NULL,
                        vjust = NULL){
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
  # whether use own annotation coordinate
  if(annoManual == FALSE){
    # annotation position
    if(annoPos %in% c('top','botomn')){
      nPoints <- length(xPosition)

      # xPos
      xPos <- xPosition
      xmin <- xPos - segWidth/2
      xmax <- xPos + segWidth/2

      # not supply yPos auto calculate
      if(is.null(yPosition)){
        # numeric or discrete
        if(is.numeric(data_y)){
          if(annoPos == 'top'){
            ymax <- max(data_y) + relSideDist*max(data_y)
            ymin <- ymax
          }else{
            ymin <- min(data_y) - relSideDist*max(data_y)
            ymax <- ymin
          }
        }else{
          if(annoPos == 'top'){
            ymax <- length(data_y) + relSideDist*length(data_y)
            ymin <- ymax
          }else{
            ymin <- -relSideDist*length(data_y)
            ymax <- ymin
          }
        }
      }else{
        ymax <- yPosition[1]
        ymin <- yPosition[1]
      }

    }else if(annoPos %in% c('left','right')){
      nPoints <- length(yPosition)

      # yPos
      yPos <- yPosition
      ymin <- yPos - segWidth/2
      ymax <- yPos + segWidth/2

      # not supply xPos auto calculate
      if(is.null(xPosition)){
        # numeric or discrete
        if(is.numeric(data_x)){
          if(annoPos == 'left'){
            xmin <- min(data_x) - relSideDist*max(data_x)
            xmax <- xmin
          }else{
            xmax <- max(data_x) + relSideDist*max(data_x)
            xmin <- xmax
          }
        }else{
          if(annoPos == 'left'){
            xmin <- -relSideDist*length(data_x)
            xmax <- xmin
          }else{
            xmax <- length(data_x) + relSideDist*length(data_x)
            xmin <- xmax
          }
        }
      }else{
        xmin <- xPosition[1]
        xmax <- xPosition[1]
      }
    }
  }else{
    # manually x and y positions

    # annotation position
    if(annoPos %in% c('top','botomn')){
      xmin <- xPosition[[1]] - segWidth/2
      xmax <- xPosition[[2]] + segWidth/2

      ymax <- yPosition[[1]]
      ymin <- yPosition[[1]]
    }else{
      xmin <- xPosition[[1]]
      xmax <- xPosition[[1]]

      ymin <- yPosition[[1]] - segWidth/2
      ymax <- yPosition[[2]] + segWidth/2
    }

    nPoints <- max(length(xmin),length(ymin))
  }

  # ============================================================================
  # color
  if(is.null(pCol)){
    pCol <- useMyCol('stallion',n = nPoints)
  }else{
    pCol <- pCol
  }

  # ============================================================================
  # plot
  if(annoPos %in% c('top','botomn')){
    # plot
    for (i in 1:nPoints)  {
      object <- object +
        # add segments
        ggplot2::annotation_custom(
          grob = grid::segmentsGrob(gp = grid::gpar(col = pCol[i],
                                                    fill = pCol[i],
                                                    lty = lty,
                                                    lwd = lwd,
                                                    lineend = lineend,
                                                    alpha = alpha),
                                    arrow = mArrow),
          xmin = ggplot2::unit(xmin[i],'native'),xmax = ggplot2::unit(xmax[i],'native'),
          ymin = ggplot2::unit(ymin,'native'),ymax = ggplot2::unit(ymax,'native'))
    }
  }else if(annoPos %in% c('left','right')){
    # plot
    for (i in 1:nPoints)  {
      object <- object +
        # add segments
        ggplot2::annotation_custom(
          grob = grid::segmentsGrob(gp = grid::gpar(col = pCol[i],
                                                    fill = pCol[i],
                                                    lty = lty,
                                                    lwd = lwd,
                                                    lineend = lineend,
                                                    alpha = alpha),
                                    arrow = mArrow),
          xmin = ggplot2::unit(xmin,'native'),xmax = ggplot2::unit(xmax,'native'),
          ymin = ggplot2::unit(ymin[i],'native'),ymax = ggplot2::unit(ymax[i],'native'))
    }
  }else{}

  # ============================================================================
  # whether add branch
  if(addBranch == TRUE){
    if(annoPos %in% c('top','botomn')){
      # calculate x
      brXmin <- c(xmin,xmax)
      brXmax <- c(xmin,xmax)

      # calculate y
      brYmin <- ymax + branRelSegLen*segWidth*branDirection
      brYmax <- ymax
    }else{
      # calculate x
      brXmin <- xmax
      brXmax <- xmax + branRelSegLen*segWidth*branDirection

      # calculate y
      brYmin <- c(ymin,ymax)
      brYmax <- c(ymin,ymax)
    }

    # new color
    pCol2 <- rep(pCol,2)
  }

  # ============================================================================
  if(addBranch == TRUE & annoPos %in% c('top','botomn')){
    # plot
    for (i in 1:(2*nPoints))  {
      object <- object +
        # add segments
        ggplot2::annotation_custom(
          grob = grid::segmentsGrob(gp = grid::gpar(col = pCol2[i],
                                                    fill = pCol2[i],
                                                    lty = lty,
                                                    lwd = lwd,
                                                    lineend = lineend,
                                                    alpha = alpha),
                                    arrow = bArrow),
          xmin = ggplot2::unit(brXmin[i],'native'),xmax = ggplot2::unit(brXmax[i],'native'),
          ymin = ggplot2::unit(brYmin,'native'),ymax = ggplot2::unit(brYmax,'native'))
    }
  }else if(addBranch == TRUE & annoPos %in% c('left','right')){
    # plot
    for (i in 1:(2*nPoints))  {
      object <- object +
        # add segments
        ggplot2::annotation_custom(
          grob = grid::segmentsGrob(gp = grid::gpar(col = pCol2[i],
                                                    fill = pCol2[i],
                                                    lty = lty,
                                                    lwd = lwd,
                                                    lineend = lineend,
                                                    alpha = alpha),
                                    arrow = bArrow),
          xmin = ggplot2::unit(brXmin,'native'),xmax = ggplot2::unit(brXmax,'native'),
          ymin = ggplot2::unit(brYmin[i],'native'),ymax = ggplot2::unit(brYmax[i],'native'))
    }
  }else{}

  # ============================================================================
  # text color
  if(is.null(textCol)){
    textCol <- useMyCol('stallion',n = nPoints)
  }else{
    textCol <- textCol
  }


  # whether add text label
  if(addText == TRUE & annoPos %in% c('top','botomn')){
    # plot
    for (i in 1:nPoints)  {
      object <- object +
        # add text
        ggplot2::annotation_custom(
          grob = grid::textGrob(gp = grid::gpar(col = textCol[i],
                                                fontsize = textSize,
                                                fontfamily = fontfamily,
                                                fontface = fontface),
                                hjust = hjust,
                                vjust = vjust,
                                label = textLabel[i],
                                check.overlap = T,
                                just = "centre",
                                rot = textRot),
          xmin = ggplot2::unit(xmin[i],'native'),xmax = ggplot2::unit(xmax[i],'native'),
          ymin = ggplot2::unit(ymin + textHVjust,'native'),ymax = ggplot2::unit(ymin + textHVjust,'native'))
    }
  }else if(addBranch == TRUE & annoPos %in% c('left','right')){
    # plot
    for (i in 1:nPoints)  {
      object <- object +
        # add text
        ggplot2::annotation_custom(
          grob = grid::textGrob(gp = grid::gpar(col = textCol[i],
                                                fontsize = textSize,
                                                fontfamily = fontfamily,
                                                fontface = fontface),
                                hjust = hjust,
                                vjust = vjust,
                                label = textLabel[i],
                                check.overlap = T,
                                just = "centre",
                                rot = textRot),
          xmin = ggplot2::unit(xmin + textHVjust,'native'),xmax = ggplot2::unit(xmax + textHVjust,'native'),
          ymin = ggplot2::unit(ymin[i],'native'),ymax = ggplot2::unit(ymin[i],'native'))
    }
  }else{}

  # ============================================================================
  # print
  print(object)
}



###############################
#' This is a test data for this package
#' test data describtion
#'
#' @name pdot
#' @docType data
#' @author Junjun Lao
"pdot"
