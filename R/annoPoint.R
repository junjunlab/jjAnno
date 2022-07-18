#' @title annoPoint
#' @name annoPoint
#' @author Junjun Lao
#' @description This function is used to add points annotations in plot.
#' @param object Your ggplot list. Default(NULL).
#' @param annoPos The position for the annotation to be added. Default("top").
#' @param relSideDist The relative distance ratio to the y axis range. Default(0.1).
#' @param xPosition The x axis coordinate for the points. Default(NULL).
#' @param yPosition The y axis coordinate for the points. Default(NULL).
#' @param pCol The point colors. Default(NULL).
#' @param ptSize The point size. Default(3).
#' @param ptShape The point shape. Default(NULL).
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
#' # default plot
#' annoPoint(object = p,
#'           annoPos = 'top',
#'           xPosition = c(1:10))
#'
#' # specify yPosition
#' annoPoint(object = p,
#'           annoPos = 'top',
#'           xPosition = c(1:10),
#'           yPosition = rep(c(2,4,2,6,4),each = 2))
#'
#' # add right
#' annoPoint(object = p,
#'           annoPos = 'right',
#'           yPosition = c(1:10))
#'
#' # left
#' annoPoint(object = p,
#'           annoPos = 'left',
#'           yPosition = c(1:10))
#'
#' # supply xPosition to ajust
#' annoPoint(object = p,
#'           annoPos = 'right',
#'           yPosition = c(1:10),
#'           xPosition = 0.3)
#'

# define functions
annoPoint <- function(object = NULL,
                      relSideDist = 0.1,
                      annoPos = 'top',
                      xPosition = NULL,
                      yPosition = NULL,
                      pCol = NULL,
                      ptSize = 3,
                      ptShape = NULL){
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
  # annotation position
  if(annoPos %in% c('top','botomn')){
    nPoints <- length(xPosition)

    # xPos
    xPos <- xPosition

    # not supply yPos auto calculate
    if(is.null(yPosition)){
      # numeric or discrete
      if(is.numeric(data_y)){
        if(annoPos == 'top'){
          yPos <- max(data_y) + relSideDist*max(data_y)
        }else{
          yPos <- min(data_y) - relSideDist*max(data_y)
        }
      }else{
        if(annoPos == 'top'){
          yPos <- length(data_y) + relSideDist*length(data_y)
        }else{
          yPos <- 0
        }
      }
    }else{
      # whether supply a single value
      if(length(yPosition) == 1){
        yPos <- rep(yPosition,nPoints)
      }else{
        yPos <- yPosition
      }
    }

  }else if(annoPos %in% c('left','right')){
    nPoints <- length(yPosition)

    # yPos
    yPos <- yPosition

    # not supply xPos auto calculate
    if(is.null(xPosition)){
      # numeric or discrete
      if(is.numeric(data_x)){
        if(annoPos == 'left'){
          xPos <- min(data_x) - relSideDist*max(data_x)
        }else{
          xPos <- max(data_x) + relSideDist*max(data_x)
        }
      }else{
        if(annoPos == 'left'){
          xPos <- 0
        }else{
          xPos <- length(data_x) + relSideDist*length(data_x)
        }
      }
    }else{
      # whether supply a single value
      if(length(xPosition) == 1){
        xPos <- rep(xPosition,nPoints)
      }else{
        xPos <- xPosition
      }
    }
  }

  # ============================================================================
  # color
  if(is.null(pCol)){
    # pCol <- pal_npg()(nPoints)
    pCol <- useMyCol('stallion',n = nPoints)
  }else{
    pCol <- pCol
  }

  # shape
  if(is.null(ptShape)){
    pchPoint <- rep(19,nPoints)
  }else{
    pchPoint <- ptShape
  }
  # ============================================================================
  # plot
  if(is.null(yPosition) & !is.null(xPosition)){
    # loop add points
    for (i in 1:nPoints)  {
      object <- object +
        # add points
        ggplot2::annotation_custom(
          grob = grid::pointsGrob(gp = grid::gpar(col = pCol[i],
                                                  fill = pCol[i]),
                                  size = ggplot2::unit(ptSize,'char'),
                                  pch = pchPoint),
          xmin = ggplot2::unit(xPos[i],'native'),xmax = ggplot2::unit(xPos[i],'native'),
          ymin = ggplot2::unit(yPos,'native'),ymax = ggplot2::unit(yPos,'native'))
    }
  }else if(is.null(xPosition) & !is.null(yPosition)){
    # loop add points
    for (i in 1:nPoints)  {
      object <- object +
        # add points
        ggplot2::annotation_custom(
          grob = grid::pointsGrob(gp = grid::gpar(col = pCol[i],
                                                  fill = pCol[i]),
                                  size = ggplot2::unit(ptSize,'char'),
                                  pch = pchPoint),
          xmin = ggplot2::unit(xPos,'native'),xmax = ggplot2::unit(xPos,'native'),
          ymin = ggplot2::unit(yPos[i],'native'),ymax = ggplot2::unit(yPos[i],'native'))

    }
  }else if(!is.null(yPosition) & !is.null(xPosition)){
    # loop add points
    for (i in 1:nPoints)  {
      object <- object +
        # add points
        ggplot2::annotation_custom(
          grob = grid::pointsGrob(gp = grid::gpar(col = pCol[i],
                                                  fill = pCol[i]),
                                  size = ggplot2::unit(ptSize,'char'),
                                  pch = pchPoint),
          xmin = ggplot2::unit(xPos[i],'native'),xmax = ggplot2::unit(xPos[i],'native'),
          ymin = ggplot2::unit(yPos[i],'native'),ymax = ggplot2::unit(yPos[i],'native'))

    }
  }else{
    print('Please supply at least xPos or yPos!')
  }

  # print
  print(object)
}



###############################
#' This is a test data for this package
#' test data describtion
#'
#' @name p
#' @docType data
#' @author Junjun Lao
"p"
