#' @title annoRect
#' @name annoRect
#' @author Junjun Lao
#' @description This function is used to add rect annotations in plot.
#' @param object Your ggplot list. Default(NULL).
#' @param relSideDist The relative distance ratio to the y axis range. Default(0.1).
#' @param aesGroup Whether use your group column to add rect annotation. Default("FALSE").
#' @param aesGroName The mapping column name. Default(NULL).
#' @param annoPos The position for the annotation to be added. Default("top").
#' @param xPosition The x axis coordinate for the rect. Default(NULL).
#' @param yPosition The y axis coordinate for the rect. Default(NULL).
#' @param pCol The rect colors. Default(NULL).
#' @param pFill The rect fill colors. Default(NULL).
#' @param rectWidth The relative rect width. Default(1).
#' @param lty The rect line type. Default(NULL).
#' @param lwd The rect line width. Default(NULL).
#' @param alpha The rect fill color alpha. Default(NULL).
#' @param annoManual Whether annotate by yourself by supplying with x and y coordinates. Default(FALSE).
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
#' # ===============================
#' # test function
#'
#' # load data
#' data(p)
#' data(pgo)
#'
#' # default plot
#' annoRect(object = p,
#'          annoPos = 'top',
#'          xPosition = c(1:10))
#'
#' # you can set y axis no expand
#' annoRect(object = p,
#'          annoPos = 'top',
#'          xPosition = c(1:10)) +
#'   ggplot2::scale_y_discrete(expand = c(0,0))
#'
#' # adjust yPosition
#' annoRect(object = p,
#'          annoPos = 'top',
#'          xPosition = c(1:10),
#'          yPosition = c(11,11.5))
#'
#'# another example annotation GO terms
#'annoRect(object = pgo,
#'         annoPos = 'right',
#'         yPosition = c(1:15),
#'         pCol = rep('transparent',15),
#'         pFill = rep(c('#F5F0BB','#C4DFAA','#90C8AC'),each = 5),
#'         xPosition = c(3,9.5),
#'         rectWidth = 1)

globalVariables(c(".data"))

# define functions
annoRect <- function(object = NULL,
                     relSideDist = 0.1,
                     aesGroup = FALSE,
                     aesGroName = NULL,
                     annoPos = 'top',
                     xPosition = NULL,
                     yPosition = NULL,
                     pCol = NULL,
                     pFill = NULL,
                     rectWidth = 1,
                     lty = NULL,
                     lwd = NULL,
                     alpha = NULL,
                     annoManual = FALSE,
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
      if(aesGroup == FALSE){
        nPoints <- length(xPosition)

        # xPos
        xPos <- xPosition
        xmin <- xPos - rectWidth/2
        xmax <- xPos + rectWidth/2
      }else{
        # order
        groupInfo <- data %>% dplyr::select(.data[[aes_x]],.data[[aesGroName]]) %>%
          unique() %>%
          dplyr::select(.data[[aesGroName]]) %>%
          table() %>%
          data.frame()

        # calculate group coordinate
        start <- c(1,groupInfo$Freq[1:(length(groupInfo$Freq) - 1)]) %>%
          cumsum()
        end <- cumsum(groupInfo$Freq)

        # xPos
        xmin <- start - rectWidth/2
        xmax <- end + rectWidth/2

        nPoints <- length(start)
      }

      # not supply yPos auto calculate
      if(is.null(yPosition)){
        # numeric or discrete
        if(is.numeric(data_y)){
          if(annoPos == 'top'){
            ymax <- max(data_y) + relSideDist*max(data_y)
            ymin <- max(data_y)
          }else{
            ymin <- min(data_y) - relSideDist*max(data_y)
            ymax <- min(data_y)
          }
        }else{
          if(annoPos == 'top'){
            ymax <- length(unique(data_y)) + relSideDist*length(unique(data_y))
            ymin <- length(unique(data_y))
          }else{
            ymax <- 0
            ymin <- -relSideDist*length(unique(data_y))
          }
        }
      }else{
        ymax <- yPosition[1]
        ymin <- yPosition[2]
      }

    }else if(annoPos %in% c('left','right')){
      # whether use group mapping
      if(aesGroup == FALSE){
        nPoints <- length(yPosition)

        # yPos
        yPos <- yPosition
        ymin <- yPos - rectWidth/2
        ymax <- yPos + rectWidth/2
      }else{
        # order
        groupInfo <- data %>% dplyr::select(.data[[aes_y]],.data[[aesGroName]]) %>%
          unique() %>%
          dplyr::select(.data[[aesGroName]]) %>%
          table() %>%
          data.frame()

        # calculate group coordinate
        start <- c(1,groupInfo$Freq[1:(length(groupInfo$Freq) - 1)]) %>%
          cumsum()

        end <- cumsum(groupInfo$Freq)

        # yPos
        ymin <- start - rectWidth/2
        ymax <- end + rectWidth/2

        nPoints <- length(start)
      }

      # not supply xPos auto calculate
      if(is.null(xPosition)){
        # numeric or discrete
        if(is.numeric(data_x)){
          if(annoPos == 'left'){
            xmin <- min(data_x) - relSideDist*max(data_x)
            xmax <- min(data_x)
          }else{
            xmin <- max(data_x)
            xmax <- max(data_x) + relSideDist*max(data_x)
          }
        }else{
          if(annoPos == 'left'){
            xmin <- -relSideDist*length(unique(data_x))
            xmax <- 0
          }else{
            xmin <- length(unique(data_x))
            xmax <- length(unique(data_x)) + relSideDist*length(unique(data_x))
          }
        }
      }else{
        xmin <- xPosition[1]
        xmax <- xPosition[2]
      }
    }
  }else{
    # manually x and y positions
    xmin <- xPosition[[1]]
    xmax <- xPosition[[2]]

    ymax <- yPosition[[1]]
    ymin <- yPosition[[2]]

    nPoints <- max(length(xmin),length(ymin))
  }
  # ============================================================================
  # color
  if(is.null(pCol) & is.null(pFill)){
    pCol <- useMyCol('stallion',n = nPoints)
    pFill <- useMyCol('stallion',n = nPoints)
  }else if(is.null(pCol)){
    pCol <- useMyCol('stallion',n = nPoints)
    pFill <- pFill
  }else if(is.null(pFill)){
    pCol <- pCol
    pFill <- useMyCol('stallion',n = nPoints)
  }else{
    pCol <- pCol
    pFill <- pFill
  }

  # ============================================================================
  if(annoPos %in% c('top','botomn')){
    if(!is.list(yPosition)){
      # plot
      for (i in 1:nPoints)  {
        object <- object +
          # add points
          ggplot2::annotation_custom(
            grob = grid::rectGrob(gp = grid::gpar(col = pCol[i],
                                                  fill = pFill[i],
                                                  lty = lty,
                                                  lwd = lwd,
                                                  alpha = alpha)),
            xmin = ggplot2::unit(xmin[i],'native'),xmax = ggplot2::unit(xmax[i],'native'),
            ymin = ggplot2::unit(ymin,'native'),ymax = ggplot2::unit(ymax,'native'))
      }
    }else{
      # plot
      for (i in 1:nPoints)  {
        object <- object +
          # add points
          ggplot2::annotation_custom(
            grob = grid::rectGrob(gp = grid::gpar(col = pCol[i],
                                                  fill = pFill[i],
                                                  lty = lty,
                                                  lwd = lwd,
                                                  alpha = alpha)),
            xmin = ggplot2::unit(xmin[i],'native'),xmax = ggplot2::unit(xmax[i],'native'),
            ymin = ggplot2::unit(ymin[i],'native'),ymax = ggplot2::unit(ymax[i],'native'))
      }
    }
  }else if(annoPos %in% c('left','right')){
    if(!is.list(xPosition)){
      # plot
      for (i in 1:nPoints)  {
        object <- object +
          # add points
          ggplot2::annotation_custom(
            grob = grid::rectGrob(gp = grid::gpar(col = pCol[i],
                                                  fill = pFill[i],
                                                  lty = lty,
                                                  lwd = lwd,
                                                  alpha = alpha)),
            xmin = ggplot2::unit(xmin,'native'),xmax = ggplot2::unit(xmax,'native'),
            ymin = ggplot2::unit(ymin[i],'native'),ymax = ggplot2::unit(ymax[i],'native'))
      }
    }else{
      # plot
      for (i in 1:nPoints)  {
        object <- object +
          # add points
          ggplot2::annotation_custom(
            grob = grid::rectGrob(gp = grid::gpar(col = pCol[i],
                                                  fill = pFill[i],
                                                  lty = lty,
                                                  lwd = lwd,
                                                  alpha = alpha)),
            xmin = ggplot2::unit(xmin[i],'native'),xmax = ggplot2::unit(xmax[i],'native'),
            ymin = ggplot2::unit(ymin[i],'native'),ymax = ggplot2::unit(ymax[i],'native'))
      }
    }

  }else{}

  # ============================================================================
  # text color
  if(is.null(textCol)){
    textCol <- useMyCol('stallion',n = nPoints)
  }else{
    textCol <- textCol
  }

  # ==================================
  # test text label origin from
  if(aesGroup == FALSE){
    textLabel <- textLabel
  }else{
    textLabel <- groupInfo[,1]
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
          ymin = ggplot2::unit(ymin + textHVjust,'native'),ymax = ggplot2::unit(ymax + textHVjust,'native'))
    }
  }else if(addText == TRUE & annoPos %in% c('left','right')){
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
          ymin = ggplot2::unit(ymin[i],'native'),ymax = ggplot2::unit(ymax[i],'native'))
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
#' @name pgo
#' @docType data
#' @author Junjun Lao
"pgo"
