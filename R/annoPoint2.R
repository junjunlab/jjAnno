#' @title annoPoint2
#' @name annoPoint2
#' @author Junjun Lao
#' @description This function is used to add points annotations in plot.
#' @param object Your ggplot list. Default(NULL).
#' @param relSideDist The relative distance ratio to the y axis range. Default(0.1).
#' @param aesGroup Whether use your group column to add rect annotation. Default("FALSE").
#' @param aesShape Whether force the point shape mapping to the aesGroName. Default("FALSE").
#' @param aesGroName The mapping column name. Default(NULL).
#' @param annoPos The position for the annotation to be added. Default("top").
#' @param xPosition The x axis coordinate for the points. Default(NULL).
#' @param yPosition The y axis coordinate for the points. Default(NULL).
#' @param pCol The point colors. Default(NULL).
#' @param pFill The point fill colors. Default(NULL).
#' @param ptSize The point size. Default(3).
#' @param ptShape The point shape. Default(NULL).
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
#'# ===============================
#'# test function
#'data(p)
#'
#'# default plot
#'annoPoint2(object = p,
#'           annoPos = 'top',
#'           xPosition = c(1:10))
#'
#'# change relative distance
#'annoPoint2(object = p,
#'           annoPos = 'top',
#'           xPosition = c(1:10),
#'           relSideDist = 0)
#'
#'# specify yPosition
#'annoPoint2(object = p,
#'           annoPos = 'top',
#'           xPosition = c(1:10),
#'           yPosition = rep(c(2,4,2,6,4),each = 2))
#'
#'# add right
#'annoPoint2(object = p,
#'           annoPos = 'right',
#'           yPosition = c(1:10))
#'
#'# left
#'annoPoint2(object = p,
#'           annoPos = 'left',
#'           yPosition = c(1:10))
#'
#'# supply xPosition to ajust
#'annoPoint2(object = p,
#'           annoPos = 'right',
#'           yPosition = c(1:10),
#'           xPosition = 0.3)
#'
#'# change point size and shape
#'p1 <- annoPoint2(object = p,
#'                 annoPos = 'top',
#'                 xPosition = c(1:10),
#'                 ptSize = 2,
#'                 ptShape = 25)
#'
#'# add to right
#'annoPoint2(object = p1,
#'           annoPos = 'right',
#'           yPosition = c(1:10),
#'           ptSize = 2,
#'           ptShape = 23)
#'
#'# add manually
#'annoPoint2(object = p,
#'           annoPos = 'right',
#'           annoManual = TRUE,
#'           yPosition = c(1:10),
#'           xPosition = c(1:10))

# define function
annoPoint2 <- function(object = NULL,
                       relSideDist = 0.1,
                       aesGroup = FALSE,
                       aesShape = FALSE,
                       aesGroName = NULL,
                       annoPos = 'top',
                       xPosition = NULL,
                       yPosition = NULL,
                       pCol = NULL,
                       pFill = NULL,
                       ptSize = 3,
                       ptShape = NULL,
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
        xmin <- xPos
        xmax <- xPos
      }else{
        # order
        groupInfo <- data %>% dplyr::select(.data[[aes_x]],.data[[aesGroName]]) %>%
          unique() %>%
          dplyr::select(.data[[aesGroName]]) %>%
          table() %>%
          data.frame()

        nPoints <- sum(groupInfo$Freq)
        xmin <- 1:nPoints
        xmax <- xmin
      }

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
            ymax <- length(unique(data_y)) + relSideDist*length(unique(data_y))
            ymin <- ymax
          }else{
            ymin <- -relSideDist*length(unique(data_y))
            ymax <- ymin
          }
        }
      }else{
        ymax <- yPosition
        ymin <- yPosition
      }

    }else if(annoPos %in% c('left','right')){
      # whether use group mapping
      if(aesGroup == FALSE){
        nPoints <- length(yPosition)

        # yPos
        yPos <- yPosition
        ymin <- yPos
        ymax <- yPos
      }else{
        # order
        groupInfo <- data %>% dplyr::select(.data[[aes_y]],.data[[aesGroName]]) %>%
          unique() %>%
          dplyr::select(.data[[aesGroName]]) %>%
          table() %>%
          data.frame()

        nPoints <- nPoints <- sum(groupInfo$Freq)
        ymin <- 1:nPoints
        ymax <- ymin
      }

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
            xmin <- -relSideDist*length(unique(data_x))
            xmax <- xmin
          }else{
            xmax <- length(unique(data_x)) + relSideDist*length(unique(data_x))
            xmin <- xmax
          }
        }
      }else{
        xmin <- xPosition
        xmax <- xPosition
      }
    }
  }else{
    # manually x and y positions

    # annotation position
    xmin <- xPosition
    xmax <- xPosition

    ymin <- yPosition
    ymax <- yPosition

    nPoints <- max(length(xmin),length(ymin))
  }
  # ============================================================================
  # color
  if(is.null(pCol) & is.null(pFill)){
    if(aesGroup == FALSE){
      pCol <- useMyCol('stallion',n = nPoints)
      pFill <- useMyCol('stallion',n = nPoints)
    }else{
      pCol <- lapply(1:length(groupInfo$Freq), function(x){
        rep(useMyCol('stallion',x)[x],groupInfo$Freq[x])
      }) %>% unlist()
      pFill <- pCol
    }
  }else if(is.null(pCol)){
    if(aesGroup == FALSE){
      pCol <- useMyCol('stallion',n = nPoints)
      pFill <- pFill
    }else{
      pCol <- lapply(1:length(groupInfo$Freq), function(x){
        rep(useMyCol('stallion',x)[x],groupInfo$Freq[x])
      }) %>% unlist()
      pFill <- pFill
    }
  }else if(is.null(pFill)){
    if(aesGroup == FALSE){
      pCol <- pCol
      pFill <- useMyCol('stallion',n = nPoints)
    }else{
      pCol <- pCol
      pFill <- lapply(1:length(groupInfo$Freq), function(x){
        rep(useMyCol('stallion',x)[x],groupInfo$Freq[x])
      }) %>% unlist()
    }
  }else{
    pCol <- pCol
    pFill <- pFill
  }

  # shape
  if(is.null(ptShape)){
    if(aesShape == FALSE){
      pchPoint <- rep(19,nPoints)
    }else{
      pchPoint <- lapply(1:nrow(groupInfo), function(x){
        rep(sample(1:25,1,replace = FALSE),groupInfo[x,2][1])
      }) %>% unlist()
    }
  }else{
    pchPoint <- ptShape
  }

  # ============================================================================
  if(annoPos %in% c('top','botomn')){
    if(is.null(yPosition) | length(yPosition) == 1){
      # loop add points
      for (i in 1:nPoints)  {
        object <- object +
          # add points
          ggplot2::annotation_custom(
            grob = grid::pointsGrob(gp = grid::gpar(col = pCol[i],
                                                    fill = pCol[i]),
                                    size = ggplot2::unit(ptSize,'char'),
                                    pch = pchPoint[i]),
            xmin = ggplot2::unit(xmin[i],'native'),
            xmax = ggplot2::unit(xmax[i],'native'),
            ymin = ggplot2::unit(ymin,'native'),
            ymax = ggplot2::unit(ymax,'native'))
      }
    }else{
      # loop add points
      for (i in 1:nPoints)  {
        object <- object +
          # add points
          ggplot2::annotation_custom(
            grob = grid::pointsGrob(gp = grid::gpar(col = pCol[i],
                                                    fill = pCol[i]),
                                    size = ggplot2::unit(ptSize,'char'),
                                    pch = pchPoint[i]),
            xmin = ggplot2::unit(xmin[i],'native'),
            xmax = ggplot2::unit(xmax[i],'native'),
            ymin = ggplot2::unit(ymin[i],'native'),
            ymax = ggplot2::unit(ymax[i],'native'))
      }
    }
  }else if(annoPos %in% c('left','right')){
    if(is.null(xPosition) | length(xPosition) == 1){
      # loop add points
      for (i in 1:nPoints)  {
        object <- object +
          # add points
          ggplot2::annotation_custom(
            grob = grid::pointsGrob(gp = grid::gpar(col = pCol[i],
                                                    fill = pCol[i]),
                                    size = ggplot2::unit(ptSize,'char'),
                                    pch = pchPoint),
            xmin = ggplot2::unit(xmin,'native'),
            xmax = ggplot2::unit(xmax,'native'),
            ymin = ggplot2::unit(ymin[i],'native'),
            ymax = ggplot2::unit(ymax[i],'native'))
      }
    }else{
      # loop add points
      for (i in 1:nPoints)  {
        object <- object +
          # add points
          ggplot2::annotation_custom(
            grob = grid::pointsGrob(gp = grid::gpar(col = pCol[i],
                                                    fill = pCol[i]),
                                    size = ggplot2::unit(ptSize,'char'),
                                    pch = pchPoint),
            xmin = ggplot2::unit(xmin[i],'native'),
            xmax = ggplot2::unit(xmax[i],'native'),
            ymin = ggplot2::unit(ymin[i],'native'),
            ymax = ggplot2::unit(ymax[i],'native'))
      }
    }
  }else{}

  # ============================================================================
  # text color
  if(is.null(textCol)){
    textCol <- pFill
  }else{
    textCol <- textCol
  }

  # ==================================
  # test text label origin from
  if(aesGroup == FALSE){
    textLabel <- textLabel
  }else{
    textLabel <- lapply(1:nrow(groupInfo), function(x){
      rep(groupInfo[x,1][1],groupInfo[x,2][1])
    }) %>% unlist()
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
          xmin = ggplot2::unit(xmin[i],'native'),
          xmax = ggplot2::unit(xmax[i],'native'),
          ymin = ggplot2::unit(ymin + textHVjust,'native'),
          ymax = ggplot2::unit(ymax + textHVjust,'native'))
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
          xmin = ggplot2::unit(xmin + textHVjust,'native'),
          xmax = ggplot2::unit(xmax + textHVjust,'native'),
          ymin = ggplot2::unit(ymin[i],'native'),
          ymax = ggplot2::unit(ymax[i],'native'))
    }
  }else{}

  # ============================================================================
  # print
  print(object)
}
