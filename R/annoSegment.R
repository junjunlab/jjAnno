#' @title annoSegment
#' @name annoSegment
#' @author Junjun Lao
#' @description This function is used to add segment annotations in plot.
#' @param object Your ggplot list. Default(NULL).
#' @param relSideDist The relative distance ratio to the y axis range. Default(0.1).
#' @param aesGroup Whether use your group column to add rect annotation. Default("FALSE").
#' @param aesGroName The mapping column name. Default(NULL).
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
#' @param myFacetGrou Your facet group name to be added with annotation when object is a faceted object. Default(NULL).
#' @param aes_x = NULL You should supply the plot X mapping name when annotate a facetd plot. Default(NULL).
#' @param aes_y = NULL You should supply the plot Y mapping name when annotate a facetd plot. Default(NULL).
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

globalVariables(c(".data"))

# define function
annoSegment <- function(object = NULL,
                        relSideDist = 0.1,
                        aesGroup = FALSE,
                        aesGroName = NULL,
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
                        vjust = NULL,
                        myFacetGrou = NULL,
                        aes_x = NULL,
                        aes_y = NULL){
  # ============================================================================
  # facet group name
  facetName <- names(object$facet$params$facets)

  # specify a group
  if(is.null(myFacetGrou) & !is.null(facetName)){
    myFacetGrou <- unique(data[,facetName])[1]
  }else if(!is.null(myFacetGrou) & !is.null(facetName)){
    myFacetGrou <- myFacetGrou
  }else{

  }

  # ============================================================================
  # get data
  data <- object$data

  # get mapping variables
  if(is.null(facetName)){
    aes_x <- ggiraphExtra::getMapping(object$mapping,"x")
    aes_y <- ggiraphExtra::getMapping(object$mapping,"y")
  }else{
    aes_x <- aes_x
    aes_y <- aes_y
  }

  # test variable type
  data_x <- data[,c(aes_x)]
  data_y <- data[,c(aes_y)]

  # ============================================================================
  # whether use own annotation coordinate
  if(annoManual == FALSE){
    # annotation position
    if(annoPos %in% c('top','botomn')){
      # whether use group mapping
      if(aesGroup == FALSE){
        nPoints <- length(xPosition)

        # xPos
        xPos <- xPosition
        xmin <- xPos - segWidth/2
        xmax <- xPos + segWidth/2
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
        xmin <- start - segWidth/2
        xmax <- end + segWidth/2

        nPoints <- length(start)
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
        ymax <- yPosition[1]
        ymin <- yPosition[1]
      }

    }else if(annoPos %in% c('left','right')){
      # whether use group mapping
      if(aesGroup == FALSE){
        nPoints <- length(yPosition)

        # yPos
        yPos <- yPosition
        ymin <- yPos - segWidth/2
        ymax <- yPos + segWidth/2
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
        ymin <- start - segWidth/2
        ymax <- end + segWidth/2

        nPoints <- length(start)
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
  ## This function allows us to specify which facet to annotate
  annotation_custom2 <- function(grob, xmin = -Inf, xmax = Inf,
                                 ymin = -Inf, ymax = Inf, data){
    ggplot2::layer(data = data, stat = StatIdentity, position = PositionIdentity,
                   geom = ggplot2::GeomCustomAnn,
                   inherit.aes = TRUE,
                   params = list(grob = grob,
                                 xmin = xmin, xmax = xmax,
                                 ymin = ymin, ymax = ymax))
  }

  # ============================================================================
  # color
  if(is.null(pCol)){
    pCol <- useMyCol('stallion',n = nPoints)
  }else{
    pCol <- pCol
  }

  # ============================================================================
  if(is.null(facetName)){
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
            xmin = xmin[i],
            xmax = xmax[i],
            ymin = ymin,
            ymax = ymax)
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
            xmin = xmin,
            xmax = xmax,
            ymin = ymin[i],
            ymax = ymax[i])
      }
    }else{}
  }else{
    # ==========================================
    # facet data
    facet_data <- data.frame(myFacetGrou)
    colnames(facet_data) <- facetName

    # plot
    if(annoPos %in% c('top','botomn')){
      # plot
      for (i in 1:nPoints)  {
        object <- object +
          # add segments
          annotation_custom2(
            grob = grid::segmentsGrob(gp = grid::gpar(col = pCol[i],
                                                      fill = pCol[i],
                                                      lty = lty,
                                                      lwd = lwd,
                                                      lineend = lineend,
                                                      alpha = alpha),
                                      arrow = mArrow),
            data = facet_data,
            xmin = xmin[i],
            xmax = xmax[i],
            ymin = ymin,
            ymax = ymax)
      }
    }else if(annoPos %in% c('left','right')){
      # plot
      for (i in 1:nPoints)  {
        object <- object +
          # add segments
          annotation_custom2(
            grob = grid::segmentsGrob(gp = grid::gpar(col = pCol[i],
                                                      fill = pCol[i],
                                                      lty = lty,
                                                      lwd = lwd,
                                                      lineend = lineend,
                                                      alpha = alpha),
                                      arrow = mArrow),
            data = facet_data,
            xmin = xmin,
            xmax = xmax,
            ymin = ymin[i],
            ymax = ymax[i])
      }
    }else{}
  }


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
  if(is.null(facetName)){
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
            xmin = brXmin[i],
            xmax = brXmax[i],
            ymin = brYmin,
            ymax = brYmax)
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
            xmin = brXmin,
            xmax = brXmax,
            ymin = brYmin[i],
            ymax = brYmax[i])
      }
    }else{}
  }else{
    # =================================
    if(addBranch == TRUE & annoPos %in% c('top','botomn')){
      # plot
      for (i in 1:(2*nPoints))  {
        object <- object +
          # add segments
          annotation_custom2(
            grob = grid::segmentsGrob(gp = grid::gpar(col = pCol2[i],
                                                      fill = pCol2[i],
                                                      lty = lty,
                                                      lwd = lwd,
                                                      lineend = lineend,
                                                      alpha = alpha),
                                      arrow = bArrow),
            data = facet_data,
            xmin = brXmin[i],
            xmax = brXmax[i],
            ymin = brYmin,
            ymax = brYmax)
      }
    }else if(addBranch == TRUE & annoPos %in% c('left','right')){
      # plot
      for (i in 1:(2*nPoints))  {
        object <- object +
          # add segments
          annotation_custom2(
            grob = grid::segmentsGrob(gp = grid::gpar(col = pCol2[i],
                                                      fill = pCol2[i],
                                                      lty = lty,
                                                      lwd = lwd,
                                                      lineend = lineend,
                                                      alpha = alpha),
                                      arrow = bArrow),
            data = facet_data,
            xmin = brXmin,
            xmax = brXmax,
            ymin = brYmin[i],
            ymax = brYmax[i])
      }
    }else{}
  }

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

  # ==================================
  # whether add text label
  if(is.null(facetName)){
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
            xmin = xmin[i],
            xmax = xmax[i],
            ymin = ymin + textHVjust,
            ymax = ymax + textHVjust)
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
            xmin = xmin + textHVjust,
            xmax = xmax + textHVjust,
            ymin = ymin[i],
            ymax = ymax[i])
      }
    }else{}
  }else{
    # ===================================
    if(addText == TRUE & annoPos %in% c('top','botomn')){
      # plot
      for (i in 1:nPoints)  {
        object <- object +
          # add text
          annotation_custom2(
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
            data = facet_data,
            xmin = xmin[i],
            xmax = xmax[i],
            ymin = ymin + textHVjust,
            ymax = ymax + textHVjust)
      }
    }else if(addText == TRUE & annoPos %in% c('left','right')){
      # plot
      for (i in 1:nPoints)  {
        object <- object +
          # add text
          annotation_custom2(
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
            data = facet_data,
            xmin = xmin + textHVjust,
            xmax = xmin + textHVjust,
            ymin = ymin[i],
            ymax = ymax[i])
      }
    }else{}
  }

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
