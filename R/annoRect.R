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
#' @param roundRect Whether add roundRect instead of rect. Default(FALSE).
#' @param roundRadius The roundRect corner radius. Default(0.1).
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
#' @param textShift The text label shift size. Default(0).
#' @param rotateRect Whether to rotate the rect annotation. Default(FALSE).
#' @param normRectShift The "top" or "right" rotated rect shift. Default(0).
#' @param rotatedRectShift The "botomn" or "left" rotated rect shift. Default(1).
#' @param rectAngle Whether rotate the rect with specified degree. Default(NULL).
#'
#' @param myFacetGrou Your facet group name to be added with annotation when object is a faceted object. Default(NULL).
#' @param aes_x = NULL You should supply the plot X mapping name when annotate a facetd plot. Default(NULL).
#' @param aes_y = NULL You should supply the plot Y mapping name when annotate a facetd plot. Default(NULL).
#'
#' @param continuesRect Whether add gradient-color-rect. Default(FALSE).
#' @param border Whether add border for gradient-color-rect. Default(FALSE).
#' @param conRectCol The colors for gradient-color-rect. Default(NULL).
#' @param conRectColBin The colors numbers for gradient-color-rect. Default(10).
#' @param interpolate Whether blur the colors. Default(TRUE).
#' @param revColV Whether ajust the colors orders vertically. Default(FALSE).
#' @param revColH Whether ajust the colors orders horizontally. Default(FALSE).
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
#' data(pdotfc)
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
                     roundRect = FALSE,
                     roundRadius = 0.1,
                     annoManual = FALSE,
                     addText = FALSE,
                     textCol = NULL,
                     textSize = NULL,
                     fontfamily = NULL,
                     fontface = NULL,
                     textLabel = NULL,
                     textRot = 0,
                     textHVjust = 0.2,
                     textShift = 0,
                     hjust = NULL,
                     vjust = NULL,
                     rotateRect = FALSE,
                     normRectShift = 0,
                     rotatedRectShift = 1,
                     rectAngle = NULL,
                     myFacetGrou = NULL,
                     aes_x = NULL,
                     aes_y = NULL,
                     continuesRect = FALSE,
                     border = FALSE,
                     conRectCol = NULL,
                     conRectColBin = 10,
                     interpolate = TRUE,
                     revColV = FALSE,
                     revColH = FALSE){
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
  # whether supply a rectAngle
  if(!is.null(rectAngle)){
    text_angle <- rectAngle
  }else{
    if(annoPos %in% c('top','botomn')){
      # retrive text angle
      text_angle <- object$theme$axis.text.x$angle
    }else if(annoPos %in% c('left','right')){
      # retrive text angle
      text_angle <- object$theme$axis.text.y$angle
    }

    # test special degree
    if(!is.null(text_angle)){
      if(text_angle %in% c(0,90,180,270,360)){
        text_angle <- 0
      }else{
        text_angle <- text_angle
      }
    }else{}
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
  # continues rect color
  if(continuesRect == TRUE){

    # 1.whether supply your own color
    if(is.null(conRectCol)){
      lapply(1:nPoints, function(x){
        tmp <- grDevices::colorRampPalette(c('white',pFill[x]))(conRectColBin)

        # 2.whether reverse color vertically
        if(revColV == TRUE) tmp <- rev(tmp)

        # 3.whether reverse color horizontally
        if(revColH == TRUE) tmp <- t(tmp)

        return(tmp)
      }) -> conrectcolor
    }else{
      lapply(1:length(conRectCol), function(x){
        tmp <- grDevices::colorRampPalette(conRectCol[[x]])(conRectColBin)

        # 2.whether reverse color vertically
        if(revColV == TRUE) tmp <- rev(tmp)

        # 3.whether reverse color horizontally
        if(revColH == TRUE) tmp <- t(tmp)

        return(tmp)
      }) -> conrectcolor
    }
  }else{

  }

  # ============================================================================
  if(is.null(facetName)){
    if(roundRect == FALSE){
      if(annoPos %in% c('top','botomn')){
        if(!is.list(yPosition)){
          ####################################
          # whether totate rect
          if(rotateRect == FALSE){
            ####################################
            # whether add continues color rect
            if(continuesRect == TRUE & border == FALSE){
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  ggplot2::annotation_custom(
                    grob = grid::rasterGrob(image = unlist(conrectcolor[[i]]),
                                            width = ggplot2::unit(1,'native'),
                                            height = ggplot2::unit(1,'native'),
                                            interpolate = interpolate),
                    xmin = ggplot2::unit(xmin[i],'native'),
                    xmax = ggplot2::unit(xmax[i],'native'),
                    ymin = ggplot2::unit(ymin,'native'),
                    ymax = ggplot2::unit(ymax,'native'))
              }
            }else if(continuesRect == TRUE & border == TRUE){
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  ggplot2::annotation_custom(
                    grob = grid::rasterGrob(image = unlist(conrectcolor[[i]]),
                                            width = ggplot2::unit(1,'native'),
                                            height = ggplot2::unit(1,'native'),
                                            interpolate = interpolate ),
                    xmin = ggplot2::unit(xmin[i],'native'),
                    xmax = ggplot2::unit(xmax[i],'native'),
                    ymin = ggplot2::unit(ymin,'native'),
                    ymax = ggplot2::unit(ymax,'native')) +
                  # add border
                  ggplot2::annotation_custom(
                    grob = grid::rectGrob(gp = grid::gpar(fill = 'transparent',
                                                          col = 'black',
                                                          lwd = lwd,
                                                          lty = lty)),
                    xmin = ggplot2::unit(xmin[i],'native'),
                    xmax = ggplot2::unit(xmax[i],'native'),
                    ymin = ggplot2::unit(ymin,'native'),
                    ymax = ggplot2::unit(ymax,'native'))
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
                    xmin = ggplot2::unit(xmin[i],'native'),
                    xmax = ggplot2::unit(xmax[i],'native'),
                    ymin = ggplot2::unit(ymin,'native'),
                    ymax = ggplot2::unit(ymax,'native'))
              }
            }
          }else{
            ####################################
            # rotate rect
            if(annoPos == 'top'){
              for (i in 1:nPoints)  {
                object <- object +
                  ggplot2::annotation_custom(
                    grob = grid::polygonGrob(x = c(0,
                                                   tan(pi*((90 - text_angle)/180))/abs(xmax[i] - xmin[i]),
                                                   1 + tan(pi*((90 - text_angle)/180))/abs(xmax[i] - xmin[i]),
                                                   1),
                                             y = c(0,1,1,0),
                                             gp = grid::gpar(fill = pFill[i],
                                                             col = pCol[i],
                                                             lty = lty,
                                                             lwd = lwd,
                                                             alpha = alpha)),
                    xmin = ggplot2::unit(xmin[i] - normRectShift,'native'),
                    xmax = ggplot2::unit(xmax[i] - normRectShift,'native'),
                    ymin = ggplot2::unit(ymin,'native'),
                    ymax = ggplot2::unit(ymax,'native'))

              }
            }else{
              for (i in 1:nPoints)  {
                object <- object +
                  ggplot2::annotation_custom(
                    grob = grid::polygonGrob(x = c(0,
                                                   tan(pi*((90 - text_angle)/180))/abs(xmax[i] - xmin[i]),
                                                   1 + tan(pi*((90 - text_angle)/180))/abs(xmax[i] - xmin[i]),
                                                   1),
                                             y = c(0,1,1,0),
                                             gp = grid::gpar(fill = pFill[i],
                                                             col = pCol[i],
                                                             lty = lty,
                                                             lwd = lwd,
                                                             alpha = alpha)),
                    xmin = ggplot2::unit(xmin[i] - rotatedRectShift,'native'),
                    xmax = ggplot2::unit(xmax[i] - rotatedRectShift,'native'),
                    ymin = ggplot2::unit(ymin,'native'),
                    ymax = ggplot2::unit(ymax,'native'))

              }
            }
          }
        }else{
          ####################################
          # whether totate rect
          if(rotateRect == FALSE){
            ####################################
            # whether add continues color rect
            if(continuesRect == TRUE & border == FALSE){
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  ggplot2::annotation_custom(
                    grob = grid::rasterGrob(image = unlist(conrectcolor[[i]]),
                                            width = ggplot2::unit(1,'native'),
                                            height = ggplot2::unit(1,'native'),
                                            interpolate = interpolate),
                    xmin = ggplot2::unit(xmin[i],'native'),
                    xmax = ggplot2::unit(xmax[i],'native'),
                    ymin = ggplot2::unit(ymin[i],'native'),
                    ymax = ggplot2::unit(ymax[i],'native'))
              }
            }else if(continuesRect == TRUE & border == TRUE){
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  ggplot2::annotation_custom(
                    grob = grid::rasterGrob(image = unlist(conrectcolor[[i]]),
                                            width = ggplot2::unit(1,'native'),
                                            height = ggplot2::unit(1,'native'),
                                            interpolate = interpolate ),
                    xmin = ggplot2::unit(xmin[i],'native'),
                    xmax = ggplot2::unit(xmax[i],'native'),
                    ymin = ggplot2::unit(ymin[i],'native'),
                    ymax = ggplot2::unit(ymax[i],'native')) +
                  # add border
                  ggplot2::annotation_custom(
                    grob = grid::rectGrob(gp = grid::gpar(fill = 'transparent',
                                                          col = 'black',
                                                          lwd = lwd,
                                                          lty = lty)),
                    xmin = ggplot2::unit(xmin[i],'native'),
                    xmax = ggplot2::unit(xmax[i],'native'),
                    ymin = ggplot2::unit(ymin[i],'native'),
                    ymax = ggplot2::unit(ymax[i],'native'))
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
                    xmin = ggplot2::unit(xmin[i],'native'),
                    xmax = ggplot2::unit(xmax[i],'native'),
                    ymin = ggplot2::unit(ymin[i],'native'),
                    ymax = ggplot2::unit(ymax[i],'native'))
              }
            }

          }else{
            ####################################
            # rotate rect
            if(annoPos == 'top'){
              for (i in 1:nPoints)  {
                object <- object +
                  ggplot2::annotation_custom(
                    grob = grid::polygonGrob(x = c(0,
                                                   tan(pi*((90 - text_angle)/180))/abs(xmax[i] - xmin[i]),
                                                   1 + tan(pi*((90 - text_angle)/180))/abs(xmax[i] - xmin[i]),
                                                   1),
                                             y = c(0,1,1,0),
                                             gp = grid::gpar(fill = pFill[i],
                                                             col = pCol[i],
                                                             lty = lty,
                                                             lwd = lwd,
                                                             alpha = alpha)),
                    xmin = ggplot2::unit(xmin[i] - normRectShift,'native'),
                    xmax = ggplot2::unit(xmax[i] - normRectShift,'native'),
                    ymin = ggplot2::unit(ymin[i],'native'),
                    ymax = ggplot2::unit(ymax[i],'native'))

              }
            }else{
              for (i in 1:nPoints)  {
                object <- object +
                  ggplot2::annotation_custom(
                    grob = grid::polygonGrob(x = c(0,
                                                   tan(pi*((90 - text_angle)/180))/abs(xmax[i] - xmin[i]),
                                                   1 + tan(pi*((90 - text_angle)/180))/abs(xmax[i] - xmin[i]),
                                                   1),
                                             y = c(0,1,1,0),
                                             gp = grid::gpar(fill = pFill[i],
                                                             col = pCol[i],
                                                             lty = lty,
                                                             lwd = lwd,
                                                             alpha = alpha)),
                    xmin = ggplot2::unit(xmin[i] - rotatedRectShift,'native'),
                    xmax = ggplot2::unit(xmax[i] - rotatedRectShift,'native'),
                    ymin = ggplot2::unit(ymin[i],'native'),
                    ymax = ggplot2::unit(ymax[i],'native'))

              }
            }
          }
        }
      }else if(annoPos %in% c('left','right')){
        if(!is.list(xPosition)){
          ####################################
          # whether totate rect
          if(rotateRect == FALSE){
            ####################################
            # whether add continues color rect
            if(continuesRect == TRUE & border == FALSE){
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  ggplot2::annotation_custom(
                    grob = grid::rasterGrob(image = unlist(conrectcolor[[i]]),
                                            width = ggplot2::unit(1,'native'),
                                            height = ggplot2::unit(1,'native'),
                                            interpolate = interpolate),
                    xmin = ggplot2::unit(xmin,'native'),
                    xmax = ggplot2::unit(xmax,'native'),
                    ymin = ggplot2::unit(ymin[i],'native'),
                    ymax = ggplot2::unit(ymax[i],'native'))
              }
            }else if(continuesRect == TRUE & border == TRUE){
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  ggplot2::annotation_custom(
                    grob = grid::rasterGrob(image = unlist(conrectcolor[[i]]),
                                            width = ggplot2::unit(1,'native'),
                                            height = ggplot2::unit(1,'native'),
                                            interpolate = interpolate ),
                    xmin = ggplot2::unit(xmin,'native'),
                    xmax = ggplot2::unit(xmax,'native'),
                    ymin = ggplot2::unit(ymin[i],'native'),
                    ymax = ggplot2::unit(ymax[i],'native')) +
                  # add border
                  ggplot2::annotation_custom(
                    grob = grid::rectGrob(gp = grid::gpar(fill = 'transparent',
                                                          col = 'black',
                                                          lwd = lwd,
                                                          lty = lty)),
                    xmin = ggplot2::unit(xmin,'native'),
                    xmax = ggplot2::unit(xmax,'native'),
                    ymin = ggplot2::unit(ymin[i],'native'),
                    ymax = ggplot2::unit(ymax[i],'native'))
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
                    xmin = ggplot2::unit(xmin,'native'),
                    xmax = ggplot2::unit(xmax,'native'),
                    ymin = ggplot2::unit(ymin[i],'native'),
                    ymax = ggplot2::unit(ymax[i],'native'))
              }
            }

          }else{
            ####################################
            # rotate rect
            if(annoPos == 'left'){
              for (i in 1:nPoints)  {
                object <- object +
                  ggplot2::annotation_custom(
                    grob = grid::polygonGrob(y = c(0,
                                                   tan(pi*((90 - text_angle)/180))/abs(ymax[i] - ymin[i]),
                                                   1 + tan(pi*((90 - text_angle)/180))/abs(ymax[i] - ymin[i]),
                                                   1),
                                             x = c(0,1,1,0),
                                             gp = grid::gpar(fill = pFill[i],
                                                             col = pCol[i],
                                                             lty = lty,
                                                             lwd = lwd,
                                                             alpha = alpha)),
                    xmin = ggplot2::unit(xmin,'native'),
                    xmax = ggplot2::unit(xmax,'native'),
                    ymin = ggplot2::unit(ymin[i] - rotatedRectShift,'native'),
                    ymax = ggplot2::unit(ymax[i] - rotatedRectShift,'native'))

              }
            }else{
              for (i in 1:nPoints)  {
                object <- object +
                  ggplot2::annotation_custom(
                    grob = grid::polygonGrob(y = c(0,
                                                   tan(pi*((90 - text_angle)/180))/abs(ymax[i] - ymin[i]),
                                                   1 + tan(pi*((90 - text_angle)/180))/abs(ymax[i] - ymin[i]),
                                                   1),
                                             x = c(0,1,1,0),
                                             gp = grid::gpar(fill = pFill[i],
                                                             col = pCol[i],
                                                             lty = lty,
                                                             lwd = lwd,
                                                             alpha = alpha)),
                    xmin = ggplot2::unit(xmin,'native'),
                    xmax = ggplot2::unit(xmax,'native'),
                    ymin = ggplot2::unit(ymin[i] - normRectShift,'native'),
                    ymax = ggplot2::unit(ymax[i] - normRectShift,'native'))

              }
            }
          }
        }else{
          ####################################
          # whether totate rect
          if(rotateRect == FALSE){
            ####################################
            # whether add continues color rect
            if(continuesRect == TRUE & border == FALSE){
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  ggplot2::annotation_custom(
                    grob = grid::rasterGrob(image = unlist(conrectcolor[[i]]),
                                            width = ggplot2::unit(1,'native'),
                                            height = ggplot2::unit(1,'native'),
                                            interpolate = interpolate),
                    xmin = ggplot2::unit(xmin[i],'native'),
                    xmax = ggplot2::unit(xmax[i],'native'),
                    ymin = ggplot2::unit(ymin[i],'native'),
                    ymax = ggplot2::unit(ymax[i],'native'))
              }
            }else if(continuesRect == TRUE & border == TRUE){
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  ggplot2::annotation_custom(
                    grob = grid::rasterGrob(image = unlist(conrectcolor[[i]]),
                                            width = ggplot2::unit(1,'native'),
                                            height = ggplot2::unit(1,'native'),
                                            interpolate = interpolate ),
                    xmin = ggplot2::unit(xmin[i],'native'),
                    xmax = ggplot2::unit(xmax[i],'native'),
                    ymin = ggplot2::unit(ymin[i],'native'),
                    ymax = ggplot2::unit(ymax[i],'native')) +
                  # add border
                  ggplot2::annotation_custom(
                    grob = grid::rectGrob(gp = grid::gpar(fill = 'transparent',
                                                          col = 'black',
                                                          lwd = lwd,
                                                          lty = lty)),
                    xmin = ggplot2::unit(xmin[i],'native'),
                    xmax = ggplot2::unit(xmax[i],'native'),
                    ymin = ggplot2::unit(ymin[i],'native'),
                    ymax = ggplot2::unit(ymax[i],'native'))
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
                    xmin = ggplot2::unit(xmin[i],'native'),
                    xmax = ggplot2::unit(xmax[i],'native'),
                    ymin = ggplot2::unit(ymin[i],'native'),
                    ymax = ggplot2::unit(ymax[i],'native'))
              }
            }

          }else{
            ####################################
            # rotate rect
            if(annoPos == 'left'){
              for (i in 1:nPoints)  {
                object <- object +
                  ggplot2::annotation_custom(
                    grob = grid::polygonGrob(y = c(0,
                                                   tan(pi*((90 - text_angle)/180))/abs(ymax[i] - ymin[i]),
                                                   1 + tan(pi*((90 - text_angle)/180))/abs(ymax[i] - ymin[i]),
                                                   1),
                                             x = c(0,1,1,0),
                                             gp = grid::gpar(fill = pFill[i],
                                                             col = pCol[i],
                                                             lty = lty,
                                                             lwd = lwd,
                                                             alpha = alpha)),
                    xmin = ggplot2::unit(xmin[i],'native'),
                    xmax = ggplot2::unit(xmax[i],'native'),
                    ymin = ggplot2::unit(ymin[i] - normRectShift,'native'),
                    ymax = ggplot2::unit(ymax[i] - normRectShift,'native'))

              }
            }else{
              for (i in 1:nPoints)  {
                object <- object +
                  ggplot2::annotation_custom(
                    grob = grid::polygonGrob(y = c(0,
                                                   tan(pi*((90 - text_angle)/180))/abs(ymax[i] - ymin[i]),
                                                   1 + tan(pi*((90 - text_angle)/180))/abs(ymax[i] - ymin[i]),
                                                   1),
                                             x = c(0,1,1,0),
                                             gp = grid::gpar(fill = pFill[i],
                                                             col = pCol[i],
                                                             lty = lty,
                                                             lwd = lwd,
                                                             alpha = alpha)),
                    xmin = ggplot2::unit(xmin[i],'native'),
                    xmax = ggplot2::unit(xmax[i],'native'),
                    ymin = ggplot2::unit(ymin[i] - rotatedRectShift,'native'),
                    ymax = ggplot2::unit(ymax[i] - rotatedRectShift,'native'))

              }
            }
          }
        }
      }else{}
    }else{
      # round rect corner
      if(annoPos %in% c('top','botomn')){
        if(!is.list(yPosition)){
          # plot
          for (i in 1:nPoints)  {
            object <- object +
              # add points
              ggplot2::annotation_custom(
                grob = grid::roundrectGrob(gp = grid::gpar(col = pCol[i],
                                                           fill = pFill[i],
                                                           lty = lty,
                                                           lwd = lwd,
                                                           alpha = alpha),
                                           r = ggplot2::unit(roundRadius, "snpc")),
                xmin = ggplot2::unit(xmin[i],'native'),
                xmax = ggplot2::unit(xmax[i],'native'),
                ymin = ggplot2::unit(ymin,'native'),
                ymax = ggplot2::unit(ymax,'native'))
          }
        }else{
          # plot
          for (i in 1:nPoints)  {
            object <- object +
              # add points
              ggplot2::annotation_custom(
                grob = grid::roundrectGrob(gp = grid::gpar(col = pCol[i],
                                                           fill = pFill[i],
                                                           lty = lty,
                                                           lwd = lwd,
                                                           alpha = alpha),
                                           r = ggplot2::unit(roundRadius, "snpc")),
                xmin = ggplot2::unit(xmin[i],'native'),
                xmax = ggplot2::unit(xmax[i],'native'),
                ymin = ggplot2::unit(ymin[i],'native'),
                ymax = ggplot2::unit(ymax[i],'native'))
          }
        }
      }else if(annoPos %in% c('left','right')){
        if(!is.list(xPosition)){
          # plot
          for (i in 1:nPoints)  {
            object <- object +
              # add points
              ggplot2::annotation_custom(
                grob = grid::roundrectGrob(gp = grid::gpar(col = pCol[i],
                                                           fill = pFill[i],
                                                           lty = lty,
                                                           lwd = lwd,
                                                           alpha = alpha),
                                           r = ggplot2::unit(roundRadius, "snpc")),
                xmin = ggplot2::unit(xmin,'native'),
                xmax = ggplot2::unit(xmax,'native'),
                ymin = ggplot2::unit(ymin[i],'native'),
                ymax = ggplot2::unit(ymax[i],'native'))
          }
        }else{
          # plot
          for (i in 1:nPoints)  {
            object <- object +
              # add points
              ggplot2::annotation_custom(
                grob = grid::roundrectGrob(gp = grid::gpar(col = pCol[i],
                                                           fill = pFill[i],
                                                           lty = lty,
                                                           lwd = lwd,
                                                           alpha = alpha),
                                           r = ggplot2::unit(roundRadius, "snpc")),
                xmin = ggplot2::unit(xmin[i],'native'),
                xmax = ggplot2::unit(xmax[i],'native'),
                ymin = ggplot2::unit(ymin[i],'native'),
                ymax = ggplot2::unit(ymax[i],'native'))
          }
        }
      }else{}
    }
  }else{
    # ==================================================
    # facet data
    facet_data <- data.frame(myFacetGrou)
    colnames(facet_data) <- facetName

    # facet plot
    if(roundRect == FALSE){
      if(annoPos %in% c('top','botomn')){
        if(!is.list(yPosition)){
          ####################################
          # whether totate rect
          if(rotateRect == FALSE){
            ####################################
            # whether add continues color rect
            if(continuesRect == TRUE & border == FALSE){
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  annotation_custom2(
                    grob = grid::rasterGrob(image = unlist(conrectcolor[[i]]),
                                            width = 1,
                                            height = 1,
                                            interpolate = interpolate),
                    data = facet_data,
                    xmin = xmin[i],
                    xmax = xmax[i],
                    ymin = ymin,
                    ymax = ymax)
              }
            }else if(continuesRect == TRUE & border == TRUE){
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  annotation_custom2(
                    grob = grid::rasterGrob(image = unlist(conrectcolor[[i]]),
                                            width = 1,
                                            height = 1,
                                            interpolate = interpolate ),
                    data = facet_data,
                    xmin = xmin[i],
                    xmax = xmax[i],
                    ymin = ymin,
                    ymax = ymax) +
                  # add border
                  annotation_custom2(
                    grob = grid::rectGrob(gp = grid::gpar(fill = 'transparent',
                                                          col = 'black',
                                                          lwd = lwd,
                                                          lty = lty)),
                    data = facet_data,
                    xmin = xmin[i],
                    xmax = xmax[i],
                    ymin = ymin,
                    ymax = ymax)
              }
            }else{
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  annotation_custom2(
                    grob = grid::rectGrob(gp = grid::gpar(col = pCol[i],
                                                          fill = pFill[i],
                                                          lty = lty,
                                                          lwd = lwd,
                                                          alpha = alpha)),
                    data = facet_data,
                    xmin = xmin[i],
                    xmax = xmax[i],
                    ymin = ymin,
                    ymax = ymax)
              }
            }

          }else{
            ####################################
            # rotate rect
            if(annoPos == 'top'){
              for (i in 1:nPoints)  {
                object <- object +
                  annotation_custom2(
                    grob = grid::polygonGrob(x = c(0,
                                                   tan(pi*((90 - text_angle)/180))/abs(xmax[i] - xmin[i]),
                                                   1 + tan(pi*((90 - text_angle)/180))/abs(xmax[i] - xmin[i]),
                                                   1),
                                             y = c(0,1,1,0),
                                             gp = grid::gpar(fill = pFill[i],
                                                             col = pCol[i],
                                                             lty = lty,
                                                             lwd = lwd,
                                                             alpha = alpha)),
                    data = facet_data,
                    xmin = xmin[i] - normRectShift,
                    xmax = xmax[i] - normRectShift,
                    ymin = ymin,
                    ymax = ymax)
              }
            }else{
              for (i in 1:nPoints)  {
                object <- object +
                  annotation_custom2(
                    grob = grid::polygonGrob(x = c(0,
                                                   tan(pi*((90 - text_angle)/180))/abs(xmax[i] - xmin[i]),
                                                   1 + tan(pi*((90 - text_angle)/180))/abs(xmax[i] - xmin[i]),
                                                   1),
                                             y = c(0,1,1,0),
                                             gp = grid::gpar(fill = pFill[i],
                                                             col = pCol[i],
                                                             lty = lty,
                                                             lwd = lwd,
                                                             alpha = alpha)),
                    data = facet_data,
                    xmin = xmin[i] - rotatedRectShift,
                    xmax = xmax[i] - rotatedRectShift,
                    ymin = ymin,
                    ymax = ymax)

              }
            }
          }
        }else{
          ####################################
          # whether totate rect
          if(rotateRect == FALSE){
            ####################################
            # whether add continues color rect
            if(continuesRect == TRUE & border == FALSE){
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  annotation_custom2(
                    grob = grid::rasterGrob(image = unlist(conrectcolor[[i]]),
                                            width = 1,
                                            height = 1,
                                            interpolate = interpolate),
                    data = facet_data,
                    xmin = xmin[i],
                    xmax = xmax[i],
                    ymin = ymin[i],
                    ymax = ymax[i])
              }
            }else if(continuesRect == TRUE & border == TRUE){
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  annotation_custom2(
                    grob = grid::rasterGrob(image = unlist(conrectcolor[[i]]),
                                            width = ggplot2::unit(1,'native'),
                                            height = ggplot2::unit(1,'native'),
                                            interpolate = interpolate ),
                    data = facet_data,
                    xmin = xmin[i],
                    xmax = xmax[i],
                    ymin = ymin[i],
                    ymax = ymax[i]) +
                  # add border
                  annotation_custom2(
                    grob = grid::rectGrob(gp = grid::gpar(fill = 'transparent',
                                                          col = 'black',
                                                          lwd = lwd,
                                                          lty = lty)),
                    data = facet_data,
                    xmin = xmin[i],
                    xmax = xmax[i],
                    ymin = ymin[i],
                    ymax = ymax[i])
              }
            }else{
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  annotation_custom2(
                    grob = grid::rectGrob(gp = grid::gpar(col = pCol[i],
                                                          fill = pFill[i],
                                                          lty = lty,
                                                          lwd = lwd,
                                                          alpha = alpha)),
                    data = facet_data,
                    xmin = xmin[i],
                    xmax = xmax[i],
                    ymin = ymin[i],
                    ymax = ymax[i])
              }
            }

          }else{
            ####################################
            # rotate rect
            if(annoPos == 'top'){
              for (i in 1:nPoints)  {
                object <- object +
                  annotation_custom2(
                    grob = grid::polygonGrob(x = c(0,
                                                   tan(pi*((90 - text_angle)/180))/abs(xmax[i] - xmin[i]),
                                                   1 + tan(pi*((90 - text_angle)/180))/abs(xmax[i] - xmin[i]),
                                                   1),
                                             y = c(0,1,1,0),
                                             gp = grid::gpar(fill = pFill[i],
                                                             col = pCol[i],
                                                             lty = lty,
                                                             lwd = lwd,
                                                             alpha = alpha)),
                    data = facet_data,
                    xmin = xmin[i] - normRectShift,
                    xmax = xmax[i] - normRectShift,
                    ymin = ymin[i],
                    ymax = ymax[i])

              }
            }else{
              for (i in 1:nPoints)  {
                object <- object +
                  annotation_custom2(
                    grob = grid::polygonGrob(x = c(0,
                                                   tan(pi*((90 - text_angle)/180))/abs(xmax[i] - xmin[i]),
                                                   1 + tan(pi*((90 - text_angle)/180))/abs(xmax[i] - xmin[i]),
                                                   1),
                                             y = c(0,1,1,0),
                                             gp = grid::gpar(fill = pFill[i],
                                                             col = pCol[i],
                                                             lty = lty,
                                                             lwd = lwd,
                                                             alpha = alpha)),
                    xmin = xmin[i] - rotatedRectShift,
                    xmax = xmax[i] - rotatedRectShift,
                    ymin = ymin[i],
                    ymax = ymax[i])

              }
            }
          }
        }
      }else if(annoPos %in% c('left','right')){
        if(!is.list(xPosition)){
          ####################################
          # whether totate rect
          if(rotateRect == FALSE){
            ####################################
            # whether add continues color rect
            if(continuesRect == TRUE & border == FALSE){
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  annotation_custom2(
                    grob = grid::rasterGrob(image = unlist(conrectcolor[[i]]),
                                            width = 1,
                                            height = 1,
                                            interpolate = interpolate),
                    data = facet_data,
                    xmin = xmin,
                    xmax = xmax,
                    ymin = ymin[i],
                    ymax = ymax[i])
              }
            }else if(continuesRect == TRUE & border == TRUE){
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  annotation_custom2(
                    grob = grid::rasterGrob(image = unlist(conrectcolor[[i]]),
                                            width = ggplot2::unit(1,'native'),
                                            height = ggplot2::unit(1,'native'),
                                            interpolate = interpolate ),
                    data = facet_data,
                    xmin = xmin,
                    xmax = xmax,
                    ymin = ymin[i],
                    ymax = ymax[i]) +
                  # add border
                  annotation_custom2(
                    grob = grid::rectGrob(gp = grid::gpar(fill = 'transparent',
                                                          col = 'black',
                                                          lwd = lwd,
                                                          lty = lty)),
                    data = facet_data,
                    xmin = xmin,
                    xmax = xmax,
                    ymin = ymin[i],
                    ymax = ymax[i])
              }
            }else{
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  annotation_custom2(
                    grob = grid::rectGrob(gp = grid::gpar(col = pCol[i],
                                                          fill = pFill[i],
                                                          lty = lty,
                                                          lwd = lwd,
                                                          alpha = alpha)),
                    data = facet_data,
                    xmin = xmin,
                    xmax = xmax,
                    ymin = ymin[i],
                    ymax = ymax[i])
              }
            }

          }else{
            ####################################
            # rotate rect
            if(annoPos == 'left'){
              for (i in 1:nPoints)  {
                object <- object +
                  annotation_custom2(
                    grob = grid::polygonGrob(y = c(0,
                                                   tan(pi*((90 - text_angle)/180))/abs(ymax[i] - ymin[i]),
                                                   1 + tan(pi*((90 - text_angle)/180))/abs(ymax[i] - ymin[i]),
                                                   1),
                                             x = c(0,1,1,0),
                                             gp = grid::gpar(fill = pFill[i],
                                                             col = pCol[i],
                                                             lty = lty,
                                                             lwd = lwd,
                                                             alpha = alpha)),
                    data = facet_data,
                    xmin = xmin,
                    xmax = xmax,
                    ymin = ymin[i] - rotatedRectShift,
                    ymax = ymax[i] - rotatedRectShift)

              }
            }else{
              for (i in 1:nPoints)  {
                object <- object +
                  annotation_custom2(
                    grob = grid::polygonGrob(y = c(0,
                                                   tan(pi*((90 - text_angle)/180))/abs(ymax[i] - ymin[i]),
                                                   1 + tan(pi*((90 - text_angle)/180))/abs(ymax[i] - ymin[i]),
                                                   1),
                                             x = c(0,1,1,0),
                                             gp = grid::gpar(fill = pFill[i],
                                                             col = pCol[i],
                                                             lty = lty,
                                                             lwd = lwd,
                                                             alpha = alpha)),
                    data = facet_data,
                    xmin = xmin,
                    xmax = xmax,
                    ymin = ymin[i] - normRectShift,
                    ymax = ymax[i] - normRectShift)
              }
            }
          }
        }else{
          ####################################
          # whether totate rect
          if(rotateRect == FALSE){
            ####################################
            # whether add continues color rect
            if(continuesRect == TRUE & border == FALSE){
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  annotation_custom2(
                    grob = grid::rasterGrob(image = unlist(conrectcolor[[i]]),
                                            width = 1,
                                            height = 1,
                                            interpolate = interpolate),
                    data = facet_data,
                    xmin = xmin[i],
                    xmax = xmax[i],
                    ymin = ymin[i],
                    ymax = ymax[i])
              }
            }else if(continuesRect == TRUE & border == TRUE){
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  annotation_custom2(
                    grob = grid::rasterGrob(image = unlist(conrectcolor[[i]]),
                                            width = ggplot2::unit(1,'native'),
                                            height = ggplot2::unit(1,'native'),
                                            interpolate = interpolate ),
                    data = facet_data,
                    xmin = xmin[i],
                    xmax = xmax[i],
                    ymin = ymin[i],
                    ymax = ymax[i]) +
                  # add border
                  annotation_custom2(
                    grob = grid::rectGrob(gp = grid::gpar(fill = 'transparent',
                                                          col = 'black',
                                                          lwd = lwd,
                                                          lty = lty)),
                    data = facet_data,
                    xmin = xmin[i],
                    xmax = xmax[i],
                    ymin = ymin[i],
                    ymax = ymax[i])
              }
            }else{
              # plot
              for (i in 1:nPoints)  {
                object <- object +
                  # add points
                  annotation_custom2(
                    grob = grid::rectGrob(gp = grid::gpar(col = pCol[i],
                                                          fill = pFill[i],
                                                          lty = lty,
                                                          lwd = lwd,
                                                          alpha = alpha)),
                    data = facet_data,
                    xmin = xmin[i],
                    xmax = xmax[i],
                    ymin = ymin[i],
                    ymax = ymax[i])
              }
            }

          }else{
            ####################################
            # rotate rect
            if(annoPos == 'left'){
              for (i in 1:nPoints)  {
                object <- object +
                  annotation_custom2(
                    grob = grid::polygonGrob(y = c(0,
                                                   tan(pi*((90 - text_angle)/180))/abs(ymax[i] - ymin[i]),
                                                   1 + tan(pi*((90 - text_angle)/180))/abs(ymax[i] - ymin[i]),
                                                   1),
                                             x = c(0,1,1,0),
                                             gp = grid::gpar(fill = pFill[i],
                                                             col = pCol[i],
                                                             lty = lty,
                                                             lwd = lwd,
                                                             alpha = alpha)),
                    data = facet_data,
                    xmin = xmin[i],
                    xmax = xmax[i],
                    ymin = ymin[i] - normRectShift,
                    ymax = ymax[i] - normRectShift)

              }
            }else{
              for (i in 1:nPoints)  {
                object <- object +
                  annotation_custom2(
                    grob = grid::polygonGrob(y = c(0,
                                                   tan(pi*((90 - text_angle)/180))/abs(ymax[i] - ymin[i]),
                                                   1 + tan(pi*((90 - text_angle)/180))/abs(ymax[i] - ymin[i]),
                                                   1),
                                             x = c(0,1,1,0),
                                             gp = grid::gpar(fill = pFill[i],
                                                             col = pCol[i],
                                                             lty = lty,
                                                             lwd = lwd,
                                                             alpha = alpha)),
                    data = facet_data,
                    xmin = xmin[i],
                    xmax = xmax[i],
                    ymin = ymin[i] - rotatedRectShift,
                    ymax = ymax[i] - rotatedRectShift)

              }
            }
          }
        }
      }else{}
    }else{
      # round rect corner
      if(annoPos %in% c('top','botomn')){
        if(!is.list(yPosition)){
          # plot
          for (i in 1:nPoints)  {
            object <- object +
              # add points
              annotation_custom2(
                grob = grid::roundrectGrob(gp = grid::gpar(col = pCol[i],
                                                           fill = pFill[i],
                                                           lty = lty,
                                                           lwd = lwd,
                                                           alpha = alpha),
                                           r = ggplot2::unit(roundRadius, "snpc")),
                data = facet_data,
                xmin = xmin[i],
                xmax = xmax[i],
                ymin = ymin,
                ymax = ymax)
          }
        }else{
          # plot
          for (i in 1:nPoints)  {
            object <- object +
              # add points
              annotation_custom2(
                grob = grid::roundrectGrob(gp = grid::gpar(col = pCol[i],
                                                           fill = pFill[i],
                                                           lty = lty,
                                                           lwd = lwd,
                                                           alpha = alpha),
                                           r = ggplot2::unit(roundRadius, "snpc")),
                data = facet_data,
                xmin = xmin[i],
                xmax = xmax[i],
                ymin = ymin[i],
                ymax = ymax[i])
          }
        }
      }else if(annoPos %in% c('left','right')){
        if(!is.list(xPosition)){
          # plot
          for (i in 1:nPoints)  {
            object <- object +
              # add points
              annotation_custom2(
                grob = grid::roundrectGrob(gp = grid::gpar(col = pCol[i],
                                                           fill = pFill[i],
                                                           lty = lty,
                                                           lwd = lwd,
                                                           alpha = alpha),
                                           r = ggplot2::unit(roundRadius, "snpc")),
                data = facet_data,
                xmin = xmin,
                xmax = xmax,
                ymin = ymin[i],
                ymax = ymax[i])
          }
        }else{
          # plot
          for (i in 1:nPoints)  {
            object <- object +
              # add points
              annotation_custom2(
                grob = grid::roundrectGrob(gp = grid::gpar(col = pCol[i],
                                                           fill = pFill[i],
                                                           lty = lty,
                                                           lwd = lwd,
                                                           alpha = alpha),
                                           r = ggplot2::unit(roundRadius, "snpc")),
                data = facet_data,
                xmin = xmin[i],
                xmax = xmax[i],
                ymin = ymin[i],
                ymax = ymax[i])
          }
        }
      }else{}
    }
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
            xmin = ggplot2::unit(xmin[i] + textShift,'native'),
            xmax = ggplot2::unit(xmax[i] + textShift,'native'),
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
            ymin = ggplot2::unit(ymin[i] + textShift,'native'),
            ymax = ggplot2::unit(ymax[i] + textShift,'native'))
      }
    }else{}
  }else{
    # ====================================
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
            xmin = xmin[i] + textShift,
            xmax = xmax[i] + textShift,
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
            xmax = xmax + textHVjust,
            ymin = ymin[i] + textShift,
            ymax = ymax[i] + textShift)
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
#' @name pdotfc
#' @docType data
#' @author Junjun Lao
"pdotfc"

###############################
#' This is a test data for this package
#' test data describtion
#'
#' @name pgo
#' @docType data
#' @author Junjun Lao
"pgo"

