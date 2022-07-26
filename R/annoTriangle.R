#' @title annoTriangle
#' @name annoTriangle
#' @author Junjun Lao
#' @description This function is used to add triangle annotations in plot.
#' @param object Your ggplot list. Default(NULL).
#' @param relSideDist The relative distance ratio to the y axis range. Default(0.1).
#' @param annoPos The position for the annotation to be added. Default("top").
#' @param xPosition The x axis coordinate for the triangle. Default(NULL).
#' @param yPosition The y axis coordinate for the triangle. Default(NULL).
#' @param fillCol The triangle fill colors. Default(NULL).
#' @param nCol The colors bins. Default(100).
#' @param addTriangle Whether add triangle annotation. Default("TRUE").
#' @param triangleType The triangle shape type, "RU"(right-up), "RD"(right-down), "LU"(left-up), "LD"(left-down). Default("RD").
#' @param addBorder Whether add border to triangle or rect annotation. Default("FALSE").
#' @param borderCol The border color. Default("black").
#' @param lty The border lty. Default(NULL).
#' @param lwd The border lwd. Default(NULL).
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
#'# load test data
#'data(p)
#'
#'p1 <- p +
#'  ggplot2::theme(plot.margin = ggplot2::margin(t = 2,unit = 'cm'))
#'
#'# default plot
#'annoTriangle(object = p1,
#'             annoPos = 'top',
#'             xPosition = c(0,10.5))
#'
#'# ajust yposition
#'annoTriangle(object = p1,
#'             annoPos = 'top',
#'             xPosition = c(0.5,10.5),
#'             yPosition = c(10.8,11.5))
#'
#'# add border
#'annoTriangle(object = p1,
#'             annoPos = 'top',
#'             xPosition = c(0.5,10.5),
#'             yPosition = c(10.8,11.5),
#'             addBorder = TRUE,
#'             lwd = 2.5)


# define function
annoTriangle <- function(object = NULL,
                         relSideDist = 0.1,
                         annoPos = 'top',
                         xPosition = NULL,
                         yPosition = NULL,
                         fillCol = NULL,
                         nCol = 100,
                         addTriangle = TRUE,
                         triangleType = "RD",
                         addBorder = FALSE,
                         borderCol = 'black',
                         lty = NULL,
                         lwd = NULL,
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

  # colors
  if(is.null(fillCol)){
    col <- grDevices::colorRampPalette(useMyCol('greenBlue',7))(nCol)
  }else{
    col <- grDevices::colorRampPalette(fillCol)(nCol)
  }

  # ============================================================================
  # calculate legend relative pos
  # annotation position
  if(annoPos %in% c('top','botomn')){

    # xPos
    xPos <- xPosition

    # calculate bin
    ratioSeg <- (xPos[2] - xPos[1] + 1)/nCol

    # xmin and xmax
    xmin <- seq(xPos[1],xPos[2],ratioSeg)
    xmax <- seq(xPos[1],xPos[2] - ratioSeg,ratioSeg) + ratioSeg

    # length
    nPoints <- length(xmin)

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

    # yPos
    yPos <- yPosition

    # calculate bin
    ratioSeg <- (yPos[2] - yPos[1] + 1)/nCol

    # xmin and xmax
    ymin <- seq(yPos[1],yPos[2],ratioSeg)
    ymax <- seq(yPos[1],yPos[2] - ratioSeg,ratioSeg) + ratioSeg

    # length
    nPoints <- length(ymin)

    # not supply xPos auto calculate
    if(is.null(xPosition)){
      # numeric or discrete
      if(is.numeric(data_x)){
        if(annoPos == 'left'){
          xmin <- min(data_x) - relSideDist*max(data_x)
          xmax <- min(data_x)
        }else{
          xmax <- max(data_x) + relSideDist*max(data_x)
          xmin <- max(data_x)
        }
      }else{
        if(annoPos == 'left'){
          xmin <- -relSideDist*length(unique(data_x))
          xmax <- 0
        }else{
          xmax <- length(unique(data_x)) + relSideDist*length(unique(data_x))
          xmin <- length(unique(data_x))
        }
      }
    }else{
      xmin <- xPosition[1]
      xmax <- xPosition[2]
    }
  }

  # ============================================================================
  # triangle type
  type_rd <- list(x = c(0,0,1),y = c(0,1,1))
  type_lu <- list(x = c(0,1,1),y = c(0,0,1))
  type_ru <- list(x = c(0,0,1),y = c(0,1,0))
  type_ld <- list(x = c(0,1,1),y = c(1,0,1))

  # choose triangle type to show
  if(triangleType == "RD"){
    triType <- type_rd
  }else if(triangleType == "LU"){
    triType <- type_lu
  }else if(triangleType == "RU"){
    triType <- type_ru
  }else if(triangleType == "LD"){
    triType <- type_ld
  }

  # border position
  if(triangleType == "RD"){
    borderType <- type_lu
  }else if(triangleType == "LU"){
    borderType <- type_rd
  }else if(triangleType == "RU"){
    borderType <- type_ld
  }else if(triangleType == "LD"){
    borderType <- type_ru
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
  if(is.null(facetName)){
    if(annoPos %in% c('top','botomn')){
      if(addTriangle == TRUE){
        for (i in 1:nPoints){
          ##################################################
          # 1.add rect
          object <- object +
            # add triangle
            ggplot2::annotation_custom(
              grob = grid::rectGrob(gp = grid::gpar(col = "transparent",
                                                    fill = col[i],
                                                    lty = NULL,
                                                    lwd = NULL)),
              xmin = ggplot2::unit(xmin[i],'native'),
              xmax = ggplot2::unit(xmax[i],'native'),
              ymin = ggplot2::unit(ymin,'native'),
              ymax = ggplot2::unit(ymax,'native')
            )
        }
        ##################################################
        # 2. add triangle
        object <- object +
          ggplot2::annotation_custom(
            grob = grid::polygonGrob(x = triType[[1]],
                                     y = triType[[2]],
                                     gp = grid::gpar(fill = 'white',
                                                     col = 'white')),
            xmin = ggplot2::unit(min(xmin),'native'),
            xmax = ggplot2::unit(max(xmax),'native'),
            ymin = ggplot2::unit(ymin,'native'),
            ymax = ggplot2::unit(ymax,'native')
          )

        ##################################################
        # 3. add border
        if(addBorder == TRUE){
          object <- object +
            ggplot2::annotation_custom(
              grob = grid::polygonGrob(x = borderType[[1]],
                                       y = borderType[[2]],
                                       gp = grid::gpar(fill = 'transparent',
                                                       col = borderCol,
                                                       lty = lty,
                                                       lwd = lwd)),
              xmin = ggplot2::unit(min(xmin),'native'),
              xmax = ggplot2::unit(max(xmax),'native'),
              ymin = ggplot2::unit(ymin,'native'),
              ymax = ggplot2::unit(ymax,'native')
            )
        }else{
          object <- object
        }
      }else{
        for (i in 1:nPoints){
          ##################################################
          # 1.add rect
          object <- object +
            # add triangle
            ggplot2::annotation_custom(
              grob = grid::rectGrob(gp = grid::gpar(col = "transparent",
                                                    fill = col[i],
                                                    lty = NULL,
                                                    lwd = NULL)),
              xmin = ggplot2::unit(xmin[i],'native'),
              xmax = ggplot2::unit(xmax[i],'native'),
              ymin = ggplot2::unit(ymin,'native'),
              ymax = ggplot2::unit(ymax,'native')
            )
        }

        ##################################################
        # 3. add border
        if(addBorder == TRUE){
          object <- object +
            # add triangle
            ggplot2::annotation_custom(
              grob = grid::rectGrob(gp = grid::gpar(col = borderCol,
                                                    fill = "transparent",
                                                    lty = lty,
                                                    lwd = lwd)),
              xmin = ggplot2::unit(min(xmin),'native'),
              xmax = ggplot2::unit(max(xmax),'native'),
              ymin = ggplot2::unit(ymin,'native'),
              ymax = ggplot2::unit(ymax,'native')
            )
        }else{
          object <- object
        }
      }
    }else if(annoPos %in% c('left','right')){
      if(addTriangle == TRUE){
        for (i in 1:nPoints){
          ##################################################
          # 1.add rect
          object <- object +
            # add triangle
            ggplot2::annotation_custom(
              grob = grid::rectGrob(gp = grid::gpar(col = "transparent",
                                                    fill = col[i],
                                                    lty = NULL,
                                                    lwd = NULL)),
              xmin = ggplot2::unit(xmin,'native'),
              xmax = ggplot2::unit(xmax,'native'),
              ymin = ggplot2::unit(ymin[i],'native'),
              ymax = ggplot2::unit(ymax[i],'native')
            )
        }
        ##################################################
        # 2. add triangle
        object <- object +
          ggplot2::annotation_custom(
            grob = grid::polygonGrob(x = triType[[1]],
                                     y = triType[[2]],
                                     gp = grid::gpar(fill = 'white',
                                                     col = 'white')),
            xmin = ggplot2::unit(xmin,'native'),
            xmax = ggplot2::unit(xmax,'native'),
            ymin = ggplot2::unit(min(ymin),'native'),
            ymax = ggplot2::unit(max(ymax),'native')
          )

        ##################################################
        # 3. add border
        if(addBorder == TRUE){
          object <- object +
            ggplot2::annotation_custom(
              grob = grid::polygonGrob(x = borderType[[1]],
                                       y = borderType[[2]],
                                       gp = grid::gpar(fill = 'transparent',
                                                       col = borderCol,
                                                       lty = lty,
                                                       lwd = lwd)),
              xmin = ggplot2::unit(xmin,'native'),
              xmax = ggplot2::unit(xmax,'native'),
              ymin = ggplot2::unit(min(ymin),'native'),
              ymax = ggplot2::unit(max(ymax),'native')
            )
        }else{
          object <- object
        }
      }else{
        for (i in 1:nPoints){
          ##################################################
          # 1.add rect
          object <- object +
            # add triangle
            ggplot2::annotation_custom(
              grob = grid::rectGrob(gp = grid::gpar(col = "transparent",
                                                    fill = col[i],
                                                    lty = NULL,
                                                    lwd = NULL)),
              xmin = ggplot2::unit(xmin,'native'),
              xmax = ggplot2::unit(xmax,'native'),
              ymin = ggplot2::unit(ymin[i],'native'),
              ymax = ggplot2::unit(ymax[i],'native')
            )
        }

        ##################################################
        # 3. add border
        if(addBorder == TRUE){
          object <- object +
            # add triangle
            ggplot2::annotation_custom(
              grob = grid::rectGrob(gp = grid::gpar(col = borderCol,
                                                    fill = "transparent",
                                                    lty = lty,
                                                    lwd = lwd)),
              xmin = ggplot2::unit(xmin,'native'),
              xmax = ggplot2::unit(xmax,'native'),
              ymin = ggplot2::unit(min(ymin),'native'),
              ymax = ggplot2::unit(max(ymax),'native')
            )
        }else{
          object <- object
        }
      }
    }else{}
  }else{
    # =================================
    # facet data
    facet_data <- data.frame(myFacetGrou)
    colnames(facet_data) <- facetName

    # facet plot
    if(annoPos %in% c('top','botomn')){
      if(addTriangle == TRUE){
        for (i in 1:nPoints){
          ##################################################
          # 1.add rect
          object <- object +
            # add triangle
            annotation_custom2(
              grob = grid::rectGrob(gp = grid::gpar(col = "transparent",
                                                    fill = col[i],
                                                    lty = NULL,
                                                    lwd = NULL)),
              data = facet_data,
              xmin = xmin[i],
              xmax = xmax[i],
              ymin = ymin,
              ymax = ymax)
        }
        ##################################################
        # 2. add triangle
        object <- object +
          annotation_custom2(
            grob = grid::polygonGrob(x = triType[[1]],
                                     y = triType[[2]],
                                     gp = grid::gpar(fill = 'white',
                                                     col = 'white')),
            data = facet_data,
            xmin = min(xmin),
            xmax = max(xmax),
            ymin = ymin,
            ymax = ymax)

        ##################################################
        # 3. add border
        if(addBorder == TRUE){
          object <- object +
            annotation_custom2(
              grob = grid::polygonGrob(x = borderType[[1]],
                                       y = borderType[[2]],
                                       gp = grid::gpar(fill = 'transparent',
                                                       col = borderCol,
                                                       lty = lty,
                                                       lwd = lwd)),
              data = facet_data,
              xmin = min(xmin),
              xmax = max(xmax),
              ymin = ymin,
              ymax = ymax)
        }else{
          object <- object
        }
      }else{
        for (i in 1:nPoints){
          ##################################################
          # 1.add rect
          object <- object +
            # add triangle
            annotation_custom2(
              grob = grid::rectGrob(gp = grid::gpar(col = "transparent",
                                                    fill = col[i],
                                                    lty = NULL,
                                                    lwd = NULL)),
              data = facet_data,
              xmin = xmin[i],
              xmax = xmax[i],
              ymin = ymin,
              ymax = ymax)
        }

        ##################################################
        # 3. add border
        if(addBorder == TRUE){
          object <- object +
            # add triangle
            annotation_custom2(
              grob = grid::rectGrob(gp = grid::gpar(col = borderCol,
                                                    fill = "transparent",
                                                    lty = lty,
                                                    lwd = lwd)),
              data = facet_data,
              xmin = min(xmin),
              xmax = max(xmax),
              ymin = ymin,
              ymax = ymax)
        }else{
          object <- object
        }
      }
    }else if(annoPos %in% c('left','right')){
      if(addTriangle == TRUE){
        for (i in 1:nPoints){
          ##################################################
          # 1.add rect
          object <- object +
            # add triangle
            annotation_custom2(
              grob = grid::rectGrob(gp = grid::gpar(col = "transparent",
                                                    fill = col[i],
                                                    lty = NULL,
                                                    lwd = NULL)),
              data = facet_data,
              xmin = xmin,
              xmax = xmax,
              ymin = ymin[i],
              ymax = ymax[i])
        }
        ##################################################
        # 2. add triangle
        object <- object +
          annotation_custom2(
            grob = grid::polygonGrob(x = triType[[1]],
                                     y = triType[[2]],
                                     gp = grid::gpar(fill = 'white',
                                                     col = 'white')),
            data = facet_data,
            xmin = xmin,
            xmax = xmax,
            ymin = min(ymin),
            ymax = max(ymax))

        ##################################################
        # 3. add border
        if(addBorder == TRUE){
          object <- object +
            annotation_custom2(
              grob = grid::polygonGrob(x = borderType[[1]],
                                       y = borderType[[2]],
                                       gp = grid::gpar(fill = 'transparent',
                                                       col = borderCol,
                                                       lty = lty,
                                                       lwd = lwd)),
              data = facet_data,
              xmin = xmin,
              xmax = xmax,
              ymin = min(ymin),
              ymax = max(ymax))
        }else{
          object <- object
        }
      }else{
        for (i in 1:nPoints){
          ##################################################
          # 1.add rect
          object <- object +
            # add triangle
            annotation_custom2(
              grob = grid::rectGrob(gp = grid::gpar(col = "transparent",
                                                    fill = col[i],
                                                    lty = NULL,
                                                    lwd = NULL)),
              data = facet_data,
              xmin = xmin,
              xmax = xmax,
              ymin = ymin[i],
              ymax = ymax[i])
        }

        ##################################################
        # 3. add border
        if(addBorder == TRUE){
          object <- object +
            # add triangle
            ggplot2::annotation_custom(
              grob = grid::rectGrob(gp = grid::gpar(col = borderCol,
                                                    fill = "transparent",
                                                    lty = lty,
                                                    lwd = lwd)),
              data = facet_data,
              xmin = xmin,
              xmax = xmax,
              ymin = min(ymin),
              ymax = max(ymax))
        }else{
          object <- object
        }
      }
    }else{}
  }
  # ============================================================================
  # print
  print(object)
}
