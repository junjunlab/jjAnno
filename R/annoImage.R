#' @title annoImage
#' @name annoImage
#' @author Junjun Lao
#' @description This function is used to add image annotations in plot.
#' @param object This function is used to add segment annotations in plot.
#' @param relSideDist The relative distance ratio to the y axis range. Default(0.1).
#' @param annoPos The position for the annotation to be added. Default("top").
#' @param xPosition The x axis coordinate for the image. Default(NULL).
#' @param yPosition The y axis coordinate for the image. Default(NULL).
#' @param images The images paths. Default(NULL).
#' @param segWidth The relative image width. Default(1).
#' @param annoManual Whether annotate by yourself by supplying with x and y coordinates. Default(FALSE).
#' @param imgWidth The image width. Default(1).
#' @param imgHeight The image height. Default(1).
#'
#' @return Return a ggplot object.
#' @export
#'
#' @examples
#' # ===============================
#' # test function
#'
#'\donttest{ # load data
#' data(p)
#'
#' img1 <- system.file("extdata/animal-img/", "1.jpg", package = "jjAnno")
#' img2 <- system.file("extdata/animal-img/", "2.jpg", package = "jjAnno")
#' img3 <- system.file("extdata/animal-img/", "3.jpg", package = "jjAnno")
#' img4 <- system.file("extdata/animal-img/", "4.jpg", package = "jjAnno")
#' img5 <- system.file("extdata/animal-img/", "5.jpg", package = "jjAnno")
#' img6 <- system.file("extdata/animal-img/", "6.jpg", package = "jjAnno")
#' img7 <- system.file("extdata/animal-img/", "7.jpg", package = "jjAnno")
#' img8 <- system.file("extdata/animal-img/", "8.jpg", package = "jjAnno")
#' img9 <- system.file("extdata/animal-img/", "9.jpg", package = "jjAnno")
#' img10 <- system.file("extdata/animal-img/", "10.jpg", package = "jjAnno")
#'
#' imgs <- c(img1,img2,img3,img4,img5,img6,img7,img8,img9,img10)
#'
#'
#' # add legend
#' annoImage(object = p,
#'           annoPos = 'top',
#'           xPosition = c(1:10),
#'           images = imgs,
#'           yPosition = c(11,12))
#'
#' # change width
#' annoImage(object = p,
#'           annoPos = 'top',
#'           xPosition = c(1:10),
#'           images = imgs,
#'           yPosition = c(11,11.8),
#'           segWidth = 0.8)
#'
#' # add to right
#' annoImage(object = p,
#'           annoPos = 'right',
#'           yPosition = c(1:10),
#'           images = imgs,
#'           xPosition = c(11,11.8),
#'           segWidth = 0.8)
#'}

# define function
annoImage <- function(object = NULL,
                      relSideDist = 0.1,
                      annoPos = 'top',
                      xPosition = NULL,
                      yPosition = NULL,
                      images = NULL,
                      segWidth = 1,
                      annoManual = FALSE,
                      imgWidth = 1,
                      imgHeight = 1){
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
            ymin <- -relSideDist*length(unique(data_y))
            ymax <- 0
          }
        }
      }else{
        ymax <- yPosition[1]
        ymin <- yPosition[2]
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
  }else{
    # manually x and y positions

    # annotation position
    if(annoPos %in% c('top','botomn')){
      xmin <- xPosition[[1]] - segWidth/2
      xmax <- xPosition[[2]] + segWidth/2

      ymax <- yPosition[[1]]
      ymin <- yPosition[[2]]
    }else{
      xmin <- xPosition[[1]]
      xmax <- xPosition[[2]]

      ymin <- yPosition[[1]] - segWidth/2
      ymax <- yPosition[[2]] + segWidth/2
    }

    nPoints <- max(length(xmin),length(ymin))
  }

  # ============================================================================
  # plot
  if(annoPos %in% c('top','botomn')){
    if(!is.list(yPosition)){
      for (i in 1:nPoints)  {
        # read image
        image <- magick::image_read(images[i])

        # plot
        object <- object +
          # add image
          ggplot2::annotation_custom(
            grob = grid::rasterGrob(image = image,
                                    width = imgWidth,
                                    height = imgHeight,
                                    interpolate = FALSE),
            xmin = xmin[i],
            xmax = xmax[i],
            ymin = ymin,
            ymax = ymax)
      }
    }else{
      for (i in 1:nPoints)  {
        # read image
        image <- magick::image_read(images[i])

        # plot
        object <- object +
          # add image
          ggplot2::annotation_custom(
            grob = grid::rasterGrob(image = image,
                                    width = imgWidth,
                                    height = imgHeight,
                                    interpolate = FALSE),
            xmin = xmin[i],
            xmax = xmax[i],
            ymin = ymin[i],
            ymax = ymax[i])
      }
    }
  }else if(annoPos %in% c('left','right')){
    if(!is.list(yPosition)){
      for (i in 1:nPoints)  {
        # read image
        image <- magick::image_read(images[i])

        # plot
        object <- object +
          # add image
          ggplot2::annotation_custom(
            grob = grid::rasterGrob(image = image,
                                    width = imgWidth,
                                    height = imgHeight,
                                    interpolate = FALSE),
            xmin = xmin,
            xmax = xmax,
            ymin = ymin[i],
            ymax = ymax[i])
      }
    }else{
      for (i in 1:nPoints)  {
        # read image
        image <- magick::image_read(images[i])

        # plot
        object <- object +
          # add image
          ggplot2::annotation_custom(
            grob = grid::rasterGrob(image = image,
                                    width = imgWidth,
                                    height = imgHeight,
                                    interpolate = FALSE),
            xmin = xmin[i],
            xmax = xmax[i],
            ymin = ymin[i],
            ymax = ymax[i])
      }
    }
  }else{}

  # ============================================================================
  # print
  print(object)
}
