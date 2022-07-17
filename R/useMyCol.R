#' @title useMyCol
#' @name useMyCol
#' @author Junjun Lao
#' @description This function is used to produce avaliable colors for plot.
#' @param platte The platte name. Default("stallion").
#' @param n The color numbers to use. Default(NULL).
#' @param showAll Whether to show all plattes. Default(FALSE).
#'
#' @return Return the color names you have choosed.
#' @export
#'
#' @examples
#' useMyCol(platte = 'stallion2',n = 5)
#' useMyCol(showAll = TRUE)

# define functions
useMyCol <- function(platte = NULL,
                     n = NULL,
                     showAll = FALSE){
  #---------------------------------------------------------------
  # Primarily Discrete Palettes
  #---------------------------------------------------------------
  # ============================================================================
  #20-colors
  stallion = c("#D51F26","#272E6A","#208A42","#89288F","#F47D2B",
               "#FEE500","#8A9FD1","#C06CAB","#E6C2DC","#90D5E4",
               "#89C75F","#F37B7D","#9983BD","#D24B27","#3BBCA8",
               "#6E4B9E","#0C727C", "#7E1416","#D8A767","#3D3D3D")

  stallion2 = c("#D51F26","#272E6A","#208A42","#89288F","#F47D2B",
                "#FEE500","#8A9FD1","#C06CAB","#E6C2DC","#90D5E4",
                "#89C75F","#F37B7D","#9983BD","#D24B27","#3BBCA8",
                "#6E4B9E","#0C727C", "#7E1416","#D8A767")

  calm = c("#7DD06F", "#844081","#688EC1", "#C17E73", "#484125",
           "#6CD3A7", "#597873","#7B6FD0", "#CF4A31", "#D0CD47",
           "#722A2D", "#CBC594", "#D19EC4", "#5A7E36", "#D4477D",
           "#403552", "#76D73C", "#96CED5", "#CE54D1", "#C48736")

  kelly = c("#FFB300", "#803E75", "#FF6800", "#A6BDD7", "#C10020",
            "#CEA262", "#817066", "#007D34", "#F6768E", "#00538A",
            "#FF7A5C", "#53377A", "#FF8E00", "#B32851", "#F4C800",
            "#7F180D", "#93AA00", "#593315", "#F13A13", "#232C16")

  #16-colors
  bear = c("#faa818", "#41a30d","#fbdf72", "#367d7d", "#d33502",
           "#6ebcbc", "#37526d","#916848", "#f5b390", "#342739",
           "#bed678","#a6d9ee", "#0d74b6",
           "#60824f","#725ca5", "#e0598b")

  #15-colors
  ironMan = c('#371377','#7700FF','#9E0142','#FF0080', '#DC494C',
              "#F88D51","#FAD510","#FFFF5F",'#88CFA4','#238B45',
              "#02401B", "#0AD7D3","#046C9A", "#A2A475", 'grey35')

  circus = c("#D52126","#88CCEE", "#FEE52C", "#117733", "#CC61B0",
             "#99C945", "#2F8AC4", "#332288","#E68316", "#661101",
             "#F97B72", "#DDCC77", "#11A579", "#89288F", "#E73F74")

  #12-colors
  paired = c("#A6CDE2","#1E78B4","#74C476","#34A047","#F59899","#E11E26",
             "#FCBF6E","#F47E1F","#CAB2D6","#6A3E98","#FAF39B","#B15928")

  #11-colors
  grove = c("#1a1334","#01545a","#017351","#03c383","#aad962",
            "#fbbf45","#ef6a32","#ed0345","#a12a5e","#710162","#3B9AB2")

  #7-colors
  summerNight = c("#2a7185","#a64027","#fbdf72","#60824f","#9cdff0",
                  "#022336","#725ca5")

  #5-colors
  zissou = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00")
  darjeeling = c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6")
  rushmore = c("#E1BD6D", "#EABE94", "#0B775E", "#35274A" , "#F2300F")
  captain = c("grey","#A1CDE1","#12477C","#EC9274","#67001E")

  #---------------------------------------------------------------
  # Primarily Continuous Palettes
  #---------------------------------------------------------------
  #10-colors
  horizon = c('#000075','#2E00FF', '#9408F7', '#C729D6', '#FA4AB5',
              '#FF6A95', '#FF8B74', '#FFAC53', '#FFCD32', '#FFFF60')

  #9-colors
  horizonExtra =c("#000436","#021EA9","#1632FB","#6E34FC","#C732D5",
                  "#FD619D","#FF9965","#FFD32B","#FFFC5A")

  blueYellow = c("#352A86","#343DAE","#0262E0","#1389D2","#2DB7A3",
                 "#A5BE6A","#F8BA43","#F6DA23","#F8FA0D")

  sambaNight = c('#1873CC','#1798E5','#00BFFF','#4AC596','#00CC00',
                 '#A2E700','#FFFF00','#FFD200','#FFA500')

  solarExtra = c('#3361A5', '#248AF3', '#14B3FF', '#88CEEF', '#C1D5DC',
                 '#EAD397', '#FDB31A', '#E42A2A', '#A31D1D')

  whitePurple = c('#f7fcfd','#e0ecf4','#bfd3e6','#9ebcda','#8c96c6',
                  '#8c6bb1','#88419d','#810f7c','#4d004b')

  whiteBlue = c('#fff7fb','#ece7f2','#d0d1e6','#a6bddb','#74a9cf',
                '#3690c0','#0570b0','#045a8d','#023858')


  comet = c("#E6E7E8","#3A97FF","#8816A7","black")

  #7-colors
  greenBlue = c('#e0f3db','#ccebc5','#a8ddb5','#4eb3d3','#2b8cbe',
                '#0868ac','#084081')

  #6-colors
  beach = c("#87D2DB","#5BB1CB","#4F66AF","#F15F30",
            "#F7962E","#FCEE2B")

  #5-colors
  coolwarm = c("#4858A7", "#788FC8", "#D6DAE1", "#F49B7C", "#B51F29")
  fireworks = c("white","#2488F0","#7F3F98","#E22929","#FCB31A")
  greyMagma = c("grey", "#FB8861FF", "#B63679FF", "#51127CFF", "#000004FF")
  fireworks2 = c("black", "#2488F0","#7F3F98","#E22929","#FCB31A")
  purpleOrange = c("#581845", "#900C3F", "#C70039", "#FF5744", "#FFC30F")

  # ============================================================================
  if(showAll == FALSE){
    if(platte == "stallion"){
      col <- stallion[1:n]
      print(paste0('This palatte have ',length(stallion),' colors!'))
    }else if(platte == "stallion2"){
      col <- stallion2[1:n]
      print(paste0('This palatte have ',length(stallion2),' colors!'))
    }else if(platte == "calm"){
      col <- calm[1:n]
      print(paste0('This palatte have ',length(calm),' colors!'))
    }else if(platte == "kelly"){
      col <- kelly[1:n]
      print(paste0('This palatte have ',length(kelly),' colors!'))
    }else if(platte == "bear"){
      col <- bear[1:n]
      print(paste0('This palatte have ',length(bear),' colors!'))
    }else if(platte == "ironMan"){
      col <- ironMan[1:n]
      print(paste0('This palatte have ',length(ironMan),' colors!'))
    }else if(platte == "circus"){
      col <- circus[1:n]
      print(paste0('This palatte have ',length(circus),' colors!'))
    }else if(platte == "paired"){
      col <- paired[1:n]
      print(paste0('This palatte have ',length(paired),' colors!'))
    }else if(platte == "grove"){
      col <- grove[1:n]
      print(paste0('This palatte have ',length(grove),' colors!'))
    }else if(platte == "summerNight"){
      col <- summerNight[1:n]
      print(paste0('This palatte have ',length(summerNight),' colors!'))
    }else if(platte == "zissou"){
      col <- zissou[1:n]
      print(paste0('This palatte have ',length(zissou),' colors!'))
    }else if(platte == "darjeeling"){
      col <- darjeeling[1:n]
      print(paste0('This palatte have ',length(darjeeling),' colors!'))
    }else if(platte == "rushmore"){
      col <- rushmore[1:n]
      print(paste0('This palatte have ',length(rushmore),' colors!'))
    }else if(platte == "captain"){
      col <- captain[1:n]
      print(paste0('This palatte have ',length(captain),' colors!'))
    }else if(platte == "horizon"){
      col <- horizon[1:n] # continues colors
      print(paste0('This palatte have ',length(horizon),' colors!'))
    }else if(platte == "horizonExtra"){
      col <- horizonExtra[1:n]
      print(paste0('This palatte have ',length(horizonExtra),' colors!'))
    }else if(platte == "blueYellow"){
      col <- blueYellow[1:n]
      print(paste0('This palatte have ',length(blueYellow),' colors!'))
    }else if(platte == "sambaNight"){
      col <- sambaNight[1:n]
      print(paste0('This palatte have ',length(sambaNight),' colors!'))
    }else if(platte == "solarExtra"){
      col <- solarExtra[1:n]
      print(paste0('This palatte have ',length(solarExtra),' colors!'))
    }else if(platte == "whitePurple"){
      col <- whitePurple[1:n]
      print(paste0('This palatte have ',length(whitePurple),' colors!'))
    }else if(platte == "whiteBlue"){
      col <- whiteBlue[1:n]
      print(paste0('This palatte have ',length(whiteBlue),' colors!'))
    }else if(platte == "comet"){
      col <- comet[1:n]
      print(paste0('This palatte have ',length(comet),' colors!'))
    }else if(platte == "greenBlue"){
      col <- greenBlue[1:n]
      print(paste0('This palatte have ',length(greenBlue),' colors!'))
    }else if(platte == "beach"){
      col <- beach[1:n]
      print(paste0('This palatte have ',length(beach),' colors!'))
    }else if(platte == "coolwarm"){
      col <- coolwarm[1:n]
      print(paste0('This palatte have ',length(coolwarm),' colors!'))
    }else if(platte == "fireworks"){
      col <- fireworks[1:n]
      print(paste0('This palatte have ',length(fireworks),' colors!'))
    }else if(platte == "greyMagma"){
      col <- greyMagma[1:n]
      print(paste0('This palatte have ',length(greyMagma),' colors!'))
    }else if(platte == "fireworks2"){
      col <- fireworks2[1:n]
      print(paste0('This palatte have ',length(fireworks2),' colors!'))
    }else if(platte == "purpleOrange"){
      col <- purpleOrange[1:n]
      print(paste0('This palatte have ',length(purpleOrange),' colors!'))
    }else{
      print('Please give the correct name!')
    }
    return(col)
  }else{
    dicrete_col <- c('stallion', 'stallion2', 'calm', 'kelly', 'bear' ,
                     'ironMan', 'circus', 'paired', 'grove', 'summerNight' ,
                     'zissou', 'darjeeling', 'rushmore', 'captain')

    continues_col <- c('horizon', 'horizonExtra', 'blueYellow', 'sambaNight', 'solarExtra',
                       'whitePurple', 'whiteBlue', 'comet', 'greenBlue', 'beach' ,
                       'coolwarm', 'fireworks', 'greyMagma', 'fireworks2', 'purpleOrange')
    return(c(dicrete_col,continues_col))
  }
}
