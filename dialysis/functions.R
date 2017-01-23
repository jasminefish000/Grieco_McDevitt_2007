#' Create lags for panel data.
#'
#' This function creates lags (or leads) of panel data variables.
#' Input data should be sorted by i, t --- e.g.
#' df <- df[order(i,t),]
#' @param x Vector or matrix to get lags of.
#' @param i unit index
#' @param t time index
#' @param lag How many time periods to lag. Can be negative if leading
#' values are desired.
#' @return Lagged copy of x.
panel.lag <- function(x, i, t, lag=1) {
  
  if (!identical(order(i,t),1:length(i))) {
    stop("inputs not sorted.")
  }
  if (is.matrix(x)) {
    return(apply(x,MARGIN=2,FUN=function(c) { panel.lag(c,i,t,lag) }))
  }
  if (length(i) != length(x) || length(i) != length(t) ) {
    stop("Inputs not same length")
  }
  if (lag>0) {
    x.lag <- x[1:(length(x)-lag)]
    x.lag[i[1:(length(i)-lag)]!=i[(1+lag):length(i)] ] <- NA
    x.lag[t[1:(length(i)-lag)]+lag!=t[(1+lag):length(i)] ] <- NA
    val <- (c(rep(NA,lag),x.lag))
  } else if (lag<0) {
    lag <- abs(lag)
    x.lag <- x[(1+lag):length(x)]
    x.lag[i[1:(length(i)-lag)]!=i[(1+lag):length(i)] ] <- NA
    x.lag[t[1:(length(i)-lag)]+lag!=t[(1+lag):length(i)] ] <- NA
    val <- (c(x.lag,rep(NA,lag)))
  } else { # lag=0
    return (x)
  }
  if (class(x)=="Date" & class(val)=="numeric") {
    stopifnot(0==as.numeric(as.Date("1970-01-01")))
    val <- as.Date(val, origin="1970-01-01")
  }
  return(val)
}


#' Descriptive statistics of data
#' @param input a vector of data
#' @return a list of items
 descriptive.statistic <- function (input) {
  x <- as.numeric(input)
  c(mean(x,na.rm= TRUE ), sd(x,na.rm = TRUE ), sum (!is.na(x)))
 }
 
 # Multiple plot function
 #
 # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
 # - cols:   Number of columns in layout
 # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
 #
 # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
 # then plot 1 will go in the upper left, 2 will go in the upper right, and
 # 3 will go all the way across the bottom.
 #
 multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
   library(grid)
   
   # Make a list from the ... arguments and plotlist
   plots <- c(list(...), plotlist)
   
   numPlots = length(plots)
   
   # If layout is NULL, then use 'cols' to determine layout
   if (is.null(layout)) {
     # Make the panel
     # ncol: Number of columns of plots
     # nrow: Number of rows needed, calculated from # of cols
     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                      ncol = cols, nrow = ceiling(numPlots/cols))
   }
   
   if (numPlots==1) {
     print(plots[[1]])
     
   } else {
     # Set up the page
     grid.newpage()
     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
     
     # Make each plot, in the correct location
     for (i in 1:numPlots) {
       # Get the i,j matrix positions of the regions that contain this subplot
       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
       
       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                       layout.pos.col = matchidx$col))
     }
   }
 }