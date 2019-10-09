#' Inspect dice
#' @description Inspects dice for fairness and plots distribution of results.
#' @details Rolls the given dice configuration repeatedly using \code{\link[aaSimulator]{roll}}.
#' @param iterations integer() The number of iterations to run.
#' @param ones parameter passed to \code{\link[aaSimulator]{roll}}.
#' @param twos parameter passed to \code{\link[aaSimulator]{roll}}.
#' @param threes parameter passed to \code{\link[aaSimulator]{roll}}.
#' @param fours parameter passed to \code{\link[aaSimulator]{roll}}.
#' @param fives parameter passed to \code{\link[aaSimulator]{roll}}.
#' @import ggplot2
#' @export
inspect_dice <- function(iterations, ones=0, twos=0, threes=0, fours=0, fives=0){

  ndice <- ones + twos + threes + fours + fives

  hitlist <- c()

  for (i in 1:iterations){
    hitlist <- c(hitlist, roll(ones, twos, threes, fours, fives))
  }

  hittable <- data.frame(hits=hitlist)

  ggplot(data=hittable, aes(hits)) +
    geom_histogram(aes(y = ..density..), binwidth = 1, color="black", fill="white") +
    ylab("% of rolls") +
    ggtitle(paste("(", iterations, " iterations)", sep=""))

}
