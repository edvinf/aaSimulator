#' plot posterior distribution
#' @description
#'  Calculate and plot posterior distribution for battle
#' @details
#'  Plotter error bars are the ranges of the reported probabilities across replicates.
#' @param simulationResults, formatted as: \code{\link[aaSimulator]{simulationResults}}
#' @param side character(), side to calculate distribution for, either 'attacker' or 'defender'
#' @param attackCol color to use when side is 'attacker'
#' @param defenceCol color to use when side is 'defender'
#' @param ylim limits for y-axis
#' @return ggplot object
#' @examples
#'  sim <- simulateBattles("30 inf 11 arm 5 ftr arm",
#'         "40 inf 3 ftr",
#'         iterations = 2000,
#'         replications = 3)
#'  makePosteriorDistributionPlot(sim, "attacker")
#'  makePosteriorDistributionPlot(sim, "defender")
#' @export
makePosteriorDistributionPlot <- function(simulationResults, side="attacker", attackCol="red", defenceCol="blue", ylim=c(0,1)){
  results <- calculatePosteriorDistribution(simulationResults, side)

  if (side == "attacker"){
    col <- attackCol
  }
  else if (side == "defender"){
    col <- defenceCol
  }
  else{
    stop(paste("Side", side, "not recognized."))
  }

  means <- colMeans(results)
  maxs <- apply(results, FUN=max, MARGIN=2)
  mins <- apply(results, FUN=min, MARGIN=2)

  dt <- data.table::as.data.table(means)
  dt$max <- maxs
  dt$min <- mins
  dt$unit <- names(means)
  dt$order <- factor(as.character(1:nrow(dt)), levels = 1:nrow(dt), ordered=T)
  pl <- ggplot2::ggplot(dt, ggplot2::aes(x=order, y=means)) +
    ggplot2::geom_bar(stat="identity", fill=col) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = min, ymax = max), width=.2) +
    ggplot2::ylim(ylim) +
    ggplot2::ylab("survival probability") +
    ggplot2::xlab("") +
    ggplot2::scale_x_discrete(labels=dt$unit) +
    ggplot2::ggtitle(paste(length(simulationResults$replicates[[1]]), "iterations,", nrow(dt), "replicates.")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
  return(pl)
}
