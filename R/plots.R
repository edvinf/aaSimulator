#' Make posterior distribution plot
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
#'         iterations = 1000,
#'         replications = 2)
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
    ggplot2::ggtitle(paste(length(simulationResults$replicates[[1]]), "iterations,", length(simulationResults$replicates), "replicates.")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
  return(pl)
}

#' Make plot of gains
#' @description Plots the cumulative probabilities of gains (cost - opponent cost) for a battle simulation
#' @param simulationResults, formatted as: \code{\link[aaSimulator]{simulationResults}}
#' @param attackCol color to use when side is 'attacker'
#' @param defenceCol color to use when side is 'defender'
#' @return ggplot object
#' @examples
#'  sim <- simulateBattles("30 inf 11 arm 5 ftr arm",
#'         "40 inf 3 ftr",
#'         iterations = 1000,
#'         replications = 2)
#'  makeCostDiffPlot(sim)
#' @export
makeCostDiffPlot <- function(simulationResults, attackCol="red", defenceCol="blue"){
  costDiffs <- lapply(simulationResults$replicates, FUN=function(x){unlist(lapply(x, function(y){y$attackerCost - y$defenderCost}))})
  gains <- c()
  probs <- c()
  side <- c()
  replicate <- c()
  for (i in 1:length(simulationResults$replicates)){
    gain <- unlist(lapply(simulationResults$replicates[[i]], function(y){y$attackerCost - y$defenderCost}))
    prob <- rank(gain) / length(gain)
    gains <- c(gains, gain)
    probs <- c(probs, prob)
    side <- c(side, rep("attacker", length(gain)))
    replicate <- c(replicate, rep(paste("#", i), length(gain)))

    gain <- unlist(lapply(simulationResults$replicates[[i]], function(y){y$defenderCost - y$attackerCost}))
    prob <- rank(gain) / length(gain)
    gains <- c(gains, gain)
    probs <- c(probs, prob)
    side <- c(side, rep("defender", length(gain)))
    replicate <- c(replicate, rep(paste("#", i), length(gain)))
  }

  dt <- data.table::data.table(gain=gains, prob=probs, side=side, replicate=replicate)

  pl <- ggplot2::ggplot(dt, ggplot2::aes(x=gain, y=prob, color=side, linetype=replicate)) +
    ggplot2::geom_line() +
    ggplot2::scale_discrete_manual("color", values=c("attacker"=attackCol, "defender"=defenceCol)) +
    ggplot2::ylab("P(g<G)") +
    ggplot2::xlab("gain (G)") +
    ggplot2::ggtitle("Gain (cost - opponent cost)") +
    ggplot2::theme_minimal()

  return(pl)
}

#' Make stat table plot
#' @description
#'  Makes a table over some key stats summarising a battle simulation
#' @param simulationResults, formatted as: \code{\link[aaSimulator]{simulationResults}}
#' @return A gtable, see \code{\link[gridExtra]{tableGrob}}.
#' @examples
#'  sim <- simulateBattles("30 inf 11 arm 5 ftr arm",
#'         "40 inf 3 ftr",
#'         iterations = 1000,
#'         replications = 2)
#'  makeBattleStatPlot(sim)
#' @export
makeBattleStatPlot <- function(simulationResults){
  stats <- calculateStats(simulationResults)
  tab <- data.table::data.table(stat=c("attacker won",
                                "defender won",
                                "draw",
                                "mean rounds",
                                "mean cost attacker",
                                "mean cost defender"),
                         value=c(paste(format(stats$averages$attackerWon*100, digits=1), "%"),
                                 paste(format(stats$averages$defenderWon*100, digits=1), "%"),
                                 paste(format(stats$averages$draw*100, digits=1), "%"),
                                 paste(format(stats$averages$meanRounds, digits=1)),
                                 paste(format(stats$averages$meanAttackerCost, digits=1)),
                                 paste(format(stats$averages$meanDefenderCost, digits=1))))

  th <- gridExtra::ttheme_minimal(core=list(fg_params=list(hjust=1, x=0.9)))
  pl <- gridExtra::tableGrob(tab, rows=NULL, cols=NULL, theme=th)
  return(pl)
}

#' Plot battle summary
#' @description
#'  Produces a panneled plot summarizing results of a battle simulation
#' @param simulationResults, formatted as: \code{\link[aaSimulator]{simulationResults}}
#' @examples
#'  sim <- simulateBattles("30 inf 11 arm 5 ftr arm",
#'         "40 inf 3 ftr",
#'         iterations = 1000,
#'         replications = 2)
#'  plotBattleSummary(sim)
#' @export
plotBattleSummary <- function(simulationResults){

  p1 <- makePosteriorDistributionPlot(simulationResults, "attacker") + ggplot2::ggtitle("")
  stats <- makeBattleStatPlot(simulationResults)
  p2 <- makePosteriorDistributionPlot(simulationResults, "defender") + ggplot2::ggtitle("")
  costdiff <- makeCostDiffPlot(simulationResults)

  gridExtra::grid.arrange(p1, stats,
                          p2, costdiff,
                          layout_matrix = rbind(c(1,1,2), c(3,3,4)),
                          top=paste(length(simulationResults$replicates[[1]]), "iterations,", length(simulationResults$replicates), "replicates."))

}
