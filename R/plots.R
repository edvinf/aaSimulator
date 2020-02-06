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
    ggplot2::ylab("survival prob.") +
    ggplot2::xlab("") +
    ggplot2::scale_x_discrete(labels=dt$unit) +
    ggplot2::ggtitle(paste(length(simulationResults$replicates[[1]]), "iterations,", length(simulationResults$replicates), "replicates.")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
  return(pl)
}

#' Make plot of costs
#' @description Plots the cumulative probabilities of costs for a battle simulation
#' @param simulationResults, formatted as: \code{\link[aaSimulator]{simulationResults}}
#' @param attackCol color to use when side is 'attacker'
#' @param defenceCol color to use when side is 'defender'
#' @param legend logical() whether to add a legend to the plot
#' @return ggplot object
#' @examples
#'  sim <- simulateBattles("30 inf 11 arm 5 ftr arm",
#'         "40 inf 3 ftr",
#'         iterations = 1000,
#'         replications = 2)
#'  makeCostPlot(sim)
#' @export
makeCostPlot <- function(simulationResults, attackCol="red", defenceCol="blue", legend=T){
  costDiffs <- lapply(simulationResults$replicates, FUN=function(x){unlist(lapply(x, function(y){y$attackerCost - y$defenderCost}))})
  costs <- c()
  probs <- c()
  side <- c()
  replicate <- c()
  for (i in 1:length(simulationResults$replicates)){
    cost <- unlist(lapply(simulationResults$replicates[[i]], function(y){y$attackerCost}))
    prob <- (max(rank(cost)) - rank(cost)) / max(rank(cost))
    costs <- c(costs, cost)
    probs <- c(probs, prob)
    side <- c(side, rep("attacker", length(cost)))
    replicate <- c(replicate, rep(paste("#", i), length(cost)))

    cost <- unlist(lapply(simulationResults$replicates[[i]], function(y){y$defenderCost}))
    prob <- (max(rank(cost)) - rank(cost)) / max(rank(cost))
    costs <- c(costs, cost)
    probs <- c(probs, prob)
    side <- c(side, rep("defender", length(cost)))
    replicate <- c(replicate, rep(paste("#", i), length(cost)))
  }

  dt <- data.table::data.table(cost=costs, prob=probs, side=side, replicate=replicate)

  pl <- ggplot2::ggplot(dt, ggplot2::aes(x=cost, y=prob, color=side, linetype=replicate)) +
    ggplot2::geom_line() +
    ggplot2::ylab("P(c \u2265 C)") +
    ggplot2::xlab("cost (C)") +
    ggplot2::theme_minimal()

  if (!legend){
    pl <- pl + ggplot2::guides(linetype=F) + ggplot2::scale_discrete_manual("color", values=c("attacker"=attackCol, "defender"=defenceCol), guide=F)
  }
  else{
    pl <- pl + ggplot2::scale_discrete_manual("color", values=c("attacker"=attackCol, "defender"=defenceCol))
  }

  return(pl)
}

#' Make battle stats
#' @noRd
makeBstats <- function(stats){
  tab <- data.table::data.table(stat=c("attacker won",
                                       "defender survived",
                                       "neither survived",
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
  return(makeBstats(stats))
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

  p1 <- makePosteriorDistributionPlot(simulationResults, "attacker") + ggplot2::ggtitle("attacker")
  stats <- makeBattleStatPlot(simulationResults)
  p2 <- makePosteriorDistributionPlot(simulationResults, "defender") + ggplot2::ggtitle("defender")
  costdiff <- makeCostPlot(simulationResults, legend=F) + ggplot2::ggtitle("")

  gridExtra::grid.arrange(p1, stats,
                          p2, costdiff,
                          layout_matrix = rbind(c(1,1,2), c(3,3,4)),
                          top=paste(length(simulationResults$replicates[[1]]), "iterations,", length(simulationResults$replicates), "replicates."))

}

#' Prep twoWaveBattleResult for plotting with plot functions defined for regular battleSimulations
#' @noRd
extractTwoWaveSimulationResult <- function(twoWaveSimulationResults){

  result <- list()
  result$firstWave <- list()
  result$firstWave$attackerStart <- twoWaveSimulationResults$firstWaveStart
  result$firstWave$defenderStart <- twoWaveSimulationResults$defenderStart
  result$firstWave$replicates <- list()
  for (i in 1:length(twoWaveSimulationResults$replicates)){
    result$firstWave$replicates[[i]] <- lapply(twoWaveSimulationResults$replicates[[i]], FUN=function(x){x$firstWave})
  }

  result$secondWave <- list()
  result$secondWave$attackerStart <- twoWaveSimulationResults$secondWaveStart
  result$secondWave$defenderStart <- twoWaveSimulationResults$defenderReinforced
  result$secondWave$replicates <- list()
  for (i in 1:length(twoWaveSimulationResults$replicates)){
    result$secondWave$replicates[[i]] <- lapply(twoWaveSimulationResults$replicates[[i]], FUN=function(x){x$secondWave})
  }

  statFirst <- calculateStats(result$firstWave)
  statSecond <- calculateStats(result$secondWave)

  totalStat <- statSecond
  totalStat$attackerStart <- NULL
  totalStat$defenderStart <- NULL
  totalStat$replicates$meanRounds <- totalStat$replicates$meanRounds + statFirst$replicates$meanRounds
  totalStat$replicates$meanAttackerCost <- totalStat$replicates$meanAttackerCost + statFirst$replicates$meanAttackerCost
  totalStat$replicates$meanDefenderCost <- totalStat$replicates$meanDefenderCost + statFirst$replicates$meanDefenderCost
  totalStat$replicates$meanCostDifference <- totalStat$replicates$meanDefenderCost - totalStat$replicates$meanAttackerCost

  totalStat$averages$attackerWon <- mean(totalStat$replicates$attackerWon)
  totalStat$averages$defenderWon <- mean(totalStat$replicates$defenderWon)
  totalStat$averages$draw <- mean(totalStat$replicates$draw)
  totalStat$averages$meanRounds <- mean(totalStat$replicates$meanRounds)
  totalStat$averages$meanAttackerCost <- mean(totalStat$replicates$meanAttackerCost)
  totalStat$averages$meanDefenderCost <- mean(totalStat$replicates$meanDefenderCost)
  totalStat$averages$meanCostDifference <- mean(totalStat$replicates$meanCostDifference)

  result$stats <- totalStat

  return(result)

}

#' Plot summary of two-wave battle
#' @description
#'  Produces a panneled plot summarizing results of a two-wave battle simulation
#' @param twoWaveSimulationResults, formatted as: \code{\link[aaSimulator]{twoWaveSimulationResults}}
#' @examples
#'  sim <- simulateTwoWaveBattles("7 inf", "7 inf 3 ftr", "7 inf",
#'                                reinforcement = function(x){c(x, rep("ftr", 2))},
#'                                replications = 5, iterations = 1000)
#'  plot2WaveSummary(sim)
#' @export
plot2WaveSummary <- function(twoWaveSimulationResults){
  result <- extractTwoWaveSimulationResult(twoWaveSimulationResults)

  p1 <- makePosteriorDistributionPlot(result$firstWave, "attacker") + ggplot2::ggtitle("first wave attacker")
  p2 <- makePosteriorDistributionPlot(result$firstWave, "defender") + ggplot2::ggtitle("first wave defender")
  p3 <- makePosteriorDistributionPlot(result$secondWave, "attacker") + ggplot2::ggtitle("second wave attacker")
  p4 <- makePosteriorDistributionPlot(result$secondWave, "defender") + ggplot2::ggtitle("second wave defender")
  stats <- makeBstats(result$stats)

  gridExtra::grid.arrange(p1, p2,
                          p3, p4,
                          stats,
                          layout_matrix = rbind(c(1,2), c(3,4), c(5,5)),
                          top=paste(length(twoWaveSimulationResults$replicates[[1]]), "iterations,", length(twoWaveSimulationResults$replicates), "replicates."))

}


