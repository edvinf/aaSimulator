#' Results from a battle
#'
#' a list with members
#' \describe{
#'  \item{unitsAttacker}{character() vector with the remaining units for attacker. Virtual units should not be included.}
#'  \item{unitsDefender}{character() vector with the remaining units for defender. Virtual units should not be included.}
#'  \item{rounds}{integer() the number of rounds in the battler}
#'  \item{attackerCost}{numeric() The cost of units lost for attacker}
#'  \item{defenderCost}{numeric() The cost of units lost for defender}
#' }
#'
#' @name battleResults
#'
NULL


#' Result from battle simulation
#'
#' For a simuation of n iterations, replicated m times this is:
#' list with members:
#' \describe{
#'  \item{attackerStart}{units for attacker at start of battle, formatted as oolAttacker}
#'  \item{defenderStart}{units for defender at start of battle, formatted as oolDefender}
#'  \item{replicates}{list of length m, with member for each replicate, each a list of length n containing \code{\link[aaSimulator]{battleResults}}}
#' }
#'
#' @name simulationResults
#'
NULL

#' OOL order of loss specification
#' A character vector specifying the units participating in a battle, ordered by their priority (first units lost first).
#' May also contain virtual units, that may be special abilities or control directives to the battle simulator.
#'
#' Exact notation is determined by the battle function used. But for purposes of harmonization, the following notation is encouraged from common units:
#'
#'\describe{
#'   \item{inf}{Infantry}
#'   \item{art}{Artillery}
#'   \item{arm}{Tank}
#'   \item{ftr}{Fighter}
#'   \item{bmb}{Bomber}
#'   \item{trn}{Transport}
#'   \item{sub}{Submarine}
#'   \item{dd}{Destroyer}
#'   \item{ac}{Aircraft Carrier}
#'   \item{bb}{Battleship}
#'}
#'
#'  Some common virtual units:
#'\describe{
#'   \item{AA}{Anti-aircraft Gun. Considered a virtual unit for rule sets where it can not be taken as casualty}
#'   \item{BBomb}{Offshore bombardment from Battleship, supporting amphibious assults.}
#'   \item{BBx}{The first hit of a battleship for rulesets with two-hit capabilities. Modelled as a virtual unit with 0 in attack and defense.}
#'   \item{SUBM}{Flags that all attacking submarines should submerge when possible, after all units preceeding this virtual unit are lost.}
#'   \item{RET}{Flags that attacker should retreat at first opportunity when all units preceeding this virtual unit are lost.}
#'}
#'
#' @name ool
#'
NULL

#' Simulate Battles
#' @description
#' Simulate Axis and Allies Battles
#' @details
#' The battle function 'FUN' implements the ruleset used.
#' FUN must accept the arguments 'oolAttacker' and 'oolDefender' and return \code{\link[aaSimulator]{battleResults}}
#'
#' @param oolAttacker character() vector of unit codes in preferred order of loss, formatted as \code{\link[aaSimulator]{ool}}
#' @param oolDefender character() vector of unit codes in preferred order of loss, formatted as \code{\link[aaSimulator]{ool}}
#' @param FUN function for running one battle
#' @param ... additional arguments passed to FUN
#' @param iterations number of iterations to simulate for each replication
#' @param replications number of replicates to run
#' @return \code{\link[aaSimulator]{simulationResults}}
#' @export
simulateBattles <- function(oolAttacker, oolDefender, FUN=play_LHTR_battle, ..., iterations=2000, replications=3){

  if (replications < 1){
    stop("Must run at least one replicate.")
  }
  if (iterations < 1){
    stop("Must run at least one iteratation.")
  }
  if ("RET" %in% oolDefender){
    stop("Flag RET is not allowed in defender OOL.")
  }
  if (sum(oolAttacker=="RET")>1){
    stop("Include RET only once in attacker OOL.")
  }
  if (sum(oolDefender=="SUBM")>1){
    stop("Include SUBM only once in each OOL.")
  }
  if (sum(oolAttacker=="SUBM")>1){
    stop("Include SUBM only once in each OOL.")
  }

  results <- list()
  results$attackerStart <- oolAttacker
  results$defenderStart <- oolDefender
  results$replicates <- list()

  for (i in 1:replications){

    firstresult <- FUN(oolAttacker, oolDefender, ...)

    outcome <- list()
    outcome[[1]] <- firstresult

    for (j in 2:iterations){
      lastresult <- FUN(oolAttacker, oolDefender, ...)
      outcome[[j]] <- lastresult
    }

    results$replicates[[i]] <- outcome
  }

  return(results)
}


#' Battle statistics for simulation
#'
#' list with the following members:
#' \describe{
#'  \item{attackerStart}{units for attacker at start of battle, formatted as oolAttacker}
#'  \item{defenderStart}{units for defender at start of battle, formatted as oolDefender}
#'  \item{avereages}{stats averaged over replicates}
#'  \item{replicates}{vectors with stats for all replicates of simulation}
#' }
#'
#' Both 'averages' and the 'replicates' contain the stats:
#' \describe{
#'  \item{attackerWon}{proportion of battles won by attacker (all defending units lost), averaged over replicates}
#'  \item{defendeWon}{proportion of battles won by defender (not all defending units lost), averaged over replicates}
#'  \item{draw}{proportion of battles drawed (all units lost), averaged over replicates}
#'  \item{meanRounds}{mean number of rounds before battle was concluded}
#'  \item{meanAttackerCost}{mean cost of battle for attacker, averaged over iterations and replicates}
#'  \item{meanDefenderCost}{mean cost of battle for defender, averaged over iterations and replicates}
#' }
#'
#' @name simulationStats
#'
NULL

#' Calculate battle statistics
#' @description Calculates battle statistics for simulation
#' @param simulationResults, formatted as: \code{\link[aaSimulator]{simulationResults}}
#' @return \code{\link[aaSimulator]{simulationStats}}
#' @export
calculateStats <- function(simulationResults){

  # function for applying to each replicate

  meanValue <- function(repl, slot){
    return(mean(unlist(lapply(repl, FUN = function(x){x[[slot]]}))))
  }

  meanCostDiff <- function(repl){
    return(mean(unlist(lapply(repl, FUN = function(x){x[["defenderCost"]] - x[["attackerCost"]]}))))
  }


  winProportion <- function(repl){
    return(sum(unlist(lapply(repl, FUN = function(x){length(x[["unitsAttacker"]]) > 0 & length(x[["unitsDefender"]]) == 0}))) / length(repl))
  }

  looseProportion <- function(repl){
    return(sum(unlist(lapply(repl, FUN = function(x){length(x[["unitsDefender"]]) > 0}))) / length(repl))
  }

  drawProportion <- function(repl){
    return(sum(unlist(lapply(repl, FUN = function(x){length(x[["unitsAttacker"]]) == 0 & length(x[["unitsDefender"]]) == 0}))) / length(repl))
  }

  stats <- list()
  stats$attackerStart <- simulationResults$attackerStart
  stats$defenderStart <- simulationResults$defenderStart
  stats$replicates <- list()
  stats$averages <- list()
  stats$averages$attackerWon <- mean(unlist(lapply(simulationResults$replicates, FUN=winProportion)))
  stats$averages$defenderWon <- mean(unlist(lapply(simulationResults$replicates, FUN=looseProportion)))
  stats$averages$draw <- mean(unlist(lapply(simulationResults$replicates, FUN=drawProportion)))
  stats$averages$meanRounds <- mean(unlist(lapply(simulationResults$replicates, FUN=meanValue, "rounds")))
  stats$averages$meanAttackerCost <- mean(unlist(lapply(simulationResults$replicates, FUN=meanValue, "attackerCost")))
  stats$averages$meanDefenderCost <- mean(unlist(lapply(simulationResults$replicates, FUN=meanValue, "defenderCost")))
  stats$averages$meanCostDifference <- mean(unlist(lapply(simulationResults$replicates, FUN=meanCostDiff)))

  stats$replicates$attackerWon <- unlist(lapply(simulationResults$replicates, FUN=winProportion))
  stats$replicates$defenderWon <- unlist(lapply(simulationResults$replicates, FUN=looseProportion))
  stats$replicates$draw <- unlist(lapply(simulationResults$replicates, FUN=drawProportion))
  stats$replicates$meanRounds <- unlist(lapply(simulationResults$replicates, FUN=meanValue, "rounds"))
  stats$replicates$meanAttackerCost <- unlist(lapply(simulationResults$replicates, FUN=meanValue, "attackerCost"))
  stats$replicates$meanDefenderCost <- unlist(lapply(simulationResults$replicates, FUN=meanValue, "defenderCost"))
  stats$replicates$meanCostDifference <- unlist(lapply(simulationResults$replicates, FUN=meanCostDiff))

  return(stats)

}

#' Roll dice
#' @description Rolls a set of six-sided dice, and determine number of hits.
#' @param n number of dice to roll
#' @param hitvalue largest value to count as a hit
#' @return numeric() number of hits
#' @export
roll <- function(n, hitvalue){

  hits <- sum(sample.int(6, n, replace = T) <= hitvalue)

  return(hits)
}

#' Determines which units should be popped (removed) units of ool
#' @param ool list of units ordered by preffered order of loss
#' @param hits number of units to pop
#' @param removeables list of valid targets for hits
#' @param skip list of units to remove if encountered before other units that are to be removed.
#' @return logical() vector with TRUE for units to keep
#' @noRd
#' @keywords internal
#' Removes first 'hits' units from ool
#' Only hits in removables are considered towards hit count,
#' while both hits in removables and skip are removed
#' but other hits are also removed (control directives such as RET and SUBM etc.)
#' @noRd
pop <- function(ool, hits, removeables, skip=c()){

  if (length(ool) == 0){
    return(logical())
  }

  mask <- logical()
  assigned <- 0
  for (i in 1:length(ool)){
    if (assigned < hits){
      if (ool[i] %in% removeables){
        mask <- c(mask, F)
        assigned <- assigned + 1
      }
      else if (ool[i] %in% skip) {
        mask <- c(mask, F)
      }
      else{
        mask <- c(mask, T)
      }
    }
    else{
      mask <- c(mask, T)
    }
  }
  return(mask)
}
