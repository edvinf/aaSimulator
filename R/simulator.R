#' unit table
#'
#' @description
#'  Table of units for an Axis and Allies version
#'
#' @details
#'  \code{\link[data.table]{data.table}} with the following columns:
#'  \describe{
#'   \item{shortcut}{character() short notation for unit, e.g. 'inf' for 'infantry'}
#'   \item{name}{character() name of unit, e.g. 'infantry}
#'   \item{cost}{integer() cost of unit}
#'   \item{baseAttack}{integer() attack value for unit, when no modifiers are applied, NA for units that can not be taken casuality when attacking}
#'   \item{baseDefence}{integer() defence for unit, when no modifiers are applied, NA for units that can not be taken casualtiy when defending}
#'   \item{move}{integer() number of moves permissible for unit}
#'   \item{type}{character() type of unit, modifies which kind of battles they can be involved in (e.g. "Land", "Sea")}
#'   \item{virtualUnit}{logical() whether precense of units counts towards resolving battle in favour of attacer/defender}
#'  }
#'
#' @name unitTable
#'
NULL

#' Results from a battle
#'
#' a list with members
#' \describe{
#'  \item{unitsAttacker}{character() vector with the remaining units for attacker. Should not contain virtual units.}
#'  \item{unitsDefender}{character() vector with the remaining units for defender. Should not contain virtual units.}
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
#'  \item{attackerStart}{units for attacker at start of battle, formatted as \code{\link[aaSimulator]{ool}}}
#'  \item{defenderStart}{units for defender at start of battle, formatted as \code{\link[aaSimulator]{ool}}}
#'  \item{replicates}{list of length m, with member for each replicate, each a list of length n containing \code{\link[aaSimulator]{battleResults}}}
#' }
#'
#' @name simulationResults
#'
NULL

#' Result from two-wave battle simulation
#'
#' For a simuation of n iterations, replicated m times this is:
#' list with members:
#' \describe{
#'  \item{firstWaveStart}{units for first wave attacker at start of battle, formatted as formatted as \code{\link[aaSimulator]{ool}}}
#'  \item{secondWaveStart}{units for second wave attacker at start of battle, formatted as formatted as \code{\link[aaSimulator]{ool}}}
#'  \item{defenderStart}{units for defender at start of battle, formatted as formatted as \code{\link[aaSimulator]{ool}}}
#'  \item{defenderReinforced}{units for defender at start of battle, with reinforcements added, formatted as \code{\link[aaSimulator]{ool}}}
#'  \item{replicates}{list of length m, with member for each replicate, each a list of length n containing two members: 'firstWave' and 'secondWave', both formatted as \code{\link[aaSimulator]{battleResults}}}
#' }
#'
#' @name twoWaveSimulationResults
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

#' string format for order of loss specification
#'
#' unit codes as for \code{\link[aaSimulator]{ool}}, but organised as a whitespace separated string rather than a vector.
#'
#' repeted units may be prefixing units with numbers (and a whitespace to separate unit from number)
#'
#' @name prefixedOol
#'
NULL


#' Expand prefixed OOL
#' @description
#'  Expand order of loss specification from prefixed form.
#' @param prefixedOol order of loss specification formatted as \code{\link[aaSimulator]{prefixedOol}}
#' @return order of loss specification formatted as \code{\link[aaSimulator]{ool}}
#' @export
expandPrefixedOOL <- function(prefixedOol){
  s <- strsplit(prefixedOol, split = "\\s+")[[1]]
  if (length(s)==0){
    return(c())
  }

  output <- c()
  prefix <- NULL
  for (e in s){
    if (grepl("^[0-9]", e)){
      if (!is.null(prefix)){
        stop(paste("Stop error parsing token", e, "A number may not follow a number in prefixed OOL."))
      }
      prefix <- suppressWarnings(as.integer(e))
      if (is.na(prefix)){
        stop(paste("Stop error parsing token", e))
      }
    } else{
      if (is.null(prefix)){
        output <- c(output, e)
      }
      if (!is.null(prefix)){
        for (i in 1:prefix){
          output <- c(output, e)
        }
        prefix <- NULL
      }
    }
  }
  return(output)
}

#' Collapse OOL
#' @description
#'  Collapse order of loss specification to prefixed form.
#' @param ool order of loss specification formatted as \code{\link[aaSimulator]{ool}}
#' @return order of loss specification formatted as \code{\link[aaSimulator]{prefixedOol}}
#' @export
collapseOol <- function(ool){

  if (length(ool) == 0){
    return("")
  }

  outstr <- NULL
  lastunit <- NULL
  unitcount <- 0
  for (u in ool){
    if (is.null(lastunit)){
      lastunit <- u
      unitcount <- 1
    }
    else if (u == lastunit){
      unitcount <- unitcount + 1
    }
    else{
      if (is.null(outstr)){
        outstr <- paste(unitcount, lastunit)
      }
      else{
        outstr <- paste(outstr, unitcount, lastunit)
      }
      lastunit <- u
      unitcount <- 1
    }
  }

  if (is.null(outstr)){
    outstr <- paste(unitcount, lastunit)
  }
  else{
    outstr <- paste(outstr, unitcount, lastunit)
  }

  return(outstr)
}

#' Simulate Battles
#' @description
#' Simulate Axis and Allies Battles
#' @details
#' The battle function 'FUN' implements the ruleset used.
#' FUN must accept the arguments 'oolAttacker' and 'oolDefender' and a logical argument suppressChecks,
#' it must return \code{\link[aaSimulator]{battleResults}}
#'
#' @param oolAttacker character() vector of unit codes in preferred order of loss, formatted as \code{\link[aaSimulator]{ool}} or \code{\link[aaSimulator]{prefixedOol}}
#' @param oolDefender character() vector of unit codes in preferred order of loss, formatted as \code{\link[aaSimulator]{ool}} or \code{\link[aaSimulator]{prefixedOol}}
#' @param FUN function for running one battle
#' @param ... additional arguments passed to FUN
#' @param iterations number of iterations to simulate for each replication
#' @param replications number of replicates to run
#' @return \code{\link[aaSimulator]{simulationResults}}
#' @export
simulateBattles <- function(oolAttacker, oolDefender, FUN=play_LHTR_battle, ..., iterations=2000, replications=3){

  if (length(oolAttacker)==1){
    oolAttacker <- expandPrefixedOOL(oolAttacker)
  }
  if (length(oolDefender)==1){
    oolDefender <- expandPrefixedOOL(oolDefender)
  }

  if (replications < 1){
    stop("Must run at least one replicate.")
  }
  if (iterations < 1){
    stop("Must run at least one iteratation.")
  }

  results <- list()
  results$attackerStart <- oolAttacker
  results$defenderStart <- oolDefender
  results$replicates <- list()

  for (i in 1:replications){

    firstresult <- FUN(oolAttacker, oolDefender, suppressChecks=F, ...)

    outcome <- list()
    outcome[[1]] <- firstresult

    for (j in 2:iterations){
      lastresult <- FUN(oolAttacker, oolDefender, suppressChecks=T, ...)
      outcome[[j]] <- lastresult
    }

    results$replicates[[i]] <- outcome
  }

  return(results)
}

#' Simulate two-wave Battles
#' @description
#' Simulate Axis and Allies two-wave Battles
#' @details
#' This function simulates two waves of attack against a defender,
#' with possible reinforcement of defence between the waves.
#' Reinforcements are specified by a function (argument 'reinforcement') that modifies the
#' surviving units from the first wave before the second wave commences.
#'
#' The battle function 'FUN' implements the ruleset used.
#' FUN must accept the arguments 'oolAttacker' and 'oolDefender' and a logical argument suppressChecks,
#' it must return \code{\link[aaSimulator]{battleResults}}
#'
#' @param oolFirstAttacker character() vector of unit codes in preferred order of loss for first attack wave, formatted as \code{\link[aaSimulator]{ool}} or \code{\link[aaSimulator]{prefixedOol}}
#' @param oolFirstAttacker character() vector of unit codes in preferred order of loss for second attack wave, formatted as \code{\link[aaSimulator]{ool}} or \code{\link[aaSimulator]{prefixedOol}}
#' @param oolDefender character() vector of unit codes in preferred order of loss for defender, formatted as \code{\link[aaSimulator]{ool}} or \code{\link[aaSimulator]{prefixedOol}}
#' @param FUN function for running one battle
#' @param ... additional arguments passed to FUN
#' @param iterations number of iterations to simulate for each replication
#' @param replications number of replicates to run
#' @param reinformcement function for reinforcing defender between waves, accepts and returns argument formatted as \code{\link[aaSimulator]{ool}}.
#' @return \code{\link[aaSimulator]{twoWaveSimulationResults}}
#' @examples
#'  # simulate a two wave attack against 10 infantry,
#'  # reinforced by 3 fighters added at the end of order of loss specification
#'  # first wave of attack consists of 10 infantry
#'  # second wave of attack consists of 10 infantry
#'  result <- simulateTwoWaveBattles("10 inf", "10 inf", "10 inf",
#'       reinforcement = function(x){c(x, rep("ftr", 3))})
#' @export
simulateTwoWaveBattles <- function(oolFirstAttacker, oolSecondAttacker, oolDefender, FUN=play_LHTR_battle, ..., iterations=2000, replications=3, reinforcement=function(x){x}){

  warning("Not finished. Need to deal with virtual units in defender (AA guns BBx, etc.")

  if (length(oolFirstAttacker)==1){
    oolFirstAttacker <- expandPrefixedOOL(oolFirstAttacker)
  }
  if (length(oolSecondAttacker)==1){
    oolSecondAttacker <- expandPrefixedOOL(oolSecondAttacker)
  }
  if (length(oolDefender)==1){
    oolDefender <- expandPrefixedOOL(oolDefender)
  }

  if (replications < 1){
    stop("Must run at least one replicate.")
  }
  if (iterations < 1){
    stop("Must run at least one iteratation.")
  }

  output <- list()

  results <- list()
  results$firstWaveStart <- oolFirstAttacker
  results$secondWaveStart <- oolSecondAttacker
  results$defenderStart <- oolDefender
  results$defenderReinforced <- reinforcement(oolDefender)
  results$replicates <- list()

  for (i in 1:replications){

    firstresultFirstWave <- FUN(results$firstWaveStart, results$defenderStart, suppressChecks=F, ...)
    firstresultSecondWave <- FUN(results$secondWaveStart, reinforcement(firstresultFirstWave$unitsDefender), suppressChecks=F, ...)

    outcome <- list()
    outcome[[1]] <- list()
    outcome[[1]]$firstWave <- firstresultFirstWave
    outcome[[1]]$secondWave <- firstresultSecondWave

    for (j in 2:iterations){
      lastresultFirstwave <- FUN(results$firstWaveStart, results$defenderStart, suppressChecks=T, ...)
      lastresultSecondWave <- FUN(results$secondWaveStart, reinforcement(lastresultFirstwave$unitsDefender), suppressChecks=T, ...)
      outcome[[j]] <- list()
      outcome[[j]]$firstWave <- lastresultFirstwave
      outcome[[j]]$secondWave <- lastresultSecondWave
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

#' Determine which of units in start survived
#' @noRd
makeSurvivalMask <- function(start, remain){

  startrev <- rev(start)
  for (e in remain){
    startrev[match(e, startrev)] <- NA
  }
  return(is.na(rev(startrev)))
}

#' posterior distribution
#' @description
#'  Calculate posterior distribution for battle
#' @param simulationResults, formatted as: \code{\link[aaSimulator]{simulationResults}}
#' @param side character(), side to calculate distribution for, either 'attacker' or 'defender'
#' @return data.table with posterior probabilities of survival for each unit in ool (in original order). Each replicate is represented by one row.
#' @export
calculatePosteriorDistribution <- function(simulationResults, side="attacker"){

  if (side == "attacker"){
    slot <- "unitsAttacker"
    startslot <- "attackerStart"
  }
  else if( side == "defender"){
    slot <- "unitsDefender"
    startslot <- "defenderStart"
  }
  else{
    stop(paste("Side", side, "not recognized."))
  }


  postDrepl <- function(repl){
    survivalmasks <- lapply(repl, FUN=function(x){makeSurvivalMask(simulationResults[[startslot]], x[[slot]])})
    post <- colSums(do.call(rbind, survivalmasks))/length(survivalmasks)
    names(post) <- simulationResults[[startslot]]
    return(post)
  }

  return(data.table::as.data.table(do.call(rbind, lapply(simulationResults$replicates, FUN=postDrepl))))
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
#' @param removeables list of valid targets for hits, if NULL, all targets are considered valid
#' @param skip list of units to remove if encountered before other units that are to be removed.
#' @return logical() vector with TRUE for units to keep
#' @noRd
#' @keywords internal
#' Removes first 'hits' units from ool
#' Only hits in removables are considered towards hit count,
#' while both hits in removables and skip are removed
#' but other hits are also removed (control directives such as RET and SUBM etc.)
#' @noRd
pop <- function(ool, hits, removeables=NULL, skip=c()){

  if (length(ool) == 0){
    return(logical())
  }

  if (is.null(removeables)){
    removable <- rep(T, length(ool))
  }
  else{
    removable <- (ool %in% removeables)
  }

  skipped <- (ool %in% skip)

  mask <- rep(T, length(ool))
  assigned <- 0
  for (i in 1:length(ool)){
    if (assigned < hits){
      if (removable[i]){
        mask[i] <- F
        assigned <- assigned + 1
      }
      else if (skipped[i]) {
        mask[i] <- F
      }
      else{
        #mask already T
      }
    }
    else{
      return(mask)
    }
  }
  return(mask)
}


#' @noRd
constructOolsOpt <- function(cost, units, unittable){
  ools <- list()
  j<-1
  for (i in 1:length(units)){
    unitcost <- unittable$cost[unittable$shortcut == units[i]]
    if (unitcost <= cost){
      recOols <- constructOolsOpt(cost - unitcost, units[i:length(units)], unittable)
      if (length(recOols) > 0){
        for (ool in recOols){
          ools[[j]] <- c(units[i], ool)
          j <- j+1
        }
      }
      else{
        ools[j] <- c(units[i])
        j <- j+1
      }
    }
  }

  return(ools)
}

#' @noRd
runBattlesOpt <- function(unitcombinations, attacker, defender, iterations, replications){
  results <- list()
  for (i in 1:length(unitcombinations)){
    if (is.null(attacker)){
      results[[i]] <- calculateStats(simulateBattles(unitcombinations[[i]], defender, iterations = iterations, replications = replications))
    }
    if (is.null(defender)){
      results[[i]] <- calculateStats(simulateBattles(attacker, unitcombinations[[i]], iterations = iterations, replications = replications))
    }
  }
  return(results)
}

#' @noRd
runBattlesOptVerbose <- function(unitcombinations, attacker, defender, iterations, replications){
  results <- list()
  for (i in 1:length(unitcombinations)){
    cat(".")
    if (is.null(attacker)){
      results[[i]] <- calculateStats(simulateBattles(unitcombinations[[i]], defender, iterations = iterations, replications = replications))
    }
    if (is.null(defender)){
      results[[i]] <- calculateStats(simulateBattles(attacker, unitcombinations[[i]], iterations = iterations, replications = replications))
    }
  }
  write("", stdout())
  return(results)
}

#' @noRd
getOpt <- function(results, attacker, defender, rank){

  if (!(rank %in% c("overlap", "all"))){
    i <- suppressWarnings(as.integer(rank))
    if (is.na(i)){
      stop("Parameter 'rank' is not an integer or a legal keyword.")
    }
  }

  if (is.null(attacker)){
    results <- results[order(unlist(lapply(results, FUN=function(x){x$averages$attackerWon})), decreasing = T)]
    if (rank == "overlap"){
      maxAttackerWon <- results[[1]]$averages$attackerWon
      optimaAttacker <- unlist(lapply(results, FUN=function(x){any(x$replicates$attackerWon >= maxAttackerWon)}))
    }
    else if (rank == "all"){
      optimaAttacker <- 1:length(results)
    }
    else{
      optimaAttacker <- 1:min(as.integer(rank), length(results))
    }

    return(results[optimaAttacker])
  }

  if (is.null(defender)){
    results <- results[order(unlist(lapply(results, FUN=function(x){x$averages$defenderWon})), decreasing = T)]
    if (rank == "overlap"){
      maxDefenderWon <- results[[1]]$averages$defenderWon
      optimaDefender <- unlist(lapply(results, FUN=function(x){any(x$replicates$defenderWon >= maxDefenderWon)}))
    }
    else if (rank == "all"){
      optimaDefender <- 1:length(results)
    }
    else{
      optimaDefender <- 1:min(as.integer(rank), length(results))
    }
    return(results[optimaDefender])
  }

  stop("Error")

}

#' Optimize units
#' @description
#'  Aproximately optimize unit configuration for fixed costs.
#' @details
#'  Runs simulation of for each possible unit configuration, respecting the order of parameter 'units',
#'  and identifies optimal configurations.
#'
#'  The optimal configurations returned are controlled by the parameter 'rank'. If 'rank' is an integer n,
#'  n first results will be returned, ordered by average win percentage.
#'  'rank' may also be a keyword which will have the following effects:
#'  \describe{
#'   \item{'overlap'}{The highest ranking result will be returned, toghether with all results where some replicate peforms at least as good as the optimum average.}
#'   \item{'all}{All results will be returned}
#'   }
#'
#'  'units' denote order of unit groups, so that units=c("inf", "arm"), will try all combinations
#'  of "inf" and "arm" allowed by the parameter 'cost', but always with all "inf" preceeding all "arm".
#'
#'  if 'iterations', 'replications' and 'rank' are vector of equal length, they specify a n iterative optimization,
#'  where the optimal unit configurations from the first round (optimization with interations[1], replications[1], and rank[1])
#'  are used as the only available unit configurations for subsequent runs.
#'
#' @param cost The cost to optimize for, unit configurations must not exceed this cost
#' @param iterations the number of iterations to run for each unit configuration.
#' @param replications the number of replicates to run for each unit configuration
#' @param rank number of keyword for determining which results to return. See details.
#' @param attacker ool for attacker, NULL if attacking units are to be optimized
#' @param defender ool for defender, NULL if defending units are to be optimized
#' @param units the units in order of loss that should be sampled for optimization, formatted as \code{\link[aaSimulator]{ool}}. See details.
#' @param unittable table of unit properties, formatted as \code{\link[aaSimulator]{unitTable}}
#' @param verbose logical() whether to write progress information to stdout
#' @return list with entries formatted as formatted as \code{\link[aaSimulator]{simulationStats}}, containing stats for the optimal unit configurations. See details.
#' @examples
#'  results <- optimizeUnits(12, defender = "2 inf", units=c("inf", "art", "arm"),
#'                               iterations=100, replications=3, rank="overlap",
#'                               verbose=TRUE)
#' @export
optimizeUnits <- function(cost, iterations=c(5,30,100,2000), replications=c(25,10,3,1), rank=c("overlap", "overlap", 10, "all"), attacker=NULL, defender=NULL, units=c(), unittable=aaSimulator::lhtr2_units, verbose=T){

  info <- function(text){
    if (verbose){
      write(text, stdout())
    }
  }

  if (is.null(attacker) && is.null(defender)){
    stop("'attacker' and 'defender' may not both be NULL.")
  }

  if (!is.null(attacker) && !is.null(defender)){
    stop("'Either attacker' or 'defender' must be NULL.")
  }

  if (!all(units %in% unittable$shortcut)){
    invalid <- units[!(units %in% unittable$shortcut)]
    stop(paste("Invalid units in 'units':", paste(invalid, collapse = ", ")))
  }

  if (length(iterations) != length(replications)){
    stop("Length of the vector 'iterations' and the vector 'replications' must match")
  }
  if (length(iterations) != length(rank)){
    stop("Length of the vector 'iterations' and the vector 'rank' must match")
  }

  unitcombinations <- unique(constructOolsOpt(cost, units, unittable))
  info(paste("Running optimisation for", length(unitcombinations), "configurations, with", iterations[1], "iterations and", replications[1], "replications ..."))
  if (verbose){
    res <- runBattlesOptVerbose(unitcombinations, attacker, defender, iterations[1], replications[1])
  }
  else{
    res <- runBattlesOpt(unitcombinations, attacker, defender, iterations[1], replications[1])
  }

  optima <- getOpt(res, attacker, defender, rank[1])
  info(paste("Retaining", length(optima), "optima."))

  if (length(iterations) > 1){
    for (i in 2:length(iterations)){
      unitcombinations <- unique(lapply(optima, FUN=function(x){x$attackerStart}))
      info(paste("Running optimisation for", length(unitcombinations), "configurations, with", iterations[i], "iterations and", replications[i], "replications ..."))
      if (verbose){
        res <- runBattlesOptVerbose(unitcombinations, attacker, defender, iterations[i], replications[i])
      }
      else{
        res <- runBattlesOpt(unitcombinations, attacker, defender, iterations[i], replications[i])
      }
      optima <- getOpt(res, attacker, defender, rank[i])
      info(paste("Retaining", length(optima), "optima."))
    }
  }

  if (verbose){
    info("######")
    if (is.null(attacker)){
      info(paste("Optimised attack"))
      info(paste("Defender: ", defender))
    }
    if (is.null(defender)){
      info(paste("Optimised defence"))
      info(paste("Attacker: ", attack))
    }
    info(paste("Optimal unit configurations at cost ", cost, ", using units ", paste(units, collapse=", "), ":"))

    for (r in optima){
      if (is.null(attacker)){
        info(paste(collapseOol(r$attackerStart), " (attacker won: ", format(r$averages$attackerWon*100, digits=1), "%)", sep=""))
      }
      if (is.null(defender)){
        info(paste(collapseOol(r$defenderStart), " (defender won: ", format(r$averages$defenderWon*100, digits=1), "%)", sep=""))
      }
    }
  }

  return(optima)

}
