
#' Simulate Battles
#' @description Simulate Axis and Allies Battles
#' @details
#'  oolAttacker and oolDefender list all units in order of preferred loss.
#'  In addition to regular units, some virtual units are added for signaling special abilities of units or battle choices.
#'  The exact list of units depends on the function provided (FUN), but the following codes are reserved, and their use is encouraged when applicable:
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
#'   \item{aa}{Anti-aircraft Gun. Most rule sets will allow only one per battle.}
#'}
#'  Some common virtual units:
#'\describe{
#'   \item{BBomb}{Battleship used for offshore bombardment}
#'   \item{BBx}{The first hit of a battleship for rulesets with two-hit capabilities. Modelled as a virtual unit with 0 in attack and defense.}
#'   \item{SUBM}{Flags submarines should submerge when possible.}
#'   \item{RET}{Flags that attacker should retreat at first opportunity when all units preceeding this virtual unit are lost.}
#'}
#' @param oolAttacker character() vector of unit codes in preferred order of loss
#' @param oolDefender character() vector of unit codes in preferred order of loss
#' @param FUN function for running one battle
#' @param ... additional arguments passed to FUN
#' @param iterations number of iterations to simulate for each replication
#' @param replications number of replications to run
#' @return list with one member for each replication. Each replicate contains the members:
#' \describe{
#'  \item{IPClossAttacker}{numeric() with IPC loss for attacker for each simulated battle}
#'  \item{IPClossDefender}{numeric() with IPC loss for defender for each simulated battle}
#'  \item{unitsLeftAttacker}{data.table() with units left after each simulated battle for attacker}
#'  \item{unitsLeftDefender}{data.table() with units left after each simulated battle for defender}
#' }
#' @export
simulateBattles <- function(oolAttacker, oolDefender, FUN=play_LHTR_battle, ..., iterations=2000, replications=3){

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
  for (i in 1:replications){

    firstresult <- FUN(oolAttacker, oolDefender, ...)

    results[[i]] <- list()
    results[[i]]$IPClossAttacker <- c(firstresult$attackerLoss)
    results[[i]]$IPClossDefender <- c(firstresult$defenderLoss)
    results[[i]]$unitsLeftAttacker <- firstresult$unitsAttacker
    results[[i]]$unitsLeftDefender <- firstresult$unitsDefender

    for (j in 2:iterations){
      lastresult <- FUN(oolAttacker, oolDefender, ...)
      results[[i]]$IPClossAttacker <- c(results[[i]]$IPClossAttacker, lastresult$attackerLoss)
      results[[i]]$IPClossDefender <- c(results[[i]]$IPClossDefender, lastresult$defenderLoss)
      results[[i]]$unitsLeftAttacker <- rbind(results[[i]]$unitsLeftAttacker, lastresult$unitsAttacker)
      results[[i]]$unitsLeftDefender <- rbind(results[[i]]$unitsLeftDefender, lastresult$unitsDefender)
    }
  }

  return(results)
}

#' Roll dice
#' @description Rolls a set of six-sided dice, and determine number of hits.
#' @param ones number of dice that should return a hit for a roll of 1
#' @param twos number of dice that should return a hit for a roll of 2 or less
#' @param threes number of dice that should return a hit for a roll of 3 or less
#' @param fours number of dice that should return a hit for a roll of 4 or less
#' @param fives number of dice that should return a hit for a roll of 5 or less
#' @return numeric() number of hits
#' @export
roll <- function(ones=0, twos=0, threes=0, fours=0, fives=0){

  hits <- 0
  if (ones > 0){
    hits <- hits + sum(sample.int(6, ones, replace = T)==1)
  }
  if (twos > 0){
    hits <- hits + sum(sample.int(6, twos, replace = T)<=2)
  }
  if (threes > 0){
    hits <- hits + sum(sample.int(6, threes, replace = T)<=3)
  }
  if (fours > 0){
    hits <- hits + sum(sample.int(6, fours, replace = T)<=4)
  }
  if (fives > 0){
    hits <- hits + sum(sample.int(6, fives, replace = T)<=5)
  }

  return(hits)
}
