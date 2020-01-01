
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
#'   \item{SUBM}{Flags that submarines should submerge when possible, after all units preceeding this virtual unit are lost.}
#'   \item{RET}{Flags that attacker should retreat at first opportunity when all units preceeding this virtual unit are lost.}
#'}
#'
#' FUN must accept the arguments oolAttacker and oolDefender and return a list with five members:
#' \describe{
#'  \item{unitsAttacker}{the remaining units for attacker}
#'  \item{unitsDefender}{the remaining units for defender}
#'  \item{rounds}{the number of rounds in the battler}
#'  \item{attackerCost}{The cost of units lost for attacker}
#'  \item{defenderCost}{The cost of units lost for defender}
#' }
#'
#' @param oolAttacker character() vector of unit codes in preferred order of loss
#' @param oolDefender character() vector of unit codes in preferred order of loss
#' @param FUN function for running one battle
#' @param ... additional arguments passed to FUN
#' @param iterations number of iterations to simulate for each replication
#' @param replications number of replications to run
#' @return list with members:
#' \describe{
#'  \item{attackerStart}{units for attacker at start of battle, formatted as oolAttacker}
#'  \item{defenderStart}{units for defender at start of battle, formatted as oolDefender}
#'  \item{replicates}{list with member for each replicate, each a list with results from FUN for each iteration}
#' }
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
