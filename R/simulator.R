
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
#' @param n number of dice to roll
#' @param hitvalue largest value to count as a hit
#' @return numeric() number of hits
#' @export
roll <- function(n, hitvalue){

  hits <- sum(sample.int(6, n, replace = T) <= hitvalue)

  return(hits)
}

#' Determines which units should be popped (removed) units of ool
#' @param n number of units to pop
#' @param type if not null, first n units of given type is removed in stead
#' @param targets list of valid targets for hits
#' @return logical() vector with TRUE for units to keep
#' @noRd
#' @keywords internal
pop <- function(ool, n=1, type=NULL, targets=c()){

  if (n == 0){
    return(rep(T, length(ool)))
  }

  if (is.null(type)){
    if (n >= length(ool[ool %in% targets])){
      return(ool %in% targets)
    }

    keep <- rep(T, length(ool))
    assigned <- 0
    for (i in 1:length(keep)){
      if (ool[i] %in% targets & assigned < n){
        keep[i] <- F
        assigned <- assigned + 1
      }
    }
    return(keep)
  }

  if (!(type %in% targets)){
    stop("Error: Removing type not in targets")
  }

  if (type %in% ool){
    lostindex <- which(ool==type)
    lostindex <- lostindex[1:max(n,length(lostindex))]
    keptindex <- 1:length(ool)
    return(keptindex[!(keptindex %in% lostindex)])
  }

}
