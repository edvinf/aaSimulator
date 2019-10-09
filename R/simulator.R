#game specific options added to OOL string ?

#' Simulate Axis and Allies Battle
#' @description
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
#'   \item{SUMB}{Flags submarines should submerge when possible.}
#'   \item{RET}{Flags that attacker should retreat at first opportunity when all units preceeding this virtual unit are lost.}
#'}
#' @parameter oolAttacker character() vector of unit codes in preferred order of loss
#' @parameter oolDefender character() vector of unit codes in preferred order of loss
#' @parameter FUN function for running one battle
#' @paramter ... additional arguments passed to FUN
#' @parameter iterations
#' @parameter replications
#' @return list with one member for each replication. Each replicate contains a member IPCloss (data.table) and a member unitsLeft (data.table), with a row for each iteration.
#' @export
simulateBattles <- function(oolAttacker, oolDdefender, FUN=play_LHTR_battle, ..., iterations=2000, replications=3){

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
    results[[i]] <- list()
    results[[i]]$IPCloss <- data.table(IPClossAttacker=character(iterations), IPClossDefender=character(iterations))
    stop("Figure out how to build data frame correctly")
    results[[i]]$unitsLeftAttacker <- data.table(matrix(ncol=length(oolAttacker), nrow=iterations))
    results[[i]]$unitsLeftDefender <- data.table(matrix(ncol=length(oolDefender), nrow=iterations))
    for (j in 1:iterations){

    }
  }

}
