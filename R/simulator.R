#game specific options added to OOL string ?

#' Simulate Axis and Allies Battle
#' @description
#' @details
#'  oolAttacker and oolDefender list all units in order of preferred loss.
#'  In addition to regular units, some virtual units are added for signaling special abilities.
#'  The exact list of units depends on the function provided (FUN), but the following convention is encouraged for common units:
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


}
