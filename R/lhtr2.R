
#' Makes unit table for LHTR 2.0
#' @noRd
#' @keywords internal
makeUnitList <- function(){
  require(data.table)
  infantry <- data.table(shortcut=as.character("inf"), name=as.character("Infantry"), cost=as.numeric(3), costOfLoss=as.numeric(3), baseAttack=as.integer(1), baseDefence=as.integer(2), move=as.integer(1), type=as.character("Land"), virtualUnit=F)
  artillery <- data.table(shortcut=as.character("art"), name=as.character("Artillery"), cost=as.numeric(4), costOfLoss=as.numeric(4), baseAttack=as.integer(2), baseDefence=as.integer(2), move=as.integer(1), type=as.character("Land"), virtualUnit=F)
  tank <- data.table(shortcut=as.character("arm"), name=as.character("Tank"), cost=as.numeric(5), costOfLoss=as.numeric(5), baseAttack=as.integer(3), baseDefence=as.integer(3), move=as.integer(2), type=as.character("Land"), virtualUnit=F)
  antiaircraft <- data.table(shortcut=as.character("aa"), name=as.character("Anti-aircraft Gun"), cost=as.numeric(5), costOfLoss=as.numeric(5), baseAttack=as.integer(NA), baseDefence=as.integer(NA), move=as.integer(1), type=as.character("Land"), virtualUnit=F)

  unitTable <- rbind(infantry, artillery, tank, antiaircraft)

  fighter <- data.table(shortcut=as.character("ftr"), name=as.character("Fighter"), cost=as.numeric(10), costOfLoss=as.numeric(10), baseAttack=as.integer(3), baseDefence=as.integer(4), move=as.integer(4), type=as.character("Air"), virtualUnit=F)
  bomber <- data.table(shortcut=as.character("bmb"), name=as.character("Bomber"), cost=as.numeric(10), costOfLoss=as.numeric(15), baseAttack=as.integer(4), baseDefence=as.integer(1), move=as.integer(6), type=as.character("Air"), virtualUnit=F)

  unitTable <- rbind(unitTable, fighter, bomber)

  battleship <- data.table(shortcut=as.character("bb"), name=as.character("Battleship"), cost=as.numeric(24), costOfLoss=as.numeric(24), baseAttack=as.integer(4), baseDefence=as.integer(4), move=as.integer(2),type=as.character("Sea"), virtualUnit=F)
  battleshipbombardment <- data.table(shortcut=as.character("BBomb"), name=as.character("Battleship offshore bombardment"), cost=as.numeric(NA), costOfLoss=as.numeric(NA), baseAttack=as.integer(4), baseDefence=as.integer(NA), move=as.integer(2), type=as.character("Land"), virtualUnit=T)
  battleshipfirst <- data.table(shortcut=as.character("BBx"), name=as.character("Battleship first hit"), cost=as.numeric(NA), costOfLoss=as.numeric(0), baseAttack=as.integer(0), baseDefence=as.integer(0), move=as.integer(2), type=as.character("Sea"), virtualUnit=T)
  destroyer <- data.table(shortcut=as.character("dd"), name=as.character("Destroyer"), cost=as.numeric(12), costOfLoss=as.numeric(12), baseAttack=as.integer(3), baseDefence=as.integer(3), move=as.integer(2), type=as.character("Sea"), virtualUnit=F)
  carrier <- data.table(shortcut=as.character("ac"), name=as.character("Aircraft Carrier"), cost=as.numeric(16), costOfLoss=as.numeric(16), baseAttack=as.integer(1), baseDefence=as.integer(3), move=as.integer(2), type=as.character("Sea"), virtualUnit=F)
  submarine <- data.table(shortcut=as.character("sub"), name=as.character("Submarine"), cost=as.numeric(8), costOfLoss=as.numeric(8), baseAttack=as.integer(2), baseDefence=as.integer(2), move=as.integer(2), type=as.character("Sea"), virtualUnit=F)
  transport <- data.table(shortcut=as.character("trn"), name=as.character("Transport"), cost=as.numeric(8), costOfLoss=as.numeric(8), baseAttack=as.integer(0), baseDefence=as.integer(1), move=as.integer(2), type=as.character("Sea"), virtualUnit=F)

  unitTable <- rbind(unitTable, battleship, battleshipbombardment, battleshipfirst, destroyer, carrier, submarine, transport)

  return(unitTable)
}

#' Play one round of battle
#' @param roundnr integer() number of the round of battle played, enumerating from 1.
#' @param submerge logical() whether submarines should submerge if possible
#' @return list() with members
#'  \item{attackerLoss}{integer() attacker IPC loss}
#'  \item{defenderLoss}{integer() defender IPC loss}
#'  \item{unitsAttacker}{data.table with units for the attacker after round}
#'  \item{unitsDefender}{data.table with units for the defender after round}
#'  \item{ret}{retreat directive encountered when resolving casualties}
#' @noRd
#' @keywords internal
play_LHTR_battle_round <- function(unitsAttacker, unitsDefender, oolAttacker, oolDefender, roundnr, submerge=F){

  if (all(c("Sea", "Land") %in% c(unitsAttacker$type, unitsDefender$type))){
    stop("Land and Sea units cannot be mixed in battle (except for offshore bombardment (BBomb) on land)")
  }

  if (roundnr == 1){
    # aa guns (roll and remove)

    # naval bombardment (roll and remove)
  }


  # subs
  # determine surprise attack (DD present: roll, DD not present roll and remove)

  # artillery (upgrade inf)

  #roll attacker

  #roll defender

  #remove


  # subs, submerge (remove, but do not add to IPC loss), make sure air units are not casualties
}


#' Play one battle
#' @description Plays one round of battle following Larry Harris Tournament Ruls for Axis and Allies Revised edition (LHTR 2.0)
#' @details
#'  In addition to the standard options, the attacker and defender OOL supports the directives:
#'  \item{+sumberge}{Submarines submerge at first opportunity (after any round of combat with no enemy destroyer present)}
#'  In addition to the standard options, the attacker OOL supports the directives:
#'  \item{+roundretreat <n>}{Attacker will retreat after round n.}
#'  \item{+ret}{Attacker will retreat when all units preceeding this directive is lost.}
#' @param oolAttacker character() order of loss for attacker
#' @param oolDefender character() order of loss for defender
#' @param submerge logical() whether submarines should submerge if possible
#' @param retreat integer() round number after which attacker should retreat, if NULL, round number will not trigger retreat
#' @return list() with members
#'  \item{attackerLoss}{integer() attacker IPC loss}
#'  \item{defenderLoss}{integer() defender IPC loss}
#'  \item{unitsAttacker}{data.table with units for the attacker after round}
#'  \item{unitsDefender}{data.table with units for the defender after round}
#' @export
play_LHTR_battle <- function(oolAttacker, oolDefender, submerge=F, retreat=NULL){

}

