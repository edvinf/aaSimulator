
#' Makes unit table for LHTR 2.0
#' @details
#'  a value for baseAttack and baseDefence indicate that the unit can be taken as a casuality.
#'  unit properties that need to be kept track of for battle calculation or reporting are represented as virtual units.
#'  actions for specifying retreat or submerges are specified as represented as control units.
#' @noRd
#' @keywords internal
makeUnitList <- function(){
  require(data.table)
  infantry <- data.table(shortcut=as.character("inf"), name=as.character("Infantry"), cost=as.numeric(3), baseAttack=as.integer(1), baseDefence=as.integer(2), move=as.integer(1), type=as.character("Land"), virtualUnit=F)
  artillery <- data.table(shortcut=as.character("art"), name=as.character("Artillery"), cost=as.numeric(4), baseAttack=as.integer(2), baseDefence=as.integer(2), move=as.integer(1), type=as.character("Land"), virtualUnit=F)
  tank <- data.table(shortcut=as.character("arm"), name=as.character("Tank"), cost=as.numeric(5), baseAttack=as.integer(3), baseDefence=as.integer(3), move=as.integer(2), type=as.character("Land"), virtualUnit=F)
  antiaircraft <- data.table(shortcut=as.character("aa"), name=as.character("Anti-aircraft Gun"), cost=as.numeric(5), baseAttack=as.integer(NA), baseDefence=as.integer(NA), move=as.integer(1), type=as.character("Land"), virtualUnit=F)

  unitTable <- rbind(infantry, artillery, tank, antiaircraft)

  fighter <- data.table(shortcut=as.character("ftr"), name=as.character("Fighter"), cost=as.numeric(10), baseAttack=as.integer(3), baseDefence=as.integer(4), move=as.integer(4), type=as.character("Air"), virtualUnit=F)
  bomber <- data.table(shortcut=as.character("bmb"), name=as.character("Bomber"), cost=as.numeric(15), baseAttack=as.integer(4), baseDefence=as.integer(1), move=as.integer(6), type=as.character("Air"), virtualUnit=F)

  unitTable <- rbind(unitTable, fighter, bomber)

  battleship <- data.table(shortcut=as.character("bb"), name=as.character("Battleship"), cost=as.numeric(24), baseAttack=as.integer(4), baseDefence=as.integer(4), move=as.integer(2),type=as.character("Sea"), virtualUnit=F)
  battleshipbombardment <- data.table(shortcut=as.character("BBomb"), name=as.character("Battleship offshore bombardment"), cost=as.numeric(NA), baseAttack=as.integer(NA), baseDefence=as.integer(NA), move=as.integer(2), type=as.character("Land"), virtualUnit=T)
  battleshipfirst <- data.table(shortcut=as.character("BBx"), name=as.character("Battleship first hit"), cost=as.numeric(NA), baseAttack=as.integer(0), baseDefence=as.integer(0), move=as.integer(NA), type=as.character("Sea"), virtualUnit=T)
  destroyer <- data.table(shortcut=as.character("dd"), name=as.character("Destroyer"), cost=as.numeric(12), baseAttack=as.integer(3), baseDefence=as.integer(3), move=as.integer(2), type=as.character("Sea"), virtualUnit=F)
  carrier <- data.table(shortcut=as.character("ac"), name=as.character("Aircraft Carrier"), cost=as.numeric(16), baseAttack=as.integer(1), baseDefence=as.integer(3), move=as.integer(2), type=as.character("Sea"), virtualUnit=F)
  submarine <- data.table(shortcut=as.character("sub"), name=as.character("Submarine"), cost=as.numeric(8), baseAttack=as.integer(2), baseDefence=as.integer(2), move=as.integer(2), type=as.character("Sea"), virtualUnit=F)
  submerged <- data.table(shortcut=as.character("subm"), name=as.character("Submerged Submarine"), cost=as.numeric(NA), baseAttack=as.integer(NA), baseDefence=as.integer(NA), move=as.integer(NA), type=as.character("Sea"), virtualUnit=T)
  transport <- data.table(shortcut=as.character("trn"), name=as.character("Transport"), cost=as.numeric(8),baseAttack=as.integer(0), baseDefence=as.integer(1), move=as.integer(2), type=as.character("Sea"), virtualUnit=F)

  unitTable <- rbind(unitTable, battleship, battleshipbombardment, battleshipfirst, destroyer, carrier, submarine, submerged, transport)

  retreat <- data.table(shortcut=as.character("RET"), name=as.character("Retreat action"), cost=as.numeric(NA), baseAttack=as.integer(NA), baseDefence=as.integer(NA), move=as.integer(NA),type=as.character("Control"), virtualUnit=T)
  submerge <- data.table(shortcut=as.character("SUBM"), name=as.character("Submerge action"), cost=as.numeric(NA), baseAttack=as.integer(NA), baseDefence=as.integer(NA), move=as.integer(NA),type=as.character("Control"), virtualUnit=T)

  unitTable <- rbind(unitTable, retreat, submerge)

  return(unitTable)
}

#' @noRd
#' @keywords internal
getPropertyLHTR <- function(unit, property){
  return(lhtr2_units[lhtr2_units$shortcut==unit,][[property]])
}

#' @noRd
#' @keywords internal
getCostLHTR <- function(units){
  cost <- 0
  for (u in units){
    cost <- cost + getPropertyLHTR(u, "cost")
  }
  return(cost)
}

#' @noRd
#' @keywords internal
attack <- function(units){
  hits <- 0
  for (u in units){
    if (!is.na(lhtr2_units[lhtr2_units$shortcut == u,][["baseAttack"]])){
      hits <- hits + roll(1, lhtr2_units[lhtr2_units$shortcut == u,][["baseAttack"]])
    }
  }
  return(hits)
}

#' @noRd
#' @keywords internal
defend <- function(units){
  hits <- 0
  for (u in units){
    if (!is.na(lhtr2_units[lhtr2_units$shortcut == u,][["baseDefence"]])){
      hits <- hits + roll(1, lhtr2_units[lhtr2_units$shortcut == u,][["baseDefence"]])
    }
  }
  return(hits)
}

#' Play round
#' @description Play one round of battle follwing Larry Harris Tournament Rules (LHTR 2.0) for Axis and Allies Revised edition.
#' @details
#'  units for oolAttacker and oolDefender accepts all units specified in \code{link[aaSimulator]{simulateBattles}}
#' @param oolAttacker character() vector of units for the attacker order by loss preference
#' @param oolDefender character() vector of units for the defender order by loss preference
#' @param roundnr integer() number of the round of battle played, enumerating from 1.
#' @param submergeAttack logical() whether attacking submarines should submerge if possible
#' @param submergeDefend logical() whether defending submarines should submerge if possible
#' @return list() with members
#'  \item{attackerLoss}{integer() attacker IPC loss}
#'  \item{defenderLoss}{integer() defender IPC loss}
#'  \item{unitsAttacker}{character() vector with units for the attacker after round}
#'  \item{unitsDefender}{character() vector with units for the defender after round}
#'  \item{ret}{retreat directive encountered when resolving casualties}
#' @noRd
#' @keywords internal
play_LHTR_battle_round <- function(oolAttacker, oolDefender, roundnr, submergeAttack=F, submergeDefend=F){


  result <- list()
  result$ret <- F
  result$attackerLoss <- c()
  result$defenderLoss <- c()
  result$unitsAttacker <- c()
  result$unitsDefender <- c()

  remove_casualties <- function(ool, hits, side, type=NULL, targetunits=lhtr2_units){

    # move to pop ?
    if (side == "attacker"){
      mask <- pop(oolAttacker, hits, type, targets = targetunits[!is.na(targetunits$baseAttack),][["shortcut"]])
      result$attackerLoss <<- c(result$attackerLoss, oolAttacker[!mask])
      oolAttacker <- oolAttacker[mask]

      if ("RET" %in% result$attackerLoss){
        result$ret <<- T
        mask <- pop(oolAttacker, 1, type, targets = targetunits[!is.na(targetunits$baseAttack),][["shortcut"]])
        result$attackerLoss <<- c(result$attackerLoss, oolAttacker[!mask])
        oolAttacker <<- oolAttacker[mask]
      }
    }
    else if (side == "defender"){
      mask <- pop(oolDefender, hits, type, targets = targetunits[!is.na(targetunits$baseDefence),][["shortcut"]])
      result$defenderLoss <<- c(result$defenderLoss, oolDefender[!mask])
      oolDefender <- oolDefender[mask]
    }
    else{
      stop()
    }
  }

  if (roundnr == 1){

    bbs <- sum(oolAttacker == "BBomb")
    oolAttacker <- oolAttacker[oolAttacker != "BBomb"]

    if ("aa" %in% oolDefender){
      #generalize to get units from table for all air
      aaftr <- sum(oolAttacker == "ftr")
      aabmb <- sum(oolAttacker == "bmb")
    }

    #
    # Anti aircraft gun
    #

    if (aaftr > 0 | aabmb > 0){
      ftrhit <- roll(aaftr, 1)
      remove_casualties(oolAttacker, ftrhit, "attacker", "ftr")

      bmbhit <- roll(aabmb, 1)
      remove_casualties(oolAttacker, bmbhit, "attacker", "bmb")
    }

    #
    # Offshore bombardment
    #

    if (bbs > 0){
      bbombhit <- roll(bbs, 4)
      remove_casualties(oolDefender, bbombhit, "defender")
    }

  }

  #
  # submarines attack, remove casualties if surprise attack conditions met
  #
  subattackhits <- attack(oolAttacker[oolAttacker == "sub"])
  if (!any("dd" %in% oolDefender)){
    remove_casualties(oolDefender, subattackhits, "defender", targetunits = lhtr2_units[lhtr2_units$type == "Sea"])
    subattackhits <- 0
  }
  subdefendhits <- attack(oolDefender[oolDefender == "sub"])
  if (!any("dd" %in% oolAttacker)){
    remove_casualties(oolAttacker, subdefendhits, "attacker", targetunits = lhtr2_units[lhtr2_units$type == "Sea"])
    subdefendhits <- 0
  }

  #roll rest attacker
  # artillery (upgrade inf)
  attackhits <- attack(oolAttacker[oolAttacker != "sub"])

  #roll rest defender
  defendhits <- defend(oolDefender[oolDefender != "sub"])


  #remove regular casualties
  remove_casualties(oolDefender, attackhits, "defender")
  remove_casualties(oolAttacker, defendhits, "attacker")

  #remove sub casualties
  remove_casualties(oolDefender, subattackhits, "defender", targetunits = lhtr2_units[lhtr2_units$type == "Sea"])
  remove_casualties(oolAttacker, subdefendhits, "attacker", targetunits = lhtr2_units[lhtr2_units$type == "Sea"])

  # subs, submerge (replace with submerged sub, do not add to IPC loss)
  if (submergeAttack){
    oolAttacker[oolAttacker == "sub"] <- "subm"
  }
  if (submergeDefend){
    oolDefender[oolDefender == "sub"] <- "subm"
  }

  result$unitsAttacker <- oolAttacker
  result$unitsDefender <- oolDefender

  return(result)
}


#' Play one battle
#' @description Plays one round of battle following Larry Harris Tournament Ruls for Axis and Allies Revised edition (LHTR 2.0)
#' @details
#'  In addition to the standard options, the attacker and defender OOL supports the directives:
#'  \describe{
#'   \item{+sumberge}{Submarines submerge at first opportunity (after any round of combat with no enemy destroyer present)}
#'   }
#'  In addition to the standard options, the attacker OOL supports the directives:
#'  \describe{
#'   \item{+roundretreat <n>}{Attacker will retreat after round n.}
#'   \item{+ret}{Attacker will retreat when all units preceeding this directive is lost.}
#'  }
#' @param oolAttacker character() order of loss for attacker
#' @param oolDefender character() order of loss for defender
#' @param retreat integer() round number after which attacker should retreat, if NULL, round number will not trigger retreat
#' @return list() with members
#' \describe{
#'  \item{attackerLoss}{integer() attacker IPC loss}
#'  \item{defenderLoss}{integer() defender IPC loss}
#'  \item{unitsAttacker}{data.table with columns for units for the attacker after battle}
#'  \item{unitsDefender}{data.table with columns for units for the defender after battle}
#'  }
#' @export
play_LHTR_battle <- function(oolAttacker, oolDefender, retreat=NULL){

  if (all(c("Sea", "Land") %in% lhtr2_units[lhtr2_units$shortcut %in% oolAttacker,][["type"]])){
    stop("Land and Sea units cannot be mixed in battle (except for offshore bombardment (BBomb) on land)")
  }
  if (all(c("Sea", "Land") %in% lhtr2_units[lhtr2_units$shortcut %in% oolDefender,][["type"]])){
    stop("Land and Sea units cannot be mixed in battle")
  }

  if ("BBomb" %in% oolDefender){
    stop("Defender OOL may not contain unit BBomb (offshore bombardment)")
  }

  if ("aa" %in% oolAttacker){
    stop("Attacker OOL may not contain unit aa (Anti-aircraft Gun)")
  }

  legalcodes <- c(lhtr2_units$shortcut, "RET", "SUBM")
  if (!all(oolAttacker %in% legalcodes)){
    illegalCodes <- oolAttacker[!(oolAttacker %in% legalcodes)]
    stop(paste("Syntax error:", illegalCodes))
  }
  if (!all(oolDefender %in% legalcodes)){
    illegalCodes <- oolDefender[!(oolDefender %in% legalcodes)]
    stop(paste("Syntax error:", illegalCodes))
  }
  if (sum(oolAttacker=="bb")!=sum(oolAttacker=="BBx")){
    stop("BB (Battleship) and BBx (Battleship extra hit) is not included the same number of times.")
  }
  if (sum(oolDefender=="bb")!=sum(oolDefender=="BBx")){
    stop("BB (Battleship) and BBx (Battleship extra hit) is not included the same number of times.")
  }

  # check that RET is not included in illegal combination with Bbomb

  submergeAttack <- "SUBM" %in% oolAttacker
  submergeDefend <- "SUBM" %in% oolDefender

  stop("Fix putting results in data.table")

  round <- 1
  lastresult <- play_LHTR_battle_round(oolAttacker, oolDefender, round, submergeAttack, submergeDefend)
  result <- lastresult
  while (T){

    # Battle termination criteria
    if (!is.null(retreat) & round>=retreat){
      return(result)
    }
    if (result$ret){
      return(result)
    }
    if (length(result$unitsAttacker)==0){
      return(result)
    }
    if (all(is.na(lhtr2_units[lhtr2_units$shortcut %in% result$unitsAttacker,][["baseAttack"]]))){
      return(result)
    }
    if (length(result$unitsDefender)==0){
      return(result)
    }
    if (all(is.na(lhtr2_units[lhtr2_units$shortcut %in% result$unitsAttacker,][["baseDefence"]]))){
      return(result)
    }


    round <- round +1
    lastresult <- play_LHTR_battle_round(oolAttacker, oolDefender, round, submergeAttack, submergeDefend)

    result$attackerLoss <- result$attackerLoss + lastresult$attackerLoss
    result$defenderLoss <- result$defenderLoss + lastresult$defenderLoss
    result$unitsAttacker <- lastresult$unitsAttacker
    result$unitsDefender <- lastresult$unitsDefender
  }
}

