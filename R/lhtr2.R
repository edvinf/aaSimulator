
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
  submerged <- data.table(shortcut=as.character("subm"), name=as.character("Submerged Submarine"), cost=as.numeric(8), baseAttack=as.integer(NA), baseDefence=as.integer(NA), move=as.integer(NA), type=as.character("Sea"), virtualUnit=T)
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

  inf <- units[units == "inf"]
  rest <- units[units != "inf"]
  nart <- sum(rest == "art")

  twos <- min(length(inf), nart)
  ones <- length(inf) - twos

  hits <- hits + roll(ones, 1)
  hits <- hits + roll(twos, 2)

  attacks <- lhtr2_units[match(rest, lhtr2_units$shortcut),"baseAttack"]

  if (length(attacks) == 0){
    return(hits)
  }

  attacks <- attacks[!is.na(attacks)]
  for (i in 1:6){
    hits <- hits + roll(sum(attacks==i), i)
  }
  return(hits)
}

#' @noRd
#' @keywords internal
defend <- function(units){
  hits <- 0
  defences <- lhtr2_units[match(units, lhtr2_units$shortcut),"baseDefence"]

  if (length(defences) == 0){
    return(0)
  }

  defences <- defences[!is.na(defences)]
  for (i in 1:6){
    hits <- hits + roll(sum(defences==i), i)
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
#' @param verbose logical() whether to print battle log to stdout
#' @return list() with members
#'  \item{attackerLoss}{integer() attacker IPC loss}
#'  \item{defenderLoss}{integer() defender IPC loss}
#'  \item{unitsAttacker}{character() vector with units for the attacker after round}
#'  \item{unitsDefender}{character() vector with units for the defender after round}
#'  \item{ret}{retreat directive encountered when resolving casualties}
#' @noRd
#' @keywords internal
play_LHTR_battle_round <- function(oolAttacker, oolDefender, roundnr, submergeAttack=F, submergeDefend=F, verbose=F){
  result <- list()
  result$ret <- F
  result$attackerLoss <- c()
  result$defenderLoss <- c()
  result$unitsAttacker <- oolAttacker
  result$unitsDefender <- oolDefender

  remove_casualties <- function(hits, side, targetunits=lhtr2_units[!is.na(lhtr2_units$baseAttack),][["shortcut"]]){

    if (side == "attacker"){
      mask <- pop(result$unitsAttacker, hits, removeables = targetunits, skip=c("SUBM", "RET"))
      result$attackerLoss <<- c(result$attackerLoss, result$unitsAttacker[!mask])
      result$unitsAttacker <<- result$unitsAttacker[mask]

      if ("RET" %in% result$attackerLoss){
        result$ret <<- T
      }
      if ("SUBM" %in% result$attackerLoss){
        stop("SUBM not implemented")
      }
    }
    else if (side == "defender"){
      mask <- pop(result$unitsDefender, hits, removeables = targetunits, skip=c("SUBM", "RET"))
      result$defenderLoss <<- c(result$defenderLoss, result$unitsDefender[!mask])
      result$unitsDefender <<- result$unitsDefender[mask]

      if ("SUBM" %in% result$defenderLoss){
        stop("SUBM not implemented")
      }

    }
    else{
      stop()
    }
  }

  #
  # opening fire
  #

  if (roundnr == 1){

    if ("aa" %in% oolDefender){

      #generalize to get units from table for all air
      aaftr <- sum(oolAttacker == "ftr")
      aabmb <- sum(oolAttacker == "bmb")

      if (verbose){
        write(paste("Firing aa gun,", aaftr + aabmb, "dice."), stdout())
      }


      #
      # Anti aircraft gun
      #

      if (aaftr > 0 | aabmb > 0){
        ftrhit <- roll(aaftr, 1)
        remove_casualties(ftrhit, "attacker", c("ftr"))

        if (verbose & ftrhit > 0){
          write(paste("Hit", ftrhit, "ftr. Casualties removed."), stdout())
        }

        bmbhit <- roll(aabmb, 1)
        remove_casualties(bmbhit, "attacker", c("bmb"))

        if (verbose & bmbhit > 0){
          write(paste("Hit", bmbhit, "bmb. Casualties removed"), stdout())
        }

      }
    }

    #
    # Offshore bombardment
    #
    bbs <- sum(oolAttacker == "BBomb")
    oolAttacker <- oolAttacker[oolAttacker != "BBomb"]

    if (bbs > 0){

      if (verbose){
        write(paste("Firing Offshore bombardment,", bbs, "dice."), stdout())
      }

      bbombhit <- roll(bbs, 4)
      remove_casualties(bbombhit, "defender")

      if (verbose & bbombhit > 0){
        write(paste("Hit: ", bbombhit, ". Casualties removed.", sep=""), stdout())
      }
    }

  }

  #
  # submarines attack, remove casualties if surprise attack conditions met
  #
  if (verbose & length(oolAttacker[oolAttacker == "sub"])>0){
    write(paste("Attacking submarines fire:,", length(oolAttacker[oolAttacker == "sub"]), "dice."), stdout())
  }
  subattackhits <- attack(oolAttacker[oolAttacker == "sub"])
  if (verbose & length(oolAttacker[oolAttacker == "sub"])>0){
    write(paste("Hit:,", subattackhits, "."))
  }
  if (!any("dd" %in% oolDefender)){
    remove_casualties(subattackhits, "defender", targetunits = lhtr2_units[lhtr2_units$type == "Sea"])
    subattackhits <- 0
    if (subattackhits > 0 & verbose){
      write(paste("No defending destroyer present. Casualties removed"), stdout())
    }
  }

  if (verbose & length(oolDefender[oolDefender == "sub"])>0){
    write(paste("Defending submarines fire:,", length(oolDefender[oolDefender == "sub"]), "dice."), stdout())
  }
  subdefendhits <- attack(oolDefender[oolDefender == "sub"])
  if (verbose & length(oolDefender[oolDefender == "sub"])>0){
    write(paste("Hit:,", subdefendhits, "."), stdout())
  }
  if (!any("dd" %in% oolAttacker)){
    remove_casualties(subdefendhits, "attacker", targetunits = lhtr2_units[lhtr2_units$type == "Sea"])
    subdefendhits <- 0
    if (subdefendhits > 0 & verbose){
      write(paste("No attacking destroyer present. Casualties removed"), stdout())
    }
  }


  #
  # / opening fire
  #

  if (verbose){
    write("Roll battle dice.", stdout())
  }

  #roll rest attacker (art dep inf upgrade in attack())
  attackhits <- attack(oolAttacker[oolAttacker != "sub"])

  #roll rest defender
  defendhits <- defend(oolDefender[oolDefender != "sub"])

  if (verbose){
    write(paste("Attacker hit:", attackhits), stdout())
    write(paste("Defender hit:", defendhits), stdout())
  }

  #remove regular casualties
  remove_casualties(attackhits, "defender")
  remove_casualties(defendhits, "attacker")

  #remove sub casualties if not already removed
  remove_casualties(subattackhits, "defender", targetunits = lhtr2_units[lhtr2_units$type == "Sea"])
  remove_casualties(subdefendhits, "attacker", targetunits = lhtr2_units[lhtr2_units$type == "Sea"])

  # subs, submerge after casualties are resolved, if possible (replace with submerged sub, do not add to IPC loss)
  if (submergeAttack){
    if (!any("dd" %in% result$unitsDefender)){
      oolAttacker[oolAttacker == "sub"] <- "subm"
    }
  }
  if (submergeDefend){
    if (!any("dd" %in% result$unitsAttacker)){
      oolDefender[oolDefender == "sub"] <- "subm"
    }
  }

  return(result)
}

#' @noRd
calculateCost <- function(ool, remaining){
  totalvalue <- sum(lhtr2_units[match(ool, lhtr2_units$shortcut),"cost"])
  restvalue <- sum(lhtr2_units[match(remaining, lhtr2_units$shortcut),"cost"])
  return(totalvalue - restvalue)
}

#' Play one battle
#' @description Plays one battle following Larry Harris Tournament Ruls for Axis and Allies Revised edition (LHTR 2.0)
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
#' @param verbose logical() whether to print battle information to stdout
#' @return list() with members
#' \describe{
#'  \item{unitsAttacker}{remaining units for attacker, formatted as oolAttacker}
#'  \item{unitsDefender}{remaining units for defender, formatted as oolDefender}
#'  }
#' @export
play_LHTR_battle <- function(oolAttacker, oolDefender, retreat=NULL, verbose=F){

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

  legalcodes <- lhtr2_units$shortcut
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

  if (sum(oolAttacker=="BBomb") > 0 & ("RET" %in% oolAttacker)){
    stop("RET directive is not supported in combination woth BBomb")
  }

  submergeAttack <- "SUBM" %in% oolAttacker
  submergeDefend <- "SUBM" %in% oolDefender

  round <- 0
  result <- list()
  result$unitsAttacker <- oolAttacker
  result$unitsDefender <- oolDefender
  result$ret <- F
  terminated <- F
  while (!terminated){

    # Battle termination criteria
    if (!is.null(retreat)){
      if (round>=retreat){
        terminated <- T
      }
    }
    if (result$ret){
      terminated <- T
    }
    if (length(result$unitsAttacker)==0){
      terminated <- T
    }
    if (all(is.na(lhtr2_units[lhtr2_units$shortcut %in% result$unitsAttacker,][["baseAttack"]]))){
      terminated <- T
    }
    if (length(result$unitsDefender)==0){
      terminated <- T
    }
    if (all(is.na(lhtr2_units[lhtr2_units$shortcut %in% result$unitsDefender,][["baseDefence"]]))){
      terminated <- T
    }

    if (!terminated){

      if (verbose){
        write(paste("------"), stdout())
        write(paste("Playing round:", round), stdout())
        write(paste("Attacker: ", paste(result$unitsAttacker, collapse = ",")), stdout())
        write(paste("Defender: ", paste(result$unitsDefender, collapse=",")), stdout())
      }

      round <- round +1
      result <- play_LHTR_battle_round(result$unitsAttacker, result$unitsDefender, round, submergeAttack, submergeDefend, verbose = verbose)
      result$rounds <- round
    }

  }

  if (verbose){
    write(paste("------"), stdout())
    write(paste("Remaining after round:", result$rounds), stdout())
    write(paste("Attacker: ", paste(result$unitsAttacker, collapse = ",")), stdout())
    write(paste("Defender: ", paste(result$unitsDefender, collapse=",")), stdout())
    write(paste("------"), stdout())
  }

  result$defenderLoss <- NULL
  result$attackerLoss <- NULL
  result$attackerCost <- calculateCost(oolAttacker, result$unitsAttacker)
  result$defenderCost <- calculateCost(oolDefender, result$unitsDefender)
  result$ret <- NULL
  return(result)
}

