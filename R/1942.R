#' Makes unit table for Axis and Allies 1942 2nd edition
#' @details
#'  a value for baseAttack and baseDefence indicate that the unit can be taken as a casuality.
#'  unit properties that need to be kept track of for battle calculation or reporting are represented as virtual units.
#'  actions for specifying retreat or submerges represented as control units.
#' @noRd
#' @keywords internal
makeUnitList1942se <- function(){

  infantry <- data.table::data.table(shortcut=as.character("inf"), name=as.character("Infantry"), cost=as.numeric(3), baseAttack=as.integer(1), baseDefence=as.integer(2), move=as.integer(1), type=as.character("Land"), virtualUnit=F)
  artillery <- data.table::data.table(shortcut=as.character("art"), name=as.character("Artillery"), cost=as.numeric(4), baseAttack=as.integer(2), baseDefence=as.integer(2), move=as.integer(1), type=as.character("Land"), virtualUnit=F)
  tank <- data.table::data.table(shortcut=as.character("arm"), name=as.character("Tank"), cost=as.numeric(6), baseAttack=as.integer(3), baseDefence=as.integer(3), move=as.integer(2), type=as.character("Land"), virtualUnit=F)
  antiaircraft <- data.table::data.table(shortcut=as.character("AA"), name=as.character("Anti-aircraft Gun"), cost=as.numeric(5), baseAttack=as.integer(NA), baseDefence=as.integer(0), move=as.integer(1), type=as.character("Land"), virtualUnit=F)

  unitTable <- rbind(infantry, artillery, tank, antiaircraft)

  fighter <- data.table::data.table(shortcut=as.character("ftr"), name=as.character("Fighter"), cost=as.numeric(10), baseAttack=as.integer(3), baseDefence=as.integer(4), move=as.integer(4), type=as.character("Air"), virtualUnit=F)
  bomber <- data.table::data.table(shortcut=as.character("bmb"), name=as.character("Bomber"), cost=as.numeric(12), baseAttack=as.integer(4), baseDefence=as.integer(1), move=as.integer(6), type=as.character("Air"), virtualUnit=F)

  unitTable <- rbind(unitTable, fighter, bomber)

  battleship <- data.table::data.table(shortcut=as.character("bb"), name=as.character("Battleship"), cost=as.numeric(20), baseAttack=as.integer(4), baseDefence=as.integer(4), move=as.integer(2),type=as.character("Sea"), virtualUnit=F)
  battleshipbombardment <- data.table::data.table(shortcut=as.character("BBomb"), name=as.character("Battleship offshore bombardment"), cost=as.numeric(NA), baseAttack=as.integer(NA), baseDefence=as.integer(NA), move=as.integer(2), type=as.character("Land"), virtualUnit=T)
  battleshipfirst <- data.table::data.table(shortcut=as.character("BBx"), name=as.character("Battleship first hit"), cost=as.numeric(NA), baseAttack=as.integer(0), baseDefence=as.integer(0), move=as.integer(NA), type=as.character("Sea"), virtualUnit=T)
  carrier <- data.table::data.table(shortcut=as.character("ac"), name=as.character("Aircraft Carrier"), cost=as.numeric(14), baseAttack=as.integer(1), baseDefence=as.integer(2), move=as.integer(2), type=as.character("Sea"), virtualUnit=F)
  cruiser <- data.table::data.table(shortcut=as.character("cc"), name=as.character("Cruicer"), cost=as.numeric(12), baseAttack=as.integer(3), baseDefence=as.integer(3), move=as.integer(2), type=as.character("Sea"), virtualUnit=F)
  cruiserbombardment <- data.table::data.table(shortcut=as.character("CBomb"), name=as.character("Cruiser offshore bombardment"), cost=as.numeric(NA), baseAttack=as.integer(NA), baseDefence=as.integer(NA), move=as.integer(2), type=as.character("Land"), virtualUnit=T)
  destroyer <- data.table::data.table(shortcut=as.character("dd"), name=as.character("Destroyer"), cost=as.numeric(8), baseAttack=as.integer(2), baseDefence=as.integer(2), move=as.integer(2), type=as.character("Sea"), virtualUnit=F)

  submarine <- data.table::data.table(shortcut=as.character("sub"), name=as.character("Submarine"), cost=as.numeric(6), baseAttack=as.integer(2), baseDefence=as.integer(1), move=as.integer(2), type=as.character("Sea"), virtualUnit=F)
  submerged <- data.table::data.table(shortcut=as.character("subm"), name=as.character("Submerged Submarine"), cost=as.numeric(8), baseAttack=as.integer(NA), baseDefence=as.integer(NA), move=as.integer(NA), type=as.character("Sea"), virtualUnit=T)
  transport <- data.table::data.table(shortcut=as.character("trn"), name=as.character("Transport"), cost=as.numeric(7),baseAttack=as.integer(0), baseDefence=as.integer(0), move=as.integer(2), type=as.character("Sea"), virtualUnit=F)

  unitTable <- rbind(unitTable, battleship, battleshipbombardment, battleshipfirst, destroyer, carrier, cruiser, cruiserbombardment, submarine, submerged, transport)

  retreat <- data.table::data.table(shortcut=as.character("RET"), name=as.character("Retreat action"), cost=as.numeric(NA), baseAttack=as.integer(NA), baseDefence=as.integer(NA), move=as.integer(NA),type=as.character("Control"), virtualUnit=T)
  submerge <- data.table::data.table(shortcut=as.character("SUBM"), name=as.character("Submerge action"), cost=as.numeric(NA), baseAttack=as.integer(NA), baseDefence=as.integer(NA), move=as.integer(NA),type=as.character("Control"), virtualUnit=T)

  unitTable <- rbind(unitTable, retreat, submerge)

  return(unitTable)
}

#' @noRd
#' @keywords internal
getProperty1942se <- function(unit, property){
  return(aaSimulator::unitlist1942se[aaSimulator::unitlist1942se$shortcut==unit,][[property]])
}

#' @noRd
#' @keywords internal
attack1942 <- function(units, unitlist=aaSimulator::unitlist1942se){
  hits <- 0

  inf <- units[units == "inf"]
  rest <- units[units != "inf"]

  nart <- sum(rest == "art")

  twos <- min(length(inf), nart)
  ones <- length(inf) - twos

  hits <- hits + roll(ones, 1)
  hits <- hits + roll(twos, 2)

  if (length(rest) == 0){
    return(hits)
  }
  for (i in 1:nrow(unitlist)){
    if (unitlist$shortcut[i] %in% rest & !is.na(unitlist$baseAttack[i])){
      attack <- unitlist$baseAttack[i]
      hits <- hits + roll(sum(unitlist$shortcut[i]==rest), attack)
    }
  }

  return(hits)
}

#' @noRd
#' @keywords internal
defend1942 <- function(units, unitlist=aaSimulator::unitlist1942se){
  hits <- 0

  rest <- units

  if (length(rest) == 0){
    return(0)
  }


  for (i in 1:nrow(unitlist)){
    if (unitlist$shortcut[i] %in% rest & !is.na(unitlist$baseDefence[i])){
      defence <- unitlist$baseDefence[i]
      hits <- hits + roll(sum(unitlist$shortcut[i]==units), defence)
    }
  }

  return(hits)
}



#' Play round
#' @description Play one round of battle follwing the rules for Axis and Allies 1942 second edition.
#' @details
#'
#' @param oolAttacker character() vector of units for the attacker order by loss preference, formatted as \code{\link[aaSimulator]{ool}}
#' @param oolDefender character() vector of units for the defender order by loss preference, formatted as \code{\link[aaSimulator]{ool}}
#' @param roundnr integer() number of the round of battle played, enumerating from 1.
#' @param submergeAttack logical() whether attacking submarines should submerge if possible
#' @param submergeDefend logical() whether defending submarines should submerge if possible
#' @param verbose logical() whether to print battle log to stdout
#' @param unitlist option for reducing units used in lookup
#' @return \code{\link[aaSimulator]{battleResults}}
#' @noRd
#' @keywords internal
play_1942se_battle_round <- function(oolAttacker, oolDefender, roundnr, submergeAttack=F, submergeDefend=F, verbose=F, unitlist=aaSimulator::unitlist1942se){

  result <- list()
  result$ret <- F
  result$submAttack <- submergeAttack
  result$submDefend <- submergeDefend
  result$attackerLoss <- c()
  result$defenderLoss <- c()
  result$unitsAttacker <- oolAttacker
  result$unitsDefender <- oolDefender

  baseAttackUnits <- unitlist$shortcut[!is.na(unitlist$baseAttack)]

  remove_casualties <- function(hits, side, targetunits=baseAttackUnits){

    if (side == "attacker"){
      mask <- pop(result$unitsAttacker, hits, removeables = targetunits, skip=c("SUBM", "RET"))
      result$attackerLoss <<- c(result$attackerLoss, result$unitsAttacker[!mask])
      result$unitsAttacker <<- result$unitsAttacker[mask]

      if ("RET" %in% result$attackerLoss){
        result$ret <<- T
      }
      if ("SUBM" %in% result$attackerLoss){
        result$submAttack <<- T
      }
    }
    else if (side == "defender"){
      mask <- pop(result$unitsDefender, hits, removeables = targetunits, skip=c("SUBM", "RET"))
      result$defenderLoss <<- c(result$defenderLoss, result$unitsDefender[!mask])
      result$unitsDefender <<- result$unitsDefender[mask]

      if ("SUBM" %in% result$defenderLoss){
        result$submDefend <<- T
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

    if ("AA" %in% oolDefender){
      aircount <- sum(oolAttacker == "ftr" | oolAttacker == "bmb")
      aacount <- sum(oolDefender == "AA")

      dice <- min(aircount, 3*aacount)

      if (verbose){
        write(paste("Firing aa guns,", dice, "dice."), stdout())
      }


      #
      # Anti aircraft gun
      #


      if (dice > 0){
        hits <- roll(dice, 1)
        remove_casualties(hits, "attacker", c("ftr", "bmb"))

        if (verbose & ftrhit > 0){
          write(paste("Hit", dice, "air units. Casualties removed."), stdout())

        }

      }

    }

    #
    # Offshore bombardment
    #

    bbs <- sum(oolAttacker == "BBomb")
    oolAttacker <- oolAttacker[oolAttacker != "BBomb"]
    offshorehits <- 0
    if (bbs > 0){

      if (verbose){
        write(paste("Firing Offshore bombardment (battleships),", bbs, "dice."), stdout())
      }

      bbombhit <- roll(bbs, 4)
      offshorehits <- offshorehits + bbombhit

      if (verbose & bbombhit > 0){
        write(paste("Hit: ", bbombhit, ".", sep=""), stdout())
      }
    }

    cds <- sum(oolAttacker == "CBomb")
    oolAttacker <- oolAttacker[oolAttacker != "CBomb"]

    if (cds > 0){

      if (verbose){
        write(paste("Firing Offshore bombardment (cruisers),", cds, "dice."), stdout())
      }

      cbombhit <- roll(cds, 3)

      offshorehits <- offshorehits + cbombhit

      if (verbose & cbombhit > 0){
        write(paste("Hit: ", cbombhit, ".", sep=""), stdout())
      }
    }

    #remove offshore bombardment from ool
    result$unitsAttacker <- result$unitsAttacker[!(result$unitsAttacker %in% c("BBomb", "CBomb"))]

  }


  # subs, submerge if possible (replace with submerged sub, do not add to IPC loss)
  stop("Submerge happens before firing")
  if (result$submAttack){
    if (!any("dd" %in% result$unitsDefender)){
      result$unitsAttacker[result$unitsAttacker == "sub"] <- "subm"
      result$unitsAttacker[result$unitsAttacker == "ssub"] <- "ssubm"
    }
  }
  if (result$submDefend){
    if (!any("dd" %in% result$unitsAttacker)){
      result$unitsDefender[result$unitsDefender == "sub"] <- "subm"
      result$unitsDefender[result$unitsDefender == "ssub"] <- "ssubm"
    }
  }


  #
  # remaining submarines attack, remove casualties if surprise attack conditions met

  #
  if (verbose & length(oolAttacker[oolAttacker == "sub" | oolAttacker == "ssub"])>0){
    write(paste("Attacking submarines fire:,", sum(oolAttacker == "sub" | oolAttacker == "ssub"), "dice."), stdout())
  }

  subattackhits <- attack(oolAttacker[oolAttacker == "sub" | oolAttacker == "ssub"], unitlist)
  if (verbose & length(oolAttacker[oolAttacker == "sub" | oolAttacker == "ssub"])>0){
    write(paste("Hit:,", subattackhits, "."))
  }
  if (!any("dd" %in% oolDefender)){
    remove_casualties(subattackhits, "defender", targetunits = unitlist$shortcut[unitlist$type == "Sea"])
    subattackhits <- 0
    if (subattackhits > 0 & verbose){
      write(paste("No defending destroyer present. Casualties removed"), stdout())
    }
  }

  if (verbose & length(oolDefender[oolDefender == "sub" | oolDefender == "ssub"])>0){
    write(paste("Defending submarines fire:,", length(oolDefender[oolDefender == "sub" | oolDefender == "ssub"]), "dice."), stdout())
  }
  subdefendhits <- defend(oolDefender[oolDefender == "sub" | oolDefender == "ssub"], unitlist)
  if (verbose & length(oolDefender[oolDefender == "sub" | oolDefender == "ssub"])>0){
    write(paste("Hit:,", subdefendhits, "."), stdout())
  }
  if (!any("dd" %in% oolAttacker)){
    remove_casualties(subdefendhits, "attacker", targetunits = unitlist$shortcut[unitlist$type == "Sea"])
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

  airhitsattacker <-  attack(result$unitsAttacker[result$unitsAttacker == "ftr" | result$unitsAttacker == "bmb"], unitlist)
  airhitsdefender <- attack(result$unitsDefender[result$unitsDefender == "ftr" | result$unitsDefender == "bmb"], unitlist)

  #roll rest attacker (art dep inf upgrade in attack())
  attackhits <- attack(result$unitsAttacker[!(result$unitsAttacker %in% c("sub", "ftr", "bmb"))], unitlist)

  #roll rest defender
  defendhits <- defend(result$unitsDefender[!(result$unitsDefender  %in% c("sub", "ftr", "bmb"))], unitlist)

  if (verbose){
    write(paste("Attacker hit:", attackhits), stdout())
    write(paste("Defender hit:", defendhits), stdout())
  }

  stop("Deal with priority between air hits and subhits (partition into air and sea hits?)")

  #remove air casualties
  if (!any("dd" %in% result$unitsDefender)){
    remove_casualties(airhitsattacker, "defender", targetunits = unitlist$shortcut[unitlist$shortcut != "sub"])
  }
  else{
    remove_casualties(airhitsattacker, "defender")
  }
  if (!any("dd" %in% result$unitsAttacker)){
    remove_casualties(airhitsdefender, "attacker", targetunits = unitlist$shortcut[unitlist$shortcut != "sub"])
  }else{
    remove_casualties(airhitsdefender, "attacker")
  }

  #remove ofshore bombardment casualties
  remove_casualties(offshorehits, "defender")

  #remove regular casualties
  remove_casualties(attackhits, "defender")
  remove_casualties(defendhits, "attacker")


  return(result)
}

#' @noRd
calculateCost <- function(ool, remaining, unitlist=aaSimulator::unitlist1942se){
  nonvirtual <- unitlist$shortcut[!unitlist$virtualUnit]
  ool <- ool[ool %in% nonvirtual]
  remaining <- remaining[remaining %in% nonvirtual]
  totalvalue <- sum(unitlist$cost[match(ool, unitlist$shortcut)])
  restvalue <- sum(unitlist$cost[match(remaining, unitlist$shortcut)])
  return(totalvalue - restvalue)
}

#' Play one battle
#' @description Plays one battle following the rules for Axis and Allies 1942 second edition
#' @details
#'  units for oolAttacker and oolDefender accepts all units specified in \code{\link[aaSimulator]{ool}}
#'
#'  In addition to the standard options, the attacker and defender OOL supports the directives:
#'  \describe{
#'   \item{SUBM}{Submarines submerge at first opportunity when all units preceeding this directive is lost.}
#'   }
#'  In addition to the standard options, the attacker OOL supports the directives:
#'  \describe{
#'   \item{RET}{Attacker will retreat when all units preceeding this directive is lost.}
#'  }
#'
#' @param oolAttacker character() order of loss for attacker
#' @param oolDefender character() order of loss for defender
#' @param retreat integer() round number after which attacker should retreat, if NULL, round number will not trigger retreat
#' @param verbose logical() whether to print battle information to stdout
#' @param suppressChecks suppressChecks on input
#' @return list() with members
#' \describe{
#'  \item{unitsAttacker}{remaining units for attacker, formatted as oolAttacker}
#'  \item{unitsDefender}{remaining units for defender, formatted as oolDefender}
#'  }
#' @examples
#'  play_1942se_battle(c("inf", "art", "ftr"), c("inf", "inf", "inf", "AA"))
#' @export
play_1942se_battle <- function(oolAttacker, oolDefender, retreat=NULL, verbose=F, suppressChecks=F){

  if (!suppressChecks){
    legalcodes <- aaSimulator::unitlist1942se$shortcut
    if (!all(oolAttacker %in% legalcodes)){
      illegalCodes <- unique(oolAttacker[!(oolAttacker %in% legalcodes)])
      stop(paste("Syntax error:", paste(illegalCodes, collapse=",")))
    }
    if (!all(oolDefender %in% legalcodes)){
      illegalCodes <- unique(oolDefender[!(oolDefender %in% legalcodes)])
      stop(paste("Syntax error:", paste(illegalCodes, collapse=",")))
    }
  }


  #reduced unit list for peformance reasons
  unitlist <- aaSimulator::unitlist1942se[aaSimulator::unitlist1942se$shortcut %in% c(oolAttacker, oolDefender),]
  nonvirtualunits <- unitlist$shortcut[!unitlist$virtualUnit]

  if (!suppressChecks){
    if (all(c("Sea", "Land") %in% unitlist[unitlist$shortcut %in% oolAttacker,][["type"]])){
      stop("Land and Sea units cannot be mixed in battle (except for offshore bombardment (BBomb) on land)")
    }
    if (all(c("Sea", "Land") %in% unitlist[unitlist$shortcut %in% oolDefender,][["type"]])){
      stop("Land and Sea units cannot be mixed in battle")
    }

    if ("BBomb" %in% oolDefender){
      stop("Defender OOL may not contain unit BBomb (offshore bombardment)")
    }

    if ("CBomb" %in% oolDefender){
      stop("Defender OOL may not contain unit CBomb (offshore bombardment)")
    }

    if ("RET" %in% oolDefender){
      stop("Defender OOL may not contain control directive RET (retreat)")
    }

    if ("AA" %in% oolAttacker){
      stop("Attacker OOL may not contain unit aa (Anti-aircraft Gun)")
    }

    if (sum(oolAttacker=="bb")!=sum(oolAttacker=="BBx")){
      stop("BB (Battleship) and BBx (Battleship extra hit) is not included the same number of times.")
    }

    stop("Check that all trn are specified last in ool for both attacker and defender")

  }

  round <- 1
  result <- list()
  result$unitsAttacker <- oolAttacker
  result$unitsDefender <- oolDefender
  result$ret <- F
  result$submAttack <- F
  result$submDefend <- F
  terminated <- F
  while (!terminated){


    if (!terminated){

      if (verbose){
        write(paste("------"), stdout())
        write(paste("Playing round:", round), stdout())
        write(paste("Attacker: ", paste(result$unitsAttacker, collapse = ",")), stdout())
        write(paste("Defender: ", paste(result$unitsDefender, collapse=",")), stdout())
      }

      result <- play_1942se_battle(result$unitsAttacker, result$unitsDefender, round, result$submAttack, result$submDefend, verbose = verbose, unitlist = unitlist)
      result$rounds <- round
      round <- round + 1
    }


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
    if (!terminated){
      terminated <- !(any(result$unitsAttacker %in% nonvirtualunits))
    }
    if (length(result$unitsDefender)==0){
      terminated <- T
    }
    if (!terminated){
      terminated <- !(any(result$unitsDefender %in% nonvirtualunits))
    }

  }

  if (verbose){
    write(paste("------"), stdout())
    write(paste("Remaining after round:", result$rounds), stdout())
    write(paste("Attacker: ", paste(result$unitsAttacker, collapse = ",")), stdout())
    write(paste("Defender: ", paste(result$unitsDefender, collapse=",")), stdout())
    write(paste("------"), stdout())
  }

  #resurface submerged subs for correct tallying.
  if (sum(result$unitsAttacker == "subm") > 0){
    result$unitsAttacker[result$unitsAttacker == "subm"] <- "sub"
    result$unitsAttacker[result$unitsAttacker == "ssubm"] <- "ssub"
  }
  if (sum(result$unitsDefender == "subm") > 0){
    result$unitsDefender[result$unitsDefender == "subm"] <- "sub"
    result$unitsDefender[result$unitsDefender == "ssubm"] <- "ssub"
  }

  if (length(result$unitsDefender)){
    result$cleared <- T
  } else{
    result$cleared <- F
  }

  result$defenderLoss <- NULL
  result$attackerLoss <- NULL
  result$attackerCost <- calculateCost(oolAttacker, result$unitsAttacker, unitlist)
  result$defenderCost <- calculateCost(oolDefender, result$unitsDefender, unitlist)
  result$ret <- NULL
  result$submAttack <- NULL
  result$submDefend <- NULL
  return(result)
}

