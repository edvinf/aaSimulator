library(aaSimulator)
checkAgainstTripleA <- function(stats, wins, draw, loose, rounds, TUVswing, triplaAiterations, descr){

  writeDiscr <- function(replicates, tripleA, name){
    if (tripleA > max(replicates) | tripleA < min(replicates)){
      write("Discrepency between tripleA and aaSimulator", stderr())
      write(paste(name, "[", min(replicates), ",", max(replicates), "]", ", triple A:", tripleA), stderr())
    }
  }

  write(paste("Checking result", descr, "against triple A", triplaAiterations, "iterations."), stdout())
  writeDiscr(stats$replicates$attackerWon, wins, "attacker won")
  writeDiscr(stats$replicates$defenderWon, loose, "defender won")
  writeDiscr(stats$replicates$draw, draw, "draw")
  writeDiscr(stats$replicates$meanRounds, rounds, "rounds")
  writeDiscr(stats$replicates$meanCostDifference, TUVswing, "mean cost difference")

}

#
# Test some examples against TripleA calculator
#
s <- simulateBattles(c("inf", "art", "arm"), c("inf", "inf", "inf"), iterations = 2000, replications = 5)
stats <- calculateStats(s)
checkAgainstTripleA(stats, .58, .06, .35, 2.78, -0.06, 2000, "inf art arm vs 3 inf")


s <- simulateBattles(c("inf", "art", "arm", "ftr"), c("inf", "inf", "inf"), iterations = 2000, replications = 5)
stats <- calculateStats(s)
checkAgainstTripleA(stats, .90, .02, .07, 2.29, 2.06, 2000, "inf art arm ftr vs 3 inf")

s <- simulateBattles(c("ftr", "art", "arm", "inf"), c("inf", "inf", "inf"), iterations = 2000, replications = 5)
stats <- calculateStats(s)
checkAgainstTripleA(stats, .85, .01, .14, 2.51, -3.85, 2000, "ftr art arm inf vs 3 inf")


#
# AA fun test
#

s <- simulateBattles(c("ftr"), c("inf", "AA"), iterations = 2000, replications = 5)
stats <- calculateStats(s)
checkAgainstTripleA(stats, .41, .20, .38, 1.42, -4.02, 2000, "inf art arm ftr vs 3 inf AA")

#
# Offshore bombardment
#
s <- simulateBattles(c("BBomb", "inf"), c("inf", "inf"), iterations = 2000, replications = 5)
stats <- calculateStats(s)
checkAgainstTripleA(stats, .17, .09, .74, 2.11, .58, 2000, "inf art arm ftr vs 3 inf")


#
# Battleship 2-hit test
#

s <- simulateBattles(c("BBx", "bb"), c("ac", "ftr"), iterations = 2000, replications = 5)
stats <- calculateStats(s)
checkAgainstTripleA(stats, .2, .2, .6, 1.84, -2.12, 2000, "BB vs ac ftr")

#
# sub-surpise test
#

s <- simulateBattles(c("BBx", "sub", "bb"), c("ac", "ftr"), iterations = 2000, replications = 5)
stats <- calculateStats(s)
checkAgainstTripleA(stats, .68, .12, .19, 2.09, 11.01, 2000, "BB vs ac ftr")

s <- simulateBattles(c("BBx", "sub", "bb"), c("ac", "dd"), iterations = 2000, replications = 5)
stats <- calculateStats(s)
checkAgainstTripleA(stats, .77, .10, .13, 2.06, 16.43, 2000, "BB vs ac dd")
