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
s <- simulateBattles(c("inf", "art", "arm"), c("inf", "inf", "inf"), iterations = 2000, replications = 3)
stats <- calculateStats(s)
checkAgainstTripleA(stats, .58, .06, .35, 2.78, -0.06, 2000, "inf art arm vs 3 inf")

#
# revisit this, there seems to be an error on TUV-cacluations for aa guns in triple A.
#
#s <- simulateBattles(rep("ftr", 4), c("AA"), iterations = 2000, replications = 3)
#stats <- calculateStats(s)
#checkAgainstTripleA(stats, 1, 0, .0, 1, -5, 2000, "4 ftr vs AA")

#s <- simulateBattles(c("inf", "art", "arm", "ftr"), c("inf", "inf", "inf", "AA"), iterations = 2000, replications = 3)
#stats <- calculateStats(s)
#checkAgainstTripleA(stats, .85, .03, .12, 2.36, .15, 2000, "inf art arm ftr vs 3 inf AA")


s <- simulateBattles(c("inf", "art", "arm", "ftr"), c("inf", "inf", "inf"), iterations = 2000, replications = 3)
stats <- calculateStats(s)
checkAgainstTripleA(stats, .90, .02, .07, 2.29, 2.06, 2000, "inf art arm ftr vs 3 inf")

s <- simulateBattles(c("ftr", "art", "arm", "inf"), c("inf", "inf", "inf"), iterations = 2000, replications = 3)
stats <- calculateStats(s)
checkAgainstTripleA(stats, .85, .01, .14, 2.51, -3.85, 2000, "ftr art arm inf vs 3 inf")
