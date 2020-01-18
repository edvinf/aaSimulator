library(aaSimulator)
#
# Real example from game with AKH
# After Ger - 9
#

# British attack
# win prob: 0.002
# mean take: 16 inf
sim <- simulateBattles("17 inf 3 art 5 arm ftr bmb", "26 inf art 2 arm 4 ftr AA", iterations = 1000, replications = 5)
plotBattleSummary(sim)

# American attack (Germany may be re-inforced with one Jap bmb)
# win prob: 0.06
sim <- simulateBattles("BBomb 20 inf art 3 arm 6 ftr bmb arm", "bmb 26 inf art 2 arm 4 ftr AA", iterations = 1000, replications = 5)
plotBattleSummary(sim)

# Rule of thumb estimation
# American attack deducting median take from British (Germany may be re-inforced with one Jap bmb)
# win prob: 1.0
sim <- simulateBattles("BBomb 20 inf art 3 arm 6 ftr bmb arm", "bmb 10 inf art 2 arm 4 ftr AA", iterations = 1000, replications = 5)
plotBattleSummary(sim)

# Two-wave attack
# win prob: .97
sim <- simulateTwoWaveBattles("17 inf 3 art 5 arm ftr bmb", "BBomb 20 inf art 3 arm 6 ftr bmb arm", "26 inf art 2 arm 4 ftr AA", reinforcement = function(x){c("bmb", x, "AA")}, replications = 5, iterations = 1000)
plot2WaveSummary(sim)


#
# Hypothetical example, to explore when rule of thumb is less clear
#

# British attack
# mean take: 14 inf
sim <- simulateBattles("17 inf 3 art 5 arm ftr bmb", "30 inf art 2 arm 4 ftr AA", iterations = 2000, replications = 5)
plotBattleSummary(sim)

# Rule of thumb estimation
# American attack deducting median take from British (Germany may be re-inforced with one Jap bmb)
# win prob: .89
sim <- simulateBattles("BBomb 20 inf art 3 arm 6 ftr bmb arm", "bmb 16 inf art 2 arm 4 ftr AA", iterations = 2000, replications = 5)
plotBattleSummary(sim)

# Two-wave attack
# win prob: .82
sim <- simulateTwoWaveBattles("17 inf 3 art 5 arm ftr bmb", "BBomb 20 inf art 3 arm 6 ftr bmb arm", "30 inf art 2 arm 4 ftr AA", reinforcement = function(x){c("bmb", x, "AA")}, replications = 5, iterations = 2000)
plot2WaveSummary(sim)

