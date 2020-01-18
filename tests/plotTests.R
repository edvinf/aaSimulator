library(aaSimulator)
s <- simulateBattles(c("inf", "art", "arm"), c("inf", "inf", "inf"), iterations = 20, replications = 3)
pl <- makePosteriorDistributionPlot(s, "attacker")
plot(pl)

s <- simulateBattles(c("inf", "art", "arm"), c("inf", "inf", "inf"), iterations = 200, replications = 3)
pl <- makePosteriorDistributionPlot(s, "attacker")
plot(pl)


s <- simulateBattles(c(rep("inf", 30), rep("art",10), rep("arm", 10), rep("ftr", 3)), c(rep("inf", 40), rep("arm", 4), rep("ftr",5), "AA"), iterations = 200, replications = 3)
pl <- makePosteriorDistributionPlot(s, "attacker")
plot(pl)
pl <- makePosteriorDistributionPlot(s, "defender")
plot(pl)

sim <- simulateBattles("30 inf 11 arm 5 ftr arm", "40 inf 3 ftr AA", iterations = 1000, replications = 2)
plotBattleSummary(sim)

sim <- simulateBattles("30 inf 11 arm 5 ftr arm", "40 inf 3 ftr AA", iterations = 100, replications = 5)
plotBattleSummary(sim)

sim <- simulateBattles("30 inf RET 11 arm 5 ftr arm", "40 inf 3 ftr AA", iterations = 100, replications = 5)
plotBattleSummary(sim)

sim <- simulateBattles("SUBM 7 sub", "2 trn ac 2 ftr", iterations = 100, replications = 5)
plotBattleSummary(sim)


# Perform attack with usual simulation
sim <- simulateBattles("7 inf 3 ftr", "7 inf 2 ftr", iterations = 1000, replications = 5)
plotBattleSummary(sim)

#repeat with empty first wave and compare results
sim <- simulateTwoWaveBattles("RET", "7 inf 3 ftr", "7 inf", reinforcement = function(x){c(x, rep("ftr", 2))}, replications = 5, iterations = 1000)
plot2WaveSummary(sim)

#check effect of adding inital wave
sim <- simulateTwoWaveBattles("7 inf", "7 inf 3 ftr", "7 inf", reinforcement = function(x){c(x, rep("ftr", 2))}, replications = 5, iterations = 1000)
plot2WaveSummary(sim)


#
# Revisit when virtual units are resolved (AA guns)
#

# Perform attack with usual simulation
#sim <- simulateBattles("7 inf 3 ftr", "5 inf AA 2 ftr", iterations = 1000, replications = 5)
#plotBattleSummary(sim)

#repeat with empty first wave and compare results
#sim <- simulateTwoWaveBattles("RET", "7 inf 3 ftr", "5 inf AA", reinforcement = function(x){c(x, rep("ftr", 2))}, replications = 5, iterations = 1000)
#plot2WaveSummary(sim)

#check effect of adding inital wave
#sim <- simulateTwoWaveBattles("5 inf", "7 inf 3 ftr", "5 inf AA", reinforcement = function(x){c(x, rep("ftr", 2))}, replications = 5, iterations = 100)
#plot2WaveSummary(sim)

