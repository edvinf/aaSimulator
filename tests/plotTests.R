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
