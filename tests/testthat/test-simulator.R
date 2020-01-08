#
# Test simulate battles
#

s <- simulateBattles(c("inf", "art"), c("inf"), iterations = 10, replications = 2)
expect_false(is.null(s$attackerStart))
expect_false(is.null(s$defenderStart))
expect_equal(length(s$replicates), 2)
expect_equal(length(s$replicates[[1]]), 10)
expect_equal(length(s$replicates[[2]]), 10)

s <- simulateBattles(c("inf"), c("inf"), iterations = 1, replications = 1)

expect_error(simulateBattles(c("inf"), c("inf"), iterations = 1, replications = 0), "Must run at least one replicate.")
expect_error(simulateBattles(c("inf"), c("inf"), iterations = 0, replications = 1), "Must run at least one iteratation.")

# corrected bug: premature termination when only aa guns are defending.
s <- simulateBattles(c("ftr"), c("AA"), iterations = 1, replications = 1)
expect_true(!is.na(s$replicates[[1]][[1]]$rounds))

# corrected bug: cost not calculated when virtual units are invovled
s <- simulateBattles(c("BBx", "sub", "sub", "bb"), c("trn", "trn", "ac", "ftr", "ftr"), iterations = 2, replications = 1)
expect_false(is.na(s$replicates[[1]][[1]]$attackerCost))

#
# Test stats calc
#
s <- simulateBattles(c("inf", "art", "arm"), c("inf", "inf", "inf"), iterations = 20, replications = 3)
stats <- calculateStats(s)
expect_false(is.null(stats$attackerStart))
expect_false(is.null(stats$defenderStart))
expect_false(is.null(stats$replicates))
expect_false(is.null(stats$averages))

#
# Test posterier distr. calc
#
s <- simulateBattles(c("inf", "art", "arm"), c("inf", "inf"), iterations = 20, replications = 3)

postAttack <- calculatePosteriorDistribution(s, "attacker")
expect_equal(nrow(postAttack), 3)
expect_equal(ncol(postAttack), 3)
expect_equal(names(postAttack), c("inf", "art", "arm"))

postDefend <- calculatePosteriorDistribution(s, "defender")
expect_equal(nrow(postDefend), 3)
expect_equal(ncol(postDefend), 2)
expect_equal(names(postDefend), c("inf", "inf"))

expect_error(calculatePosteriorDistribution(s, "otherside"))

#
# Test pop
#

popped <- pop(rep("ftr", 5), 4, c("ftr"))
expect_equal(sum(popped), 1)
expect_equal(rep("ftr", 5)[popped][1], "ftr")

popped <- pop(c("inf", rep("ftr", 5)), 4, c("ftr"))
expect_true(popped[1])
expect_true(popped[6])
expect_false(any(popped[2:5]))

popped <- pop(c("inf", "RET", rep("ftr", 5)), 4, c("ftr"), c("RET"))
expect_true(popped[1])
expect_true(popped[7])
expect_false(any(popped[2:6]))


#expand prefixed OOL
expanded <- expandPrefixedOOL("inf art")
expect_equal(length(expanded), 2)
expect_true("inf" %in% expanded)
expect_true("art" %in% expanded)

expanded <- expandPrefixedOOL("4 inf art")
expect_equal(length(expanded), 5)
expect_true("inf" %in% expanded)
expect_true("art" %in% expanded)

expanded <- expandPrefixedOOL("4 inf 2 art")
expect_equal(length(expanded), 6)
expect_true("inf" %in% expanded)
expect_true("art" %in% expanded)

expect_error(expandPrefixedOOL("4inf 2 art"))
expect_error(expandPrefixedOOL("4 4 inf 2 art"))


# optimizeUnits
res<-optimizeUnits(6, defender = "2 inf", units=c("inf", "art", "arm"), iterations=10, replications=1, verbose=F)
expect_false(is.null(res[[1]]$attackerStart))
expect_false(is.null(res[[1]]$defenderStart))
expect_false(is.null(res[[1]]$replicates))
expect_false(is.null(res[[1]]$averages))

res<-optimizeUnits(6, defender = "2 inf", units=c("inf", "art", "arm"), iterations=c(2,10), replications=c(1, 2), verbose=F)
expect_false(is.null(res[[1]]$attackerStart))
expect_false(is.null(res[[1]]$defenderStart))
expect_false(is.null(res[[1]]$replicates))
expect_false(is.null(res[[1]]$averages))

expect_error(optimizeUnits(6, defender = "2 inf", units=c("2 inf", "art", "arm"), iterations=c(2,10), replications=c(1, 2), verbose=F))
expect_error(optimizeUnits(6, defender = "2 inf", units=c("inf", "art", "arm"), iterations=c(2,10), replications=c(1, 2, 3), verbose=F))
expect_error(optimizeUnits(6, attacker= "2 inf", defender = "2 inf", units=c("inf", "art", "arm"), iterations=c(2,10), replications=c(1, 2), verbose=F))
expect_error(optimizeUnits(6, units=c("inf", "art", "arm"), iterations=c(2,10), replications=c(1, 2), verbose=F))
