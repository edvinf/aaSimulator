s <- simulateBattles(c("inf", "art"), c("inf"), iterations = 10, replications = 2)
expect_false(is.null(s$attackerStart))
expect_false(is.null(s$defenderStart))
expect_equal(length(s$replicates), 2)
expect_equal(length(s$replicates[[1]]), 10)
expect_equal(length(s$replicates[[2]]), 10)

s <- simulateBattles(c("inf"), c("inf"), iterations = 1, replications = 1)

expect_error(simulateBattles(c("inf"), c("inf"), iterations = 1, replications = 0), "Must run at least one replicate.")
expect_error(simulateBattles(c("inf"), c("inf"), iterations = 0, replications = 1), "Must run at least one iteratation.")
