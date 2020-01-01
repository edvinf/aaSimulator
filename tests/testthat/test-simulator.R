s <- simulateBattles(c("inf", "art"), c("inf"), iterations = 10, replications = 2)
expect_false(is.null(s$attackerStart))
expect_false(is.null(s$defenderStart))
expect_equal(length(s$replicates), 2)
expect_equal(length(s$replicates[[1]]), 10)
expect_equal(length(s$replicates[[2]]), 10)

s <- simulateBattles(c("inf"), c("inf"), iterations = 1, replications = 1)

expect_error(simulateBattles(c("inf"), c("inf"), iterations = 1, replications = 0), "Must run at least one replicate.")
expect_error(simulateBattles(c("inf"), c("inf"), iterations = 0, replications = 1), "Must run at least one iteratation.")


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
