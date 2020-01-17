context("play_LHTR_battle")
s<-play_LHTR_battle(c("inf"), c("inf"))
expect_false(is.null(s$unitsAttacker))
expect_false(is.null(s$unitsDefender))
expect_gte(s$rounds, 1)

s<-play_LHTR_battle(c("inf"), c("inf", "AA"))
expect_false(is.null(s$unitsAttacker))
expect_false(is.null(s$unitsDefender))
expect_gte(s$rounds, 1)
expect_gte(s$attackerCost, 0)
expect_gte(s$defenderCost, 0)
expect_gte(s$defenderCost + s$attackerCost, 2)

s<-play_LHTR_battle(c("BBomb", "inf"), c("inf", "AA"))
expect_false(is.null(s$unitsAttacker))
expect_false(is.null(s$unitsDefender))
expect_false("AA" %in% s$unitsDefender)
expect_gte(s$rounds, 1)

expect_error(play_LHTR_battle(c("BBomb", "inf"), c("BBomb", "inf", "AA")))
expect_error(play_LHTR_battle(c("bbomb", "inf"), c("inf", "AA")))

s<-play_LHTR_battle(c("DBomb", "inf"), c("inf", "AA"))
expect_false(is.null(s$unitsAttacker))
expect_false(is.null(s$unitsDefender))
expect_gte(s$rounds, 1)

expect_error(play_LHTR_battle(c("inf"), c("DBomb", "inf", "AA")))
expect_error(play_LHTR_battle(c("dbomb", "inf"), c("inf", "AA")))


#bugfix: virtual units was counted as lost in cost derermination
cost <- calculateCost(c("AA", "inf"), c("inf"))
expect_equal(cost, 0)

cost <- calculateCost(c("AA", "inf"), c())
expect_equal(cost, 3)

# RET flag
result <- play_LHTR_battle(expandPrefixedOOL("inf RET 100 inf"), expandPrefixedOOL("100 inf"))
expect_lte(result$rounds, 1)
expect_error(play_LHTR_battle(expandPrefixedOOL("100 inf"), expandPrefixedOOL("inf RET 100 inf")))

# SUBM flag
result <- play_LHTR_battle(expandPrefixedOOL("sub SUBM 99 sub"), expandPrefixedOOL("100 trn"))
expect_lte(result$rounds, 1)
result <- play_LHTR_battle(expandPrefixedOOL("100 sub"), expandPrefixedOOL("SUBM 100 sub"))
expect_lte(result$rounds, 1)

result <- play_LHTR_battle(expandPrefixedOOL("SUBM 10 sub"), expandPrefixedOOL("15 trn dd"))
expect_true(length(result$unitsDefender) == 0 | length(result$unitsAttacker) == 0 )

result <- play_LHTR_battle(expandPrefixedOOL("10 sub dd"), expandPrefixedOOL("SUBM 15 sub"))
expect_true(length(result$unitsDefender) == 0 | length(result$unitsAttacker) == 0 )

result <- play_LHTR_battle(expandPrefixedOOL("10 sub"), expandPrefixedOOL("SUBM 15 sub"))
expect_lte(result$rounds, 1)
