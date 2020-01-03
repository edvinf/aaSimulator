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
expect_gte(s$rounds, 1)

expect_error(play_LHTR_battle(c("BBomb", "inf"), c("BBomb", "inf", "AA")))
expect_error(play_LHTR_battle(c("bbomb", "inf"), c("inf", "AA")))

#bugfix: virtual units was counted as lost in cost derermination
cost <- calculateCost(c("AA", "inf"), c("inf"))
expect_equal(cost, 0)

cost <- calculateCost(c("AA", "inf"), c())
expect_equal(cost, 3)
