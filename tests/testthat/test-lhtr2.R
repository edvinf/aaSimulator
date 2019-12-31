s<-play_LHTR_battle(c("inf"), c("inf"))
expect_false(is.null(s$unitsAttacker))
expect_false(is.null(s$unitsDefender))
expect_gte(s$lastround, 1)
