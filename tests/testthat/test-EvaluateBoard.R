# https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf

# library(CreateTTTTree)
# source("tic-tac-tow-dataSet.R")

context("EvaluateBoard")

test_that("EvaluateBoard works correctly.", {
  game <- rep(0, 9) # Empty board

  expect_equal(EvaluateBoard(game), 0)

  #across top
  game[1] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[2] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[3] <- 1
  expect_equal(EvaluateBoard(game), 1)
  game[1] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[2] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[3] <- -1
  expect_equal(EvaluateBoard(game),-1)

  #across middle
  game <- rep(0, 9) # Empty board
  game[4] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[5] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[6] <- 1
  expect_equal(EvaluateBoard(game), 1)
  game[4] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[5] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[6] <- -1
  expect_equal(EvaluateBoard(game),-1)

  #across bottom
  game <- rep(0, 9) # Empty board
  game[7] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[8] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[9] <- 1
  expect_equal(EvaluateBoard(game), 1)
  game[7] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[8] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[9] <- -1
  expect_equal(EvaluateBoard(game),-1)

  #down left
  game <- rep(0, 9) # Empty board
  game[1] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[4] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[7] <- 1
  expect_equal(EvaluateBoard(game), 1)
  game[1] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[4] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[7] <- -1
  expect_equal(EvaluateBoard(game),-1)

  #down middle
  game <- rep(0, 9) # Empty board
  game[2] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[5] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[8] <- 1
  expect_equal(EvaluateBoard(game), 1)
  game[2] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[5] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[8] <- -1
  expect_equal(EvaluateBoard(game),-1)

  #down right
  game <- rep(0, 9) # Empty board
  game[3] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[6] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[9] <- 1
  expect_equal(EvaluateBoard(game), 1)
  game[3] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[6] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[9] <- -1
  expect_equal(EvaluateBoard(game),-1)

  #diag left->right
  game <- rep(0, 9) # Empty board
  game[1] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[5] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[9] <- 1
  expect_equal(EvaluateBoard(game), 1)
  game[1] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[5] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[9] <- -1
  expect_equal(EvaluateBoard(game),-1)

  #diag right->left
  game <- rep(0, 9) # Empty board
  game[3] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[5] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[7] <- 1
  expect_equal(EvaluateBoard(game), 1)
  game[3] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[5] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[7] <- -1
  expect_equal(EvaluateBoard(game),-1)

  #tie
  game <- rep(0, 9) # Empty board
  game[1] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[2] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[6] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[7] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[8] <- 1
  expect_equal(EvaluateBoard(game), 0)
  game[3] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[4] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[5] <- -1
  expect_equal(EvaluateBoard(game), 0)
  game[9] <- -1
  expect_equal(EvaluateBoard(game), 0)

})
