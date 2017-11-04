# https://dratewka.wordpress.com/2013/03/15/ai-overkill-teaching-a-neural-network-to-play-tic-tac-toe/
#
# Such a network is actually a multilayer perceptron with the three layers of neurons (circles) connected by weighted links (arrows).
# It works by receiving some inputs delivered to the input layer, processing them using the neurons and returning output through the
# output layer.
# Initially such a network is pretty useless until its trained. Training basically means trying to figure out how the weight values
# should be set. So how do we teach one to play tic-tac-toe?
#
# Let’s start from the beginning and define an empty board of a given size and write a function (in R) which evaluates whether
# somebody has won:

board.size <- 3
# The game board is a matrix with: 1 - tic, 0 - empty, -1 - tac
GenerateEmptyBoard <- function() {
  matrix(0, ncol = board.size, nrow = board.size)
}

DisplayBoard <- function(board) {
  b <- factor(board,
              levels = c(-1, 0, 1),
              labels = c("X", "*", "O"))
  dim(b) <- dim(board)
  b
}

FlipMatrix <- function(m) {
  apply(m, 2, rev)
}

# Checking whether somebody has won
EvaluateBoard <- function(board) {
  sums <- c(colSums(board),
            rowSums(board),
            sum(diag(board)),
            sum(diag(FlipMatrix(board))))

  if (max(sums) == board.size) {
    return(1)
  }
  if (min(sums) == -board.size) {
    return(-1)
  }
  0
}

# Now let’s create functions for our neural network to let it play the game.
# A reasonable approach is to assume that the network will be taught to evaluate
# the board situation. Is such case when the computer will be making its move it
# will use the neural network to evaluate each possible move and choose the best one.

neurons <- 1
numWeights <- neurons * board.size ^ 2 + 2 * neurons

# Creating an empty neural network which we represent as a matrix of weights
InitNN <- function() {
  n.weights <- numWeights
  matrix(runif(n.weights), nrow = neurons)
}

# Calculating the network
RunNN <- function(nn, board) {
  w.out <- nn[, 1]
  w0.in <- nn[, 2]
  w.in <- nn[, 3:ncol(nn)]
  t(w.out) %*% tanh(w.in %*% as.vector(board) + w0.in)
}


# Evaluating every move possible using the network
RunAI <- function(ai, board, side) {
  res <- sapply(1:length(board), function(i) {
    b <- board
    b[[i]] <- side
    RunNN(ai, b)
  })
  # We don't want the AI to cheat
  res[board != 0] = -Inf

  # We choose the best one
  which.max(res)
}

Move <- function(ai, board, side) {
  move <- RunAI(ai, board, side)
  board[[move]] <- side
  board
  list(board, move)
}

IsMovePossible <- function(board) {
  length(which(board == 0)) > 0
}

# So now we have all we need to make a neural network play tic-tac-toe,
# but it won’t be very good at it – until we teach it. One way to do it it to
# sit in front of the computer for a couple of weeks, play a game after game against
# against our pet network until it is worthy… or: behold the Random Player!


NNvsRandomPlayer <- function(tic.ai) {
  side <- sample(c(-1, 1), 1)
  board <- GenerateEmptyBoard()
  eval <- 0

  while (eval == 0 && IsMovePossible(board)) {
    if (side == 1) {
      x <- Move(tic.ai, board, side)
      board <- x[[1]]
    } else{
      # Make a valid move completely at random and see what happens
      move <- sample(which(board == 0), 1)
      board[[move]] <- side
    }
    # print(DisplayBoard(board))
    eval <- EvaluateBoard(board)
    side <- side * -1
  }
  eval
}

NNvsRandom <- function(num) {
  results <- c(0, 0, 0)
  i <- 0
  while (i < num) {
    winner <-
      NNvsRandomPlayer(matrix(
        c(
          -0.091560,
          0.016083,
          0.019564,
          0.069865,
          0.033497,
          0.081025,-0.013193,
          0.083981,-0.069655,-0.038064,-0.083860
        ),
        nrow = 1,
        ncol = numWeights
      ))

    # winner <- NNvsRandomPlayer(matrix(c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5), nrow = 1, ncol = 11))
    sprintf("Winner %i", winner)
    if (winner == 1) {
      results[1] <- results[1] + 1
    } else {
      if (winner == 0) {
        results[2] <- results[2] + 1
      } else {
        results[3] <- results[3] + 1
      }
    }
    i <- i + 1
  }
  results
}

# So now we have a worthy opponent let’s see how our network does in a series of 10,000 games:

# Wins: 40.53
# Ties: 13.04
# Losses: 46.43

# Hmm… not very good, but let’s train it:

# install.packages('DEoptim')
library(DEoptim)

TrainAI <- function() {
  # Function to evaluate how good is our network
  Eval <- function(w) {
    tic.ai <- matrix(w, nrow = neurons)
    # Playing against a random player is nondeterministic, so we need to stabilise the results
    ev <-
      median(sapply(1:20, function(j)
        mean(
          sapply(1:20, function(i)
            NNvsRandomPlayer(tic.ai))
        )))

    ev <- -1 * (ev)
  }
  TrainAIEval(Eval,DEoptim::DEoptim.control(
    trace = 0,
    parallelType = 0,
    NP = 10,
    VTR = -1.0
  ))
}
TrainAIEval <- function(eval,ctrl) {

  len <- length(InitNN())
  # This is a global optimisation method, so using we need an appropriate method - a differential evolution
  # algorithm seems sufficient
  res <- DEoptim::DEoptim(
    eval,
    rep(-0.1, len),
    rep(0.1, len),
    ctrl
  )

  matrix(res$optim$bestmem, nrow = neurons)

}

# After the training our impressive single-hidden-neuron-network becomes better (again 10,000 games):

# Wins: 75.85
# Ties: 04.80
# Losses: 19.35

