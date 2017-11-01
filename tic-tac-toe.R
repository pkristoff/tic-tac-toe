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

# Creating an empty neural network which we represent as a matrix of weights
InitNN <- function() {
  n.weights <- neurons * board.size ^ 2 + 2 * neurons
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
        ncol = 11
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

  len <- length(InitNN())
  # This is a global optimisation method, so using we need an appropriate method - a differential evolution
  # algorithm seems sufficient
  res <- DEoptim::DEoptim(
    Eval,
    rep(-0.1, len),
    rep(0.1, len),
    DEoptim::DEoptim.control(
      trace = 1,
      parallelType = 0,
      NP = 10,
      VTR = -1.0
    )
  )

  matrix(res$optim$bestmem, nrow = neurons)
}

# After the training our impressive single-hidden-neuron-network becomes better (again 10,000 games):

# Wins: 75.85
# Ties: 04.80
# Losses: 19.35


# install.packages("shiny")
library(shiny)
# install.packages("shinyjs")
library(shinyjs)

GenerateShinyBoard <- function(data) {
  matrix(data, ncol = board.size, nrow = board.size)
}
GetSide <- function(isX, isY) {
  if (isX) {
    -1
  } else if (isY)
  {
    1
  } else{
    0
  }
}

library(xtable)

cbCol1 <- 3
cbCol2 <- 3
cbCol3 <- 3
ui <- pageWithSidebar(
  headerPanel("Tic Tac Toe"),

  sidebarPanel(shinyjs::useShinyjs(),
               fixedRow(
                 column(
                   9,
                   style = "background-color:pink;",
                   fixedRow(
                     column(
                       cbCol1,
                       style = "background-color:aqua;",
                       checkboxInput("Cell_1_1_X", "X", FALSE),
                       checkboxInput("Cell_1_1_O", "O", FALSE)
                     )
                     ,
                     column(
                       cbCol2,
                       style = "background-color:aqua;",
                       checkboxInput("Cell_1_2_X", "X", FALSE),
                       checkboxInput("Cell_1_2_O", "O", FALSE)
                     )
                     ,
                     column(
                       cbCol3,
                       style = "background-color:aqua;",
                       checkboxInput("Cell_1_3_X", "X", FALSE),
                       checkboxInput("Cell_1_3_O", "O", FALSE)
                     )
                   ),
                   fixedRow(
                     column(
                       cbCol1,
                       style = "background-color:aqua;",
                       checkboxInput("Cell_2_1_X", "X", FALSE),
                       checkboxInput("Cell_2_1_O", "O", FALSE)
                     )
                     ,
                     column(
                       cbCol2,
                       style = "background-color:aqua;",
                       checkboxInput("Cell_2_2_X", "X", FALSE),
                       checkboxInput("Cell_2_2_O", "O", FALSE)
                     )
                     ,
                     column(
                       cbCol3,
                       style = "background-color:aqua;",
                       checkboxInput("Cell_2_3_X", "X", FALSE),
                       checkboxInput("Cell_2_3_O", "O", FALSE)
                     )
                   ),
                   fixedRow(
                     column(
                       cbCol1,
                       style = "background-color:aqua;",
                       checkboxInput("Cell_3_1_X", "X", FALSE),
                       checkboxInput("Cell_3_1_O", "O", FALSE)
                     )
                     ,
                     column(
                       cbCol2,
                       style = "background-color:aqua;",
                       checkboxInput("Cell_3_2_X", "X", FALSE),
                       checkboxInput("Cell_3_2_O", "O", FALSE)
                     )
                     ,
                     column(
                       cbCol3,
                       style = "background-color:aqua;",
                       checkboxInput("Cell_3_3_X", "X", FALSE),
                       checkboxInput("Cell_3_3_O", "O", FALSE)
                     )
                   )
                 )
               )),

  mainPanel(fixedRow(
    column(
      12,
      #style = "background-color:pink;",
      fixedRow(column(3, style = "background-color:aqua;", uiOutput('board'))),
      fixedRow(column(3, style = "background-color:aqua;", textOutput('xWinsText'))),
      fixedRow(column(3, style = "background-color:aqua;", textOutput('oWinsText'))),
      fixedRow(column(
        3, style = "background-color:aqua;", actionButton("startNewGame", "Start New Game")
      ))
    )
  ))
)

server <- function(input, output, session) {
  xSide <- -1
  oSide <- 1
  humanSide <- 0
  computerSide <- 0
  xWins <- 0
  oWins <- 0

  IsMachineTurn <- function (board) {
    # length(which(board == computerSide)) > length(which(board == humanSide))
    if (computerSide != humanSide) {
      numX <- 0
      numO <- 0
      for (rowNum in 1:3) {
        for (colNum in 1:3) {
          if (board[rowNum, colNum] == computerSide) {
            numX <- numX + 1
          } else if (board[rowNum, colNum] == humanSide) {
            numO <- numO + 1
          }
        }
      }
      print(paste("numX=", numX, "numO=", numO))
      numX != numO
    } else {
      FALSE
    }
  }
  SetHumanSide <- function(board) {
    xLen <- length(which(board == xSide))
    oLen <- length(which(board == oSide))
    if (xLen > oLen) {
      humanSide <<- xSide
      computerSide <<- oSide
    } else if (xLen < oLen) {
      humanSide <<- oSide
      computerSide <<- xSide
    }
  }
  UpdateBoard <- function(input) {
    reactive({
      board <- GenerateEmptyBoard()
      board[[1]] <- GetSide(input$Cell_1_1_X, input$Cell_1_1_O)
      board[[4]] <- GetSide(input$Cell_1_2_X, input$Cell_1_2_O)
      board[[7]] <- GetSide(input$Cell_1_3_X, input$Cell_1_3_O)
      board[[2]] <- GetSide(input$Cell_2_1_X, input$Cell_2_1_O)
      board[[5]] <- GetSide(input$Cell_2_2_X, input$Cell_2_2_O)
      board[[8]] <- GetSide(input$Cell_2_3_X, input$Cell_2_3_O)
      board[[3]] <- GetSide(input$Cell_3_1_X, input$Cell_3_1_O)
      board[[6]] <- GetSide(input$Cell_3_2_X, input$Cell_3_2_O)
      board[[9]] <- GetSide(input$Cell_3_3_X, input$Cell_3_3_O)
      board
    })
  }
  UpdateInputForMove <- function(localBoard, move, side) {
    if (side == xSide) {
      cellIndex <- 1
      labelText <- "X"
    } else {
      cellIndex <- 2
      labelText <- "O"
      }
    cell <- UICellForIndex(move)[[cellIndex]]
    updateCheckboxInput(session, cell, label = labelText, value = TRUE)
  }
  DisableWholeBoard <- function(localBoard, enableWidget) {
    for (index in 1:9) {
      cells = UICellForIndex(index)
      if (enableWidget) {
        shinyjs::disable(cells[[1]])
        shinyjs::disable(cells[[2]])
      } else {
        shinyjs::enable(cells[[1]])
        shinyjs::enable(cells[[2]])
      }
    }
  }
  DisableBoard <- function(localBoard) {
    for (index in 1:9) {
      cells = UICellForIndex(index)
      if (localBoard[index] != 0) {
        shinyjs::disable(cells[[1]])
        shinyjs::disable(cells[[2]])
      }
      if (computerSide == xSide) {
        shinyjs::disable(cells[[1]])
      } else if (computerSide == oSide) {
        shinyjs::disable(cells[[2]])
      }
    }
  }

  ResetGame <- function() {
    print("ResetGame")
    DisableWholeBoard(localBoard, FALSE)
    humanSide <<- 0
    computerSide <<- 0
    for (index in 1:9) {
      cells <- UICellForIndex(index)
      updateCheckboxInput(session, cells[[1]], label = "X", value = FALSE)
      updateCheckboxInput(session, cells[[2]], label = "O", value = FALSE)
    }
  }

  UICellForIndex <- function(index) {
    switch(
      index,
      list("Cell_1_1_X", "Cell_1_1_O"),
      list("Cell_2_1_X", "Cell_2_1_O"),
      list("Cell_3_1_X", "Cell_3_1_O"),
      list("Cell_1_2_X", "Cell_1_2_O"),
      list("Cell_2_2_X", "Cell_2_2_O"),
      list("Cell_3_2_X", "Cell_3_2_O"),
      list("Cell_1_3_X", "Cell_1_3_O"),
      list("Cell_2_3_X", "Cell_2_3_O"),
      list("Cell_3_3_X", "Cell_3_3_O"),
      list("Unknown Cell X", "Unknown Cell O")
    )
  }
  tic.ai <-
    (matrix(
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
      ncol = 11
    ))


  observeEvent(input$startNewGame, {
    ResetGame()
  })

  board <- UpdateBoard(input)

  output$board <- renderTable({
    localBoard <- board()
    if (humanSide == 0) {
      SetHumanSide(localBoard)
      print(paste("humanSide=", humanSide, "xSide=", xSide, "oSide=", oSide))
    }
    winner <- EvaluateBoard(localBoard)
    if (winner == 0 &&
        IsMachineTurn(localBoard) && IsMovePossible(localBoard)) {
      print(paste("Move happening: computerSide=", computerSide))
      x <- Move(tic.ai, localBoard, computerSide)
      localBoard <- x[[1]]
      move <- x[[2]]
      UpdateInputForMove(localBoard, move, computerSide)
    }
    # print(DisplayBoard(localBoard))
    # print(paste("winner=",winner))
    if (winner != 0) {
      if (winner == xSide) {
        xWins <<- xWins + 1
      } else {
        oWins <<- oWins + 1
      }
      DisableWholeBoard(localBoard, TRUE)
    } else {
      DisableBoard(localBoard)
    }
    output$xWinsText <- renderText(paste("X Wins: ", xWins))
    output$oWinsText <- renderText(paste("O Wins: ", oWins))
    # print(DisplayBoard(localBoard))
    xtable(matrix(
      factor(
        localBoard,
        levels = c(xSide, 0, oSide),
        labels = c("X", "*", "O")
      ),
      ncol = board.size,
      nrow = board.size
    ))
  })
}

shinyApp(ui = ui, server = server)
