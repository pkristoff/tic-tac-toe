



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
ui <- function () {
  pageWithSidebar(
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
        fixedRow(column(3, style = "background-color:aqua;", textOutput('tiesText'))),
        fixedRow(column(
          3, style = "background-color:aqua;", actionButton("startNewGame", "Start New Game")
        ))
      )
    ))
  )
}

server <- function(input, output, session) {
  xSide <- -1
  oSide <- 1
  humanSide <- 0
  computerSide <- 0
  xWins <- 0
  oWins <- 0
  ties <- 0
  tic.ai.history <- list()

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
      # print(paste("numX=", numX, "numO=", numO))
      numX != numO
    } else {
      FALSE
    }
  }
  SetHumanSide <- function(board) {
    if (humanSide == 0) {
      xLen <- length(which(board == xSide))
      oLen <- length(which(board == oSide))
      if (xLen > oLen) {
        humanSide <<- xSide
        computerSide <<- oSide
      } else if (xLen < oLen) {
        humanSide <<- oSide
        computerSide <<- xSide
      }
      print(paste("humanSide=", humanSide, "xSide=", xSide, "oSide=", oSide))
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
        0.081025,
        -0.013193,
        0.083981,
        -0.069655,
        -0.038064,
        -0.083860
      ),
      nrow = neurons,
      ncol = numWeights
    ))

  AddTicAIHistory <- function(newTicAI) {
    print(length(tic.ai.history))
    x <- tic.ai.history
    x[[length(x) + 1]] <- newTicAI
    tic.ai.history <<- x
    print(tic.ai.history)
  }

  AddTicAIHistory(tic.ai)

  observeEvent(input$startNewGame, {
    ResetGame()
  })

  ComputerMove <-
    function(winner,
             localBoard,
             computerSide,
             tic.ai) {
      if (winner == 0 &&
          IsMachineTurn(localBoard) && IsMovePossible(localBoard)) {
        # print(paste("Move happening: computerSide=", computerSide))
        x <- Move(tic.ai, localBoard, computerSide)
        localBoard <- x[[1]]
        move <- x[[2]]
        UpdateInputForMove(localBoard, move, computerSide)
      }
    }

  UpdateWins <- function(winner, localBoard) {
    gameOver <- FALSE
    if (winner != 0) {
      if (winner == xSide) {
        # print(paste("winner=xSide"))
        xWins <<- xWins + 1
      } else if (winner == oSide) {
        # print(paste("winner=oSide"))
        oWins <<- oWins + 1
      }
      DisableWholeBoard(localBoard, TRUE)
      gameOver <- TRUE
    } else if (!IsMovePossible(localBoard)) {
      # print(paste("winner=tie"))
      ties <<- ties + 1
      DisableWholeBoard(localBoard, TRUE)
      gameOver <- TRUE
    }
    output$xWinsText <- renderText(paste("X Wins: ", xWins))
    output$oWinsText <- renderText(paste("O Wins: ", oWins))
    output$tiesText <- renderText(paste("Ties: ", ties))
    gameOver
  }

  Train <- function(winner) {
    tic.ai <- TrainAIEval(function(w) {
      # print(paste('Train called:', winner))
      winner
    },
    DEoptim::DEoptim.control(
      trace = 0,
      parallelType = 0,
      NP = 10,
      VTR = -1.0
    ))
    AddTicAIHistory(tic.ai)
    tic.ai
  }

  board <- UpdateBoard(input)

  output$board <- renderTable({
    localBoard <- board()
    SetHumanSide(localBoard)
    winner <- EvaluateBoard(localBoard)
    ComputerMove(winner, localBoard, computerSide, tic.ai)
    gameOver <- UpdateWins(winner, localBoard)
    if (gameOver) {
      tic.ai <<- Train(winner)
    } else {
      DisableBoard(localBoard)
    }
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

shinyApp(ui = ui(), server = server)
