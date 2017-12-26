

# install.packages("shiny")
library(shiny)
# install.packages("shinyjs")
library(shinyjs)

# =================common=================
board.size = 3

# Checking whether somebody has won
EvaluateBoard <- function(board) {
  FlipMatrix <- function(m) {
    apply(m, 2, rev)
  }
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
GenerateEmptyBoard <- function() {
  matrix(0, ncol = board.size, nrow = board.size)
}

IsMovePossible <- function(board) {
  length(which(board == 0)) > 0
}
# =======================================

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
  humanSide2 <- 0
  nextPlayer <- NULL
  xWins <- 0
  oWins <- 0
  ties <- 0

  #' figures out which was the first pick in the UI and that is the humanSide.
  #'
  #' @param board: 3x3 matrix
  #'
  #' @return Integer: 1,-1,0
  #'
  #' @examples
  #'
  #'
  HumanSide <- function(board) {
    if (humanSide == 0) {
      xLen <- length(which(board == xSide))
      oLen <- length(which(board == oSide))
      if (xLen > oLen) {
        xSide
      } else if (xLen < oLen) {
        oSide
      } else {
        0
      }
    } else {
      humanSide
    }
  }

  #' Connects the server side with
  #'
  #' @param input: shiny input
  #'
  #' @examples
  #'
  #'
  UpdateBoard <- function(input) {

    #' converts boolean to board value
    #'
    #' @param isX: X has been selected
    #'        isO: O has been selected
    #'
    #' @return Integer: 1 (isX), -1 (isO), 0
    #'
    #' @examples
    #'
    #'
    GetSide <- function(isX, isO) {
      if (isX) {
        -1
      } else if (isO)
      {
        1
      } else{
        0
      }
    }
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

  #'  disable & enable whole board.
  #'
  #' @param enableWidget: Boolean: TRUE: disables all checkboxes - game over
  #'                              FALSE: enables all checkboxes - start new game.
  #'
  #' @return
  #'
  #' @examples
  #'
  #'
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

  #'  disables/enables checkboxes based on whether the XorO has been chosen and who's turn it is.
  #'  if cell on local board has been chosen then disable.
  #'  based on whose turn it is then enable for that player.
  #'
  #' @param localBoard: vector of Integer (1,0,-1)
  #'
  #' @return
  #'
  #' @examples
  #'
  #'
  DisableBoard <- function(localBoard, nextPlayer) {
    for (index in 1:9) {
      cells = UICellForIndex(index)
      if (localBoard[index] != 0) { # someone has played this cell.
        shinyjs::disable(cells[[1]])
        shinyjs::disable(cells[[2]])
      } else {
        if (nextPlayer == xSide) {
          shinyjs::enable(cells[[1]])
          shinyjs::disable(cells[[2]])
        } else if (nextPlayer == oSide) {
          shinyjs::enable(cells[[2]])
          shinyjs::disable(cells[[1]])
        }
      }
    }
  }

  #' Resets for new game.
  #'
  #' @param
  #'
  #' @return
  #'
  #' @examples
  #'
  #'
  ResetGame <- function() {
    print("ResetGame")
    DisableWholeBoard(localBoard, FALSE)
    humanSide <<- 0
    humanSide2 <<- 0
    for (index in 1:9) {
      cells <- UICellForIndex(index)
      updateCheckboxInput(session, cells[[1]], label = "X", value = FALSE)
      updateCheckboxInput(session, cells[[2]], label = "O", value = FALSE)
    }
  }

  #' Converts a board location to a UI cell location.
  #'
  #' @param index: Integer (1:9)
  #'
  #' @return list of UI checkbox names.
  #'
  #' @examples
  #'
  #'
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

  #' called if New Game button is called.
  #'
  #' @param
  #'
  #' @return
  #'
  #' @examples
  #'
  #'
  observeEvent(input$startNewGame, {
    ResetGame()
  })

  #' If game over then update xWins, OWins, & ties.  It
  #' also updates the UI.
  #'
  #' @param winner: Integer: who won the current game
  #'        localBoard: 3x3 matrix
  #'
  #' @return: Boolean: whether game is over not
  #'
  #' @examples
  #'
  #'
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

  board <- UpdateBoard(input)

  output$board <- renderTable({
    localBoard <- board()
    print(
      paste(
        "before-humanSide2=",
        humanSide2,
        "humanSide=",
        humanSide,
        "xSide=",
        xSide,
        "oSide=",
        oSide,
        "nextPlayer=",
        nextPlayer
      )
    )
    humanSide <<- HumanSide(localBoard)
    humanSide2 <<- humanSide * -1
    nextPlayer <<- ifelse(humanSide == 0, -999, ifelse(nextPlayer == -999, humanSide2, nextPlayer * -1))
    print(
      paste(
        "after-humanSide2=",
        humanSide2,
        "humanSide=",
        humanSide,
        "xSide=",
        xSide,
        "oSide=",
        oSide,
        "nextPlayer=",
        nextPlayer
      )
    )
    winner <- EvaluateBoard(localBoard)
    gameOver <- UpdateWins(winner, localBoard)
    if (!gameOver) {
      DisableBoard(localBoard, nextPlayer)
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
