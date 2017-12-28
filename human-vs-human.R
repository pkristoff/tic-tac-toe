

library(tttShiny)
library(xtable)

board.size = tttShiny::BoardSize()

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
    tttShiny::DisableWholeBoard(localBoard, FALSE)
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
      tttShiny::DisableWholeBoard(localBoard, TRUE)
      gameOver <- TRUE
    } else if (!tttShiny::IsMovePossible(localBoard)) {
      # print(paste("winner=tie"))
      ties <<- ties + 1
      tttShiny::DisableWholeBoard(localBoard, TRUE)
      gameOver <- TRUE
    }
    output$xWinsText <- renderText(paste("X Wins: ", xWins))
    output$oWinsText <- renderText(paste("O Wins: ", oWins))
    output$tiesText <- renderText(paste("Ties: ", ties))
    gameOver
  }

  board <- tttShiny::UpdateBoard(input, xSide, oSide)

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
    winner <- tttShiny::EvaluateBoard(localBoard)
    gameOver <- UpdateWins(winner, localBoard)
    if (!gameOver) {
      tttShiny::DisableBoard(localBoard, nextPlayer*-1, xSide, oSide)
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

shinyApp(ui = tttShiny::shinyTTTUI(), server = server)
