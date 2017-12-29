
library(tttShiny)
library(CreateTTTTree)
library(xtable)

board.size = tttShiny::BoardSize()

#' shiny server
#'
#' @param input: shiny input
#' @param output: shiny output
#' @param session: shiny session
#'
#' @examples
#'
server <- function(input, output, session) {
  xSide <- -1
  oSide <- 1
  humanSide <- xSide
  computerSide <- oSide
  xWins <- 0
  oWins <- 0
  ties <- 0
  tree <- CreateTTTTree::GenerateTable()
  initialTree <- tree
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
      # print(paste("humanSide=", humanSide, "xSide=", xSide, "oSide=", oSide))
    }
  }

  #' reset game
  #'
  #' @examples
  #'
  ResetGame <- function() {
    print("ResetGame")
    tree <<- initialTree
    tttShiny::DisableWholeBoard(localBoard, FALSE)
    humanSide <<- 0
    computerSide <<- 0
    for (index in 1:9) {
      cells <- tttShiny::UICellForIndex(index)
      shiny::updateCheckboxInput(session, cells[[1]], label = "X", value = FALSE)
      shiny::updateCheckboxInput(session, cells[[2]], label = "O", value = FALSE)
    }
  }

  observeEvent(input$startNewGame, {
    ResetGame()
  })

  #' convert row and column to a location in the localBoard.
  #'
  #' @param tree: tree representing the remaining possible moves for the game
  #' @param localBoard: Vector 1:9
  #' @param side: Integer: 1,-1
  #'
  #' @return List: localBoard, move, childTree
  #'
  #' @examples
  #'
  TreeMove <- function(tree, board, side) {
    bestMoveNode <- NULL
    bestValue <- -Inf
    # print(paste("treemove Side=", side))
    # print(DisplayBoard(board))
    for (node in tree$tree) {
      CreateTTTTree::PrintNode(node)
      if (node$bestValueOppX > bestValue) {
        bestMoveNode <- node
        bestValue <- node$bestValueOppX
      }
    }
    # PrintNode(bestMoveNode)
    move <-
      CalculateMove(bestMoveNode$rootRow, bestMoveNode$rootCol)
    board[[move]] <- side
    # print("exit treemove")
    list(localBoard = board,
         move = move,
         tree = bestMoveNode)
  }

  #' convert row and column to a location in the localBoard.
  #'
  #' @param r: Integer: 1:3
  #' @param c: Integer: 1:3
  #'
  #' @return Integer  1:9 location in localBoard.
  #'
  #' @examples
  #'
  CalculateMove <- function(r, c) {
    xxx <- ((c - 1) * 3) + r
    # print(paste("  CalculateMove [", r, ",", c, "] move=", xxx))
    xxx
  }

  #' compute computer move.
  #'
  #' @param winner: Integer: 1,-1  Who won the name
  #' @param localBoard: Vector 1:9
  #' @param computerSide: Integer: 1,-1
  #' @param tree: tree representing the remaining possible moves for the game
  #'
  #' @return List  localBoard, move, tree for move
  #'
  #' @examples
  #'
  ComputerMove <-
    function(winner,
             localBoard,
             computerSide,
             tree) {

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
      if (winner == 0 &&
          IsMachineTurn(localBoard) && IsMovePossible(localBoard)) {
        # print(paste("Move happening: computerSide=", computerSide))
        x <- TreeMove(tree, localBoard, computerSide)
        localBoard <- x$localBoard
        move <- x$move
        tttShiny::UpdateInputForMove(localBoard, move, computerSide, xSide, session)
        x
      }
    }

  #' Update wins by X and O.  Also, show ties.
  #'
  #' @param winner: Integer: 1,-1  Who won the name
  #' @param localBoard: Vector 1:9
  #'
  #' @return Boolean gameOver
  #'
  #' @examples
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
    } else if (!IsMovePossible(localBoard)) {
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

  #' Find the child node of the current tree that
  #' the human has made.
  #'
  #' @param localBoard: Vector 1:9
  #' @param tree: represting the last move by computer before human move
  #'
  #' @return child tree
  #'
  #' @examples
  #'
  GetTree <- function(localBoard, tree) {
    found <- NULL
    # print("gettree - begin tree")
    # PrintNode(tree)
    # print(DisplayBoard(board))
    for (node in tree$tree) {
      # PrintNode(node)
      if (localBoard[node$rootRow, node$rootCol] != 0) {
        found <- node
        break()
      }
    }
    found
  }

  board <- tttShiny::UpdateBoard(input, xSide, oSide)

  output$board <- renderTable({
    print("*************start****************")
    localBoard <- board()
    SetHumanSide(localBoard)
    localTree <- GetTree(localBoard, tree)
    CreateTTTTree::PrintMove(localTree, 'Human')
    if (!is.null(localTree)) {
      tree <<- localTree
      winner <- tttShiny::EvaluateBoard(localBoard)
      # print(paste("Winner=",winner))
      if (winner == 0 && tttShiny::IsMovePossible(localBoard)) {
        x <<- ComputerMove(winner, localBoard, computerSide, tree)
        localBoard <- x$localBoard
        tree <<- x$tree
        # PrintNode(tree)
        winner <- tttShiny::EvaluateBoard(localBoard)
        CreateTTTTree::PrintMove(tree, 'Computer')
      }
      gameOver <- UpdateWins(winner, localBoard)
      if (gameOver) {
        # tic.ai <<- Train(winner)
      } else {
        tttShiny::DisableBoard(localBoard, computerSide, xSide, oSide)
      }
    }
    print("*********end****************")
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

print(paste("Testing"))
shinyApp(ui = tttShiny::shinyTTTUI(), server = server)
