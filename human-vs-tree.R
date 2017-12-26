

# install.packages("shiny")
library(shiny)
# install.packages("shinyjs")
library(shinyjs)

GenerateShinyBoard <- function(data) {
  matrix(data, ncol = board.size, nrow = board.size)
}

library(CreateTTTTree)
library(xtable)

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
  humanSide <- xSide
  computerSide <- oSide
  xWins <- 0
  oWins <- 0
  ties <- 0
  tree <- GenerateTable()
  initialTree <- tree

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
      # print(paste("humanSide=", humanSide, "xSide=", xSide, "oSide=", oSide))
    }
  }
  UpdateBoard <- function(input) {
    GetSide <- function(isX, isY) {
      if (isX) {
        xSide
      } else if (isY)
      {
        oSide
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
    tree <<- initialTree
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

  observeEvent(input$startNewGame, {
    ResetGame()
  })

  TreeMove <- function(tree, board, side) {
    bestMoveNode <- NULL
    bestValue <- -Inf
    # print(paste("treemove Side=", side))
    # print(DisplayBoard(board))
    for (node in tree$tree) {
      PrintNode(node)
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

  CalculateMove <- function(r, c) {
    xxx <- ((c - 1) * 3) + r
    # print(paste("  CalculateMove [", r, ",", c, "] move=", xxx))
    xxx
  }

  PrintNode <- function(node, space = "") {
    cat(
      space,
      "level=",
      node$level,
      "rootPick=[",
      node$rootRow,
      node$rootCol,
      "]",
      "sideMadeThisMove=",
      node$sideMadeThisMove,
      " bestValueMeX=",
      node$bestValueMeX,
      " bestValueOppX=",
      node$bestValueOppX,
      " numSubNodes=",
      length(node$tree),
      "\n"
    )
  }

  ComputerMove <-
    function(winner,
             localBoard,
             computerSide,
             tree) {
      if (winner == 0 &&
          IsMachineTurn(localBoard) && IsMovePossible(localBoard)) {
        # print(paste("Move happening: computerSide=", computerSide))
        x <- TreeMove(tree, localBoard, computerSide)
        localBoard <- x$localBoard
        move <- x$move
        UpdateInputForMove(localBoard, move, computerSide)
        x
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

  GetTree <- function(board, tree) {
    found <- NULL
    # print("gettree - begin tree")
    # PrintNode(tree)
    # print(DisplayBoard(board))
    for (node in tree$tree) {
      # PrintNode(node)
      if (board[node$rootRow, node$rootCol] != 0) {
        found <- node
      }
    }
    # if (!is.null(found)) {
      # PrintNode(found)
      # print("exit gettree")
    # } else {
      # if (tree$level != 0){
      #   vvv
      # }
      # PrintNode(tree)
      # print("exit gettree no tree")
    # }
    found
  }
  PrintMove <- function(node, whoMoved) {
    if (is.null(node)) {
      cat("No Move by ", whoMoved, "\n")
    } else {
      cat(
        "Move by ",
        whoMoved,
        " level=",
        node$level,
        "rootPick=[",
        node$rootRow,
        node$rootCol,
        "]",
        "sideMadeThisMove=",
        node$sideMadeThisMove,
        " bestValueMeX=",
        node$bestValueMeX,
        " bestValueOppX=",
        node$bestValueOppX,
        " numSubNodes=",
        length(node$tree),
        "\n"
      )
    }
  }

  board <- UpdateBoard(input)

  output$board <- renderTable({
    print("*************start****************")
    localBoard <- board()
    SetHumanSide(localBoard)
    localTree <- GetTree(localBoard, tree)
    PrintMove(localTree, 'Human')
    if (!is.null(localTree)) {
      tree <<- localTree
      winner <- EvaluateBoard(localBoard)
      # print(paste("Winner=",winner))
      if (winner == 0 && IsMovePossible(localBoard)) {
        x <<- ComputerMove(winner, localBoard, computerSide, tree)
        localBoard <- x$localBoard
        tree <<- x$tree
        # PrintNode(tree)
        winner <- EvaluateBoard(localBoard)
        PrintMove(tree, 'Computer')
      }
      gameOver <- UpdateWins(winner, localBoard)
      if (gameOver) {
        # tic.ai <<- Train(winner)
      } else {
        DisableBoard(localBoard)
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


shinyApp(ui = ui(), server = server)
