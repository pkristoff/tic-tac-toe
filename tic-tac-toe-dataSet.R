# https://ipub.com/tic-tac-toe/
# https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html#get
#

# In this post, we do a brute force solution of Tic-Tac-Toe, the well-known 3*3 game.
# You’ll learn how data.tree can be used to build a tree of game history, and how the resulting
# data.tree structure can be used to analyse the game.

# This post is based on data.tree 0.2.1, which you can get from CRAN.

# We want to set up the problem in a way such that each Node is a move of a player,
# and each path describes the entire history of a game.

# install.packages("data.tree")
library(data.tree)
library(CreateTTTTree)

#' The data.tree representing all games in tic-tac-toe, takes advantage of rotating
#' the board 90 degrees is the exact same board.  Based on initMove this function sets the translation
#' array: gameTreeConversionMapping.
#'
#' @param initMove An integer.  The human's first click on the board.
#'
#' @return a function that takes two parameters:
#'     conversion: String ['toTree', 'toGame']
#'     value: Integer The value to be converted.
#'
#' @examples
#' GameTreeConv(5)
#'
GameTreeConv <- function (initMove) {
  gameTreeConversionMapping <<-
    if (initMove == 5 || initMove == 7 || initMove == 8) {
      c(1, 2, 3, 4, 5, 6, 7, 8, 9)
    } else if (initMove == 5 || initMove == 1 || initMove == 4) {
      c(7, 4, 1, 8, 5, 2, 9, 6, 3)
    } else if (initMove == 5 || initMove == 2 || initMove == 3) {
      c(9, 8, 7, 6, 5, 4, 3, 2, 1)
    } else if (initMove == 5 || initMove == 6 || initMove == 9) {
      c(3, 6, 9, 2, 5, 8, 1, 4, 7)
    }
  return(function(conversion, value) {
    if (conversion == 'toTree') {
      return(gameTreeConversionMapping[value])
    } else if (conversion == 'toGame') {
      return(which(gameTreeConversionMapping == value))
    } else {
      return(NULL)
    }
  })
}

#' Implements the minimax algorithm
#'
#' @param board: vector: the game board.
#' @param player: integer: The player who's move it is.
#' @param player: integer: The player who's move it is.
#'
#' @return a list:
#'   childValue: integer: The chosen value for the children.
#'   child: child of tree: corresponds to bestValu
#'
#' @examples
#' MiniMax(rep(1:9), 1, tree)
#'
MiniMax <- function(board, player, tree) {
  winner = EvaluateBoard(board)
  if (winner == player)
    return(list(childValue = 10, child = tree))
  if (winner == -player)
    return(list(childValue = -10, child = tree))
  if (winner == 0 &&
      tree$isLeaf)
    return(list(childValue = 0, child = tree))
  isMax = player != tree$player
  bestValue = ifelse(isMax, -Inf, +Inf)
  bestChild <- NULL
  for (child in tree$children) {
    board[GameTreeConv.treeToGame(child$f)] <- child$player
    result = MiniMax(board, player, child)
    childValue = result$childValue * 0.5
    board[GameTreeConv.treeToGame(child$f)] <- 0
    if (isMax) {
      if (childValue > bestValue) {
        bestValue <- childValue
        bestChild <- child
      }
    } else {
      if (childValue < bestValue) {
        bestValue <- childValue
        bestChild <- child
      }
    }
  }
  list(childValue = bestValue, child = bestChild)
}


#' Implements the minimax algorithm
#'
#' @param board: vector: the game board.
#'
#' @return an integer:
#'   0:  no one has one
#'   1:  player 1 has won
#'   -1: player -1 has won
#'
#' @examples
#' EvaluateBoard(rep(1:9))
#'
EvaluateBoard <- function(board) {
  FlipMatrix <- function(m) {
    apply(m, 2, rev)
  }
  mboard <- matrix(board, ncol = 3, nrow = 3)
  sums <- c(colSums(mboard),
            rowSums(mboard),
            sum(diag(mboard)),
            sum(diag(FlipMatrix(mboard))))

  if (max(sums) == 3) {
    return(1)
  }
  if (min(sums) == -3) {
    return(-1)
  }
  0
}

#' Main game
#'
#' @param ttt: data.tree: the full list of moves for every game.
#' @param player1: String: the optional name of player1.
#' @param player2: String: the optional name of player2.
#'
#' @return an integer:
#'   0:  no one has one
#'   1:  player 1 has won
#'   -1: player -1 has won
#'
#' @examples
#' ttt <- TTTTree()
#' tic.tac.toe(ttt)
#'
tic.tac.toe <-
  function(ttt,
           player1 = "human",
           player2 = "computer") {
    #' Draw the game board - using plots
    #'
    #' @param game: vector of length 9
    #'
    #' @examples
    #' ttt <- TTTTree()
    #' draw.board(rep(1:9))
    #'
    draw.board <- function(game) {
      xo <- c("X", " ", "O") # Symbols
      par(mar = rep(1, 4))
      plot.new()
      plot.window(xlim = c(0, 30), ylim = c(0, 30))
      abline(h = c(10, 20),
             col = "darkgrey",
             lwd = 4)
      abline(v = c(10, 20),
             col = "darkgrey",
             lwd = 4)
      text(rep(c(5, 15, 25), 3), c(rep(25, 3), rep(15, 3), rep(5, 3)), xo[game + 2], cex = 4)
      # Identify location of any three in a row
      square <- t(matrix(game, nrow = 3))
      hor <- abs(rowSums(square))
      if (any(hor == 3))
        hor <- (4 - which(hor == 3)) * 10 - 5
      else
        hor <- 0
      ver <- abs(colSums(square))
      if (any(ver == 3))
        ver <- which(ver == 3) * 10 - 5
      else
        ver <- 0
      diag1 <- sum(diag(square))
      diag2 <- sum(diag(t(apply(square, 2, rev))))
      # Draw winning lines
      if (all(hor > 0))
        for (i in hor)
          lines(c(0, 30), rep(i, 2), lwd = 10, col = "red")
      if (all(ver > 0))
        for (i in ver)
          lines(rep(i, 2), c(0, 30), lwd = 10, col = "red")
      if (abs(diag1) == 3)
        lines(c(2, 28), c(28, 2), lwd = 10, col = "red")
      if (abs(diag2) == 3)
        lines(c(2, 28), c(2, 28), lwd = 10, col = "red")
    }

    # Human player enters a move

    #' Main game
    #'
    #' @param ttt: data.tree: the full list of moves for every game.
    #' @param player1: String: the optional name of player1.
    #' @param player2: String: the optional name of player2.
    #'
    #' @return an integer:
    #'   0:  no one has one
    #'   1:  player 1 has won
    #'   -1: player -1 has won
    #'
    #' @examples
    #' ttt <- TTTTree()
    #' tic.tac.toe(ttt)
    #'
    move.human <- function(game) {
      text(4,
           0,
           "Click on screen to move",
           col = "grey",
           cex = .7)
      empty <- which(game == 0)
      move <- 0
      while (!move %in% empty) {
        coords <- locator(n = 1) # add lines
        coords$x <- floor(abs(coords$x) / 10) + 1
        coords$y <- floor(abs(coords$y) / 10) + 1
        move <- coords$x + 3 * (3 - coords$y)
      }
      return (move)
    }

    #' print a node in the tree.
    #'
    #' @param node: data.tree: the full list of moves for every game.
    #' @param label: String: the optional description of which node this represents
    #'
    #' @examples
    #' PrintNode(node)
    #' tic.tac.toe(ttt, label='child')
    #'
    PrintNode <- function(node, label = "") {
      print(
        paste(
          label,
          ifelse((label != ""), ":", ""),
          "levelName=",
          node$levelName,
          "level=",
          node$level,
          " move=",
          GameTreeConv.treeToGame(node$f),
          " f=",
          node$f,
          " player=",
          node$player,
          " wp1=",
          node$wp1,
          " wp2=",
          node$wp2
        )
      )
    }

    game <- rep(0, 9) # Empty board
    winner <- FALSE # Define winner
    player <- 1 # First player
    players <- c(player1, player2)
    draw.board(game)
    currentNode <- ttt
    currentLevel <- 0
    while (0 %in% game && !winner) {
      # Keep playing until win or full board
      if (players[(player + 3) %% 3] == "human") {
        # Human player
        move <- move.human(game)
        if (is.null(GameTreeConversion))
          GameTreeConversion <<- InitilizeGameTreeConversion(move)
        treeMove <- GameTreeConv.gameToTree(move)
        # print(paste("Move=", move, " treeMove=", treeMove))
        xxx <- NULL
        for (child in currentNode$children) {
          # PrintNode(child, 'Human')
          if (child$f == treeMove) {
            xxx = child
            currentLevel = child$level
            break
          }
        }
        currentNode <- xxx
      }
      else {
        mm <- MiniMax(game, player, currentNode)
        currentNode <- mm$child
        currentLevel = currentNode$level
        move <- GameTreeConv.treeToGame(currentNode$f)
        print(paste("Move=", move, " treeMove=", currentNode$f))
      }
      game[move] <- player # Change board
      draw.board(game)
      winner <- EvaluateBoard(game)
      player <- -player # Change player
    }
  }
# ttt <- TTTTree()
# print(system.time(ttt <- data("ttt", package="CreateTTTTree")))
# tic.tac.toe(ttt)
