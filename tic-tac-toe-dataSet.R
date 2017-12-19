# https://ipub.com/tic-tac-toe/
#

# In this post, we do a brute force solution of Tic-Tac-Toe, the well-known 3*3 game.
# You’ll learn how data.tree can be used to build a tree of game history, and how the resulting
# data.tree structure can be used to analyse the game.

# This post is based on data.tree 0.2.1, which you can get from CRAN.

# We want to set up the problem in a way such that each Node is a move of a player,
# and each path describes the entire history of a game.

# install.packages("data.tree")
library(data.tree)


CreateTTT <- function () {
  # We number the fields from 1 to 9. Additionally, for easy readability, we label the Nodes
  # in an Excel-like manner, such that field 9, say, is ‘c3’:

  fields <- expand.grid(letters[1:3], 1:3)
  fields
  ##   Var1 Var2
  ## 1    a    1
  ## 2    b    1
  ## 3    c    1
  ## 4    a    2
  ## 5    b    2
  ## 6    c    2
  ## 7    a    3
  ## 8    b    3
  ## 9    c    3

  ttt <- Node$new("ttt")

  # To speed up things a bit, we consider rotation, so that, say, the first move in a3 and a1 are considered equal,
  # because they could be achieved with a 90 degree rotation of the board. This leaves us with only a3, b3, and b2
  # for the first move of player 1:

  #consider rotation, so first move is explicit
  ttt$AddChild("a3")
  ttt$a3$f <- 7
  ttt$AddChild("b3")
  ttt$b3$f <- 8
  ttt$AddChild("b2")
  ttt$b2$f <- 5

  ttt$Set(player = 1, filterFun = isLeaf)

  # Now we traverse the tree recursively, and add possible moves to the leaves along the way,
  # growing it to eventually hold all possible games. To do this, we define a method which,
  # based on a Node’s path, adds possible moves as children.
  #
  AddPossibleMoves <- function(node) {
    t <- Traverse(node, traversal = "ancestor", filterFun = isNotRoot)
    available <- rownames(fields)[!rownames(fields) %in% Get(t, "f")]
    node$points1 <- 0
    node$points2 <- 0
    for (f in available) {
      child <- node$AddChild(paste0(fields[f, 1], fields[f, 2]))
      child$f <- as.numeric(f)
      # child$player <- ifelse(node$player == 1, 2, 1)
      child$player <- -node$player
      hasWon <- HasWon(child)
      if (!hasWon && child$level <= 10) {
        AddPossibleMoves(child)
      }
      if (hasWon) {
        child$result <- child$player
        # print(paste("Player ", child$player, "wins!"))
        # child$points1 <- ifelse(node$player == 1, 10, 0)
        # child$points2 <- ifelse(node$player == 2, 10, 0)
        child$points1 <- ifelse(node$player == 1, 10, 0)
        child$points2 <- ifelse(node$player == -1, -10, 0)
      } else if (child$level == 10) {
        # print("Tie!")
        child$result <- 0
        child$points1 <- 0
        child$points2 <- 0
      }
      node$points1 <- node$points1 + child$points1
      node$points2 <- node$points2 + child$points2
    }
    return (node)
  }
  PrintBoard <- function(node) {
    mineV <- rep(0, 9)

    t <-
      Traverse(
        node,
        traversal = "ancestor",
        filterFun = function(x)
          ! x$isRoot && x$player == 1
      )
    field <- Get(t, "f")
    value <- Get(t, function(x)
      paste0("X", x$level - 1))
    mineV[field] <- value

    # t <- Traverse(node, traversal = "ancestor", filterFun = function(x) !x$isRoot && x$player == 2)
    t <-
      Traverse(
        node,
        traversal = "ancestor",
        filterFun = function(x)
          ! x$isRoot && x$player == -1
      )
    field <- Get(t, "f")
    value <- Get(t, function(x)
      paste0("O", x$level - 1))
    mineV[field] <- value

    mineM <- matrix(mineV, 3, 3, byrow = TRUE)
    rownames(mineM) <- letters[1:3]
    colnames(mineM) <- as.character(1:3)
    mineM
  }

  # The following code plays all possible games. Depending on your computer, this might take a few minutes:
  #
  print(system.time(for (child in ttt$children)
    AddPossibleMoves(child)))
  ##    user  system elapsed
  ## 345.645   3.245 346.445
  ##

  # print(paste("ttt$leafCount=", ttt$leafCount))
  # print(paste("ttt$totalCount=", ttt$totalCount))
  # print(paste("What is the average length of a game? ", mean(ttt$Get(function(x) x$level - 1, filterFun = isLeaf))))
  # print(paste("What is the average branching factor? ", ttt$averageBranchingFactor))

  # winnerOne <- Traverse(ttt, filterFun = function(x) x$isLeaf && x$result == 1)
  # winnerTwo <- Traverse(ttt, filterFun = function(x) x$isLeaf && x$result == 2)
  # ties <- Traverse(ttt, filterFun = function(x) x$isLeaf && x$result == 0)
  # print(c(winnerOne = length(winnerOne), winnerTwo = length(winnerTwo), ties = length(ties)))
  #
  #

  # print(PrintBoard(ties[[1]]))

  ttt

}

# Note that we store additional info along the way. For example, in the line
# child$player <- ifelse(node$player == 1, 2, 1) , the player is deferred from the parent Node ,
# and set as an attribute in the Node .

# Our algorithm stops whenever either player has won, or when all 9 fields are taken.
# Whether a player has won is determined by this function:

HasWon <- function(node) {
  t <-
    Traverse(
      node,
      traversal = "ancestor",
      filterFun = function(x)
        ! x$isRoot && x$player == node$player
    )
  mine <- Get(t, "f")
  mineV <- rep(0, 9)
  mineV[mine] <- 1
  mineM <- matrix(mineV, 3, 3, byrow = TRUE)
  result <- any(rowSums(mineM) == 3) ||
    any(colSums(mineM) == 3) ||
    sum(diag(mineM)) == 3 ||
    sum(diag(t(mineM))) == 3
  return (result)
}

# Draw the game board
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
move.human <- function(game) {
  text(4, 0, "Click on screen to move", col = "grey", cex = .7)
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

FlipMatrix <- function(m) {
  apply(m, 2, rev)
}

# Checking whether somebody has won
EvaluateBoard <- function(board) {
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

GameTreeConv.gameToTree <- NULL
GameTreeConv.treeToGame <- NULL

GameTreeConv <- function (initMove) {
  conversion <- if (initMove == 5 || initMove == 7 || initMove == 8) {
    c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  } else if (initMove == 5 || initMove == 1 || initMove == 4) {
    c(7, 4, 1, 8, 5, 2, 9, 6, 3)
  } else if (initMove == 5 || initMove == 2 || initMove == 3) {
    c(9, 8, 7, 6, 5, 4, 3, 2, 1)
  } else if (initMove == 5 || initMove == 6 || initMove == 9) {
    c(7, 6, 9, 2, 5, 8, 1, 4, 7)
  }
  GameTreeConv.gameToTree <<- function (gameCell){
    conversion[gameCell]
  }
  GameTreeConv.treeToGame <<- function (treeCell){
    which(conversion == treeCell)
  }
}

# Main game engine
tic.tac.toe <- function(ttt, player1 = "human", player2 = "computer") {
  game <- rep(0, 9) # Empty board
  winner <- FALSE # Define winner
  player <- 1 # First player
  players <- c(player1, player2)
  draw.board(game)
  currentNode <- ttt
  currentLevel <- 0
  while (0 %in% game &
         !winner) {
    # Keep playing until win or full board
    if (players[(player + 3) %% 3] == "human") {
      # Human player
      move <- move.human(game)
      if (is.null(GameTreeConv.gameToTree)) GameTreeConv(move)
      treeMove <- GameTreeConv.gameToTree(move)
      print(paste("Move=", move, " treeMove=", treeMove))
      for (child in currentNode$children) {
        PrintNode(child, 'Human')
        if (child$f == treeMove) {
          currentNode = child
          currentLevel = child$level
        }
      }
    }
    else {
      # Computer player
      mm <- minimax(game, player, currentNode)
      currentNode = mm$child
      currentLevel = currentNode$level
      move <- GameTreeConv.treeToGame(mm$treeMove)
      print(paste("Move=", move, " treeMove=", mm$treeMove))
    }
    print(paste("currentLevel=", currentLevel))
    game[move] <- player # Change board
    draw.board(game)
    #winner <- max(eval.game(game, 1), abs(eval.game(game, -1))) == 6 # Winner, winner, chicken dinner?
    winner <- EvaluateBoard(game)
    player <- -player # Change player
  }
}

minimax <- function(game, player, tree) {
  PrintNode(tree, "Minimax-tree")
  print(paste("player=", player, " level=", tree$level))
  maxValue <- -Inf
  bestValue <- NULL
  for (child in tree$children) {
    PrintNode(child, "Minimax-child")
    if (child$points1 > maxValue) {
      maxValue <- child$points1
      bestChild <- child
    }
  }
  list(treeMove = bestChild$f, child = bestChild)
}

PrintNode <- function(node, label=""){
  print(
    paste(
      label, ifelse((label != ""), ":", ""),
      "levelName=",
      node$levelName,
      "level=",
      node$level,
      " f=",
      node$f,
      " player=",
      node$player,
      " points1=",
      node$points1,
      " points2=",
      node$points2
    )
  )
}
ttt <- CreateTTT()
tic.tac.toe(ttt)