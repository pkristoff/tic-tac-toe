# terminology
# X always goes first
# meFirst means I am X
# oppFirst means I am O

GenerateTable <- function () {
  emptyBoard <- matrix(0, 3, 3)
  x <- -1
  o <- 1

  GenerateTree <-
    function(board,
             isXTurn, # turn for subnode
             level,
             curRow,
             curCol) {
      if (isXTurn) {
        sideMakingSubMove <- x
      } else {
        sideMakingSubMove <- o
      }
      winner <- EvaluateBoard(board)
      if (winner == 0) {
        if (isXTurn) { #subNode
          bestValueMeX = -Inf
          bestValueOppX = +Inf
        } else {
          bestValueMeX = +Inf
          bestValueOppX = -Inf
        }
        subNodes = list()
        for (rowNum in 1:3) {
          for (colNum in 1:3) {
            if (board[rowNum, colNum] == 0) {
              nextBoard = board
              nextBoard[rowNum, colNum] <- sideMakingSubMove
              subNode = GenerateTree(nextBoard,!isXTurn,
                                     level + 1,
                                     rowNum,
                                     colNum)
              subNodes <- c(subNodes, list(subNode))
              if (isXTurn) {
                # maximizing player
                if (subNode$bestValueMeX > bestValueMeX) {
                  bestValueMeX <- subNode$bestValueMeX
                }
                # minimizing player
                if (subNode$bestValueOppX < bestValueOppX) {
                  bestValueOppX <- subNode$bestValueOppX
                }
              } else {
                # minimizing player
                if (subNode$bestValueMeX < bestValueMeX) {
                  bestValueMeX <- subNode$bestValueMeX
                }
                # maximizing player
                if (subNode$bestValueOppX > bestValueOppX) {
                  bestValueOppX <- subNode$bestValueOppX
                }
              }
            }
          }
        }
        # if (bestValue != 0) {
        #   print(paste("GLOBAL level=", level, " bestValue=", bestValue))
        # }
        # if subNodes is empty then we ended in a draw.
        CreateNode(
          board,
          level,
          sideMakingSubMove*-1,
          curRow,
          curCol,
          bestValueMeX*0.5,
          bestValueOppX*0.5,
          subNodes
        )
      } else {
        # leaf node with winner
        if (! isXTurn) {
          value = winner * -1
        } else {
          value = winner
        }
        # print(paste("Winner=", winner, " isXTurn=", isXTurn))
        # print(DisplayBoard(board))
        CreateNode(board,
                   level,
                   sideMakingSubMove*-1,
                   curRow,
                   curCol,
                   value,
                   value * -1,
                   list())
      }
    }
  CreateNode <- function(board,
                         level,
                         sideMadeThisMove,
                         rootRow,
                         rootCol,
                         bestValueMeX,
                         bestValueOppX,
                         subNodes) {
    list(
      board = board,
      level = level,
      sideMadeThisMove = sideMadeThisMove,
      # the side that made this move (rootCol, rootRow)
      rootCol = rootCol,
      rootRow = rootRow,
      bestValueMeX = bestValueMeX,
      bestValueOppX = bestValueOppX,
      tree = subNodes
    )
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

  PrintTree <- function(node, space = "") {
    # print(node)
    cat(
      space,
      "level=",
      node$level,
      "rootPick=[",
      node$rootRow,
      node$rootCol,
      "]",
      "side=",
      node$side,
      " numSubNodes=",
      length(node$tree),
      " bestValue=",
      node$bestValue,
      "\n"
    )
    treeLen = length(node$tree)
    if (treeLen > 0) {
      for (i in 1:treeLen) {
        PrintTree(node$tree[[i]], append(space, "-"))
      }
    } else {
      # print(DisplayBoard(node$board))
      node$board
    }
  }
  # print(emptyBoard)
  # board, move, level, side, tree
  # node <- CreateTree(CreateNode(emptyBoard, 0, 1, 0, 0))
  # print(paste("Node Count = ", (EvalTree(node))))
  # PrintTree(node)
  # PrintTree(node)


  node <- GenerateTree(emptyBoard,
                       TRUE,
                       0,
                       0,
                       0)
  # PrintNode(node)
  node

}
