GenerateTable <- function (mySide) {
  emptyBoard <- matrix(0, 3, 3)

  GenerateTree <-
    function(board,
             sideMakingNextMove,
             level,
             curRow,
             curCol) {
      sideMadeThisMove <- sideMakingNextMove * -1
      winner <- EvaluateBoard(board)
      if (winner == 0) {
        bestValue = 0
        subNodes = list()
        for (rowNum in 1:3) {
          for (colNum in 1:3) {
            if (board[rowNum, colNum] == 0) {
              nextBoard = board
              nextBoard[rowNum, colNum] <- sideMakingNextMove
              subNode = GenerateTree(nextBoard,
                                     sideMakingNextMove * -1,
                                     level + 1,
                                     rowNum,
                                     colNum)
              subNodes <- c(subNodes, list(subNode))
              if (mySide == sideMadeThisMove) {

                if (subNode$bestValue < bestValue)
                  # print(paste("Min's turn level=", level, " bestValue=", bestValue, " subNode$bestValue=", subNode$bestValue))
                  bestValue <<- subNode$bestValue
              } else {
                # max's turn - min just moved
                if (subNode$bestValue > bestValue)
                  # print(paste("Max's turn level=", level, " bestValue=", bestValue, " subNode$bestValue=", subNode$bestValue))
                  bestValue <<- subNode$bestValue
              }
              if (subNode$bestValue != 0) {
                # print(paste("LOCAL level=", level, " bestValue=", bestValue, " subNode$bestValue=", subNode$bestValue))
              }
            }
          }
        }
        if (bestValue != 0) {
          print(paste("GLOBAL level=", level, " bestValue=", bestValue))
        }
        # if subNodes is empty then we ended in a draw.
        CreateNode(board,
                   level,
                   sideMadeThisMove,
                   curRow,
                   curCol,
                   bestValue,
                   subNodes)
      } else {
        # leaf node with winner
        CreateNode(board, level, sideMadeThisMove, curRow, curCol, winner, list())
      }
    }
  CreateNode <- function(board,
                         level,
                         sideMadeThisMove,
                         rootRow,
                         rootCol,
                         bestValue,
                         subNodes) {
    list(
      board = board,
      level = level,
      sideMadeThisMove = sideMadeThisMove,
      # the side that made this move (rootCol, rootRow)
      rootCol = rootCol,
      rootRow = rootRow,
      bestValue = bestValue,
      tree = subNodes
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
      print(DisplayBoard(node$board))
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
                       mySide,
                       0,
                       0,
                       0)
  node

}
