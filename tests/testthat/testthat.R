# https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf

# library(CreateTTTTree)
# source("tic-tac-tow-dataSet.R")

context("tic.tac.toe")

test_that("Test that conversions between game and tree work correctly.", {
  # conversionMap145 = c(7, 4, 1, 8, 5, 2, 9, 6, 3)

  for (i in 1:9){
    conversion <- GameTreeConv(i)
    for (gamecell in 1:9){
      treecell <- conversion('toTree', gamecell)
      if (conversion('toGame', treecell) != gamecell) print(paste("broken","i=",i, "gamecell=",gamecell, "treecell=",treecell ))
      expect_equal(conversion('toTree', gamecell), treecell)
      expect_equal(conversion('toGame', treecell), gamecell)
      }
  }

  # conversion <- GameTreeConv(1)
  #
  # gamecell <- 1
  # for (treecell in conversionMap145){
  #   expect_equal(conversion('toTree', gamecell), treecell)
  #   expect_equal(conversion('toGame', treecell), gamecell)
  #   gamecell <- gamecell+1
  # }

})
