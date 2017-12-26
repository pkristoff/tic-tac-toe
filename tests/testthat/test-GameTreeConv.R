

context("GameTreeConv")

test_that("Test that conversions between game and tree work correctly.", {

  for (i in 1:9) {
    conversion <- GameTreeConv(i)
    for (gamecell in 1:9) {
      treecell <- conversion('toTree', gamecell)
      if (conversion('toGame', treecell) != gamecell)
        print(paste(
          "broken",
          "i=",
          i,
          "gamecell=",
          gamecell,
          "treecell=",
          treecell
        ))
      expect_equal(conversion('toTree', gamecell), treecell)
      expect_equal(conversion('toGame', treecell), gamecell)
    }
  }

})