context("plot-coordmap")
library(ggplot2)

# Sort a list by the names of its keys
sortList <- function(x) {
  x[sort(names(x))]
}

test_that("ggplot coordmap", {
  dat <- data.frame(xvar = c(0, 5), yvar = c(10, 20))

  tmpfile <- tempfile("test-shiny", fileext = ".png")
  on.exit(rm(tmpfile))

  # Basic scatterplot
  p <- ggplot(dat, aes(xvar, yvar)) + geom_point() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  png(tmpfile)
  m <- getGgplotCoordmap(p, 1)
  dev.off()

  # Check mapping vars
  expect_equal(m[[1]]$mapping, list(x = "xvar", y = "yvar"))
  # Check domain
  expect_equal(
    sortList(m[[1]]$domain),
    sortList(list(left=0, right=5, bottom=10, top=20))
  )


  # Scatterplot where aes() is declared in geom
  p <- ggplot(dat, aes(xvar)) + geom_point(aes(y=yvar))
  png(tmpfile)
  m <- getGgplotCoordmap(p, 1)
  dev.off()

  # Check mapping vars
  expect_equal(m[[1]]$mapping, list(x = "xvar", y = "yvar"))


  # Plot with computed variable (histogram)
  p <- ggplot(dat, aes(xvar)) + geom_histogram(binwidth=1)
  png(tmpfile)
  m <- getGgplotCoordmap(p, 1)
  dev.off()

  # Check mapping vars - no value for y
  expect_equal(m[[1]]$mapping, list(x = "xvar", y = NULL))
})


test_that("ggplot coordmap with facet_wrap", {
  dat <- data.frame(xvar = c(0, 5, 10), yvar = c(10, 20, 30),
                    g = c("a", "b", "c"))

  tmpfile <- tempfile("test-shiny", fileext = ".png")
  on.exit(rm(tmpfile))

  # facet_wrap
  p <- ggplot(dat, aes(xvar, yvar)) + geom_point() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    facet_wrap(~ g, ncol = 2)
  png(tmpfile)
  m <- getGgplotCoordmap(p, 1)
  dev.off()

  # Should have 3 panels
  expect_equal(length(m), 3)
  expect_equal(m[[1]]$panel, 1)
  expect_equal(m[[1]]$row, 1)
  expect_equal(m[[1]]$col, 1)
  expect_equal(m[[2]]$panel, 2)
  expect_equal(m[[2]]$row, 1)
  expect_equal(m[[2]]$col, 2)
  expect_equal(m[[3]]$panel, 3)
  expect_equal(m[[3]]$row, 2)
  expect_equal(m[[3]]$col, 1)

  # Check mapping vars
  expect_equal(m[[1]]$mapping, list(x = "xvar", y = "yvar", panelvar1 = "g"))
  expect_equal(m[[1]]$mapping, m[[2]]$mapping)
  expect_equal(m[[2]]$mapping, m[[3]]$mapping)
  # Check domain
  expect_equal(
    sortList(m[[1]]$domain),
    sortList(list(left=0, right=10, bottom=10, top=30))
  )
  expect_equal(sortList(m[[1]]$domain), sortList(m[[2]]$domain))
  expect_equal(sortList(m[[2]]$domain), sortList(m[[3]]$domain))

  # Check panel vars
  factor_vals <- dat$g
  expect_equal(m[[1]]$panel_vars, list(panelvar1 = factor_vals[1]))
  expect_equal(m[[2]]$panel_vars, list(panelvar1 = factor_vals[2]))
  expect_equal(m[[3]]$panel_vars, list(panelvar1 = factor_vals[3]))
})


test_that("ggplot coordmap with facet_grid", {
  dat <- data.frame(xvar = c(0, 5, 10), yvar = c(10, 20, 30),
                    g = c("a", "b", "c"))

  tmpfile <- tempfile("test-shiny", fileext = ".png")
  on.exit(rm(tmpfile))

  p <- ggplot(dat, aes(xvar, yvar)) + geom_point() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  # facet_grid horizontal
  p1 <- p + facet_grid(. ~ g)
  png(tmpfile)
  m <- getGgplotCoordmap(p1, 1)
  dev.off()

  # Should have 3 panels
  expect_equal(length(m), 3)
  expect_equal(m[[1]]$panel, 1)
  expect_equal(m[[1]]$row, 1)
  expect_equal(m[[1]]$col, 1)
  expect_equal(m[[2]]$panel, 2)
  expect_equal(m[[2]]$row, 1)
  expect_equal(m[[2]]$col, 2)
  expect_equal(m[[3]]$panel, 3)
  expect_equal(m[[3]]$row, 1)
  expect_equal(m[[3]]$col, 3)

  # Check mapping vars
  expect_equal(m[[1]]$mapping, list(x = "xvar", y = "yvar", panelvar1 = "g"))
  expect_equal(m[[1]]$mapping, m[[2]]$mapping)
  expect_equal(m[[2]]$mapping, m[[3]]$mapping)
  # Check domain
  expect_equal(
    sortList(m[[1]]$domain),
    sortList(list(left=0, right=10, bottom=10, top=30))
  )
  expect_equal(sortList(m[[1]]$domain), sortList(m[[2]]$domain))
  expect_equal(sortList(m[[2]]$domain), sortList(m[[3]]$domain))

  # Check panel vars
  factor_vals <- dat$g
  expect_equal(m[[1]]$panel_vars, list(panelvar1 = factor_vals[1]))
  expect_equal(m[[2]]$panel_vars, list(panelvar1 = factor_vals[2]))
  expect_equal(m[[3]]$panel_vars, list(panelvar1 = factor_vals[3]))


  # facet_grid vertical
  p1 <- p + facet_grid(g ~ .)
  png(tmpfile)
  m <- getGgplotCoordmap(p1, 1)
  dev.off()

  # Should have 3 panels
  expect_equal(length(m), 3)
  expect_equal(m[[1]]$panel, 1)
  expect_equal(m[[1]]$row, 1)
  expect_equal(m[[1]]$col, 1)
  expect_equal(m[[2]]$panel, 2)
  expect_equal(m[[2]]$row, 2)
  expect_equal(m[[2]]$col, 1)
  expect_equal(m[[3]]$panel, 3)
  expect_equal(m[[3]]$row, 3)
  expect_equal(m[[3]]$col, 1)

  # Check mapping vars
  expect_equal(m[[1]]$mapping, list(x = "xvar", y = "yvar", panelvar1 = "g"))
  expect_equal(m[[1]]$mapping, m[[2]]$mapping)
  expect_equal(m[[2]]$mapping, m[[3]]$mapping)
  # Check domain
  expect_equal(
    sortList(m[[1]]$domain),
    sortList(list(left=0, right=10, bottom=10, top=30))
  )
  expect_equal(sortList(m[[1]]$domain), sortList(m[[2]]$domain))
  expect_equal(sortList(m[[2]]$domain), sortList(m[[3]]$domain))

  # Check panel vars
  factor_vals <- dat$g
  expect_equal(m[[1]]$panel_vars, list(panelvar1 = factor_vals[1]))
  expect_equal(m[[2]]$panel_vars, list(panelvar1 = factor_vals[2]))
  expect_equal(m[[3]]$panel_vars, list(panelvar1 = factor_vals[3]))
})


test_that("ggplot coordmap with 2D facet_grid", {
  dat <- data.frame(xvar = c(0, 5, 10, 15), yvar = c(10, 20, 30, 40),
                    g = c("a", "b"), h = c("i", "j"))

  tmpfile <- tempfile("test-shiny", fileext = ".png")
  on.exit(rm(tmpfile))

  p <- ggplot(dat, aes(xvar, yvar)) + geom_point() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  p1 <- p + facet_grid(g ~ h)
  png(tmpfile)
  m <- getGgplotCoordmap(p1, 1)
  dev.off()

  # Should have 4 panels
  expect_equal(length(m), 4)
  expect_equal(m[[1]]$panel, 1)
  expect_equal(m[[1]]$row, 1)
  expect_equal(m[[1]]$col, 1)
  expect_equal(m[[2]]$panel, 2)
  expect_equal(m[[2]]$row, 1)
  expect_equal(m[[2]]$col, 2)
  expect_equal(m[[3]]$panel, 3)
  expect_equal(m[[3]]$row, 2)
  expect_equal(m[[3]]$col, 1)
  expect_equal(m[[4]]$panel, 4)
  expect_equal(m[[4]]$row, 2)
  expect_equal(m[[4]]$col, 2)

  # Check mapping vars
  expect_equal(m[[1]]$mapping, list(x = "xvar", y = "yvar", panelvar1 = "h", panelvar2 = "g"))
  expect_equal(m[[1]]$mapping, m[[2]]$mapping)
  expect_equal(m[[2]]$mapping, m[[3]]$mapping)
  expect_equal(m[[4]]$mapping, m[[4]]$mapping)
  # Check domain
  expect_equal(
    sortList(m[[1]]$domain),
    sortList(list(left=0, right=15, bottom=10, top=40))
  )
  expect_equal(sortList(m[[1]]$domain), sortList(m[[2]]$domain))
  expect_equal(sortList(m[[2]]$domain), sortList(m[[3]]$domain))
  expect_equal(sortList(m[[3]]$domain), sortList(m[[4]]$domain))

  # Check panel vars
  expect_equal(m[[1]]$panel_vars, list(panelvar1 = dat$h[1], panelvar2 = dat$g[1]))
  expect_equal(m[[2]]$panel_vars, list(panelvar1 = dat$h[2], panelvar2 = dat$g[1]))
  expect_equal(m[[3]]$panel_vars, list(panelvar1 = dat$h[1], panelvar2 = dat$g[2]))
  expect_equal(m[[4]]$panel_vars, list(panelvar1 = dat$h[2], panelvar2 = dat$g[2]))
})
