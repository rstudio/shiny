context("plot-coordmap")
library(ggplot2)

# Sort a list by the names of its keys
sortList <- function(x) {
  x[sort(names(x))]
}

# This will create print.ggplot in the current environment
print.ggplot <- custom_print.ggplot


test_that("ggplot coordmap", {
  dat <- data.frame(xvar = c(0, 5), yvar = c(10, 20))

  tmpfile <- tempfile("test-shiny", fileext = ".png")
  on.exit(rm(tmpfile))

  # Basic scatterplot
  p <- ggplot(dat, aes(xvar, yvar)) + geom_point() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  png(tmpfile, width = 500, height = 500)
  m <- getGgplotCoordmap(print(p), 1, 72)
  dev.off()

  # Check mapping vars
  expect_equal(m[[1]]$mapping, list(x = "xvar", y = "yvar"))
  # Check domain
  expect_equal(
    sortList(m[[1]]$domain),
    sortList(list(left=0, right=5, bottom=10, top=20))
  )
  # Check for no log bases
  expect_equal(
    sortList(m[[1]]$log),
    sortList(list(x=NULL, y=NULL))
  )
  # panel_vars should be an empty named list
  expect_identical(m[[1]]$panel_vars, list(a=1)[0])
  # Sanity check for ranges. Checking exact range values isn't feasible due to
  # variations in graphics devices, and possible changes to positioning in
  # ggplot2.
  expect_true(m[[1]]$range$left    >  20 && m[[1]]$range$left   <  70)
  expect_true(m[[1]]$range$right   > 480 && m[[1]]$range$right  < 499)
  expect_true(m[[1]]$range$bottom  > 450 && m[[1]]$range$bottom < 490)
  expect_true(m[[1]]$range$top     > 1   && m[[1]]$range$top    <  20)


  # Scatterplot where aes() is declared in geom
  p <- ggplot(dat, aes(xvar)) + geom_point(aes(y=yvar))
  png(tmpfile)
  m <- getGgplotCoordmap(print(p), 1, 72)
  dev.off()

  # Check mapping vars
  expect_equal(sortList(m[[1]]$mapping), list(x = "xvar", y = "yvar"))


  # Plot with an expression in aes, and a computed variable (histogram)
  p <- ggplot(dat, aes(xvar/2)) + geom_histogram(binwidth=1)
  png(tmpfile)
  m <- getGgplotCoordmap(print(p), 1, 72)
  dev.off()

  # Check mapping vars - no value for y
  expect_equal(sortList(m[[1]]$mapping), list(x = "xvar/2", y = NULL))
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
  m <- getGgplotCoordmap(print(p), 1, 72)
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
  m <- getGgplotCoordmap(print(p1), 1, 72)
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
  m <- getGgplotCoordmap(print(p1), 1, 72)
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
  m <- getGgplotCoordmap(print(p1), 1, 72)
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


test_that("ggplot coordmap with various data types", {
  tmpfile <- tempfile("test-shiny", fileext = ".png")
  on.exit(rm(tmpfile))

  # Factors
  dat <- expand.grid(xvar = letters[1:3], yvar = LETTERS[1:4])
  p <- ggplot(dat, aes(xvar, yvar)) + geom_point() +
    scale_x_discrete(expand = c(0 ,0)) +
    scale_y_discrete(expand = c(0, 0))
  png(tmpfile)
  m <- getGgplotCoordmap(print(p), 1, 72)
  dev.off()

  # Check domain
  expect_equal(
    sortList(m[[1]]$domain),
    sortList(list(left=1, right=3, bottom=1, top=4))
  )

  # Dates and date-times
  dat <- data.frame(
    xvar = as.Date("2016-09-27") + c(0, 10),
    yvar = as.POSIXct("2016-09-27 09:00:00", origin = "1960-01-01", tz = "GMT") + c(3600, 0)
  )
  p <- ggplot(dat, aes(xvar, yvar)) + geom_point() +
    scale_x_date(expand = c(0 ,0)) +
    scale_y_datetime(expand = c(0, 0))
  png(tmpfile)
  m <- getGgplotCoordmap(print(p), 1, 72)
  dev.off()

  # Check domain
  expect_equal(
    sortList(m[[1]]$domain),
    sortList(list(
      left   = as.numeric(dat$xvar[1]),
      right  = as.numeric(dat$xvar[2]),
      bottom = as.numeric(dat$yvar[2]),
      top    = as.numeric(dat$yvar[1])
    ))
  )
})

test_that("ggplot coordmap with various scales and coords", {
  tmpfile <- tempfile("test-shiny", fileext = ".png")
  on.exit(rm(tmpfile))

  # Reversed scales
  dat <- data.frame(xvar = c(0, 5), yvar = c(10, 20))
  p <- ggplot(dat, aes(xvar, yvar)) + geom_point() +
    scale_x_continuous(expand = c(0 ,0)) +
    scale_y_reverse(expand = c(0, 0))
  png(tmpfile)
  m <- getGgplotCoordmap(print(p), 1, 72)
  dev.off()

  # Check domain (y reversed)
  expect_equal(
    sortList(m[[1]]$domain),
    sortList(list(left=0, right=5, bottom=20, top=10))
  )

  # coord_flip
  p <- ggplot(dat, aes(xvar, yvar)) + geom_point() +
    scale_x_continuous(expand = c(0 ,0)) +
    scale_y_continuous(expand = c(0 ,0)) +
    coord_flip()
  png(tmpfile)
  m <- getGgplotCoordmap(print(p), 1, 72)
  dev.off()

  # Check mapping vars
  expect_equal(m[[1]]$mapping, list(x = "yvar", y = "xvar"))
  # Check domain (y reversed)
  expect_equal(
    sortList(m[[1]]$domain),
    sortList(list(left=10, right=20, bottom=0, top=5))
  )

  # Log scales and log coord transformations
  dat <- data.frame(xvar = c(10^-1, 10^3), yvar = c(2^-2, 2^4))
  p <- ggplot(dat, aes(xvar, yvar)) + geom_point() +
    scale_x_log10(expand = c(0 ,0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_trans(y = "log2")
  png(tmpfile)
  m <- getGgplotCoordmap(print(p), 1, 72)
  dev.off()

  # Check log bases
  expect_equal(
    sortList(m[[1]]$log),
    sortList(list(x=10, y=2))
  )
  # Check domains
  expect_equal(
    sortList(m[[1]]$domain),
    sortList(list(left=-1, right=3, bottom=-2, top=4))
  )
})
