context("app")

testServer(expr = {
  # Init count... 0
  expect_equal(sum(df()$selected_), 0)
  expect_equal(output$summary, "0 observation(s) selected")

  # Select a region
  session$setInputs(
    `scatters-brush` =
      list(xmin = 0.84909732337501, xmax = 1.289072630224, ymin = 23.228930276968,
        ymax = 29.434482709514, coords_css = list(xmin = 105.5999755859,
            xmax = 176.5999755859, ymin = 172.2000007629, ymax = 236.2000007629),
        coords_img = list(xmin = 263.99993896475, xmax = 441.49993896475,
            ymin = 430.50000190725, ymax = 590.50000190725), img_css_ratio = list(
            x = 2.5, y = 2.5), mapping = list(colour = "selected_",
            x = "drv", y = "hwy"), domain = list(left = 0.4, right = 3.6,
            bottom = 10.4, top = 45.6, discrete_limits = list(x = list(
                "4", "f", "r"))), range = list(left = 82.8198280399793,
            right = 1373.80136986301, bottom = 921.272945432678,
            top = 13.6986301369863), log = list(x = NULL, y = NULL),
        direction = "xy", brushId = "scatters-brush", outputId = "scatters-plot2"
      )
  )

  # Check the value of the reactiveVal `count()`
  expect_equal(sum(df()$selected_), 23)
  expect_equal(output$summary, "23 observation(s) selected")
})
