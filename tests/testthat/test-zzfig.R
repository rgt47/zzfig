library(testthat)
library(ggplot2)
library(dplyr)

# Create a sample dataset for testing
test_df <- data.frame(
  rid = rep(1:10, each = 3),
  x = rep(c("bl", "visit1", "visit2"), 10),
  group = rep(c("A", "B", "C"), times = 10),
  y = rnorm(30, mean = 10, sd = 2)
)

# Unit Tests for zzfig

test_that("zzfig generates an observed plot correctly", {
  result <- zzfig(test_df, y ~ x | group, ytype = "obs")
  expect_s3_class(result, "ggplot")
  expect_true("GeomLine" %in% sapply(result$layers, function(layer) class(layer$geom)[1]))
})

test_that("zzfig generates a change plot correctly", {
  result <- zzfig(test_df, y ~ x | group, ytype = "cng")
  expect_s3_class(result, "ggplot")
  expect_true("GeomErrorbar" %in% sapply(result$layers, function(layer) class(layer$geom)[1]))
})

test_that("zzfig returns combined plots for ytype='both'", {
  result <- zzfig(test_df, y ~ x | group, ytype = "both")
  expect_s3_class(result, "ggplot")
  # Check for at least two ggplot layers
  expect_true(length(result$layers) > 1)
})

test_that("zzfig validates inputs and throws an error for missing columns", {
  bad_df <- test_df %>% select(-group)
  expect_error(
    zzfig(bad_df, y ~ x | group),
    "The following required columns are missing in `df`"
  )
})

test_that("validate_inputs enforces correct ytype and etype", {
  expect_error(validate_inputs(test_df, y ~ x | group, NULL, "invalid", "bar"), "`ytype` must be 'obs', 'cng', or 'both'.")
  expect_error(validate_inputs(test_df, y ~ x | group, NULL, "obs", "invalid"), "`etype` must be 'bar' or 'band'.")
})

test_that("summarize_data computes expected summary statistics", {
  result <- summarize_data(test_df, "y", "x", "group", c("group", "x"))
  expect_true(all(c("mn", "cng_mn", "sd", "N", "se", "bl", "bu") %in% colnames(result)))
  expect_equal(nrow(result), 9)  # 3 groups * 3 x-values
})

test_that("add_common_layers applies consistent styling", {
  p <- ggplot(test_df, aes(x = x, y = y, group = group, color = group))
  result <- add_common_layers(p, "group", position_dodge(0.15), "bar")
  expect_s3_class(result, "ggplot")
  expect_true("GeomErrorbar" %in% sapply(result$layers, function(layer) class(layer$geom)[1]))
})

test_that("zzfig handles faceting correctly", {
  result <- zzfig(test_df, y ~ x | group, facet_form = x ~ group)
  expect_s3_class(result, "ggplot")
  expect_true(!is.null(result$facet))
})

test_that("zzfig accepts custom colors and linetypes", {
  custom_colors <- c("red", "green", "blue")
  custom_linetypes <- c("dotdash", "twodash", "longdash")
  result <- zzfig(test_df, y ~ x | group, colors = custom_colors, linetypes = custom_linetypes)
  scale <- result$scales$scales[[1]]
  expect_equal(scale$palette()(3), custom_colors)
})

test_that("zzfig handles empty datasets gracefully", {
  empty_df <- test_df[0, ]
  expect_error(zzfig(empty_df, y ~ x | group), "The following required columns are missing")
})
