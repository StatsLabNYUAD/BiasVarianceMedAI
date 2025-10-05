test_that("app function exists", {
  expect_true(exists("run_app", where = asNamespace("BiasVarianceMedAI")))
})
