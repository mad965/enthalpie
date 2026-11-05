test_that("loi_de_hess() retrouve la formation de CO", {
  h <- loi_de_hess(
    c("C(s) + O2(g) -> CO2(g)", "CO(g) + 1/2 O2(g) -> CO2(g)"),
    enthalpies = c(-393.51, -282.98),
    coefficients = c(1, -1)
  )
  expect_s3_class(h, "hess")
  expect_equal(format(h$reaction), "C(s) + 1/2 O2(g) -> CO(g)")
  expect_equal(h$dH, -110.53)
  expect_equal(h$dH, dfH("CO"))
  expect_equal(nrow(h$etapes), 2)
})

test_that("loi_de_hess() retrouve la formation du methane par les combustions", {
  h <- loi_de_hess(
    c("C(s) + O2(g) -> CO2(g)",
      "H2(g) + 1/2 O2(g) -> H2O(l)",
      "CH4(g) + 2 O2(g) -> CO2(g) + 2 H2O(l)"),
    enthalpies = c(-393.51, -285.83, -890.30),
    coefficients = c(1, 2, -1)
  )
  expect_equal(format(h$reaction), "C(s) + 2 H2(g) -> CH4(g)")
  expect_equal(h$dH, -74.87)
})

test_that("loi_de_hess() recycle un coefficient unique et affiche le bilan", {
  h <- loi_de_hess(c("H2(g) + 1/2 O2(g) -> H2O(l)", "H2O(l) -> H2O(g)"),
                   enthalpies = c(-285.83, 44.00))
  expect_equal(format(h$reaction), "H2(g) + 1/2 O2(g) -> H2O(g)")
  expect_equal(h$dH, -241.83)
  expect_output(print(h), "-241.83", fixed = TRUE)
  expect_output(print(h), "Hess")
})

test_that("loi_de_hess() verifie ses arguments", {
  expect_error(loi_de_hess("H2 -> 2 H", enthalpies = c(1, 2)), "autant")
  expect_error(loi_de_hess("H2(g) -> H2(g)", enthalpies = 0), "compensent")
})

test_that("la loi de Hess est coherente avec enthalpie_reaction()", {
  r <- enthalpie_reaction("CH4(g) + 2 O2(g) -> CO2(g) + 2 H2O(l)")
  h <- loi_de_hess(
    c("C(s) + O2(g) -> CO2(g)", "H2(g) + 1/2 O2(g) -> H2O(l)",
      "C(s) + 2 H2(g) -> CH4(g)"),
    enthalpies = c(dfH("CO2"), dfH("H2O"), dfH("CH4")),
    coefficients = c(1, 2, -1)
  )
  expect_equal(h$dH, r$dH)
})
