test_that("kirchhoff() avec un Delta Cp constant", {
  expect_equal(kirchhoff(-92, dCp = -20, T = 298.15), -92)
  expect_equal(kirchhoff(-92, dCp = -20, T = c(298.15, 500, 700)),
               -92 + -20 * (c(298.15, 500, 700) - 298.15) / 1000)
  expect_equal(kirchhoff(-92, dCp = 0, T = 1000), -92)
})

test_that("kirchhoff() avec un Delta Cp variable", {
  dcp <- function(t) -25 + 0.01 * t
  attendu <- -92 + (-25 * (600 - 298.15) + 0.005 * (600^2 - 298.15^2)) / 1000
  expect_equal(kirchhoff(-92, dCp = dcp, T = 600), attendu, tolerance = 1e-7)
  expect_equal(kirchhoff(-92, dCp = dcp, T = 298.15), -92)
  # temperature plus basse que T0
  bas <- -92 + (-25 * (250 - 298.15) + 0.005 * (250^2 - 298.15^2)) / 1000
  expect_equal(kirchhoff(-92, dCp = dcp, T = 250), bas, tolerance = 1e-7)
})

test_that("kirchhoff() verifie ses arguments", {
  expect_error(kirchhoff("a", dCp = 1, T = 300), "nombre")
  expect_error(kirchhoff(-92, dCp = "x", T = 300), "nombre ou une fonction")
  expect_error(kirchhoff(-92, dCp = c(1, 2), T = 300), "nombre ou une fonction")
})

test_that("born_haber() calcule l'enthalpie de reseau de NaCl", {
  bh <- born_haber(dfH = -411.12, sublimation = 107.5, ionisation = 495.8,
                   atomisation = 121.7, affinite = -349.0)
  expect_s3_class(bh, "born_haber")
  expect_equal(bh$reseau, -787.12)
  expect_equal(bh$dfH, -411.12)
  expect_equal(nrow(bh$etapes), 5)
  expect_equal(sum(bh$etapes$dH), bh$dfH)
})

test_that("born_haber() fait aussi le calcul inverse", {
  bh <- born_haber(reseau = -787.12, sublimation = 107.5, ionisation = 495.8,
                   atomisation = 121.7, affinite = -349.0)
  expect_equal(bh$dfH, -411.12)
})

test_that("born_haber() additionne les ionisations multiples", {
  # MgO : on ne verifie ici que le principe de la somme des termes multiples
  bh <- born_haber(dfH = -601.24, sublimation = 147.1,
                   ionisation = c(737.7, 1450.7), atomisation = 249.2,
                   affinite = c(-141, 798))
  expect_equal(bh$etapes$dH[2], 737.7 + 1450.7)
  expect_equal(sum(bh$etapes$dH), -601.24)
})

test_that("born_haber() exige exactement dfH ou reseau", {
  expect_error(born_haber(sublimation = 1, ionisation = 1, atomisation = 1,
                          affinite = 1), "exactement")
  expect_error(born_haber(dfH = 1, reseau = 1, sublimation = 1, ionisation = 1,
                          atomisation = 1, affinite = 1), "exactement")
})

test_that("print() et plot() de born_haber() fonctionnent", {
  bh <- born_haber(dfH = -411.12, sublimation = 107.5, ionisation = 495.8,
                   atomisation = 121.7, affinite = -349.0)
  expect_output(print(bh), "-787.12", fixed = TRUE)
  expect_output(print(bh), "-411.12", fixed = TRUE)
  pdf(NULL)
  expect_no_error(plot(bh))
  invisible(dev.off())
})

test_that("enthalpie_liaisons() estime une enthalpie de reaction", {
  e <- c("H-H" = 436, "Cl-Cl" = 243, "H-Cl" = 432)
  r <- enthalpie_liaisons(c("H-H" = 1, "Cl-Cl" = 1), c("H-Cl" = 2), e)
  expect_s3_class(r, "enthalpie_liaisons")
  expect_equal(r$rompues, 679)
  expect_equal(r$formees, 864)
  expect_equal(r$dH, -185)
  expect_output(print(r), "-185.00", fixed = TRUE)
})

test_that("enthalpie_liaisons() verifie ses arguments", {
  e <- c("H-H" = 436)
  expect_error(enthalpie_liaisons(c("H-H" = 1), c("H-Cl" = 2), e), "manquante")
  expect_error(enthalpie_liaisons(c(1, 2), c("H-H" = 1), e), "nomm")
})
