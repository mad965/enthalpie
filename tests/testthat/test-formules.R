test_that("compter_elements() lit les formules simples et a parentheses", {
  expect_equal(compter_elements("H2O"), c(H = 2, O = 1))
  expect_equal(compter_elements("CO2"), c(C = 1, O = 2))
  expect_equal(compter_elements("C6H12O6"), c(C = 6, H = 12, O = 6))
  expect_equal(compter_elements("Ca(OH)2"), c(Ca = 1, O = 2, H = 2))
  expect_equal(compter_elements("Al2(SO4)3"), c(Al = 2, S = 3, O = 12))
})

test_that("compter_elements() cumule un element qui revient et ignore l'etat", {
  expect_equal(compter_elements("C2H5OH"), c(C = 2, H = 6, O = 1))
  expect_equal(compter_elements("H2O(l)"), c(H = 2, O = 1))
  expect_equal(compter_elements(" NaCl (s)"), c(Na = 1, Cl = 1))
})

test_that("compter_elements() gere les parentheses imbriquees", {
  expect_equal(compter_elements("Fe(OH(H2O)2)3"),
               c(Fe = 1, O = 9, H = 15))
})

test_that("compter_elements() refuse les formules invalides", {
  expect_error(compter_elements("H2O)"), "Parenth")
  expect_error(compter_elements("(OH"), "Parenth")
  expect_error(compter_elements("h2o"), "invalide")
  expect_error(compter_elements("H2O+"), "invalide")
  expect_error(compter_elements(""), "vide")
  expect_error(compter_elements(c("H2", "O2")), "longueur 1")
  expect_error(compter_elements(NA_character_), "longueur 1")
})

test_that("masse_molaire() est coherente avec les masses atomiques", {
  expect_equal(masse_molaire("H2O"), 18.015)
  expect_equal(masse_molaire("CO2"), 44.009)
  expect_equal(masse_molaire("C6H12O6"), 180.156)
  expect_equal(masse_molaire(c("H2", "CH4")), c(2.016, 16.043))
  expect_error(masse_molaire("XeF6"), "absent")
  expect_error(masse_molaire(18), "cha")
})
