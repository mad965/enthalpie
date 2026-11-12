test_that("extraire_n_O fonctionne", {
  expect_equal(extraire_n_O("H2O(l)"), 1)
  expect_equal(extraire_n_O("CO2(g)"), 2)
  expect_equal(extraire_n_O("C6H12O6(s)"), 6)
  expect_equal(extraire_n_O("CH4(g)"), 0)
})

test_that("extraire_etat fonctionne", {
  expect_equal(extraire_etat("H2O(l)"), "liquide")
  expect_equal(extraire_etat("CO2(g)"), "gaz")
  expect_equal(extraire_etat("NaCl(s)"), "solide")
})

test_that("le modèle s'entraîne et prédit", {
  modele <- modele_nO_etat(enthalpies_formation_1)
  expect_s3_class(modele, "lm")
  pred <- predire_et_evaluer(modele, enthalpies_formation_1)
  expect_true("dfH_predit" %in% names(pred))
  expect_true("erreur" %in% names(pred))
})