# tests/testthat/test-modele_CH_etat.R

test_that("le modèle s'entraîne et prédit", {
  modele <- modele_CH_etat(enthalpies_formation)
  expect_s3_class(modele, "lm")
  
  pred <- predire_et_evaluer(modele, enthalpies_formation_1)
  expect_true("dfH_predit" %in% names(pred))
  expect_true("erreur" %in% names(pred))
})

test_that("les prédictions sont numériques et sans NA", {
  modele <- modele_CH_etat(enthalpies_formation)
  pred <- predire_et_evaluer(modele, enthalpies_formation_1)
  expect_type(pred$dfH_predit, "double")
  expect_false(any(is.na(pred$dfH_predit)))
  expect_false(any(is.na(pred$erreur)))
})

test_that("le modèle capture l'effet de l'état physique", {
  modele <- modele_CH_etat(enthalpies_formation)
  coefs <- coef(modele)
  # Les solides devraient avoir une enthalpie plus basse que les gaz
  expect_true(coefs["etat_normsolide"] < 0)
})