test_that("ajuster_modele_dfH ajuste un modèle linéaire valide", {
  modele <- ajuster_modele_dfH(enthalpies_formation)
  expect_s3_class(modele, "lm")
})

test_that("les prédictions du modèle sont cohérentes avec la réalité", {
  modele <- ajuster_modele_dfH(enthalpies_formation)
  pred <- predict(modele, newdata = enthalpies_formation)
  expect_equal(length(pred), nrow(enthalpies_formation))
  expect_false(any(is.na(pred)))
})