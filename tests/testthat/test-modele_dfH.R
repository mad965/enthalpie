# tests/testthat/test-modele_dfH.R

test_that("ajuster_modele_dfH ajuste un modèle linéaire valide", {
  modele <- ajuster_modele_dfH()
  
  # Vérifier la classe de l'objet retourné
  expect_s3_class(modele, "lm")
  
  # Intercept + nC + nH + nO = 4 coefficients
  expect_length(coef(modele), 4)
  
  # R² élevé attendu sur les composés C-H-O
  expect_gt(summary(modele)$r.squared, 0.90)
})

test_that("les prédictions du modèle sont cohérentes avec la réalité", {
  df <- preparer_donnees_modele()
  modele <- ajuster_modele_dfH()
  
  predictions <- predict(modele, newdata = df)
  erreurs <- abs(predictions - df$dfH)
  
  # Erreur moyenne raisonnable (< 50 kJ/mol pour un modèle additif simple)
  expect_lt(mean(erreurs), 50)
})

test_that("tracer_modele_dfH retourne un objet ggplot", {
  modele <- ajuster_modele_dfH()
  graphe <- tracer_modele_dfH(modele)
  
  expect_s3_class(graphe, "ggplot")
  expect_gte(length(graphe$layers), 2)  # points + ligne y=x
})

test_that("le modèle prédit raisonnablement un composé retiré (glucose)", {
  df <- preparer_donnees_modele()
  
  # Retirer le glucose de l'entraînement
  df_train <- subset(df, espece != "C6H12O6(s)")
  df_test  <- subset(df, espece == "C6H12O6(s)")
  
  modele_train <- lm(dfH ~ nC + nH + nO, data = df_train)
  pred <- predict(modele_train, newdata = df_test)
  
  # Tolérance large : le glucose est un cas difficile
  expect_lt(abs(pred - df_test$dfH), 100)
})