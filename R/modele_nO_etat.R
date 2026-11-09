# R/modele_nO_etat.R

#' Construire le modèle enthalpie ~ n_O + état
#'
#' Entraîne une régression linéaire de l'enthalpie standard de formation
#' sur le nombre d'atomes d'oxygène et l'état physique.
#'
#' @param data data.frame contenant au moins les colonnes `formule` et `Hf`
#' @return Un objet `lm` ajusté
#' @export
modele_nO_etat <- function(data) {
  data$n_O  <- extraire_n_O(data$formule)
  data$etat <- extraire_etat(data$formule)
  data <- subset(data, !is.na(etat))
  lm(dfH ~ n_O + etat, data = data)
}

#' Prédire et évaluer le modèle sur un nouveau jeu
#'
#' @param modele Objet `lm` issu de `modele_nO_etat()`
#' @param nouveau data.frame contenant `formule` et `dfH`
#' @return Le data.frame avec les colonnes `Hf_predit` et `erreur` ajoutées
#' @export
predire_et_evaluer <- function(modele, nouveau) {
  nouveau$n_O  <- extraire_n_O(nouveau$formule)
  nouveau$etat <- extraire_etat(nouveau$formule)
  nouveau <- subset(nouveau, !is.na(etat))
  nouveau$dfH_predit <- predict(modele, newdata = nouveau)
  nouveau$erreur     <- nouveau$dfH - nouveau$dfH_predit
  nouveau
}