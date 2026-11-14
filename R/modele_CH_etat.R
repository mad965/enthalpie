# R/modele_CH_etat.R

#' Construire le modèle enthalpie ~ n_C + n_H + état
#'
#' Entraîne une régression linéaire multiple de l'enthalpie standard de
#' formation sur le nombre d'atomes de carbone, le nombre d'atomes
#' d'hydrogène et l'état physique (solide, liquide, gaz).
#'
#' @param data data.frame contenant au moins les colonnes `formule`,
#'   `etat` et `dfH`.
#' @return Un objet `lm` ajusté.
#' @export
#'
#' @examples
#' \dontrun{
#' modele <- modele_CH_etat(enthalpies_formation)
#' summary(modele)
#' }
modele_CH_etat <- function(data) {
  data <- preparer_donnees_modele(data)
  
  # Extraire l'état
  if ("etat" %in% names(data) && any(!is.na(data$etat))) {
    etat_source <- data$etat
  } else if ("espece" %in% names(data)) {
    etat_source <- sub(".*\\(([slg])\\)$", "\\1", data$espece)
  } else {
    etat_source <- sub(".*\\(([slg])\\)$", "\\1", data$formule)
  }
  
  data$etat_norm <- ifelse(
    etat_source %in% c("s", "(s)"), "solide",
    ifelse(etat_source %in% c("l", "(l)"), "liquide",
           ifelse(etat_source %in% c("g", "(g)"), "gaz", NA)))
  
  data <- subset(data, !is.na(etat_norm) & !is.na(dfH))
  lm(dfH ~ nC + nH + etat_norm, data = data)
}


#' Prédire et évaluer le modèle sur un nouveau jeu
#'
#' @param modele Objet `lm` issu de `modele_CH_etat()`.
#' @param nouveau data.frame contenant `formule`, `etat` et `dfH`.
#' @return Le data.frame avec les colonnes `dfH_predit` et `erreur` ajoutées.
#' @export
predire_et_evaluer <- function(modele, nouveau) {
  # 1. Préparer les données (ajoute nC, nH, nO)
  nouveau <- preparer_donnees_modele(nouveau)
  
  # 2. Extraire l'état depuis `etat` ou `espece` ou `formule`
  if ("etat" %in% names(nouveau) && any(!is.na(nouveau$etat))) {
    etat_source <- nouveau$etat
  } else if ("espece" %in% names(nouveau)) {
    etat_source <- sub(".*\\(([slg])\\)$", "\\1", nouveau$espece)
  } else {
    etat_source <- sub(".*\\(([slg])\\)$", "\\1", nouveau$formule)
  }
  
  nouveau$etat_norm <- ifelse(
    etat_source %in% c("s", "(s)"), "solide",
    ifelse(etat_source %in% c("l", "(l)"), "liquide",
           ifelse(etat_source %in% c("g", "(g)"), "gaz", NA)))
  
  # 3. Filtrer et prédire
  nouveau <- subset(nouveau, !is.na(etat_norm) & !is.na(dfH))
  nouveau$dfH_predit <- predict(modele, newdata = nouveau)
  nouveau$erreur     <- nouveau$dfH - nouveau$dfH_predit
  nouveau
}
