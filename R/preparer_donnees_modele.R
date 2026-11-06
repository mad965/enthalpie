#' Préparer les données pour la régression multiple
#'
#' Ajoute les colonnes \code{nC}, \code{nH}, \code{nO} au jeu de données et
#' filtre les composés pertinents (C-H-O, \code{dfH} non nul).
#'
#' @param data Jeu de données (par défaut \code{enthalpies_formation}).
#' @return Un data.frame avec les colonnes \code{nC}, \code{nH}, \code{nO}.
#' @export
preparer_donnees_modele <- function(data = NULL) {
  if (is.null(data)) data <- enthalpies_formation
  
  # Ajouter les colonnes si elles n'existent pas déjà
  if (!"nC" %in% names(data)) {
    data$nC <- sapply(data$formule, compter_atomes, element = "C")
    data$nH <- sapply(data$formule, compter_atomes, element = "H")
    data$nO <- sapply(data$formule, compter_atomes, element = "O")
  }
  
  data
}