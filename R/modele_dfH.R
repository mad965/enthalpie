#' Ajuster un modèle linéaire pour prédire l'enthalpie de formation
#'
#' Ajuste une régression linéaire multiple de l'enthalpie standard de formation
#' sur des descripteurs moléculaires (par exemple le nombre d'atomes de carbone,
#' d'hydrogène et d'oxygène).
#'
#' @param data Un data.frame contenant au moins les colonnes `formule` et une
#'   colonne d'enthalpie (par exemple `dfH`).
#' @param formule Une formule de modèle R (par défaut `dfH ~ n_C + n_H + n_O`).
#'
#' @return Un objet `lm` ajusté.
#' @export
#' @examples
#' \dontrun{
#' modele <- ajuster_modele_dfH(enthalpies_formation)
#' summary(modele)
#' }
ajuster_modele_dfH <- function(data = NULL) {
  df_modele <- preparer_donnees_modele(data)
  lm(dfH ~ nC + nH + nO, data = df_modele)
}

#' Tracer les prédictions d'un modèle d'enthalpie de formation
#'
#' Produit un graphique comparant les enthalpies observées et les enthalpies
#' prédites par un modèle ajusté avec [ajuster_modele_dfH()].
#'
#' @param modele Un objet `lm` issu de [ajuster_modele_dfH()].
#' @param data Le data.frame utilisé pour l'ajustement (doit contenir la
#'   colonne d'enthalpie observée).
#'
#' @return Un objet `ggplot` (ou un graphique de base R, selon votre
#'   implémentation).
#' @export
#' @examples
#' \dontrun{
#' modele <- ajuster_modele_dfH(enthalpies_formation)
#' tracer_modele_dfH(modele, enthalpies_formation)
#' }
tracer_modele_dfH <- function(modele, data = NULL) {
  df_modele <- preparer_donnees_modele(data)
  df_modele$prediction <- predict(modele, newdata = df_modele)
  
  ggplot2::ggplot(df_modele, ggplot2::aes(x = dfH, y = prediction)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         color = "grey50") +
    ggplot2::geom_point(ggplot2::aes(color = nO > 0), size = 2.5) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "#E4572E", "FALSE" = "#2C7FB8"),
      labels = c("TRUE" = "Contient O", "FALSE" = "Sans O"),
      name = NULL
    ) +
    ggplot2::labs(
      title = "Enthalpie de formation : réalité vs modèle (C, H, O)",
      x = "ΔfH° réel (kJ/mol)",
      y = "ΔfH° prédit (kJ/mol)"
    ) +
    ggplot2::theme_minimal()
}