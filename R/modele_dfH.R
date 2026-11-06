#' @export
ajuster_modele_dfH <- function(data = NULL) {
  df_modele <- preparer_donnees_modele(data)
  lm(dfH ~ nC + nH + nO, data = df_modele)
}

#' @export
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