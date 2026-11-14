# R/graphiques.R

#' Diagramme en barres des enthalpies de formation
#'
#' Trace horizontalement \eqn{\Delta_f H^\circ} pour une sélection d'espèces :
#' bleu pour les valeurs négatives (formation exothermique), orange pour les
#' valeurs positives, gris pour zéro.
#'
#' @param especes Vecteur d'espèces (par exemple `c("CO2", "H2O", "NH3")`).
#'   Par défaut : tous les composés à l'état le plus stable dont l'enthalpie
#'   n'est pas nulle.
#' @param donnees Table de données ; par défaut [enthalpies_formation].
#' @param trier Si `TRUE`, trie par valeur croissante.
#' @param main Titre du graphique.
#'
#' @return Invisiblement, le data.frame tracé (colonnes `espece` et `dfH`).
#'
#' @examples
#' graphique_dfH(c("CO2(g)", "H2O(l)", "NH3(g)", "NO(g)", "C2H2(g)"))
#' @export
graphique_dfH <- function(especes = NULL, donnees = enthalpie::enthalpies_formation,
                          trier = TRUE,
                          main = "Enthalpies standard de formation") {
  if (is.null(especes)) {
    sel <- donnees$stable & donnees$dfH != 0
    d <- data.frame(espece = donnees$espece[sel], dfH = donnees$dfH[sel],
                    stringsAsFactors = FALSE)
  } else {
    idx <- vapply(especes, .ligne_espece, integer(1), donnees = donnees,
                  USE.NAMES = FALSE)
    if (anyNA(idx)) {
      stop("Esp\u00e8ce(s) introuvable(s) : ",
           paste(especes[is.na(idx)], collapse = ", "), ".", call. = FALSE)
    }
    d <- data.frame(espece = donnees$espece[idx], dfH = donnees$dfH[idx],
                    stringsAsFactors = FALSE)
  }
  if (trier) {
    d <- d[order(d$dfH), , drop = FALSE]
  }
  couleurs <- ifelse(d$dfH < 0, "#2c7fb8", ifelse(d$dfH > 0, "#d95f0e", "grey60"))
  op <- graphics::par(mar = c(4, 7, 3, 1))
  on.exit(graphics::par(op))
  graphics::barplot(d$dfH, names.arg = d$espece, horiz = TRUE, las = 1,
                    col = couleurs, border = NA, main = main,
                    xlab = expression(Delta[f] * H^degree ~ "(kJ/mol)"))
  graphics::abline(v = 0)
  invisible(d)
}


#' Tracer les prédictions du modèle dfH, avec état et dégradé
#'
#' Trace un graphique prédit vs observé. La couleur des points dépend de
#' l'état physique (solide, liquide, gaz, diamant, graphite, etc.), et
#' l'intensité du dégradé dépend d'une variable numérique au choix
#' de l'utilisateur (`nC`, `nH` ou `nO`).
#'
#' @param modele Objet `lm` issu de `ajuster_modele_dfH()`.
#' @param data data.frame (par défaut `enthalpies_formation`).
#' @param variable_gradient Variable numérique pour le dégradé
#'   (`"nC"`, `"nH"` ou `"nO"`).
#'
#' @return Un objet `ggplot`.
#' @export
#'
#' @examples
#' \dontrun{
#' modele <- ajuster_modele_dfH(enthalpies_formation)
#' tracer_modele_dfH_ameliore(modele, variable_gradient = "nC")
#' tracer_modele_dfH_ameliore(modele, variable_gradient = "nH")
#' tracer_modele_dfH_ameliore(modele, variable_gradient = "nO")
#' }
tracer_modele_dfH_ameliore <- function(modele,
                                       data = NULL,
                                       variable_gradient = c("nC", "nH", "nO")) {
  variable_gradient <- match.arg(variable_gradient)
  
  # 1. Préparer les données
  df <- preparer_donnees_modele(data)
  df$dfH_predit <- predict(modele, newdata = df)
  df$erreur     <- df$dfH - df$dfH_predit
  
  # 2. Extraire l'état (normalisé)
  etat_source <- if ("etat" %in% names(df) && any(!is.na(df$etat))) {
    df$etat
  } else if ("espece" %in% names(df)) {
    sub(".*\\(([a-z]+)\\)$", "\\1", df$espece)
  } else {
    sub(".*\\(([a-z]+)\\)$", "\\1", df$formule)
  }
  
  df$etat_norm <- ifelse(
    etat_source %in% c("s", "(s)"), "solide",
    ifelse(etat_source %in% c("l", "(l)"), "liquide",
           ifelse(etat_source %in% c("g", "(g)"), "gaz",
                  ifelse(etat_source %in% c("diamant"), "diamant",
                         ifelse(etat_source %in% c("graphite"), "graphite",
                                "autre")))))
  
  # 3. Graphique prédit vs observé
  ggplot2::ggplot(df, ggplot2::aes(x = dfH, y = dfH_predit)) +
    ggplot2::geom_abline(intercept = 0, slope = 1,
                         linetype = "dashed", color = "grey40") +
    ggplot2::geom_point(ggplot2::aes(color = etat_norm,
                                     alpha = .data[[variable_gradient]]),
                        size = 3) +
    ggplot2::scale_color_brewer(palette = "Set1", name = "État") +
    ggplot2::scale_alpha_continuous(name = variable_gradient, range = c(0.3, 1)) +
    ggplot2::labs(
      x = expression(Delta[f]*H*degree~"observé (kJ/mol)"),
      y = expression(Delta[f]*H*degree~"prédit (kJ/mol)"),
      title = "Prédit vs observé : modèle dfH",
      subtitle = paste("Dégradé :", variable_gradient)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "right")
}