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
