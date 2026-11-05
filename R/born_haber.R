#' Cycle de Born-Haber d'un solide ionique
#'
#' Pour un solide ionique MX, l'enthalpie standard de formation se décompose en
#' \deqn{\Delta_f H^\circ = \Delta_{sub} H + \sum E_i + \Delta_{at} H +
#' \Delta_{ea} H + \Delta_{res} H.}
#' Tous les termes sont des enthalpies **signées** (kJ/mol) : sublimation du
#' métal, ionisation(s) du métal, atomisation du non-métal (la moitié de
#' l'enthalpie de dissociation de X2 pour un halogénure), attachement
#' électronique (négatif pour un halogène) et formation du réseau cristallin
#' à partir des ions gazeux (négative). Il faut fournir exactement une des deux
#' grandeurs `dfH` ou `reseau` : l'autre est calculée. Les termes multiples
#' (par exemple deux ionisations) sont additionnés.
#'
#' @param dfH Enthalpie de formation du solide (kJ/mol), ou `NULL` pour la
#'   calculer.
#' @param sublimation Enthalpie de sublimation du métal.
#' @param ionisation Enthalpie(s) d'ionisation du métal.
#' @param atomisation Enthalpie d'atomisation du non-métal.
#' @param affinite Enthalpie d'attachement électronique (signée).
#' @param reseau Enthalpie de formation du réseau (signée, négative), ou
#'   `NULL` pour la calculer.
#'
#' @return Un objet de classe `born_haber` : liste avec `etapes` (data.frame),
#'   `dfH` et `reseau`. `print()` détaille le bilan, `plot()` trace le diagramme
#'   en escalier.
#'
#' @examples
#' # NaCl : enthalpie de réseau à partir de l'enthalpie de formation
#' bh <- born_haber(dfH = -411.12, sublimation = 107.5, ionisation = 495.8,
#'                  atomisation = 121.7, affinite = -349.0)
#' bh
#' @export
born_haber <- function(dfH = NULL, sublimation, ionisation, atomisation,
                       affinite, reseau = NULL) {
  if (is.null(dfH) == is.null(reseau)) {
    stop("Fournir exactement l'un des deux arguments `dfH` ou `reseau`.",
         call. = FALSE)
  }
  somme <- sum(sublimation) + sum(ionisation) + sum(atomisation) + sum(affinite)
  if (is.null(reseau)) {
    reseau <- dfH - somme
  } else {
    dfH <- somme + reseau
  }
  etapes <- data.frame(
    etape = c("Sublimation du m\u00e9tal", "Ionisation du m\u00e9tal",
              "Atomisation du non-m\u00e9tal", "Affinit\u00e9 \u00e9lectronique",
              "Formation du r\u00e9seau cristallin"),
    dH = c(sum(sublimation), sum(ionisation), sum(atomisation),
           sum(affinite), reseau),
    stringsAsFactors = FALSE
  )
  structure(list(etapes = etapes, dfH = dfH, reseau = reseau),
            class = "born_haber")
}

#' @export
print.born_haber <- function(x, ...) {
  cat("Cycle de Born-Haber\n")
  for (i in seq_len(nrow(x$etapes))) {
    cat("  ", .cadrer(x$etapes$etape[i], 36),
        sprintf(" %+9.2f kJ/mol\n", x$etapes$dH[i]), sep = "")
  }
  cat("  ", .cadrer("", 36), sprintf(" %9s\n", "---------"), sep = "")
  cat("  ", .cadrer("\u0394fH\u00b0 du solide ionique", 36),
      sprintf(" %+9.2f kJ/mol\n", x$dfH), sep = "")
  invisible(x)
}

#' @export
plot.born_haber <- function(x, main = "Cycle de Born-Haber", ...) {
  niv <- c(0, cumsum(x$etapes$dH))
  k <- length(niv)
  amp <- diff(range(niv))
  if (amp == 0) {
    amp <- 1
  }
  graphics::plot(NULL, xlim = c(0.4, k + 0.6),
                 ylim = range(niv) + c(-0.12, 0.15) * amp,
                 xaxt = "n", xlab = "", ylab = "Enthalpie (kJ/mol)",
                 main = main, bty = "l", ...)
  graphics::segments(seq_len(k) - 0.35, niv, seq_len(k) + 0.35, niv, lwd = 3)
  graphics::arrows(seq_len(k - 1) + 0.35, niv[-k], seq_len(k - 1) + 0.65,
                   niv[-1], length = 0.08)
  graphics::text(seq_len(k), niv, sprintf("%.1f", niv), pos = 3, cex = 0.8)
  graphics::axis(1, at = seq_len(k), tick = FALSE, cex.axis = 0.65,
                 labels = c("M(s) +\n1/2 X2(g)", "M(g) +\n1/2 X2(g)",
                            "M+(g) + e-\n+ 1/2 X2(g)", "M+(g) + e-\n+ X(g)",
                            "M+(g) +\nX-(g)", "MX(s)"))
  invisible(x)
}
