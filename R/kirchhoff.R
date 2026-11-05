#' Loi de Kirchhoff : enthalpie de réaction à une autre température
#'
#' Si \eqn{\Delta C_p} est la différence des capacités thermiques molaires
#' (produits moins réactifs), alors
#' \deqn{\Delta_r H^\circ(T) = \Delta_r H^\circ(T_0) + \int_{T_0}^{T} \Delta C_p \, dT.}
#' La référence est en général \eqn{T_0 = 298{,}15} K, où l'on connaît
#' \eqn{\Delta_r H^\circ} grâce aux enthalpies de formation. Aucune
#' transition de phase ne doit être franchie entre \eqn{T_0} et \eqn{T}.
#'
#' @param dH298 Enthalpie de réaction à `T0`, en kJ/mol.
#' @param dCp Soit un nombre (\eqn{\Delta C_p} constant, en J/(mol K)), soit une
#'   fonction de la température (en K) renvoyant des J/(mol K).
#' @param T Température(s) visée(s), en K.
#' @param T0 Température de référence, en K (298,15 par défaut).
#'
#' @return Un vecteur numérique : l'enthalpie de réaction à chaque température
#'   `T`, en kJ/mol.
#'
#' @examples
#' # Delta Cp constant de -20 J/(mol K)
#' kirchhoff(-92, dCp = -20, T = c(298.15, 500, 700))
#' # Delta Cp variable avec la température
#' kirchhoff(-92, dCp = function(t) -25 + 0.01 * t, T = 600)
#' @export
kirchhoff <- function(dH298, dCp, T, T0 = 298.15) {
  if (!is.numeric(dH298) || length(dH298) != 1L) {
    stop("`dH298` doit \u00eatre un nombre.", call. = FALSE)
  }
  if (is.function(dCp)) {
    integrande <- function(u) vapply(u, dCp, numeric(1))
    integrale <- function(t) {
      if (t == T0) {
        return(0)
      }
      if (t < T0) {
        return(-stats::integrate(integrande, lower = t, upper = T0)$value)
      }
      stats::integrate(integrande, lower = T0, upper = t)$value
    }
    delta <- vapply(T, integrale, numeric(1))
  } else if (is.numeric(dCp) && length(dCp) == 1L) {
    delta <- dCp * (T - T0)
  } else {
    stop("`dCp` doit \u00eatre un nombre ou une fonction.", call. = FALSE)
  }
  dH298 + delta / 1000
}
