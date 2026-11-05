#' Estimation de l'enthalpie de réaction par les énergies de liaison
#'
#' Estimation grossière (phase gazeuse) :
#' \deqn{\Delta_r H^\circ \approx \sum E(\mathrm{liaisons\ rompues}) -
#' \sum E(\mathrm{liaisons\ formées}).}
#' Les énergies de liaison ne sont **pas** fournies par le paquet : elles
#' dépendent de la table utilisée (voir celle de votre cours). Elles se donnent
#' sous forme de vecteur nommé, en kJ/mol.
#'
#' @param casse Vecteur nommé : nombre de liaisons rompues de chaque type, par
#'   exemple `c("C-H" = 4, "O=O" = 2)`.
#' @param formees Vecteur nommé : nombre de liaisons formées de chaque type.
#' @param energies Vecteur nommé : énergie de chaque type de liaison (kJ/mol,
#'   positive).
#'
#' @return Un objet de classe `enthalpie_liaisons` : liste avec `rompues`,
#'   `formees` et `dH` (kJ).
#'
#' @examples
#' # Valeurs fictives, à remplacer par celles de votre table
#' e <- c("H-H" = 436, "Cl-Cl" = 243, "H-Cl" = 432)
#' enthalpie_liaisons(c("H-H" = 1, "Cl-Cl" = 1), c("H-Cl" = 2), e)
#' @export
enthalpie_liaisons <- function(casse, formees, energies) {
  somme <- function(nb) {
    if (is.null(names(nb)) || !is.numeric(nb)) {
      stop("`casse` et `formees` doivent \u00eatre des vecteurs num\u00e9riques nomm\u00e9s.",
           call. = FALSE)
    }
    manque <- setdiff(names(nb), names(energies))
    if (length(manque) > 0L) {
      stop("\u00c9nergie manquante pour la liaison : ",
           paste(manque, collapse = ", "), ".", call. = FALSE)
    }
    sum(nb * energies[names(nb)])
  }
  rompues <- somme(casse)
  creees <- somme(formees)
  structure(list(rompues = rompues, formees = creees, dH = rompues - creees),
            class = "enthalpie_liaisons")
}

#' @export
print.enthalpie_liaisons <- function(x, ...) {
  cat(sprintf("\u00c9nergie des liaisons rompues : %9.2f kJ\n", x$rompues))
  cat(sprintf("\u00c9nergie des liaisons form\u00e9es : %9.2f kJ\n", x$formees))
  cat(sprintf("\u0394rH\u00b0 estim\u00e9e (rompues - form\u00e9es) : %.2f kJ\n", x$dH))
  invisible(x)
}
