#' Compter le nombre d'atomes d'un élément dans une formule chimique
#'
#' Extrait le nombre d'atomes d'un élément donné à partir d'une formule
#' brute (par exemple \code{"C2H5OH"}). L'élément peut apparaître plusieurs
#' fois dans la formule ; les occurrences sont sommées.
#'
#' @param formule Chaîne de caractères représentant une formule chimique
#'   (par exemple \code{"C6H12O6"}).
#' @param element Symbole de l'élément à compter (par exemple \code{"C"},
#'   \code{"H"}, \code{"O"}).
#'
#' @return Un entier : le nombre total d'atomes de l'élément dans la formule.
#'   Retourne 0 si l'élément est absent.
#'
#' @export
#'
#' @examples
#' compter_atomes("C2H5OH", "C")  # 2
#' compter_atomes("C2H5OH", "H")  # 6
#' compter_atomes("C6H12O6", "O") # 6
#' compter_atomes("CH4", "N")     # 0
compter_atomes <- function(formule, element) {
  motif <- paste0(element, "([0-9]*)")
  sapply(formule, function(f) {
    m <- regmatches(f, gregexpr(motif, f))[[1]]
    if (length(m) == 0) return(0L)
    nombres <- sub(motif, "\\1", m)
    nombres[nombres == ""] <- "1"
    sum(as.integer(nombres))
  }, USE.NAMES = FALSE)
}