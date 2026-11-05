# Acces aux enthalpies standard de formation du jeu de donnees ---------------

# Renvoie l'indice de la ligne de `donnees` correspondant a `espece`
# ("H2O(l)", "CH4", "C(diamant)"...), ou NA_integer_ si introuvable.
# Sans etat precise : ligne `stable` de la formule ; a defaut, ligne unique.
.ligne_espece <- function(espece, donnees) {
  espece <- trimws(espece)
  f <- .sans_etat(espece)
  et <- .etat_de(espece)
  if (!is.na(et)) {
    i <- which(donnees$formule == f & donnees$etat == et)
  } else {
    i <- which(donnees$formule == f & donnees$stable)
    if (length(i) == 0L) {
      i <- which(donnees$formule == f)
    }
  }
  if (length(i) == 1L) i else NA_integer_
}

#' Enthalpie standard de formation d'une ou plusieurs espèces
#'
#' Renvoie \eqn{\Delta_f H^\circ} (en kJ/mol, à 298,15 K et 1 bar) tirée du jeu
#' de données [enthalpies_formation]. Une espèce s'écrit avec ou sans état
#' physique : `"H2O(g)"` désigne la vapeur, `"H2O"` l'état le plus stable
#' (liquide) ; `"C(diamant)"` désigne le diamant, `"C"` le graphite.
#'
#' @param espece Vecteur de chaînes : formules, avec état éventuel.
#' @param donnees Table de données (colonnes `formule`, `etat`, `dfH`,
#'   `stable`) ; par défaut [enthalpies_formation].
#'
#' @return Un vecteur numérique (kJ/mol), sans noms.
#'
#' @examples
#' dfH("CO2")
#' dfH(c("H2O(l)", "H2O(g)"))
#' dfH("C(diamant)")
#' @export
dfH <- function(espece, donnees = enthalpie::enthalpies_formation) {
  if (!is.character(espece)) {
    stop("`espece` doit \u00eatre un vecteur de cha\u00eenes de caract\u00e8res.", call. = FALSE)
  }
  un <- function(e) {
    i <- .ligne_espece(e, donnees)
    if (is.na(i)) {
      stop("Esp\u00e8ce introuvable dans les donn\u00e9es : \"", e,
           "\". Voir chercher_espece().", call. = FALSE)
    }
    donnees$dfH[i]
  }
  vapply(espece, un, numeric(1), USE.NAMES = FALSE)
}

#' Chercher une espèce dans le jeu de données
#'
#' Cherche une expression régulière (sans tenir compte de la casse) dans la
#' colonne `espece` et dans le nom français.
#'
#' @param motif Expression régulière, par exemple `"^CH"` ou `"acide"`.
#' @param donnees Table de données ; par défaut [enthalpies_formation].
#'
#' @return Un data.frame (colonnes `espece`, `nom`, `dfH`, `source`).
#'
#' @examples
#' chercher_espece("acide")
#' chercher_espece("^C[0-9]H")
#' @export
chercher_espece <- function(motif, donnees = enthalpie::enthalpies_formation) {
  sel <- grepl(motif, donnees$espece, ignore.case = TRUE) |
    grepl(motif, donnees$nom, ignore.case = TRUE)
  res <- donnees[sel, c("espece", "nom", "dfH", "source"), drop = FALSE]
  rownames(res) <- NULL
  res
}
