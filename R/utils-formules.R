# Utilitaires : lecture des formules chimiques et masses molaires --------------

# Motif des etats physiques reconnus en fin d'espece : (s), (l), (g), (aq),
# (diamant) ; "(graphite)" est accepte et ramene a "(s)".
.RE_ETAT <- "\\((s|l|g|aq|graphite|diamant)\\)$"

# Retire l'etat physique final : "H2O(l)" -> "H2O".
.sans_etat <- function(x) {
  sub(.RE_ETAT, "", x)
}

# Extrait l'etat physique final ("H2O(l)" -> "l"), NA s'il n'y en a pas.
.etat_de <- function(x) {
  etat <- rep(NA_character_, length(x))
  i <- grepl(.RE_ETAT, x)
  etat[i] <- sub(paste0("^.*", .RE_ETAT), "\\1", x[i])
  etat[!is.na(etat) & etat == "graphite"] <- "s"
  etat
}

# Complete `x` par des espaces a droite jusqu'a `largeur` caracteres (et non
# octets : sprintf("%-14s") n'est pas fiable avec des lettres accentuees).
.cadrer <- function(x, largeur) {
  paste0(x, strrep(" ", pmax(0L, largeur - nchar(x))))
}

# Ajoute deux vecteurs nommes de comptes d'atomes.
.ajouter_comptes <- function(a, b) {
  for (el in names(b)) {
    if (el %in% names(a)) {
      a[el] <- a[el] + b[el]
    } else {
      a[el] <- b[el]
    }
  }
  a
}

#' Compter les atomes d'une formule chimique
#'
#' Analyse une formule brute (avec parenthèses imbriquées éventuelles)
#' et renvoie le nombre d'atomes de chaque élément. L'état physique final,
#' par exemple `(g)` ou `(aq)`, est ignoré.
#'
#' @param formule Une chaîne de caractères (longueur 1), par exemple
#'   `"Ca(OH)2"` ou `"C6H12O6"`.
#'
#' @return Un vecteur d'entiers nommé par les symboles des éléments, dans
#'   l'ordre de première apparition.
#'
#' @examples
#' compter_elements("H2O")
#' compter_elements("Ca(OH)2")
#' compter_elements("Al2(SO4)3")
#' @export
compter_elements <- function(formule) {
  if (!is.character(formule) || length(formule) != 1L || is.na(formule)) {
    stop("`formule` doit \u00eatre une cha\u00eene de caract\u00e8res (longueur 1).",
         call. = FALSE)
  }
  f <- gsub("[[:space:]]+", "", .sans_etat(formule))
  if (!nzchar(f)) {
    stop("Formule vide.", call. = FALSE)
  }
  jetons <- regmatches(f, gregexpr("[A-Z][a-z]?|[0-9]+|\\(|\\)", f))[[1]]
  if (!identical(paste(jetons, collapse = ""), f)) {
    stop("Formule invalide : \"", formule, "\".", call. = FALSE)
  }
  n <- length(jetons)
  pos <- 1L

  lire_nombre <- function() {
    if (pos <= n && grepl("^[0-9]+$", jetons[pos])) {
      k <- as.integer(jetons[pos])
      pos <<- pos + 1L
      k
    } else {
      1L
    }
  }

  lire_groupe <- function() {
    cpt <- integer(0)
    while (pos <= n && jetons[pos] != ")") {
      if (jetons[pos] == "(") {
        pos <<- pos + 1L
        interne <- lire_groupe()
        if (pos > n || jetons[pos] != ")") {
          stop("Parenth\u00e8se ouvrante sans parenth\u00e8se fermante dans \"",
               formule, "\".", call. = FALSE)
        }
        pos <<- pos + 1L
        k <- lire_nombre()
        cpt <- .ajouter_comptes(cpt, interne * k)
      } else if (grepl("^[A-Z]", jetons[pos])) {
        el <- jetons[pos]
        pos <<- pos + 1L
        k <- lire_nombre()
        cpt <- .ajouter_comptes(cpt, stats::setNames(k, el))
      } else {
        stop("Nombre inattendu dans \"", formule, "\".", call. = FALSE)
      }
    }
    cpt
  }

  res <- lire_groupe()
  if (pos <= n) {
    stop("Parenth\u00e8se fermante sans parenth\u00e8se ouvrante dans \"",
         formule, "\".", call. = FALSE)
  }
  res
}

# Masses molaires atomiques (g/mol), valeurs usuelles arrondies (IUPAC).
.MASSES_ATOMIQUES <- c(
  H = 1.008, C = 12.011, N = 14.007, O = 15.999, F = 18.998,
  Na = 22.990, Mg = 24.305, Al = 26.982, Si = 28.085, P = 30.974,
  S = 32.06, Cl = 35.45, K = 39.098, Ca = 40.078, Fe = 55.845,
  Cu = 63.546, Zn = 65.38, Br = 79.904, I = 126.90
)

#' Masse molaire d'une espèce
#'
#' Calcule la masse molaire (en g/mol) à partir de la formule brute, avec une
#' table de masses atomiques usuelles (H, C, N, O, F, Na, Mg, Al, Si, P, S, Cl,
#' K, Ca, Fe, Cu, Zn, Br, I).
#'
#' @param formule Un vecteur de formules, par exemple `c("H2O", "CO2")`.
#'
#' @return Un vecteur numérique de masses molaires en g/mol.
#'
#' @examples
#' masse_molaire("H2O")
#' masse_molaire(c("CO2", "C6H12O6"))
#' @export
masse_molaire <- function(formule) {
  if (!is.character(formule)) {
    stop("`formule` doit \u00eatre un vecteur de cha\u00eenes de caract\u00e8res.", call. = FALSE)
  }
  un <- function(f) {
    cpt <- compter_elements(f)
    inconnus <- setdiff(names(cpt), names(.MASSES_ATOMIQUES))
    if (length(inconnus) > 0L) {
      stop("\u00c9l\u00e9ment(s) absent(s) de la table des masses : ",
           paste(inconnus, collapse = ", "), ".", call. = FALSE)
    }
    sum(.MASSES_ATOMIQUES[names(cpt)] * cpt)
  }
  vapply(formule, un, numeric(1), USE.NAMES = FALSE)
}
# R/utils_formule.R

#' Extraire le nombre d'atomes d'oxygène d'une formule chimique
#'
#' @param formule Vecteur de formules chimiques (ex : "H2O(l)", "CO2(g)")
#' @return Vecteur numérique du nombre d'atomes O
#' @export
extraire_n_O <- function(formule) {
  sapply(formule, function(f) {
    f_clean <- sub("\\([slg]\\)$", "", f)
    matches <- regmatches(f_clean, gregexpr("O([0-9]*)", f_clean))[[1]]
    if (length(matches) == 0) return(0)
    sum(sapply(matches, function(m) {
      num <- sub("O", "", m)
      if (num == "") 1 else as.numeric(num)
    }))
  }, USE.NAMES = FALSE)   # <--- AJOUT ICI
}

extraire_etat <- function(formule) {
  sapply(formule, function(f) {
    if (grepl("\\(s\\)$", f)) return("solide")
    if (grepl("\\(l\\)$", f)) return("liquide")
    if (grepl("\\(g\\)$", f)) return("gaz")
    return(NA)
  }, USE.NAMES = FALSE)   # <--- AJOUT ICI
}

