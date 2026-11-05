# Lecture, equilibrage et affichage des equations de reaction ----------------

# Lit un coefficient ("2", "0.5", "1/2") ; chaine vide -> 1.
.lire_coefficient <- function(s) {
  if (!nzchar(s)) {
    return(1)
  }
  if (grepl("/", s, fixed = TRUE)) {
    p <- strsplit(s, "/", fixed = TRUE)[[1]]
    return(as.numeric(p[1]) / as.numeric(p[2]))
  }
  as.numeric(s)
}

# Lit un membre de l'equation ("CH4 + 2 O2(g)") -> data.frame d'especes.
.lire_membre <- function(membre, cote) {
  termes <- trimws(strsplit(membre, "+", fixed = TRUE)[[1]])
  # strsplit() ignore un "+" final : on le detecte a part.
  if (length(termes) == 0L || any(!nzchar(termes)) ||
        grepl("\\+[[:space:]]*$", membre)) {
    stop("Membre vide ou terme manquant autour d'un \"+\".", call. = FALSE)
  }
  motif <- "^([0-9]+(\\.[0-9]+)?(/[0-9]+)?)?[[:space:]]*"
  coef_txt <- sub(paste0(motif, ".*$"), "\\1", termes)
  esp_txt <- sub(motif, "", termes)
  if (any(!nzchar(esp_txt))) {
    stop("Esp\u00e8ce manquante apr\u00e8s un coefficient.", call. = FALSE)
  }
  coef <- vapply(coef_txt, .lire_coefficient, numeric(1), USE.NAMES = FALSE)
  if (any(!is.finite(coef)) || any(coef <= 0)) {
    stop("Les coefficients doivent \u00eatre des nombres strictement positifs.",
         call. = FALSE)
  }
  formules <- .sans_etat(esp_txt)
  for (f in formules) {
    compter_elements(f)
  }
  data.frame(formule = formules, etat = .etat_de(esp_txt), coef = coef,
             cote = cote, stringsAsFactors = FALSE)
}

#' Lire une équation de réaction
#'
#' Transforme une équation écrite sous forme de texte en objet
#' `reaction_chimique`. Le séparateur réactifs/produits peut être `->`, `=>`,
#' une flèche Unicode ou `=`. Chaque espèce peut être précédée d'un
#' coefficient (entier, décimal ou fraction comme `1/2`) et suivie d'un état
#' physique entre parenthèses : `(s)`, `(l)`, `(g)`, `(aq)`.
#'
#' @param texte Une chaîne de caractères, par exemple
#'   `"CH4 + 2 O2 -> CO2 + 2 H2O"`, ou un objet `reaction_chimique` (renvoyé
#'   tel quel).
#'
#' @return Un objet de classe `reaction_chimique`, liste contenant `especes`
#'   (data.frame : `formule`, `etat`, `coef`, `cote`) et `texte`.
#'
#' @examples
#' r <- lire_reaction("CH4(g) + 2 O2(g) -> CO2(g) + 2 H2O(l)")
#' r
#' r$especes
#' @export
lire_reaction <- function(texte) {
  if (inherits(texte, "reaction_chimique")) {
    return(texte)
  }
  if (!is.character(texte) || length(texte) != 1L || is.na(texte)) {
    stop("`texte` doit \u00eatre une cha\u00eene de caract\u00e8res (longueur 1).",
         call. = FALSE)
  }
  brut <- gsub("\u2192", "->", texte, fixed = TRUE)
  membres <- trimws(strsplit(brut, "->|=>|=", perl = TRUE)[[1]])
  if (length(membres) != 2L || any(!nzchar(membres))) {
    stop("Une \u00e9quation doit avoir deux membres s\u00e9par\u00e9s par \"->\" ou \"=\".",
         call. = FALSE)
  }
  esp <- rbind(.lire_membre(membres[1], "reactif"),
               .lire_membre(membres[2], "produit"))
  rownames(esp) <- NULL
  structure(list(especes = esp, texte = texte), class = "reaction_chimique")
}

# Bilan d'atomes (reactifs - produits) pour chaque element.
.bilan_elements <- function(r) {
  esp <- r$especes
  bilan <- numeric(0)
  for (j in seq_len(nrow(esp))) {
    cj <- compter_elements(esp$formule[j])
    signe <- if (esp$cote[j] == "reactif") 1 else -1
    for (el in names(cj)) {
      ancien <- if (el %in% names(bilan)) bilan[[el]] else 0
      bilan[el] <- ancien + signe * esp$coef[j] * cj[[el]]
    }
  }
  bilan
}

#' Une équation est-elle équilibrée ?
#'
#' Vérifie que, pour chaque élément, le nombre d'atomes est le même côté
#' réactifs et côté produits.
#'
#' @param reaction Texte d'une équation ou objet `reaction_chimique`.
#'
#' @return `TRUE` ou `FALSE`.
#'
#' @examples
#' est_equilibree("2 H2 + O2 -> 2 H2O")
#' est_equilibree("H2 + O2 -> H2O")
#' @export
est_equilibree <- function(reaction) {
  r <- lire_reaction(reaction)
  all(abs(.bilan_elements(r)) < 1e-8)
}

# Plus grand commun diviseur de deux entiers (stockes en numeric).
.pgcd <- function(a, b) {
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  a
}

#' Équilibrer une équation de réaction
#'
#' Calcule les plus petits coefficients entiers qui conservent chaque élément.
#' On écrit la conservation des atomes sous la forme d'un système linéaire
#' homogène `A x = 0`, où `A` est la matrice éléments x espèces (produits
#' comptés négativement) ; une décomposition en valeurs singulières donne le
#' noyau de `A`, qui doit être de dimension 1 pour que la solution soit unique.
#'
#' @param reaction Texte d'une équation (les coefficients éventuels sont
#'   ignorés) ou objet `reaction_chimique`.
#'
#' @return Un objet `reaction_chimique` aux coefficients entiers minimaux.
#'
#' @examples
#' equilibrer("C3H8 + O2 -> CO2 + H2O")
#' equilibrer("Fe + O2 -> Fe2O3")
#' @export
equilibrer <- function(reaction) {
  r <- lire_reaction(reaction)
  esp <- r$especes
  n <- nrow(esp)
  comptes <- lapply(esp$formule, compter_elements)
  elements <- unique(unlist(lapply(comptes, names)))
  A <- matrix(0, nrow = length(elements), ncol = n,
              dimnames = list(elements, NULL))
  for (j in seq_len(n)) {
    signe <- if (esp$cote[j] == "reactif") 1 else -1
    A[names(comptes[[j]]), j] <- signe * comptes[[j]]
  }
  sv <- svd(A, nu = 0, nv = n)
  tol <- max(dim(A)) * .Machine$double.eps * max(sv$d)
  rang <- sum(sv$d > tol)
  if (n - rang != 1L) {
    stop("L'\u00e9quation n'admet pas une solution unique (dimension du noyau : ",
         n - rang, ").", call. = FALSE)
  }
  v <- sv$v[, n]
  if (v[1] < 0) {
    v <- -v
  }
  if (any(v < 1e-8)) {
    stop("Aucune combinaison de coefficients strictement positifs ne convient.",
         call. = FALSE)
  }
  v <- v / min(v)
  k <- NA_integer_
  for (essai in 1:200) {
    if (all(abs(essai * v - round(essai * v)) < 1e-6)) {
      k <- essai
      break
    }
  }
  if (is.na(k)) {
    stop("Impossible de trouver des coefficients entiers raisonnables.",
         call. = FALSE)
  }
  coef <- round(k * v)
  coef <- coef / Reduce(.pgcd, coef)
  r$especes$coef <- coef
  r$texte <- format(r)
  r
}

# Formate un coefficient : entier, sinon fraction simple, sinon decimal.
.formater_coef <- function(x) {
  if (abs(x - round(x)) < 1e-9) {
    return(as.character(as.integer(round(x))))
  }
  for (d in 2:12) {
    num <- x * d
    if (abs(num - round(num)) < 1e-9) {
      return(paste0(round(num), "/", d))
    }
  }
  format(signif(x, 6))
}

#' @export
format.reaction_chimique <- function(x, ...) {
  esp <- x$especes
  terme <- function(i) {
    cf <- esp$coef[i]
    prefixe <- if (abs(cf - 1) < 1e-9) "" else paste0(.formater_coef(cf), " ")
    suffixe <- if (is.na(esp$etat[i])) "" else paste0("(", esp$etat[i], ")")
    paste0(prefixe, esp$formule[i], suffixe)
  }
  gauche <- vapply(which(esp$cote == "reactif"), terme, character(1))
  droite <- vapply(which(esp$cote == "produit"), terme, character(1))
  paste(paste(gauche, collapse = " + "), "->", paste(droite, collapse = " + "))
}

#' @export
print.reaction_chimique <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}
