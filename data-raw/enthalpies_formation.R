# Regenere data/enthalpies_formation.R a partir de data-raw/enthalpies_formation.csv
#
# Utilisation (depuis la racine du paquet) :
#   source("data-raw/enthalpies_formation.R")
#
# Pour ajouter une espece : ajouter une ligne au CSV (colonnes formule, nom,
# etat, dfH, source, stable), puis relancer ce script. Le fichier produit ne
# contient que des caracteres ASCII (accents ecrits en \uXXXX), comme l'exige
# R CMD check.

d <- utils::read.csv("data-raw/enthalpies_formation.csv",
                     stringsAsFactors = FALSE, fileEncoding = "UTF-8")

stopifnot(
  identical(names(d), c("formule", "nom", "etat", "dfH", "source", "stable")),
  !anyNA(d), is.numeric(d$dfH), is.logical(d$stable)
)

d$espece <- paste0(d$formule, "(", d$etat, ")")
stopifnot(!anyDuplicated(d$espece))
d <- d[, c("formule", "nom", "etat", "espece", "dfH", "source", "stable")]

# Texte R ASCII d'une chaine : guillemets, antislash et lettres accentuees
# sont echappes.
litteral <- function(s) {
  s <- enc2utf8(s)
  sortie <- vapply(s, function(un) {
    codes <- utf8ToInt(un)
    morceaux <- vapply(codes, function(k) {
      if (k == 34L) {
        "\\\""
      } else if (k == 92L) {
        "\\\\"
      } else if (k < 128L) {
        intToUtf8(k)
      } else {
        sprintf("\\u%04x", k)
      }
    }, character(1))
    paste0("\"", paste(morceaux, collapse = ""), "\"")
  }, character(1), USE.NAMES = FALSE)
  sortie
}

vecteur <- function(nom, valeurs) {
  paste0("  ", nom, " = c(", paste(valeurs, collapse = ", "), "),")
}

lignes <- c(
  "# Fichier genere par data-raw/enthalpies_formation.R : ne pas modifier a la main.",
  "enthalpies_formation <- data.frame(",
  vecteur("formule", litteral(d$formule)),
  vecteur("nom", litteral(d$nom)),
  vecteur("etat", litteral(d$etat)),
  vecteur("espece", litteral(d$espece)),
  vecteur("dfH", format(d$dfH, trim = TRUE, nsmall = 2)),
  vecteur("source", litteral(d$source)),
  vecteur("stable", ifelse(d$stable, "TRUE", "FALSE")),
  "  stringsAsFactors = FALSE",
  ")"
)

writeLines(lignes, "data/enthalpies_formation.R")
message("data/enthalpies_formation.R ecrit (", nrow(d), " especes).")
