# Loi de Hess : combinaison lineaire d'equations ------------------------------

#' Combiner des réactions par la loi de Hess
#'
#' L'enthalpie étant une fonction d'état, l'enthalpie d'une réaction obtenue
#' en combinant plusieurs réactions est la même combinaison de leurs
#' enthalpies. La fonction additionne les équations (chacune multipliée par son
#' coefficient ; un coefficient négatif inverse l'équation), simplifie les
#' espèces qui apparaissent des deux côtés et additionne les enthalpies.
#'
#' Deux espèces ne s'annulent que si leur formule **et** leur état sont
#' identiques : écrire de façon cohérente `H2O(l)` ou `H2O(g)` dans toutes
#' les équations.
#'
#' @param reactions Vecteur de textes d'équations (ou liste d'objets
#'   `reaction_chimique`).
#' @param enthalpies Vecteur numérique des enthalpies de réaction (kJ), dans le
#'   même ordre.
#' @param coefficients Coefficients multiplicateurs (un par réaction ; 1 par
#'   défaut, recyclé).
#'
#' @return Un objet de classe `hess` : liste avec `reaction` (équation globale),
#'   `dH` (enthalpie globale en kJ) et `etapes` (data.frame récapitulatif).
#'
#' @examples
#' # C(s) + O2 -> CO2 puis CO + 1/2 O2 -> CO2 : formation de CO
#' loi_de_hess(
#'   c("C(s) + O2(g) -> CO2(g)", "CO(g) + 1/2 O2(g) -> CO2(g)"),
#'   enthalpies = c(-393.51, -282.98),
#'   coefficients = c(1, -1)
#' )
#' @export
loi_de_hess <- function(reactions, enthalpies, coefficients = 1) {
  reactions <- as.list(reactions)
  m <- length(reactions)
  if (length(enthalpies) != m) {
    stop("`enthalpies` doit avoir autant d'\u00e9l\u00e9ments que `reactions`.", call. = FALSE)
  }
  coefficients <- rep_len(coefficients, m)
  rs <- lapply(reactions, lire_reaction)
  bilan <- list()
  modele <- list()
  for (i in seq_len(m)) {
    esp <- rs[[i]]$especes
    for (j in seq_len(nrow(esp))) {
      etat <- if (is.na(esp$etat[j])) "" else esp$etat[j]
      cle <- paste0(esp$formule[j], "|", etat)
      signe <- if (esp$cote[j] == "produit") 1 else -1
      val <- signe * coefficients[i] * esp$coef[j]
      ancien <- if (is.null(bilan[[cle]])) 0 else bilan[[cle]]
      bilan[[cle]] <- ancien + val
      modele[[cle]] <- c(esp$formule[j], etat)
    }
  }
  net <- unlist(bilan)
  garde <- abs(net) > 1e-9
  if (!any(garde)) {
    stop("Les \u00e9quations se compensent exactement : \u00e9quation globale vide.",
         call. = FALSE)
  }
  cles <- names(net)[garde]
  formules <- vapply(cles, function(k) modele[[k]][1], character(1),
                     USE.NAMES = FALSE)
  etats <- vapply(cles, function(k) modele[[k]][2], character(1),
                  USE.NAMES = FALSE)
  etats[etats == ""] <- NA_character_
  valeurs <- unname(net[garde])
  esp_net <- data.frame(formule = formules, etat = etats,
                        coef = abs(valeurs),
                        cote = ifelse(valeurs > 0, "produit", "reactif"),
                        stringsAsFactors = FALSE)
  esp_net <- esp_net[order(esp_net$cote == "produit"), , drop = FALSE]
  rownames(esp_net) <- NULL
  r_net <- structure(list(especes = esp_net, texte = NA_character_),
                     class = "reaction_chimique")
  r_net$texte <- format(r_net)
  etapes <- data.frame(
    reaction = vapply(rs, format, character(1)),
    coef = coefficients,
    dH = as.numeric(enthalpies),
    stringsAsFactors = FALSE
  )
  structure(list(reaction = r_net, dH = sum(coefficients * enthalpies),
                 etapes = etapes),
            class = "hess")
}

#' @export
print.hess <- function(x, ...) {
  cat("Loi de Hess\n")
  for (i in seq_len(nrow(x$etapes))) {
    cat(sprintf("  (%d) coefficient %s  \u0394H = %.2f kJ   %s\n", i,
                .formater_coef(x$etapes$coef[i]), x$etapes$dH[i],
                x$etapes$reaction[i]))
  }
  cat("\n\u00c9quation globale : ", format(x$reaction), "\n", sep = "")
  cat(sprintf("\u0394H global = %.2f kJ\n", x$dH))
  invisible(x)
}
