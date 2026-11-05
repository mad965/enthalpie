# Enthalpie standard de reaction, de combustion, pouvoir calorifique ---------

# Formate un nombre stoechiometrique signe : "-1", "+2", "-1/2".
.formater_nu <- function(nu) {
  paste0(if (nu > 0) "+" else "", .formater_coef(nu))
}

#' Enthalpie standard de réaction par la formule de Hess
#'
#' Calcule
#' \deqn{\Delta_r H^\circ = \sum_i \nu_i \, \Delta_f H^\circ_i}
#' où \eqn{\nu_i} est positif pour un produit et négatif pour un réactif.
#' L'équation doit être équilibrée, sauf si `equilibrer_auto = TRUE`.
#'
#' @param reaction Texte d'une équation ou objet `reaction_chimique`.
#' @param par Espèce de référence (optionnelle) pour exprimer aussi
#'   l'enthalpie par mole de cette espèce, par exemple `"CH4"`.
#' @param unite `"kJ"` (défaut) ou `"kcal"` (1 cal = 4,184 J).
#' @param equilibrer_auto Si `TRUE`, équilibre l'équation quand elle ne l'est
#'   pas.
#' @param donnees Table de données ; par défaut [enthalpies_formation].
#'
#' @return Un objet de classe `enthalpie_reaction` : liste avec `reaction`,
#'   `tableau` (espèce, nu, dfH, contribution), `dH` (enthalpie de réaction pour
#'   l'équation telle qu'écrite), `dH_mol` (par mole de `par`, `NA` sinon),
#'   `unite`, `par` et `nature` (exothermique ou endothermique). `as.numeric()`
#'   renvoie `dH` ; `plot()` trace le diagramme de niveaux d'enthalpie.
#'
#' @examples
#' r <- enthalpie_reaction("CH4(g) + 2 O2(g) -> CO2(g) + 2 H2O(l)", par = "CH4")
#' r
#' as.numeric(r)
#' enthalpie_reaction("N2 + H2 -> NH3", equilibrer_auto = TRUE)
#' @export
enthalpie_reaction <- function(reaction, par = NULL, unite = c("kJ", "kcal"),
                               equilibrer_auto = FALSE,
                               donnees = enthalpie::enthalpies_formation) {
  unite <- match.arg(unite)
  r <- lire_reaction(reaction)
  if (!est_equilibree(r)) {
    if (isTRUE(equilibrer_auto)) {
      r <- equilibrer(r)
    } else {
      stop("L'\u00e9quation n'est pas \u00e9quilibr\u00e9e : utilisez equilibrer() ",
           "ou equilibrer_auto = TRUE.", call. = FALSE)
    }
  }
  esp <- r$especes
  noms <- ifelse(is.na(esp$etat), esp$formule,
                 paste0(esp$formule, "(", esp$etat, ")"))
  nu <- ifelse(esp$cote == "produit", esp$coef, -esp$coef)
  h <- dfH(noms, donnees)
  facteur <- if (unite == "kcal") 1 / 4.184 else 1
  tableau <- data.frame(espece = noms, nu = nu, dfH = h * facteur,
                        contribution = nu * h * facteur + 0,
                        stringsAsFactors = FALSE)
  dH <- sum(tableau$contribution)
  dH_mol <- NA_real_
  if (!is.null(par)) {
    k <- match(par, noms)
    if (is.na(k)) {
      k <- match(.sans_etat(par), esp$formule)
    }
    if (is.na(k)) {
      stop("L'esp\u00e8ce `par` (\"", par, "\") n'appara\u00eet pas dans l'\u00e9quation.",
           call. = FALSE)
    }
    dH_mol <- dH / esp$coef[k]
  }
  nature <- if (abs(dH) < 1e-12) {
    "athermique"
  } else if (dH < 0) {
    "exothermique"
  } else {
    "endothermique"
  }
  structure(list(reaction = r, tableau = tableau, dH = dH, dH_mol = dH_mol,
                 unite = unite, par = par, nature = nature),
            class = "enthalpie_reaction")
}

#' @export
print.enthalpie_reaction <- function(x, ...) {
  tab <- x$tableau
  cat("R\u00e9action : ", format(x$reaction), "\n\n", sep = "")
  cat("  ", .cadrer("Esp\u00e8ce", 14),
      sprintf(" %6s %14s %14s\n", "nu", paste0("dfH (", x$unite, "/mol)"),
              paste0("nu*dfH (", x$unite, ")")), sep = "")
  for (i in seq_len(nrow(tab))) {
    cat("  ", .cadrer(tab$espece[i], 14),
        sprintf(" %6s %14.2f %14.2f\n", .formater_nu(tab$nu[i]), tab$dfH[i],
                tab$contribution[i]), sep = "")
  }
  cat(sprintf("\n\u0394rH\u00b0 = %.2f %s  (%s)\n", x$dH, x$unite, x$nature))
  if (!is.na(x$dH_mol)) {
    cat(sprintf("\u0394rH\u00b0 par mole de %s : %.2f %s/mol\n", x$par, x$dH_mol,
                x$unite))
  }
  invisible(x)
}

#' @export
as.double.enthalpie_reaction <- function(x, ...) {
  x$dH
}

#' @export
plot.enthalpie_reaction <- function(x, main = NULL, ...) {
  tab <- x$tableau
  h_r <- -sum(tab$contribution[tab$nu < 0])
  h_p <- sum(tab$contribution[tab$nu > 0])
  niveaux <- c(0, h_r, h_p)
  amp <- diff(range(niveaux))
  if (amp == 0) {
    amp <- 1
  }
  # Delta et degre sont ecrits en notation mathematique (plotmath) : un
  # caractere Unicode brut fait echouer certains peripheriques graphiques
  # (pdf, png...) quand leur encodage n'est pas UTF-8.
  if (is.null(main)) {
    main <- as.expression(bquote(Delta[r] * H^degree ~ "=" ~
                                   .(sprintf("%.2f", x$dH)) ~ .(x$unite)))
  }
  graphics::plot(NULL, xlim = c(0, 10),
                 ylim = range(niveaux) + c(-0.25, 0.25) * amp,
                 xaxt = "n", xlab = "",
                 ylab = paste0("Enthalpie (", x$unite, ")"),
                 main = main, bty = "l", ...)
  graphics::segments(c(0.3, 3.5, 7), niveaux, c(2.5, 5.5, 9.2), niveaux,
                     lwd = 3, col = c("grey40", "#2c7fb8", "#d95f0e"))
  graphics::text(c(1.4, 4.5, 8.1), niveaux,
                 c("\u00e9l\u00e9ments", "r\u00e9actifs", "produits"), pos = 3)
  graphics::text(c(4.5, 8.1), c(h_r, h_p), sprintf("%.1f", c(h_r, h_p)), pos = 1)
  graphics::segments(5.5, h_r, 6.2, h_r, lty = 3)
  graphics::segments(6.2, h_p, 7, h_p, lty = 3)
  if (abs(h_p - h_r) > 1e-9) {
    graphics::arrows(6.2, h_r, 6.2, h_p, length = 0.1, lwd = 2)
    graphics::text(6.3, (h_r + h_p) / 2,
                   as.expression(bquote(Delta[r] * H^degree ~ "=" ~
                                          .(sprintf("%.1f", x$dH)))),
                   pos = 4)
  }
  invisible(x)
}

#' Enthalpie standard de combustion
#'
#' Construit l'équation de combustion complète d'une espèce « C, H, O » dans
#' le dioxygène, produisant `CO2(g)` et `H2O(l)`, puis calcule
#' \eqn{\Delta_c H^\circ} par mole de combustible.
#'
#' @param espece Formule, avec état éventuel (`"C2H5OH"`, `"CH4(g)"`...).
#'   Seuls C, H et O sont admis.
#' @param unite `"kJ"` ou `"kcal"`.
#' @param donnees Table de données ; par défaut [enthalpies_formation].
#'
#' @return Un objet `enthalpie_reaction` (voir [enthalpie_reaction()]).
#'
#' @examples
#' enthalpie_combustion("CH4")
#' enthalpie_combustion("C2H5OH")
#' @export
enthalpie_combustion <- function(espece, unite = c("kJ", "kcal"),
                                 donnees = enthalpie::enthalpies_formation) {
  unite <- match.arg(unite)
  if (!is.character(espece) || length(espece) != 1L) {
    stop("`espece` doit \u00eatre une cha\u00eene de caract\u00e8res (longueur 1).", call. = FALSE)
  }
  f <- .sans_etat(espece)
  cpt <- compter_elements(f)
  autres <- setdiff(names(cpt), c("C", "H", "O"))
  if (length(autres) > 0L) {
    stop("Seuls C, H et O sont admis (trouv\u00e9 : ",
         paste(autres, collapse = ", "), ").", call. = FALSE)
  }
  compte <- function(el) if (el %in% names(cpt)) cpt[[el]] else 0L
  nC <- compte("C")
  nH <- compte("H")
  nO <- compte("O")
  coef_o2 <- nC + nH / 4 - nO / 2
  if (coef_o2 <= 0) {
    stop("\"", espece, "\" n'est pas un combustible (pas de dioxyg\u00e8ne consomm\u00e9).",
         call. = FALSE)
  }
  produits <- character(0)
  if (nC > 0) {
    produits <- c(produits, paste0(.formater_coef(nC), " CO2(g)"))
  }
  if (nH > 0) {
    produits <- c(produits, paste0(.formater_coef(nH / 2), " H2O(l)"))
  }
  texte <- paste0(espece, " + ", .formater_coef(coef_o2), " O2(g) -> ",
                  paste(produits, collapse = " + "))
  enthalpie_reaction(texte, par = espece, unite = unite, donnees = donnees)
}

#' Pouvoir calorifique d'un combustible
#'
#' Énergie libérée par la combustion complète d'un gramme de combustible, en
#' kJ/g (pouvoir calorifique supérieur : l'eau formée est liquide).
#'
#' @param espece Vecteur de formules (C, H, O uniquement).
#' @param donnees Table de données ; par défaut [enthalpies_formation].
#'
#' @return Un vecteur numérique nommé (kJ/g).
#'
#' @examples
#' pouvoir_calorifique(c("H2", "CH4", "C2H5OH", "C6H12O6"))
#' @export
pouvoir_calorifique <- function(espece, donnees = enthalpie::enthalpies_formation) {
  un <- function(e) {
    rc <- enthalpie_combustion(e, donnees = donnees)
    -rc$dH / masse_molaire(e)
  }
  vapply(espece, un, numeric(1))
}
