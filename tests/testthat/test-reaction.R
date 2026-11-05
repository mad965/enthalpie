test_that("lire_reaction() lit coefficients, fractions et etats", {
  r <- lire_reaction("H2(g) + 1/2 O2(g) -> H2O(l)")
  expect_s3_class(r, "reaction_chimique")
  expect_equal(r$especes$formule, c("H2", "O2", "H2O"))
  expect_equal(r$especes$coef, c(1, 0.5, 1))
  expect_equal(r$especes$etat, c("g", "g", "l"))
  expect_equal(r$especes$cote, c("reactif", "reactif", "produit"))
})

test_that("lire_reaction() accepte plusieurs separateurs et l'absence d'etat", {
  a <- lire_reaction("2 H2 + O2 -> 2 H2O")
  expect_equal(a$especes$coef, c(2, 1, 2))
  expect_true(all(is.na(a$especes$etat)))
  expect_equal(lire_reaction("2H2 + O2 = 2H2O")$especes$coef, c(2, 1, 2))
  expect_equal(lire_reaction("2 H2 + O2 => 2 H2O")$especes$coef, c(2, 1, 2))
  expect_equal(lire_reaction("2 H2 + O2 \u2192 2 H2O")$especes$coef, c(2, 1, 2))
})

test_that("lire_reaction() reconnait les etats graphite et diamant", {
  r <- lire_reaction("C(graphite) -> C(diamant)")
  expect_equal(r$especes$etat, c("s", "diamant"))
})

test_that("lire_reaction() refuse les equations mal formees", {
  expect_error(lire_reaction("H2 + O2"), "deux membres")
  expect_error(lire_reaction("H2 -> O2 -> H2O"), "deux membres")
  expect_error(lire_reaction("H2 + -> H2O"), "terme manquant|vide")
  expect_error(lire_reaction("h2 -> o2"), "invalide")
  expect_error(lire_reaction(42), "cha")
})

test_that("format() et print() restituent l'equation lisiblement", {
  r <- lire_reaction("CH4(g) + 2 O2(g) -> CO2(g) + 2 H2O(l)")
  expect_equal(format(r), "CH4(g) + 2 O2(g) -> CO2(g) + 2 H2O(l)")
  expect_output(print(r), "CH4(g) + 2 O2(g) -> CO2(g) + 2 H2O(l)", fixed = TRUE)
  expect_equal(format(lire_reaction("H2 + 1/2 O2 -> H2O")), "H2 + 1/2 O2 -> H2O")
})

test_that("est_equilibree() compte les atomes", {
  expect_true(est_equilibree("2 H2 + O2 -> 2 H2O"))
  expect_false(est_equilibree("H2 + O2 -> H2O"))
  expect_true(est_equilibree("H2 + 1/2 O2 -> H2O"))
  expect_true(est_equilibree("CH4 + 2 O2 -> CO2 + 2 H2O"))
})

test_that("equilibrer() trouve les plus petits coefficients entiers", {
  coef <- function(txt) equilibrer(txt)$especes$coef
  expect_equal(coef("H2 + O2 -> H2O"), c(2, 1, 2))
  expect_equal(coef("C3H8 + O2 -> CO2 + H2O"), c(1, 5, 3, 4))
  expect_equal(coef("Fe + O2 -> Fe2O3"), c(4, 3, 2))
  expect_equal(coef("Al + O2 -> Al2O3"), c(4, 3, 2))
  expect_equal(coef("C6H12O6 + O2 -> CO2 + H2O"), c(1, 6, 6, 6))
  expect_equal(coef("Ca(OH)2 + H3PO4 -> Ca3(PO4)2 + H2O"), c(3, 2, 1, 6))
  expect_equal(coef("N2 + H2 -> NH3"), c(1, 3, 2))
})

test_that("equilibrer() renvoie une equation equilibree et bien formatee", {
  r <- equilibrer("C3H8 + O2 -> CO2 + H2O")
  expect_true(est_equilibree(r))
  expect_equal(format(r), "C3H8 + 5 O2 -> 3 CO2 + 4 H2O")
  expect_equal(r$texte, format(r))
})

test_that("equilibrer() conserve les etats physiques", {
  r <- equilibrer("N2(g) + H2(g) -> NH3(g)")
  expect_equal(format(r), "N2(g) + 3 H2(g) -> 2 NH3(g)")
})

test_that("equilibrer() signale les equations impossibles ou ambigues", {
  expect_error(equilibrer("H2 -> O2"), "solution|positifs")
  # deux reactions independantes : noyau de dimension 2
  expect_error(equilibrer("H2 + O2 + N2 -> H2O + NO"), "unique")
})
