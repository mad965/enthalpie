d <- enthalpies_formation

test_that("le jeu de donnees a la structure annoncee", {
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 72)
  expect_equal(names(d),
               c("formule", "nom", "etat", "espece", "dfH", "source", "stable"))
  expect_type(d$dfH, "double")
  expect_type(d$stable, "logical")
  expect_false(anyNA(d))
})

test_that("les identifiants d'especes sont uniques et coherents", {
  expect_false(anyDuplicated(d$espece) > 0)
  expect_equal(d$espece, paste0(d$formule, "(", d$etat, ")"))
})

test_that("chaque formule a au plus un etat stable", {
  nb_stables <- tapply(d$stable, d$formule, sum)
  expect_true(all(nb_stables <= 1))
})

test_that("les elements a l'etat de reference ont une enthalpie nulle", {
  elements <- c("H2", "O2", "N2", "Cl2", "F2", "Br2", "I2", "C", "S", "Na", "K",
                "Mg", "Ca", "Al", "Fe", "Cu", "Zn")
  sel <- d$formule %in% elements & d$stable
  expect_equal(sum(sel), length(elements))
  expect_true(all(d$dfH[sel] == 0))
})

test_that("toutes les formules sont lisibles et ont une masse molaire", {
  formules <- unique(d$formule)
  expect_no_error(lapply(formules, compter_elements))
  expect_no_error(masse_molaire(formules))
})

test_that("les sources sont toutes reconnues", {
  expect_true(all(d$source %in%
                    c("convention", "ET", "WIKI", "JANAF", "NIST", "MANUEL")))
})

test_that("dfH() lit la bonne ligne selon l'etat", {
  expect_equal(dfH("H2O"), -285.83)
  expect_equal(dfH("H2O(l)"), -285.83)
  expect_equal(dfH("H2O(g)"), -241.83)
  expect_equal(dfH("C"), 0)
  expect_equal(dfH("C(graphite)"), 0)
  expect_equal(dfH("C(diamant)"), 1.9)
  expect_equal(dfH("O3"), 142.7)
  expect_equal(dfH(c("CO2", "NH3")), c(-393.51, -45.94))
  expect_equal(dfH("NaCl(aq)"), -407.27)
})

test_that("dfH() refuse les especes inconnues", {
  expect_error(dfH("XeF6"), "introuvable")
  expect_error(dfH("H2O(s)"), "introuvable")
  expect_error(dfH(285), "cha")
})

test_that("dfH() accepte une table personnalisee", {
  perso <- data.frame(formule = "XY", nom = "xy", etat = "g", espece = "XY(g)",
                      dfH = 12.5, source = "test", stable = TRUE,
                      stringsAsFactors = FALSE)
  expect_equal(dfH("XY", donnees = perso), 12.5)
})

test_that("chercher_espece() filtre par formule ou par nom", {
  res <- chercher_espece("acide")
  expect_true(all(c("CH3COOH(l)", "HNO3(l)", "H2SO4(l)") %in% res$espece))
  expect_equal(names(res), c("espece", "nom", "dfH", "source"))
  expect_true("CH4(g)" %in% chercher_espece("^CH4")$espece)
  expect_equal(nrow(chercher_espece("zzzzzz")), 0)
})

test_that("graphique_dfH() trace sans erreur et renvoie les donnees", {
  pdf(NULL)
  res <- graphique_dfH(c("CO2(g)", "H2O(l)", "NH3(g)", "NO(g)"))
  graphique_dfH()
  invisible(dev.off())
  expect_equal(res$espece, c("CO2(g)", "H2O(l)", "NH3(g)", "NO(g)"))
  expect_equal(res$dfH, c(-393.51, -285.83, -45.94, 90.29))
  expect_error(graphique_dfH("XeF6"), "introuvable")
})
