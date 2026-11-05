test_that("enthalpie_reaction() applique la formule de Hess (methane)", {
  r <- enthalpie_reaction("CH4(g) + 2 O2(g) -> CO2(g) + 2 H2O(l)", par = "CH4")
  expect_s3_class(r, "enthalpie_reaction")
  expect_equal(r$dH, -890.30)
  expect_equal(r$dH_mol, -890.30)
  expect_equal(r$nature, "exothermique")
  expect_equal(as.numeric(r), -890.30)
  expect_equal(r$tableau$espece, c("CH4(g)", "O2(g)", "CO2(g)", "H2O(l)"))
  expect_equal(r$tableau$nu, c(-1, -2, 1, 2))
  expect_equal(r$tableau$dfH, c(-74.87, 0, -393.51, -285.83))
  expect_equal(sum(r$tableau$contribution), r$dH)
})

test_that("enthalpie_reaction() : synthese de l'ammoniac et thermite", {
  haber <- enthalpie_reaction("N2(g) + 3 H2(g) -> 2 NH3(g)", par = "NH3")
  expect_equal(haber$dH, -91.88)
  expect_equal(haber$dH_mol, -45.94)
  thermite <- enthalpie_reaction("Fe2O3(s) + 2 Al(s) -> Al2O3(s) + 2 Fe(s)")
  expect_equal(thermite$dH, -851.5)
  expect_true(is.na(thermite$dH_mol))
})

test_that("enthalpie_reaction() distingue les etats physiques", {
  vap <- enthalpie_reaction("H2O(l) -> H2O(g)")
  expect_equal(vap$dH, 44.00)
  expect_equal(vap$nature, "endothermique")
  diam <- enthalpie_reaction("C(s) -> C(diamant)")
  expect_equal(diam$dH, 1.90)
  # reaction de formation : pour un seul produit, dH = dfH
  form <- enthalpie_reaction("C(s) + O2(g) -> CO2(g)")
  expect_equal(form$dH, dfH("CO2"))
})

test_that("enthalpie_reaction() convertit en kcal", {
  r <- enthalpie_reaction("CH4(g) + 2 O2(g) -> CO2(g) + 2 H2O(l)", unite = "kcal")
  expect_equal(r$unite, "kcal")
  expect_equal(r$dH, -890.30 / 4.184)
})

test_that("enthalpie_reaction() equilibre a la demande ou refuse", {
  expect_error(enthalpie_reaction("H2 + O2 -> H2O"), "quilibr")
  r <- enthalpie_reaction("H2 + O2 -> H2O", equilibrer_auto = TRUE)
  expect_equal(r$dH, 2 * dfH("H2O"))
  expect_equal(format(r$reaction), "2 H2 + O2 -> 2 H2O")
  expect_error(enthalpie_reaction("CH4 + 2 O2 -> CO2 + 2 H2O", par = "NH3"),
               "appara")
  expect_error(enthalpie_reaction("XeF6 -> Xe + 3 F2"), "introuvable")
})

test_that("print() affiche le tableau et le resultat", {
  r <- enthalpie_reaction("CH4(g) + 2 O2(g) -> CO2(g) + 2 H2O(l)", par = "CH4")
  expect_output(print(r), "-890.30", fixed = TRUE)
  expect_output(print(r), "exothermique", fixed = TRUE)
  expect_output(print(r), "CH4(g)", fixed = TRUE)
})

test_that("plot() s'execute sans erreur", {
  r <- enthalpie_reaction("CH4(g) + 2 O2(g) -> CO2(g) + 2 H2O(l)")
  pdf(NULL)
  expect_no_error(plot(r))
  expect_no_error(plot(enthalpie_reaction("H2O(l) -> H2O(g)")))
  invisible(dev.off())
})

test_that("enthalpie_combustion() retrouve les valeurs usuelles", {
  expect_equal(enthalpie_combustion("CH4")$dH, -890.30)
  expect_equal(enthalpie_combustion("C3H8")$dH, -2220.05)
  expect_equal(enthalpie_combustion("C4H10")$dH, -2876.89)
  expect_equal(enthalpie_combustion("C2H5OH")$dH, -1366.91)
  expect_equal(enthalpie_combustion("C6H12O6")$dH, -2802.74)
  expect_equal(enthalpie_combustion("C8H18")$dH, -5470.45)
  expect_equal(enthalpie_combustion("H2")$dH, dfH("H2O"))
  expect_equal(enthalpie_combustion("C")$dH, dfH("CO2"))
  expect_equal(enthalpie_combustion("CO")$dH, dfH("CO2") - dfH("CO"))
})

test_that("enthalpie_combustion() ecrit l'equation attendue", {
  expect_equal(format(enthalpie_combustion("CH4")$reaction),
               "CH4 + 2 O2(g) -> CO2(g) + 2 H2O(l)")
  expect_equal(format(enthalpie_combustion("H2")$reaction),
               "H2 + 1/2 O2(g) -> H2O(l)")
  expect_equal(format(enthalpie_combustion("C8H18")$reaction),
               "C8H18 + 25/2 O2(g) -> 8 CO2(g) + 9 H2O(l)")
  expect_equal(enthalpie_combustion("CH4")$dH_mol, enthalpie_combustion("CH4")$dH)
})

test_that("enthalpie_combustion() refuse ce qui ne brule pas", {
  expect_error(enthalpie_combustion("NH3"), "C, H et O")
  expect_error(enthalpie_combustion("CO2"), "combustible")
  expect_error(enthalpie_combustion("H2O"), "combustible")
  expect_error(enthalpie_combustion(c("CH4", "C2H6")), "longueur 1")
})

test_that("pouvoir_calorifique() est en kJ/g", {
  pc <- pouvoir_calorifique(c("H2", "CH4", "C2H5OH", "C6H12O6"))
  expect_equal(names(pc), c("H2", "CH4", "C2H5OH", "C6H12O6"))
  expect_equal(unname(pc["H2"]), 285.83 / 2.016)
  expect_equal(unname(pc["CH4"]), 890.30 / 16.043)
  expect_equal(unname(pc["C6H12O6"]), 2802.74 / 180.156)
  expect_true(all(pc > 0))
  # l'hydrogene est le meilleur combustible par gramme
  expect_equal(names(which.max(pc)), "H2")
})
