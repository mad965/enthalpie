# Sequence de travail du developpeur (a lancer depuis la racine du paquet).
# Ligne par ligne dans RStudio, pas en bloc.

# 0. Une seule fois : outils
# install.packages(c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown"))

# 1. Regenerer les donnees si le CSV a change (facultatif)
# source("data-raw/enthalpies_formation.R")

# 2. Generer man/ et verifier NAMESPACE a partir des commentaires roxygen2
devtools::document()

# 3. Charger le paquet sans l'installer et l'essayer
devtools::load_all()
enthalpie_combustion("CH4")

# 4. Tests unitaires
devtools::test()

# 5. Verification complete (documentation, exemples, tests, vignette)
#    Resultat attendu : 0 errors | 0 warnings | 0 notes
devtools::check()

# 6. Installer localement, puis construire le .tar.gz si besoin
devtools::install()
# devtools::build()

# 7. Publier sur GitHub : voir README.md (section « Publication sur GitHub »)
