# enthalpie

<!-- badges: start -->
[![R-CMD-check](https://github.com/mad965/enthalpie/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mad965/enthalpie/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Un paquet R pédagogique autour de l'**enthalpie standard de formation**
$\Delta_f H^\circ$ : jeu de données, lecture et équilibrage d'équations, calcul
de $\Delta_r H^\circ$ par la loi de Hess, enthalpies de combustion, pouvoir
calorifique, loi de Kirchhoff, cycle de Born-Haber, modélisation et graphiques.

Projet réalisé pour le cours d'introduction au langage R (Licence 3 de
mathématiques, Université de Bertoua).

> Remplacez `VOTRE-UTILISATEUR` par votre nom d'utilisateur GitHub dans
> ce fichier et dans `DESCRIPTION` (champs `URL` et `BugReports`), ainsi que
> l'adresse e-mail dans `Authors@R`.

## Installation

    # install.packages("remotes")
    remotes::install_github("mad965/enthalpie")

## Exemple

    library(enthalpie)
    
    dfH(c("CO2", "H2O", "H2O(g)"))     # kJ/mol : -393.51 -285.83 -241.83
    
    enthalpie_reaction("CH4(g) + 2 O2(g) -> CO2(g) + 2 H2O(l)", par = "CH4")
    #> Réaction : CH4(g) + 2 O2(g) -> CO2(g) + 2 H2O(l)
    #>
    #>   Espèce             nu   dfH (kJ/mol)    nu*dfH (kJ)
    #>   CH4(g)             -1         -74.87          74.87
    #>   O2(g)              -2           0.00           0.00
    #>   CO2(g)             +1        -393.51        -393.51
    #>   H2O(l)             +2        -285.83        -571.66
    #>
    #> ΔrH° = -890.30 kJ  (exothermique)
    #> ΔrH° par mole de CH4 : -890.30 kJ/mol
    
    equilibrer("C3H8 + O2 -> CO2 + H2O")
    #> C3H8 + 5 O2 -> 3 CO2 + 4 H2O
    
    enthalpie_combustion("C2H5OH")$dH   # -1366.91 kJ/mol
    pouvoir_calorifique("H2")           # ≈ 141,8 kJ/g
    
    # Modélisation et visualisation
    modele <- ajuster_modele_dfH(enthalpies_formation)
    tracer_modele_dfH_ameliore(modele, variable_gradient = "nC")

## Fonctions

| Fonction | Rôle |
|---|---|
| `enthalpies_formation` | Jeu de données : 72 espèces avec la source de chaque valeur |
| `dfH()`, `chercher_espece()` | Lire une valeur, chercher une espèce |
| `lire_reaction()`, `est_equilibree()`, `equilibrer()` | Lire, vérifier, équilibrer une équation |
| `compter_elements()`, `compter_atomes()`, `masse_molaire()` | Analyse d'une formule brute |
| `enthalpie_reaction()` | $\Delta_r H^\circ = \sum \nu\,\Delta_f H^\circ$, `print()` et `plot()` |
| `enthalpie_combustion()`, `pouvoir_calorifique()` | Combustion complète (C-H-O), kJ/mol et kJ/g |
| `loi_de_hess()` | Combinaison linéaire de réactions |
| `kirchhoff()` | $\Delta_r H^\circ(T)$ à partir de $\Delta C_p$ |
| `born_haber()` | Cycle de Born-Haber d'un solide ionique |
| `enthalpie_liaisons()` | Estimation par énergies de liaison (valeurs fournies par l'utilisateur) |
| `graphique_dfH()` | Diagramme en barres de plusieurs $\Delta_f H^\circ$ |
| `ajuster_modele_dfH()` | Régression multiple $\Delta_f H^\circ \sim n_C + n_H + n_O$ |
| `tracer_modele_dfH_ameliore()` | Prédit vs observé, couleurs par état, dégradé par $n_C$, $n_H$ ou $n_O$ |
| `preparer_donnees_modele()` | Ajoute `nC`, `nH`, `nO` à un data.frame |

La vignette `vignette("introduction", package = "enthalpie")` déroule tout cela
avec le rappel théorique.

## Modélisation

Le package inclut un modèle de régression multiple pour l'enthalpie standard
de formation :

$$
\widehat{\Delta_f H^\circ} = \beta_0 + \beta_1 n_C + \beta_2 n_H + \beta_3 n_O
$$

où $n_C$, $n_H$ et $n_O$ sont les nombres d'atomes de carbone, d'hydrogène
et d'oxygène. Les coefficients sont estimés par moindres carrés ordinaires
(OLS), via `lm()`.

**Limite assumée** : ce modèle capture une tendance globale mais échoue sur
les isomères, les états physiques, les liaisons multiples et les cycles. Il
sert de point de comparaison pour justifier des descripteurs plus fins.

    library(enthalpie)
    
    # Ajustement sur les 72 espèces
    modele <- ajuster_modele_dfH(enthalpies_formation)
    summary(modele)
    
    # Prédit vs observé, couleurs par état, dégradé par nC
    tracer_modele_dfH_ameliore(modele, variable_gradient = "nC")
    
    # Variantes : nH ou nO
    tracer_modele_dfH_ameliore(modele, variable_gradient = "nH")
    tracer_modele_dfH_ameliore(modele, variable_gradient = "nO")

## Données et sources

Chaque ligne de `enthalpies_formation` indique sa source (colonne `source`) :

| Code | Source |
|---|---|
| `convention` | élément à l'état de référence : 0 par définition |
| `ET` | [EngineeringToolbox](https://www.engineeringtoolbox.com), tables d'enthalpies de formation |
| `WIKI` | Wikipédia, [Standard enthalpy change of formation (data table)](https://en.wikipedia.org/wiki/Standard_enthalpy_change_of_formation_(data_table)) |
| `JANAF` | [Tables NIST-JANAF](https://janaf.nist.gov) |
| `NIST` | [NIST Chemistry WebBook](https://webbook.nist.gov) |
| `MANUEL` | valeur classique de manuel, saisie **sans** avoir été revérifiée dans une table en ligne |

Les tables ne s'accordent pas toujours au dixième de kJ/mol. Pour un devoir,
**la table du cours fait foi** : modifiez `data-raw/enthalpies_formation.csv`,
puis lancez `source("data-raw/enthalpies_formation.R")` pour régénérer
`data/enthalpies_formation.R`.

Les énergies de liaison ne sont pas embarquées : `enthalpie_liaisons()` les
demande en argument.

## Développement

Depuis la racine du dépôt, dans R :

    install.packages(c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown",
                       "ggplot2"))
    
    devtools::document()   # génère man/ et NAMESPACE depuis les commentaires roxygen2
    devtools::test()       # tests unitaires (testthat)
    devtools::check()      # vérification complète : viser 0 errors, 0 warnings, 0 notes

Organisation :

    DESCRIPTION  NAMESPACE  LICENSE  NEWS.md  README.md
    R/            code du paquet (un fichier par thème)
    data/         jeu de données (enthalpies_formation.R, ASCII)
    data-raw/     CSV source + script de régénération
    tests/        tests testthat
    vignettes/    introduction.Rmd
    dev/          séquence de commandes du développeur
    beamer/       présentation (enthalpie_presentation.tex et .pdf, compilation : xelatex)
    .github/      vérification automatique (GitHub Actions)

Le code R ne contient que des caractères ASCII (les accents des chaînes sont
écrits `é`, etc.), comme l'exige `R CMD check` ; les commentaires roxygen2
peuvent, eux, contenir des accents.

## Publication sur GitHub

1. Sur <https://github.com>, créez un dépôt **vide** nommé `enthalpie`
   (ne cochez ni README, ni .gitignore, ni licence).
2. Dans R, depuis la racine du paquet, lancez `devtools::document()` : cela
   crée le dossier `man/` (pages d'aide), à versionner avec le reste. Lancez
   aussi `devtools::check()` et corrigez ce qu'il signale.
3. Dans un terminal, toujours depuis le dossier du paquet :

       git init -b main
       git add .
       git commit -m "Version initiale du paquet enthalpie"
       git remote add origin https://github.com/VOTRE-UTILISATEUR/enthalpie.git
       git push -u origin main

   GitHub n'accepte plus le mot de passe du compte pour `git push` : au moment
   de l'authentification, utilisez un *Personal Access Token* (GitHub →
   Settings → Developer settings), ou connectez-vous avec GitHub Desktop ou
   `gh auth login`.
4. Vérifiez l'onglet **Actions** du dépôt : la vérification `R-CMD-check`
   doit passer au vert.
5. N'importe qui peut alors installer le paquet avec
   `remotes::install_github("VOTRE-UTILISATEUR/enthalpie")`.

Variante entièrement depuis R : `usethis::use_git()` puis `usethis::use_github()`.

## Licence

MIT, voir `LICENSE`.