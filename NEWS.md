# enthalpie 0.1.0

Première version.

* Jeu de données `enthalpies_formation` (72 espèces, 298,15 K, 1 bar) avec la source de chaque valeur.
* Lecture et équilibrage d'équations : `lire_reaction()`, `est_equilibree()`, `equilibrer()`, `compter_elements()`, `masse_molaire()`.
* Enthalpies : `dfH()`, `chercher_espece()`, `enthalpie_reaction()`, `enthalpie_combustion()`, `pouvoir_calorifique()`, `loi_de_hess()`.
* Autres températures et solides ioniques : `kirchhoff()`, `born_haber()`.
* Estimation par énergies de liaison (valeurs fournies par l'utilisateur) : `enthalpie_liaisons()`.
* Graphiques : `graphique_dfH()`, `plot()` pour les réactions et les cycles de Born-Haber.
