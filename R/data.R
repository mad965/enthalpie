#' Enthalpies standard de formation de 72 espèces
#'
#' Enthalpies standard de formation \eqn{\Delta_f H^\circ} (kJ/mol, 298,15 K,
#' 1 bar) d'éléments dans leur état de référence et de composés usuels
#' (inorganiques et organiques). Les éléments à l'état de référence ont une
#' enthalpie de formation nulle **par convention**.
#'
#' @format Un `data.frame` de 72 lignes et 7 colonnes :
#' \describe{
#'   \item{formule}{Formule brute, par exemple `"CH4"`, `"Ca(OH)2"`.}
#'   \item{nom}{Nom français de l'espèce.}
#'   \item{etat}{État physique : `"s"` (solide), `"l"` (liquide), `"g"` (gaz),
#'     `"aq"` (dissous) ou `"diamant"`.}
#'   \item{espece}{Identifiant `formule(etat)`, par exemple `"H2O(l)"`.}
#'   \item{dfH}{Enthalpie standard de formation, en kJ/mol.}
#'   \item{source}{Code de la source (voir ci-dessous).}
#'   \item{stable}{`TRUE` si c'est l'état le plus stable de la formule dans
#'     les conditions standard ; c'est la ligne utilisée quand l'état n'est
#'     pas précisé (`"H2O"` désigne l'eau liquide, `"C"` le graphite).}
#' }
#'
#' @details
#' Les valeurs diffèrent parfois de quelques dixièmes de kJ/mol d'une table à
#' l'autre (méthodes de mesure, révisions). Pour un devoir, les tables du cours
#' font foi : on peut modifier `data-raw/enthalpies_formation.csv` puis
#' regénérer le jeu de données avec `data-raw/enthalpies_formation.R`.
#'
#' Codes de la colonne `source` :
#' * `convention` : élément à l'état de référence, valeur nulle par définition ;
#' * `ET` : tables de l'EngineeringToolbox ;
#' * `WIKI` : tableau « Standard enthalpy change of formation (data table) »
#'   de Wikipédia ;
#' * `JANAF` : tables NIST-JANAF ;
#' * `NIST` : NIST Chemistry WebBook ;
#' * `MANUEL` : valeur classique de manuel, saisie sans avoir été revérifiée
#'   dans une table en ligne pendant la réalisation du projet.
#'
#' @source Voir le détail ci-dessus ; références complètes dans le fichier
#'   `README.md` du dépôt.
#'
#' @examples
#' head(enthalpies_formation)
#' enthalpies_formation[enthalpies_formation$stable & enthalpies_formation$dfH > 100, ]
"enthalpies_formation"
