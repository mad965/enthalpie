#' enthalpie : enthalpie standard de formation en R
#'
#' L'enthalpie standard de formation \eqn{\Delta_f H^\circ} d'une espèce est
#' la variation d'enthalpie de la réaction qui forme une mole de cette espèce à
#' partir de ses éléments pris dans leur état de référence, à 1 bar et
#' (conventionnellement) 298,15 K. Le paquet permet de :
#'
#' * consulter un jeu de données d'enthalpies de formation
#'   ([enthalpies_formation], [dfH()], [chercher_espece()]) ;
#' * lire et équilibrer des équations ([lire_reaction()], [equilibrer()]) ;
#' * calculer \eqn{\Delta_r H^\circ} par la loi de Hess
#'   ([enthalpie_reaction()], [loi_de_hess()]) et les enthalpies de combustion
#'   ([enthalpie_combustion()], [pouvoir_calorifique()]) ;
#' * passer à une autre température ([kirchhoff()]) ;
#' * décomposer la formation d'un solide ionique ([born_haber()]) ;
#' * estimer une enthalpie par les énergies de liaison
#'   ([enthalpie_liaisons()]) ;
#' * tracer des diagrammes ([graphique_dfH()] et les méthodes `plot()`).
#'
#' @keywords internal
"_PACKAGE"
