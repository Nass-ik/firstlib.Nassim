#' Données de comptage de vélos à Nantes (Toussaint 2025)
#'
#' Un jeu de données contenant les comptages horaires des boucles de
#' comptage de Nantes Métropole durant les vacances de la Toussaint 2025.
#'
#' @format Un data.frame avec les colonnes suivantes :
#' \describe{
#'   \item{Numéro de boucle}{Identifiant unique de la boucle}
#'   \item{Jour}{Date du comptage}
#'   \item{Total}{Nombre total de passages sur la journée}
#'   \item{Jour de la semaine}{Identifiant du jour (1 pour lundi, etc.)}
#'   \item{Probabilité de présence d'anomalies}{Indicateur de fiabilité des données}
#'   \item{Boucle de comptage}{Nom complet et direction de la boucle}
#'   # Ajoute ici les autres colonnes importantes si nécessaire
#' }
#' @source \url{https://data.nantesmetropole.fr/}
"df_velo"
