#' Filtrer les anomalies
#'
#' @description Conserve uniquement les trajets où aucune anomalie n'est détectée.
#' @param trajet Un data.frame contenant les données vélos.
#' @return Un data.frame filtré.
#' @importFrom dplyr filter
#' @export
filtre_anomalie <- function(trajet){
  trajet |>
    filter(is.na(`Probabilité de présence d'anomalies`))
}

#' Compter le nombre de trajets
#'
#' @description Calcule le total des trajets dans le jeu de données.
#' @param trajet Un data.frame contenant une colonne 'Total'.
#' @return La somme totale des trajets.
#' @importFrom dplyr pull
#' @export
compter_nombre_trajets <- function(trajet){
  trajet |>
    pull(Total) |>
    sum()
}

#' Compter le nombre de boucles
#' @description Calcule le nombre de boucles de comptage uniques.
#' @param trajet Un data.frame contenant une colonne 'Numéro de boucle'.
#' @return Le nombre de boucles distinctes.
#' @importFrom dplyr pull n_distinct
#' @export
compter_nombre_boucle <- function(trajet){
  trajet |>
    pull(`Numéro de boucle`) |>
    n_distinct()
}

#' Trouver le trajet maximum
#'
#' @description Isole la ligne comportant le plus grand nombre de trajets.
#' @param trajet Un data.frame contenant les colonnes 'Total', 'Jour' et 'Boucle de comptage'.
#' @return Un data.frame d'une ligne avec le record.
#' @importFrom dplyr slice_max select
#' @export
trouver_trajet_max <- function(trajet){
  trajet |>
    slice_max(Total) |>
    select(`Boucle de comptage`, Jour, Total)
}

#' Calculer la distribution par semaine
#' @description Groupe et additionne les trajets par jour de la semaine.
#' @param trajet Un data.frame contenant une colonne 'Jour de la semaine'.
#' @return Un data.frame avec le total par jour.
#' @importFrom dplyr count
#' @export
calcul_distribution_semaine <- function(trajet){
  trajet |>
    count(`Jour de la semaine`, wt = Total, sort = TRUE, name = "trajets")
}

#' Tracer la distribution par semaine
#' @description Génère un graphique en barres de la fréquentation par jour.
#' @param trajet Un data.frame de trajets bruts.
#' @return Un graphique ggplot.
#' @importFrom dplyr mutate
#' @importFrom forcats fct_recode
#' @importFrom ggplot2 ggplot aes geom_col
#' @export
plot_distribution_semaine <- function(trajet) {
  trajet_weekday <- trajet |>
    filtre_anomalie() |>
    calcul_distribution_semaine() |>
    mutate(
      jour = fct_recode(
        factor(`Jour de la semaine`),
        "lundi" = "1",
        "mardi" = "2",
        "mercredi" = "3",
        "jeudi" = "4",
        "vendredi" = "5",
        "samedi" = "6",
        "dimanche" = "7"
      )
    )

  ggplot(trajet_weekday) +
    aes(x = jour, y = trajets) +
    geom_col()
}

#' Filtrer les trajets par numéro de boucle
#'
#' @description Conserve uniquement les données correspondant aux boucles spécifiées.
#' @param trajet Un data.frame contenant les données vélos.
#' @param boucle Un vecteur contenant les numéros de boucle à garder. Si NULL, ne filtre pas.
#' @return Un data.frame filtré.
#' @importFrom dplyr filter
#' @export
filtrer_trajet <- function(trajet, boucle = NULL) {
  # Si boucle est NULL, on renvoie le tableau intact tout de suite
  if (is.null(boucle)) {
    return(trajet)
  }

  # Sinon, on filtre normalement
  trajet |>
    dplyr::filter(`Numéro de boucle` %in% boucle)
}
