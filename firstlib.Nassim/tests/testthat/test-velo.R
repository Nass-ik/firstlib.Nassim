# 1. Création d'un mini jeu de données pour les tests
donnees_test <- data.frame(
  `Numéro de boucle` = c("880", "880", "881"),
  `Boucle de comptage` = c("Boucle 1", "Boucle 1", "Boucle 2"),
  Total = c(10, 50, 20),
  `Probabilité de présence d'anomalies` = c(NA, "Forte", NA),
  `Jour de la semaine` = c(1, 1, 2),
  Jour = as.Date(c("2023-01-02", "2023-01-02", "2023-01-03")),
  check.names = FALSE # Obligatoire pour autoriser les espaces dans les noms de colonnes
)

# 2. Les tests unitaires
test_that("filtre_anomalie enlève bien les lignes non-NA", {
  res <- filtre_anomalie(donnees_test)
  # Sur 3 lignes, 1 a une anomalie "Forte", il doit donc rester 2 lignes
  expect_equal(nrow(res), 2)
})

test_that("compter_nombre_trajets fait bien la somme", {
  # 10 + 50 + 20 = 80
  expect_equal(compter_nombre_trajets(donnees_test), 80)
})

test_that("compter_nombre_boucle compte les valeurs uniques", {
  # "880" et "881", ça fait 2 boucles uniques
  expect_equal(compter_nombre_boucle(donnees_test), 2)
})

test_that("trouver_trajet_max isole la bonne ligne", {
  res <- trouver_trajet_max(donnees_test)
  # Le total maximum dans le mini-tableau est 50
  expect_equal(res$Total, 50)
})

test_that("filtrer_trajet conserve la bonne boucle", {
  res <- filtrer_trajet(donnees_test, boucle = c("881"))
  # Seule la boucle 881 est demandée, il y a 1 seule ligne de ce type
  expect_equal(nrow(res), 1)
  expect_equal(res$`Numéro de boucle`, "881")
})

test_that("calcul_distribution_semaine regroupe bien par jour", {
  res <- calcul_distribution_semaine(donnees_test)
  # Dans donnees_test, on a le jour 1 et le jour 2, il doit donc y avoir 2 lignes de résultat
  expect_equal(nrow(res), 2)
  # La somme du jour 2 est de 20
  expect_equal(res$trajets[res$`Jour de la semaine` == 2], 20)
})

test_that("plot_distribution_semaine genere bien un graphique", {
  # On teste simplement que la fonction arrive au bout et renvoie bien un objet ggplot
  res_plot <- plot_distribution_semaine(donnees_test)
  expect_true(inherits(res_plot, "ggplot"))
})
