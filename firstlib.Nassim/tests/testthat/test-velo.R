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

test_that("calcul_distribution_semaine gère bien le paramètre filtre", {
  # 1. Test SANS le filtre (comportement par défaut)
  res_sans_filtre <- calcul_distribution_semaine(donnees_test, filtre = FALSE)

  # Le jour 1 doit avoir 60 trajets (10 + 50 car on garde l'anomalie)
  expect_equal(res_sans_filtre$trajets[res_sans_filtre$`Jour de la semaine` == 1], 60)

  # 2. Test AVEC le filtre
  res_avec_filtre <- calcul_distribution_semaine(donnees_test, filtre = TRUE)

  # Le jour 1 doit avoir 10 trajets (la ligne à 50 a été retirée par filtre_anomalie)
  expect_equal(res_avec_filtre$trajets[res_avec_filtre$`Jour de la semaine` == 1], 10)
})

test_that("plot_distribution_semaine genere bien un graphique", {
  # On utilise suppressWarnings pour dire à R d'ignorer l'avertissement sur les jours manquants
  res_plot <- suppressWarnings(plot_distribution_semaine(donnees_test))
  expect_true(inherits(res_plot, "ggplot"))
})

test_that("filtrer_trajet ne filtre rien si boucle est NULL", {
  # On appelle la fonction avec boucle = NULL
  res <- filtrer_trajet(donnees_test, boucle = NULL)

  # Le résultat doit avoir exactement le même nombre de lignes que le tableau de départ
  expect_equal(nrow(res), nrow(donnees_test))
})
