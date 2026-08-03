# ==============================================================================
# 06_statistiques_descriptives.R
# Etape 7 (Statistiques descriptives) du workflow Data Analyst
#
# Resume les variables numeriques cles (age, cout de traitement) : tendance
# centrale, dispersion et distribution -- globalement et par sous-groupe.
# ==============================================================================

source("scripts/00_utils.R")
suppressPackageStartupMessages(library(readr))

dir.create("resultats/tables", showWarnings = FALSE, recursive = TRUE)
df <- read_csv(CHEMIN_FINAL, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) |>
  restaurer_facteurs()

resumer_variable <- function(x) {
  x <- x[!is.na(x)]
  tibble::tibble(
    n           = length(x),
    moyenne     = round(mean(x), 2),
    mediane     = round(median(x), 2),
    ecart_type  = round(sd(x), 2),
    variance    = round(var(x), 2),
    min         = round(min(x), 2),
    q1          = round(quantile(x, 0.25), 2),
    q3          = round(quantile(x, 0.75), 2),
    max         = round(max(x), 2)
  )
}

# ------------------------------------------------------------------------------
# Statistiques globales
# ------------------------------------------------------------------------------
titre_section("STATISTIQUES DESCRIPTIVES -- AGE DES PATIENTS (global)")
stats_age_global <- resumer_variable(df$patient_age)
print(stats_age_global)

titre_section("STATISTIQUES DESCRIPTIVES -- COUT DE TRAITEMENT (global)")
stats_cout_global <- resumer_variable(df$treatment_cost)
print(stats_cout_global)

# ------------------------------------------------------------------------------
# Statistiques par sous-groupe
# ------------------------------------------------------------------------------
stats_age_par_diagnostic <- df |>
  filter(diagnosis != "Non renseigné", !is.na(patient_age)) |>
  group_by(diagnosis) |>
  summarise(
    n = n(), moyenne = round(mean(patient_age), 1), mediane = round(median(patient_age), 1),
    ecart_type = round(sd(patient_age), 1), .groups = "drop"
  ) |>
  arrange(desc(moyenne))

stats_cout_par_region <- df |>
  filter(!is.na(treatment_cost)) |>
  group_by(region) |>
  summarise(
    n = n(), moyenne = round(mean(treatment_cost), 2), mediane = round(median(treatment_cost), 2),
    ecart_type = round(sd(treatment_cost), 2), .groups = "drop"
  ) |>
  arrange(desc(moyenne))

stats_cout_par_type_consultation <- df |>
  filter(!is.na(treatment_cost)) |>
  group_by(consultation_type) |>
  summarise(
    n = n(), moyenne = round(mean(treatment_cost), 2), mediane = round(median(treatment_cost), 2),
    ecart_type = round(sd(treatment_cost), 2), .groups = "drop"
  ) |>
  arrange(desc(moyenne))

# ------------------------------------------------------------------------------
# Table de distribution par tranche d'age (frequence + pourcentage)
# ------------------------------------------------------------------------------
distribution_tranche_age <- df |>
  filter(!is.na(tranche_age)) |>
  count(tranche_age, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1))

titre_section("REPARTITION PAR TRANCHE D'AGE")
print(distribution_tranche_age)

titre_section("AGE MOYEN PAR DIAGNOSTIC (du plus age au plus jeune en moyenne)")
print(stats_age_par_diagnostic)

titre_section("COUT DE TRAITEMENT PAR REGION")
print(stats_cout_par_region)

titre_section("COUT DE TRAITEMENT PAR TYPE DE CONSULTATION")
print(stats_cout_par_type_consultation)

# ------------------------------------------------------------------------------
# Enregistrement
# ------------------------------------------------------------------------------
write_csv(stats_age_global, "resultats/tables/stats_age_global.csv")
write_csv(stats_cout_global, "resultats/tables/stats_cout_global.csv")
write_csv(stats_age_par_diagnostic, "resultats/tables/stats_age_par_diagnostic.csv")
write_csv(stats_cout_par_region, "resultats/tables/stats_cout_par_region.csv")
write_csv(stats_cout_par_type_consultation, "resultats/tables/stats_cout_par_type_consultation.csv")
write_csv(distribution_tranche_age, "resultats/tables/distribution_tranche_age.csv")

cat("\nTables de statistiques descriptives enregistrees dans resultats/tables/\n")
