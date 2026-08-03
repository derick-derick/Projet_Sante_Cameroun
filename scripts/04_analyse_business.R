# ==============================================================================
# 04_analyse_business.R
# Etape 5 (Analyse business) du workflow Data Analyst
#
# Repond aux questions metier posees par un programme de sante publique :
#   Q1. Ou se concentre le volume de consultations ?
#   Q2. Quels sont les diagnostics les plus frequents, et a quel cout ?
#   Q3. Ou les ruptures de stock de medicaments sont-elles les plus critiques ?
#   Q4. Quelle est la couverture d'assurance et influence-t-elle le cout ?
#   Q5. Qui sont les patients touches par chaque pathologie (age/genre) ?
#   Q6. Le volume de consultations varie-t-il dans l'annee (saisonnalite) ?
#
# Chaque reponse est une table exploitable par le dashboard Shiny et le
# rapport final, et enregistree dans resultats/tables/.
# ==============================================================================

source("scripts/00_utils.R")
suppressPackageStartupMessages(library(readr))

dir.create("resultats/tables", showWarnings = FALSE, recursive = TRUE)

df <- read_csv(CHEMIN_FINAL, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) |>
  restaurer_facteurs()

# ------------------------------------------------------------------------------
# Q1. Volume de consultations par region et par district
# ------------------------------------------------------------------------------
volume_region <- df |>
  count(region, name = "nb_consultations") |>
  arrange(desc(nb_consultations)) |>
  mutate(part_pct = round(100 * nb_consultations / sum(nb_consultations), 2))

volume_district <- df |>
  count(region, district, name = "nb_consultations") |>
  arrange(desc(nb_consultations))

# ------------------------------------------------------------------------------
# Q2. Diagnostics les plus frequents et cout moyen associe
# ------------------------------------------------------------------------------
diagnostics_frequence <- df |>
  filter(diagnosis != "Non renseigné") |>
  count(diagnosis, name = "nb_cas") |>
  arrange(desc(nb_cas)) |>
  mutate(part_pct = round(100 * nb_cas / sum(nb_cas), 2))

cout_moyen_diagnostic <- df |>
  filter(diagnosis != "Non renseigné", !is.na(treatment_cost)) |>
  group_by(diagnosis) |>
  summarise(
    n = n(),
    cout_moyen = round(mean(treatment_cost), 2),
    cout_median = round(median(treatment_cost), 2),
    .groups = "drop"
  ) |>
  arrange(desc(cout_moyen))

# ------------------------------------------------------------------------------
# Q3. Ruptures de stock : ou et sur quels diagnostics ?
# ------------------------------------------------------------------------------
rupture_par_region <- df |>
  group_by(region) |>
  summarise(
    n = n(),
    taux_rupture_pct = round(100 * mean(en_rupture_stock), 2),
    .groups = "drop"
  ) |>
  arrange(desc(taux_rupture_pct))

rupture_par_diagnostic <- df |>
  filter(diagnosis != "Non renseigné") |>
  group_by(diagnosis) |>
  summarise(taux_rupture_pct = round(100 * mean(en_rupture_stock), 2), .groups = "drop") |>
  arrange(desc(taux_rupture_pct))

rupture_par_etablissement <- df |>
  group_by(region, district, facility_name) |>
  summarise(n = n(), taux_rupture_pct = round(100 * mean(en_rupture_stock), 2), .groups = "drop") |>
  filter(n >= 20) |>                      # seuil de fiabilite statistique
  arrange(desc(taux_rupture_pct))

# ------------------------------------------------------------------------------
# Q4. Couverture d'assurance et impact sur le cout
# ------------------------------------------------------------------------------
assurance_par_region <- df |>
  group_by(region) |>
  summarise(taux_assurance_pct = round(100 * mean(est_assure), 2), .groups = "drop") |>
  arrange(desc(taux_assurance_pct))

cout_par_assurance <- df |>
  filter(!is.na(treatment_cost)) |>
  group_by(insurance_status) |>
  summarise(cout_moyen = round(mean(treatment_cost), 2), n = n(), .groups = "drop")

# ------------------------------------------------------------------------------
# Q5. Profil demographique par diagnostic (age / genre)
# ------------------------------------------------------------------------------
profil_age_diagnostic <- df |>
  filter(diagnosis != "Non renseigné", !is.na(tranche_age)) |>
  count(diagnosis, tranche_age) |>
  group_by(diagnosis) |>
  mutate(part_pct = round(100 * n / sum(n), 1)) |>
  ungroup()

profil_genre_diagnostic <- df |>
  filter(diagnosis != "Non renseigné", gender != "Non renseigné") |>
  count(diagnosis, gender) |>
  group_by(diagnosis) |>
  mutate(part_pct = round(100 * n / sum(n), 1)) |>
  ungroup()

# ------------------------------------------------------------------------------
# Q6. Tendance mensuelle des consultations (saisonnalite)
# ------------------------------------------------------------------------------
tendance_mensuelle <- df |>
  count(mois, mois_label, name = "nb_consultations") |>
  arrange(mois)

tendance_mensuelle_diagnostic <- df |>
  filter(diagnosis != "Non renseigné") |>
  count(mois, mois_label, diagnosis, name = "nb_cas") |>
  arrange(diagnosis, mois)

# ------------------------------------------------------------------------------
# Enregistrement des tables de resultats
# ------------------------------------------------------------------------------
enregistrer_tables <- function(objets, noms) {
  for (i in seq_along(objets)) {
    write_csv(objets[[i]], file.path("resultats/tables", paste0(noms[i], ".csv")))
  }
}

objets <- list(volume_region, volume_district, diagnostics_frequence, cout_moyen_diagnostic,
                rupture_par_region, rupture_par_diagnostic, rupture_par_etablissement,
                assurance_par_region, cout_par_assurance, profil_age_diagnostic,
                profil_genre_diagnostic, tendance_mensuelle, tendance_mensuelle_diagnostic)
noms <- c("volume_region", "volume_district", "diagnostics_frequence", "cout_moyen_diagnostic",
          "rupture_par_region", "rupture_par_diagnostic", "rupture_par_etablissement",
          "assurance_par_region", "cout_par_assurance", "profil_age_diagnostic",
          "profil_genre_diagnostic", "tendance_mensuelle", "tendance_mensuelle_diagnostic")
enregistrer_tables(objets, noms)

# ------------------------------------------------------------------------------
# SYNTHESE -- reponses directes aux questions metier
# ------------------------------------------------------------------------------
titre_section("Q1. VOLUME DE CONSULTATIONS PAR REGION")
print(volume_region)

titre_section("Q2. DIAGNOSTICS LES PLUS FREQUENTS")
print(diagnostics_frequence)
titre_section("COUT MOYEN PAR DIAGNOSTIC")
print(cout_moyen_diagnostic)

titre_section("Q3. TAUX DE RUPTURE DE STOCK PAR REGION")
print(rupture_par_region)
titre_section("ETABLISSEMENTS LES PLUS CRITIQUES (rupture de stock)")
print(head(rupture_par_etablissement, 10))

titre_section("Q4. COUVERTURE D'ASSURANCE")
cat("Taux de couverture global :", round(100 * mean(df$est_assure), 2), "%\n")
print(assurance_par_region)
print(cout_par_assurance)

titre_section("Q6. TENDANCE MENSUELLE DES CONSULTATIONS")
print(tendance_mensuelle)

cat("\nToutes les tables de resultats ont ete enregistrees dans resultats/tables/\n")
