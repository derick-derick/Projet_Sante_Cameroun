# ==============================================================================
# 03_transformation.R
# Etape 4 (Transformation) du workflow Data Analyst
#
# Cree les variables derivees necessaires aux analyses business, aux
# visualisations et au dashboard Shiny.
# ==============================================================================

source("scripts/00_utils.R")
suppressPackageStartupMessages(library(readr))

consultations <- read_csv(CHEMIN_CLEAN, show_col_types = FALSE,
                           locale = locale(encoding = "UTF-8"))

consultations_final <- consultations |>
  mutate(
    # --- Temporel -----------------------------------------------------------
    annee          = year(consultation_date),
    mois           = month(consultation_date),
    mois_label     = factor(MOIS_FR[mois], levels = MOIS_FR),
    trimestre      = paste0("T", quarter(consultation_date)),
    jour_semaine   = factor(JOURS_FR[wday(consultation_date)], levels = JOURS_FR),

    # --- Demographie ----------------------------------------------------------
    tranche_age    = cut(patient_age, breaks = BORNES_AGE, labels = LABELS_AGE, right = TRUE),

    # --- Cout -------------------------------------------------------------
    categorie_cout = case_when(
      is.na(treatment_cost)      ~ NA_character_,
      treatment_cost < 10        ~ "Faible (< 10)",
      treatment_cost < 25        ~ "Modéré (10-25)",
      TRUE                        ~ "Élevé (> 25)"
    ),
    categorie_cout = factor(categorie_cout, levels = CATEGORIES_COUT),

    # --- Indicateurs booleens utiles au dashboard --------------------------
    en_rupture_stock   = medication_available == "Stockout",
    est_assure         = insurance_status == "Insured",

    # --- Qualite de la ligne (pour le tableau "Qualite des donnees") --------
    donnee_incomplete  = is.na(patient_age) | is.na(treatment_cost) |
      diagnosis == "Non renseigné" | gender == "Non renseigné" |
      region == "Non renseigné" | insurance_status == "Non renseigné"
  )

titre_section("APERCU DES VARIABLES CREEES")
consultations_final |>
  select(consultation_date, annee, mois_label, trimestre, jour_semaine,
         patient_age, tranche_age, treatment_cost, categorie_cout,
         en_rupture_stock, est_assure, donnee_incomplete) |>
  glimpse()

write_csv(consultations_final, CHEMIN_FINAL)
cat("\nDataset final (nettoye + transforme) ecrit dans :", CHEMIN_FINAL, "\n")
cat("Colonnes :", ncol(consultations_final), " | Lignes :", nrow(consultations_final), "\n")
