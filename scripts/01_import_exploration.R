# ==============================================================================
# 01_import_exploration.R
# Etape 1 (Import) et Etape 2 (Exploration) du workflow Data Analyst
#
# Objectif : charger le dataset brut de consultations et comprendre sa
# structure, sa qualite et ses problemes AVANT toute action de nettoyage.
# ==============================================================================

source("scripts/00_utils.R")
suppressPackageStartupMessages(library(readr))

# ------------------------------------------------------------------------------
# 1. IMPORT
# ------------------------------------------------------------------------------
titre_section("1. IMPORT DES DONNEES")

consultations_brutes <- read_csv(
  CHEMIN_BRUT,
  col_types = cols(
    patient_id            = col_character(),
    consultation_date     = col_character(),  # texte : formats de date melanges
    region                = col_character(),
    district              = col_character(),
    facility_name         = col_character(),
    patient_age           = col_double(),
    gender                = col_character(),
    diagnosis             = col_character(),
    treatment_cost        = col_double(),
    medication_available  = col_character(),
    consultation_type     = col_character(),
    insurance_status      = col_character()
  ),
  locale = locale(encoding = "UTF-8")
)

cat("Dimensions du dataset brut :", nrow(consultations_brutes), "lignes x",
    ncol(consultations_brutes), "colonnes\n")

# ------------------------------------------------------------------------------
# 2. EXPLORATION -- comprendre avant d'agir
# ------------------------------------------------------------------------------
titre_section("2. STRUCTURE GENERALE")
glimpse(consultations_brutes)

titre_section("3. VALEURS MANQUANTES PAR COLONNE")
rapport_na <- consultations_brutes |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  tidyr::pivot_longer(everything(), names_to = "colonne", values_to = "nb_na") |>
  mutate(pct_na = round(100 * nb_na / nrow(consultations_brutes), 2)) |>
  arrange(desc(nb_na))
print(rapport_na, n = Inf)

titre_section("4. DOUBLONS")
cat("Lignes strictement identiques      :", sum(duplicated(consultations_brutes)), "\n")
cat("Doublons patient_id + date         :",
    sum(duplicated(consultations_brutes[, c("patient_id", "consultation_date")])), "\n")
cat("Identifiants patients uniques      :", n_distinct(consultations_brutes$patient_id), "\n")

titre_section("5. CATEGORIES INCOHERENTES (fautes de frappe, casse, abreviations)")
cat("--- region ---\n")
print(sort(unique(consultations_brutes$region)))
cat("\n--- gender ---\n")
print(sort(unique(consultations_brutes$gender)))
cat("\n--- consultation_type ---\n")
print(sort(unique(consultations_brutes$consultation_type)))
cat("\n--- medication_available ---\n")
print(sort(unique(consultations_brutes$medication_available)))
cat("\n--- insurance_status ---\n")
print(sort(unique(consultations_brutes$insurance_status)))
cat("\n--- diagnosis ---\n")
print(sort(unique(consultations_brutes$diagnosis)))

titre_section("6. FORMATS DE DATE MELANGES")
dates_iso_invalides <- sum(is.na(suppressWarnings(ymd(consultations_brutes$consultation_date, quiet = TRUE))))
cat("Dates non conformes au format AAAA-MM-JJ :", dates_iso_invalides,
    "(probablement au format JJ/MM/AAAA)\n")

titre_section("7. VALEURS ABERRANTES -- AGE")
print(summary(consultations_brutes$patient_age))
cat("Ages negatifs      :", sum(consultations_brutes$patient_age < AGE_MIN_PLAUSIBLE, na.rm = TRUE), "\n")
cat("Ages > 100 ans      :", sum(consultations_brutes$patient_age > AGE_MAX_PLAUSIBLE, na.rm = TRUE), "\n")

titre_section("8. VALEURS ABERRANTES -- COUT DE TRAITEMENT")
print(summary(consultations_brutes$treatment_cost))
cat("Couts negatifs                  :", sum(consultations_brutes$treatment_cost < 0, na.rm = TRUE), "\n")
cat("Couts extremes (> 500 USD)      :", sum(consultations_brutes$treatment_cost > COUT_MAX_PLAUSIBLE, na.rm = TRUE), "\n")

titre_section("9. SYNTHESE DES PROBLEMES IDENTIFIES")
cat("
  - Valeurs manquantes   : region, gender, diagnosis, treatment_cost, insurance_status (~5-8% chacune)
  - Doublons              : lignes identiques + doublons patient_id/date (double saisie)
  - Categories incoherentes : region (CENTER/Ctr/centre...), gender (M/F/male/Masculin...)
  - Dates incoherentes    : melange AAAA-MM-JJ et JJ/MM/AAAA
  - Valeurs aberrantes    : ages negatifs ou > 100 ans ; couts negatifs ou = 50000 (erreur de saisie)

  --> Voir scripts/02_nettoyage.R pour le traitement de chacun de ces problemes.
\n")
