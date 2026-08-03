# ==============================================================================
# 02_nettoyage.R
# Etape 3 (Data Cleaning) du workflow Data Analyst
#
# Traite, dans l'ordre, chacun des problemes identifies lors de l'exploration :
#   a) doublons (lignes identiques + doublons patient_id/date)
#   b) categories incoherentes (region, gender)
#   c) dates incoherentes (formats melanges)
#   d) valeurs aberrantes (age, cout de traitement)
#   e) valeurs manquantes (categorielles -> "Non renseigne", numeriques -> NA
#      conserve, a exclure explicitement des agregations avec na.rm = TRUE)
#
# Choix methodologique : on NE SUPPRIME PAS les lignes a cause d'un NA dans une
# seule colonne (ex : diagnosis manquant) car les autres colonnes restent
# exploitables. On documente la qualite avant/apres au lieu de masquer le
# probleme.
# ==============================================================================

source("scripts/00_utils.R")
suppressPackageStartupMessages({
  library(readr)
  library(tidyr)
})

consultations_brutes <- read_csv(CHEMIN_BRUT, show_col_types = FALSE,
                                  locale = locale(encoding = "UTF-8"))
n_brut <- nrow(consultations_brutes)

# ------------------------------------------------------------------------------
# a) DOUBLONS
# ------------------------------------------------------------------------------
consultations <- consultations_brutes |> distinct()
n_apres_doublons_exacts <- nrow(consultations)

consultations <- consultations |>
  distinct(patient_id, consultation_date, .keep_all = TRUE)
n_apres_doublons_id_date <- nrow(consultations)

# ------------------------------------------------------------------------------
# b) CATEGORIES INCOHERENTES
# ------------------------------------------------------------------------------
consultations <- consultations |>
  mutate(
    region = standardiser_region(region),
    gender = standardiser_genre(gender),
    facility_name = str_trim(facility_name),
    district = str_trim(district)
  )

# ------------------------------------------------------------------------------
# c) DATES INCOHERENTES
# ------------------------------------------------------------------------------
consultations <- consultations |>
  mutate(consultation_date = parser_date_mixte(consultation_date))

n_dates_invalides <- sum(is.na(consultations$consultation_date))

# ------------------------------------------------------------------------------
# d) VALEURS ABERRANTES -> mises a NA (traitees comme donnees manquantes)
# ------------------------------------------------------------------------------
n_age_aberrant  <- sum(consultations$patient_age < AGE_MIN_PLAUSIBLE |
                          consultations$patient_age > AGE_MAX_PLAUSIBLE, na.rm = TRUE)
n_cout_aberrant <- sum(consultations$treatment_cost < COUT_MIN_PLAUSIBLE |
                          consultations$treatment_cost > COUT_MAX_PLAUSIBLE, na.rm = TRUE)

consultations <- consultations |>
  mutate(
    patient_age = if_else(patient_age < AGE_MIN_PLAUSIBLE | patient_age > AGE_MAX_PLAUSIBLE,
                           NA_real_, patient_age),
    treatment_cost = if_else(treatment_cost < COUT_MIN_PLAUSIBLE | treatment_cost > COUT_MAX_PLAUSIBLE,
                              NA_real_, treatment_cost)
  )

# ------------------------------------------------------------------------------
# e) VALEURS MANQUANTES CATEGORIELLES -> categorie explicite "Non renseigne"
#    (on garde la ligne : les autres variables restent utiles pour l'analyse)
# ------------------------------------------------------------------------------
consultations <- consultations |>
  mutate(
    region              = replace_na(region, "Non renseigné"),
    gender              = replace_na(gender, "Non renseigné"),
    diagnosis           = replace_na(diagnosis, "Non renseigné"),
    insurance_status    = replace_na(insurance_status, "Non renseigné")
    # patient_age et treatment_cost restent NA : ce sont des variables numeriques
    # continues -- toute imputation (moyenne/mediane) biaiserait les analyses de
    # cout et de distribution d'age. Elles seront exclues via na.rm = TRUE.
  )

n_final <- nrow(consultations)

# ------------------------------------------------------------------------------
# RAPPORT DE QUALITE (avant / apres)
# ------------------------------------------------------------------------------
titre_section("RAPPORT DE NETTOYAGE")
cat("Lignes brutes                              :", n_brut, "\n")
cat("Apres suppression doublons exacts          :", n_apres_doublons_exacts,
    " (-", n_brut - n_apres_doublons_exacts, ")\n", sep = "")
cat("Apres suppression doublons patient_id+date :", n_apres_doublons_id_date,
    " (-", n_apres_doublons_exacts - n_apres_doublons_id_date, ")\n", sep = "")
cat("Dates valides apres correction des formats :",
    sum(!is.na(consultations$consultation_date)), "sur", n_final, "\n")
cat("Dates encore invalides apres correction    :", n_dates_invalides, "\n")
cat("Ages aberrants -> NA                       :", n_age_aberrant, "\n")
cat("Couts aberrants -> NA                      :", n_cout_aberrant, "\n")
cat("Lignes finales                             :", n_final, "\n")

rapport_qualite <- tibble::tibble(
  etape = c("Lignes brutes", "Doublons exacts supprimes", "Doublons patient_id+date supprimes",
            "Ages aberrants -> NA", "Couts aberrants -> NA", "Lignes finales"),
  valeur = c(n_brut, n_brut - n_apres_doublons_exacts,
             n_apres_doublons_exacts - n_apres_doublons_id_date,
             n_age_aberrant, n_cout_aberrant, n_final)
)

dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
write_csv(rapport_qualite, CHEMIN_QUALITE)
write_csv(consultations, CHEMIN_CLEAN)

cat("\nDonnees propres ecrites dans :", CHEMIN_CLEAN, "\n")
cat("Rapport de qualite ecrit dans :", CHEMIN_QUALITE, "\n")
