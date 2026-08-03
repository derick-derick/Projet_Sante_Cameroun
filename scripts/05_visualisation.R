# ==============================================================================
# 05_visualisation.R
# Etape 6 (Visualisation) du workflow Data Analyst
#
# Produit les graphiques cles qui illustrent les reponses aux questions
# metier (script 04). Charte graphique "sante" coherente : theme_sante(),
# palette clinique (bleu = magnitude, couleurs de statut reservees aux
# indicateurs critiques comme la rupture de stock).
# Chaque graphique est enregistre en PNG haute resolution dans resultats/figures/.
# ==============================================================================

source("scripts/00_utils.R")
suppressPackageStartupMessages({
  library(readr)
  library(ggplot2)
  library(scales)
  library(forcats)
})

dir.create("resultats/figures", showWarnings = FALSE, recursive = TRUE)
df <- read_csv(CHEMIN_FINAL, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) |>
  restaurer_facteurs()

enregistrer <- function(plot, nom, largeur = 8, hauteur = 5) {
  ggsave(file.path("resultats/figures", paste0(nom, ".png")), plot,
         width = largeur, height = hauteur, dpi = 300, bg = PALETTE_SANTE$surface)
}

# ------------------------------------------------------------------------------
# 1. Volume de consultations par region
# ------------------------------------------------------------------------------
g1_data <- df |> count(region, name = "n") |> mutate(region = fct_reorder(region, n))

g1 <- ggplot(g1_data, aes(x = n, y = region)) +
  geom_col(fill = PALETTE_SANTE$primaire, width = 0.65) +
  geom_text(aes(label = comma(n)), hjust = -0.15, size = 3.2, colour = PALETTE_SANTE$neutre_moyen) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = "Volume de consultations par région",
       subtitle = "Nombre total de consultations enregistrées en 2025",
       x = "Nombre de consultations", y = NULL,
       caption = "Source : données de consultations, ministère de la Santé (nettoyées)") +
  theme_sante()
enregistrer(g1, "01_volume_par_region")

# ------------------------------------------------------------------------------
# 2. Diagnostics les plus frequents
# ------------------------------------------------------------------------------
g2_data <- df |> filter(diagnosis != "Non renseigné") |>
  count(diagnosis, name = "n") |> mutate(diagnosis = fct_reorder(diagnosis, n))

g2 <- ggplot(g2_data, aes(x = n, y = diagnosis)) +
  geom_col(fill = PALETTE_SANTE$primaire, width = 0.65) +
  geom_text(aes(label = comma(n)), hjust = -0.15, size = 3.2, colour = PALETTE_SANTE$neutre_moyen) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = "Diagnostics les plus fréquents",
       subtitle = "Nombre de cas enregistrés par pathologie",
       x = "Nombre de cas", y = NULL,
       caption = "Diagnostics non renseignés exclus de ce graphique") +
  theme_sante()
enregistrer(g2, "02_diagnostics_frequents")

# ------------------------------------------------------------------------------
# 3. Cout moyen de traitement par diagnostic
# ------------------------------------------------------------------------------
g3_data <- df |> filter(diagnosis != "Non renseigné", !is.na(treatment_cost)) |>
  group_by(diagnosis) |> summarise(cout_moyen = mean(treatment_cost), .groups = "drop") |>
  mutate(diagnosis = fct_reorder(diagnosis, cout_moyen))

g3 <- ggplot(g3_data, aes(x = cout_moyen, y = diagnosis)) +
  geom_col(fill = PALETTE_SANTE$primaire_fonce, width = 0.65) +
  geom_text(aes(label = dollar(cout_moyen, prefix = "", suffix = " $")),
            hjust = -0.15, size = 3.2, colour = PALETTE_SANTE$neutre_moyen) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Coût moyen de traitement par diagnostic",
       subtitle = "Les maladies chroniques (diabète, tuberculose, hypertension) coûtent le plus cher",
       x = "Coût moyen (USD)", y = NULL) +
  theme_sante()
enregistrer(g3, "03_cout_moyen_diagnostic")

# ------------------------------------------------------------------------------
# 4. Taux de rupture de stock par region -- couleurs de statut (seuils fixes)
# ------------------------------------------------------------------------------
g4_data <- df |> group_by(region) |>
  summarise(taux = 100 * mean(en_rupture_stock), .groups = "drop") |>
  mutate(
    statut = case_when(
      taux >= 20 ~ "critique",
      taux >= 15 ~ "à surveiller",
      TRUE       ~ "sous contrôle"
    ),
    statut = factor(statut, levels = c("sous contrôle", "à surveiller", "critique")),
    region = fct_reorder(region, taux)
  )

g4 <- ggplot(g4_data, aes(x = taux, y = region, fill = statut)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(round(taux, 1), "%")), hjust = -0.15, size = 3.2,
            colour = PALETTE_SANTE$neutre_moyen) +
  scale_fill_manual(values = c("sous contrôle" = PALETTE_SANTE$bon,
                                "à surveiller"  = PALETTE_SANTE$avertissement,
                                "critique"      = PALETTE_SANTE$critique),
                     name = "Statut") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2)), labels = label_percent(scale = 1)) +
  labs(title = "Taux de rupture de stock de médicaments par région",
       subtitle = "Seuils : < 15 % sous contrôle · 15-20 % à surveiller · ≥ 20 % critique",
       x = "Taux de rupture de stock", y = NULL) +
  theme_sante()
enregistrer(g4, "04_rupture_stock_region")

# ------------------------------------------------------------------------------
# 5. Tendance mensuelle des consultations
# ------------------------------------------------------------------------------
g5_data <- df |> count(mois, mois_label, name = "n")

g5 <- ggplot(g5_data, aes(x = mois, y = n)) +
  geom_line(colour = PALETTE_SANTE$primaire, linewidth = 1) +
  geom_point(colour = PALETTE_SANTE$primaire, size = 2.2) +
  scale_x_continuous(breaks = g5_data$mois, labels = g5_data$mois_label) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "Évolution mensuelle du nombre de consultations",
       subtitle = "Volume relativement stable sur l'année, sans pic saisonnier marqué",
       x = NULL, y = "Nombre de consultations") +
  theme_sante()
enregistrer(g5, "05_tendance_mensuelle")

# ------------------------------------------------------------------------------
# 6. Distribution des couts de traitement par statut d'assurance
# ------------------------------------------------------------------------------
g6_data <- df |> filter(!is.na(treatment_cost), insurance_status != "Non renseigné")

g6 <- ggplot(g6_data, aes(x = insurance_status, y = treatment_cost, fill = insurance_status)) +
  geom_boxplot(width = 0.45, alpha = 0.9, outlier.size = 0.8, outlier.colour = PALETTE_SANTE$neutre_clair) +
  scale_fill_manual(values = c("Insured" = PALETTE_SANTE$primaire, "Uninsured" = PALETTE_SANTE$accent_orange),
                     guide = "none") +
  scale_y_continuous(labels = dollar_format(prefix = "", suffix = " $")) +
  labs(title = "Distribution du coût de traitement selon la couverture d'assurance",
       subtitle = "Le coût médian est proche entre patients assurés et non assurés",
       x = NULL, y = "Coût de traitement (USD)") +
  theme_sante()
enregistrer(g6, "06_cout_par_assurance")

# ------------------------------------------------------------------------------
# 7. Profil d'age par diagnostic (heatmap -- une seule teinte sequentielle)
# ------------------------------------------------------------------------------
g7_data <- df |> filter(diagnosis != "Non renseigné", !is.na(tranche_age)) |>
  count(diagnosis, tranche_age) |>
  group_by(diagnosis) |>
  mutate(part_pct = 100 * n / sum(n)) |>
  ungroup()

g7 <- ggplot(g7_data, aes(x = tranche_age, y = diagnosis, fill = part_pct)) +
  geom_tile(colour = PALETTE_SANTE$surface, linewidth = 1.5) +
  geom_text(aes(label = paste0(round(part_pct), "%")), size = 3,
            colour = ifelse(g7_data$part_pct > 22, "white", PALETTE_SANTE$neutre_fonce)) +
  scale_fill_gradient(low = "#cde2fb", high = PALETTE_SANTE$primaire_fonce, name = "% des cas") +
  labs(title = "Répartition des tranches d'âge par diagnostic",
       subtitle = "Part (%) de chaque tranche d'âge au sein de chaque pathologie",
       x = NULL, y = NULL) +
  theme_sante() +
  theme(panel.grid = element_blank(), legend.position = "right")
enregistrer(g7, "07_profil_age_diagnostic", largeur = 9, hauteur = 5.5)

# ------------------------------------------------------------------------------
# 8. Couverture d'assurance par region
# ------------------------------------------------------------------------------
g8_data <- df |> group_by(region) |>
  summarise(taux = 100 * mean(est_assure), .groups = "drop") |>
  mutate(region = fct_reorder(region, taux))

g8 <- ggplot(g8_data, aes(x = taux, y = region)) +
  geom_col(fill = PALETTE_SANTE$secondaire, width = 0.65) +
  geom_text(aes(label = paste0(round(taux, 1), "%")), hjust = -0.15, size = 3.2,
            colour = PALETTE_SANTE$neutre_moyen) +
  geom_vline(xintercept = 100 * mean(df$est_assure), linetype = "dashed",
             colour = PALETTE_SANTE$neutre_clair, linewidth = 0.4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2)), labels = label_percent(scale = 1)) +
  labs(title = "Taux de couverture d'assurance par région",
       subtitle = paste0("Moyenne nationale : ", round(100 * mean(df$est_assure), 1),
                          "% — ligne pointillée"),
       x = "Taux de couverture", y = NULL) +
  theme_sante()
enregistrer(g8, "08_assurance_par_region")

# ------------------------------------------------------------------------------
# 9. Repartition des types de consultation
# ------------------------------------------------------------------------------
g9_data <- df |> count(consultation_type, name = "n") |> mutate(consultation_type = fct_reorder(consultation_type, n))

g9 <- ggplot(g9_data, aes(x = n, y = consultation_type)) +
  geom_col(fill = PALETTE_SANTE$primaire, width = 0.65) +
  geom_text(aes(label = comma(n)), hjust = -0.15, size = 3.2, colour = PALETTE_SANTE$neutre_moyen) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = "Répartition des consultations par type",
       subtitle = "Les 5 types de consultation sont représentés de façon quasi équilibrée",
       x = "Nombre de consultations", y = NULL) +
  theme_sante()
enregistrer(g9, "09_types_consultation")

cat("9 graphiques enregistres dans resultats/figures/\n")
