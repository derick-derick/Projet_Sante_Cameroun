# ==============================================================================
# app.R -- Dashboard Shiny "Santé Cameroun"
# Etape 8 (Dashboard) du workflow Data Analyst
#
# Dashboard interactif de pilotage des consultations dans les centres de sante
# du Cameroun. Charte graphique clinique moderne (bslib + palette validee).
#
# Lancer depuis RStudio : ouvrir ce fichier puis cliquer "Run App"
# Lancer en ligne de commande : shiny::runApp("app")
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(forcats)
  library(ggplot2)
  library(plotly)
  library(scales)
  library(DT)
})

# ------------------------------------------------------------------------------
# Localisation robuste des fichiers. L'app fonctionne dans 3 contextes :
#   1. Deploiement autonome (Posit Cloud/Connect, shinyapps.io) : le dossier
#      app/ est deploye seul et contient ses propres copies de 00_utils.R et
#      des donnees nettoyees (voir scripts/07_preparer_deploiement.R).
#   2. Developpement local lance depuis la racine du projet.
#   3. Developpement local lance depuis le dossier app/ (RStudio "Run App").
# ------------------------------------------------------------------------------
localiser <- function(candidats) {
  trouve <- candidats[file.exists(candidats)]
  if (length(trouve) == 0) NA_character_ else trouve[1]
}

chemin_utils <- localiser(c("00_utils.R", "scripts/00_utils.R", "../scripts/00_utils.R"))
if (is.na(chemin_utils)) stop("00_utils.R introuvable -- verifiez la structure du projet.")
source(chemin_utils)

# racine_projet == emplacement du dossier scripts/ (pour reconstruire le pipeline
# de nettoyage si besoin) ; NA en deploiement autonome, ou les donnees sont deja
# pretes et le pipeline complet n'est pas embarque.
racine_projet <- switch(chemin_utils,
  "scripts/00_utils.R"    = ".",
  "../scripts/00_utils.R" = "..",
  NA_character_
)

# ------------------------------------------------------------------------------
# Chargement des donnees : utilise le pipeline nettoye si disponible, sinon le
# reconstruit a la volee (uniquement possible si scripts/ et data/ sont accessibles).
# ------------------------------------------------------------------------------
charger_donnees <- function() {
  candidats <- c("data/clean/consultations_final.csv",
                 if (!is.na(racine_projet)) file.path(racine_projet, CHEMIN_FINAL))
  chemin_final <- localiser(candidats)

  if (is.na(chemin_final)) {
    if (is.na(racine_projet)) {
      stop("Donnees introuvables et reconstruction impossible en mode autonome. ",
           "Executez scripts/07_preparer_deploiement.R avant de deployer.")
    }
    old_wd <- getwd()
    setwd(racine_projet)
    source("scripts/02_nettoyage.R")
    source("scripts/03_transformation.R")
    setwd(old_wd)
    chemin_final <- file.path(racine_projet, CHEMIN_FINAL)
  }

  read_csv(chemin_final, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) |>
    restaurer_facteurs()
}

donnees_completes <- charger_donnees()

chemin_qualite <- localiser(c("data/clean/rapport_qualite.csv",
                               if (!is.na(racine_projet)) file.path(racine_projet, CHEMIN_QUALITE)))
rapport_qualite <- if (!is.na(chemin_qualite)) {
  tryCatch(read_csv(chemin_qualite, show_col_types = FALSE), error = function(e) NULL)
} else NULL

BORNES_DATES <- range(donnees_completes$consultation_date, na.rm = TRUE)

# ------------------------------------------------------------------------------
# Theme bslib -- charte graphique "sante" (Bootstrap 5)
# ------------------------------------------------------------------------------
theme_dashboard <- bs_theme(
  version     = 5,
  bg          = "#f4f7fb",
  fg          = "#0b0b0b",
  primary     = PALETTE_SANTE$primaire,
  secondary   = PALETTE_SANTE$secondaire,
  success     = PALETTE_SANTE$bon,
  warning     = PALETTE_SANTE$avertissement,
  danger      = PALETTE_SANTE$critique,
  base_font   = font_collection("Segoe UI", "system-ui", "sans-serif"),
  heading_font= font_collection("Segoe UI", "system-ui", "sans-serif"),
  "border-radius" = "0.7rem",
  "card-border-color" = "#e6eaf1"
) |>
  bs_add_rules("
    .navbar-brand svg { margin-right: 6px; }
  ")

# ------------------------------------------------------------------------------
# Fonctions utilitaires d'affichage
# ------------------------------------------------------------------------------
carte_kpi <- function(titre, valeur, icone, couleur = PALETTE_SANTE$primaire, sous_texte = NULL) {
  value_box(
    title = titre,
    value = valeur,
    showcase = bs_icon(icone, size = "1.8rem"),
    theme = value_box_theme(bg = "white", fg = couleur),
    p(sous_texte, style = paste0("color:", PALETTE_SANTE$neutre_clair, "; margin-top:-6px;")),
    full_screen = FALSE
  )
}

badge_statut <- function(taux, seuils = c(15, 20)) {
  cls <- if (taux >= seuils[2]) "badge-sante-critique"
  else if (taux >= seuils[1]) "badge-sante-avertissement"
  else "badge-sante-bon"
  lbl <- if (taux >= seuils[2]) "Critique" else if (taux >= seuils[1]) "À surveiller" else "Sous contrôle"
  span(class = paste("badge", cls), style = "font-size:0.8rem; padding:5px 10px; border-radius:20px;", lbl)
}

appliquer_theme_plotly <- function(p) {
  p |> layout(
    font = list(family = "Segoe UI, system-ui, sans-serif", color = PALETTE_SANTE$neutre_fonce),
    paper_bgcolor = PALETTE_SANTE$surface,
    plot_bgcolor  = PALETTE_SANTE$surface,
    margin = list(t = 50, l = 60, r = 20, b = 50),
    legend = list(orientation = "h", y = 1.12, x = 0)
  ) |> config(displaylogo = FALSE, modeBarButtonsToRemove = list(
    "lasso2d", "select2d", "autoScale2d", "hoverCompareCartesian"
  ))
}

# ==============================================================================
# UI
# ==============================================================================
barre_filtres <- sidebar(
  title = NULL,
  width = 280,
  open = "open",
  div(class = "sidebar-title", bs_icon("funnel"), " Filtres"),
  dateRangeInput("filtre_dates", "Période",
                  start = BORNES_DATES[1], end = BORNES_DATES[2],
                  min = BORNES_DATES[1], max = BORNES_DATES[2],
                  language = "fr", format = "dd/mm/yyyy"),
  selectizeInput("filtre_region", "Région",
                  choices = sort(unique(donnees_completes$region)), multiple = TRUE,
                  options = list(placeholder = "Toutes les régions")),
  selectizeInput("filtre_diagnostic", "Diagnostic",
                  choices = sort(unique(donnees_completes$diagnosis)), multiple = TRUE,
                  options = list(placeholder = "Tous les diagnostics")),
  selectizeInput("filtre_type_consult", "Type de consultation",
                  choices = sort(unique(donnees_completes$consultation_type)), multiple = TRUE,
                  options = list(placeholder = "Tous les types")),
  selectizeInput("filtre_assurance", "Statut d'assurance",
                  choices = c("Insured", "Uninsured"), multiple = TRUE,
                  options = list(placeholder = "Tous les statuts")),
  hr(),
  actionButton("reinitialiser", "Réinitialiser les filtres",
               icon = icon_svg <- bs_icon("arrow-counterclockwise"), class = "btn-outline-primary w-100"),
  div(style = "margin-top:18px; font-size:0.75rem; color:#898781;",
      "Source : registre des consultations, centres de santé du Cameroun (2025). ",
      "Données nettoyées et transformées via le pipeline R du projet.")
)

pied_de_page <- div(class = "pied-de-page",
  "Dashboard Santé Cameroun — Analyse de données avec R · Projet formation Data Analyst · ",
  format(Sys.Date(), "%Y")
)

ui <- page_navbar(
  title = tagList(bs_icon("heart-pulse-fill"), "Santé Cameroun · Suivi des consultations"),
  theme = theme_dashboard,
  fillable = TRUE,
  sidebar = barre_filtres,
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  # ---------------------------------------------------------------- Vue d'ensemble
  nav_panel(
    title = "Vue d'ensemble", icon = bs_icon("speedometer2"),
    layout_columns(
      col_widths = c(12, 12, 12, 12, 12),
      uiOutput("kpi_row"),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Évolution mensuelle des consultations"), plotlyOutput("graphe_tendance", height = 320)),
        card(card_header("Répartition par type de consultation"), plotlyOutput("graphe_types", height = 320))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Volume de consultations par région"), plotlyOutput("graphe_volume_region", height = 340)),
        card(card_header("Diagnostics les plus fréquents"), plotlyOutput("graphe_diagnostics", height = 340))
      )
    )
  ),

  # ---------------------------------------------------------------- Analyse geographique
  nav_panel(
    title = "Analyse géographique", icon = bs_icon("geo-alt"),
    layout_columns(
      col_widths = c(6, 6, 12),
      card(card_header("Taux de rupture de stock par région"), plotlyOutput("graphe_rupture_region", height = 380)),
      card(card_header("Couverture d'assurance par région"), plotlyOutput("graphe_assurance_region", height = 380)),
      card(
        card_header("Détail par district"),
        DTOutput("table_districts")
      )
    )
  ),

  # ---------------------------------------------------------------- Analyse clinique
  nav_panel(
    title = "Analyse clinique", icon = bs_icon("clipboard2-pulse"),
    layout_columns(
      col_widths = c(6, 6, 12),
      card(card_header("Coût moyen de traitement par diagnostic"), plotlyOutput("graphe_cout_diagnostic", height = 360)),
      card(card_header("Répartition par genre et diagnostic"), plotlyOutput("graphe_genre_diagnostic", height = 360)),
      card(card_header("Profil d'âge par diagnostic (% au sein de chaque pathologie)"),
           plotlyOutput("graphe_heatmap_age", height = 420))
    )
  ),

  # ---------------------------------------------------------------- Analyse financiere
  nav_panel(
    title = "Analyse financière", icon = bs_icon("cash-coin"),
    layout_columns(
      col_widths = c(4, 4, 4, 12),
      uiOutput("kpi_cout_moyen"),
      uiOutput("kpi_cout_assures"),
      uiOutput("kpi_cout_non_assures"),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Distribution du coût par statut d'assurance"), plotlyOutput("graphe_cout_assurance", height = 380)),
        card(card_header("Coût moyen par type de consultation"), plotlyOutput("graphe_cout_type", height = 380))
      )
    )
  ),

  # ---------------------------------------------------------------- Qualite des donnees
  nav_panel(
    title = "Qualité des données", icon = bs_icon("shield-check"),
    layout_columns(
      col_widths = c(4, 8, 12),
      card(
        card_header("Entonnoir de nettoyage"),
        tableOutput("table_qualite")
      ),
      card(
        card_header("Complétude des champs (dataset final)"),
        plotlyOutput("graphe_completude", height = 300)
      ),
      card(
        card_header("Méthodologie de nettoyage appliquée"),
        HTML("
          <ul>
            <li><b>Doublons</b> : suppression des lignes strictement identiques, puis des doublons patient + date de consultation.</li>
            <li><b>Catégories incohérentes</b> : régions et genres standardisés (fautes de frappe, casse, abréviations : « Ctr », « SW », « M »...).</li>
            <li><b>Dates incohérentes</b> : deux formats détectés (AAAA-MM-JJ et JJ/MM/AAAA), reconnus et unifiés automatiquement.</li>
            <li><b>Valeurs aberrantes</b> : âges hors de 0-100 ans et coûts hors de 0-500 $ traités comme valeurs manquantes plutôt que supprimés.</li>
            <li><b>Valeurs manquantes</b> : catégories manquantes conservées sous « Non renseigné » (aucune ligne supprimée) ; variables numériques manquantes exclues des moyennes via <code>na.rm = TRUE</code>.</li>
          </ul>
        ")
      )
    )
  ),

  # ---------------------------------------------------------------- Donnees
  nav_panel(
    title = "Données détaillées", icon = bs_icon("table"),
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center", style = "width:100%;",
            span("Consultations filtrées"),
            downloadButton("telecharger_donnees", "Exporter (CSV)", class = "btn-sm btn-primary"))
      ),
      DTOutput("table_donnees")
    )
  ),

  nav_spacer(),
  nav_item(pied_de_page)
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  observeEvent(input$reinitialiser, {
    updateDateRangeInput(session, "filtre_dates", start = BORNES_DATES[1], end = BORNES_DATES[2])
    updateSelectizeInput(session, "filtre_region", selected = character(0))
    updateSelectizeInput(session, "filtre_diagnostic", selected = character(0))
    updateSelectizeInput(session, "filtre_type_consult", selected = character(0))
    updateSelectizeInput(session, "filtre_assurance", selected = character(0))
  })

  donnees_filtrees <- reactive({
    d <- donnees_completes |>
      filter(consultation_date >= input$filtre_dates[1],
             consultation_date <= input$filtre_dates[2])
    if (length(input$filtre_region) > 0)        d <- filter(d, region %in% input$filtre_region)
    if (length(input$filtre_diagnostic) > 0)    d <- filter(d, diagnosis %in% input$filtre_diagnostic)
    if (length(input$filtre_type_consult) > 0)  d <- filter(d, consultation_type %in% input$filtre_type_consult)
    if (length(input$filtre_assurance) > 0)     d <- filter(d, insurance_status %in% input$filtre_assurance)
    d
  })

  # ---------------------------------------------------------------- KPI (vue d'ensemble)
  output$kpi_row <- renderUI({
    d <- donnees_filtrees()
    n_total       <- nrow(d)
    n_patients    <- n_distinct(d$patient_id)
    taux_assur    <- if (n_total > 0) round(100 * mean(d$est_assure), 1) else 0
    taux_rupture  <- if (n_total > 0) round(100 * mean(d$en_rupture_stock), 1) else 0
    completude    <- if (n_total > 0) round(100 * mean(!d$donnee_incomplete), 1) else 0

    layout_columns(
      col_widths = c(3, 3, 3, 3),
      carte_kpi("Consultations", comma(n_total), "clipboard2-pulse", PALETTE_SANTE$primaire,
                paste(comma(n_patients), "patients uniques")),
      carte_kpi("Couverture d'assurance", paste0(taux_assur, " %"), "shield-plus", PALETTE_SANTE$secondaire,
                "des consultations filtrées"),
      carte_kpi("Rupture de stock", paste0(taux_rupture, " %"), "exclamation-triangle",
                if (taux_rupture >= 20) PALETTE_SANTE$critique else if (taux_rupture >= 15) PALETTE_SANTE$avertissement else PALETTE_SANTE$bon,
                "des consultations concernées"),
      carte_kpi("Complétude des données", paste0(completude, " %"), "check2-circle", PALETTE_SANTE$primaire_fonce,
                "lignes sans valeur manquante clé")
    )
  })

  # ---------------------------------------------------------------- Vue d'ensemble : graphes
  output$graphe_tendance <- renderPlotly({
    d <- donnees_filtrees() |> count(mois, mois_label, name = "n") |> arrange(mois)
    validate(need(nrow(d) > 0, "Aucune donnée pour cette sélection."))
    p <- ggplot(d, aes(x = mois_label, y = n, group = 1,
                        text = paste0(mois_label, " : ", comma(n), " consultations"))) +
      geom_line(colour = PALETTE_SANTE$primaire, linewidth = 1) +
      geom_point(colour = PALETTE_SANTE$primaire, size = 2) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Consultations") +
      theme_sante()
    ggplotly(p, tooltip = "text") |> appliquer_theme_plotly()
  })

  output$graphe_types <- renderPlotly({
    d <- donnees_filtrees() |> count(consultation_type, name = "n") |>
      mutate(consultation_type = fct_reorder(consultation_type, n))
    validate(need(nrow(d) > 0, "Aucune donnée pour cette sélection."))
    p <- ggplot(d, aes(x = n, y = consultation_type,
                        text = paste0(consultation_type, " : ", comma(n)))) +
      geom_col(fill = PALETTE_SANTE$secondaire, width = 0.65) +
      scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Consultations", y = NULL) +
      theme_sante()
    ggplotly(p, tooltip = "text") |> appliquer_theme_plotly()
  })

  output$graphe_volume_region <- renderPlotly({
    d <- donnees_filtrees() |> count(region, name = "n") |> mutate(region = fct_reorder(region, n))
    validate(need(nrow(d) > 0, "Aucune donnée pour cette sélection."))
    p <- ggplot(d, aes(x = n, y = region, text = paste0(region, " : ", comma(n)))) +
      geom_col(fill = PALETTE_SANTE$primaire, width = 0.65) +
      scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Consultations", y = NULL) +
      theme_sante()
    ggplotly(p, tooltip = "text") |> appliquer_theme_plotly()
  })

  output$graphe_diagnostics <- renderPlotly({
    d <- donnees_filtrees() |> filter(diagnosis != "Non renseigné") |>
      count(diagnosis, name = "n") |> mutate(diagnosis = fct_reorder(diagnosis, n))
    validate(need(nrow(d) > 0, "Aucune donnée pour cette sélection."))
    p <- ggplot(d, aes(x = n, y = diagnosis, text = paste0(diagnosis, " : ", comma(n)))) +
      geom_col(fill = PALETTE_SANTE$primaire_fonce, width = 0.65) +
      scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Cas", y = NULL) +
      theme_sante()
    ggplotly(p, tooltip = "text") |> appliquer_theme_plotly()
  })

  # ---------------------------------------------------------------- Geographique
  output$graphe_rupture_region <- renderPlotly({
    d <- donnees_filtrees() |> group_by(region) |>
      summarise(taux = 100 * mean(en_rupture_stock), .groups = "drop") |>
      mutate(
        statut = case_when(taux >= 20 ~ "Critique", taux >= 15 ~ "À surveiller", TRUE ~ "Sous contrôle"),
        statut = factor(statut, levels = c("Sous contrôle", "À surveiller", "Critique")),
        region = fct_reorder(region, taux)
      )
    validate(need(nrow(d) > 0, "Aucune donnée pour cette sélection."))
    p <- ggplot(d, aes(x = taux, y = region, fill = statut,
                        text = paste0(region, " : ", round(taux, 1), "% (", statut, ")"))) +
      geom_col(width = 0.65) +
      scale_fill_manual(values = c("Sous contrôle" = PALETTE_SANTE$bon,
                                    "À surveiller"  = PALETTE_SANTE$avertissement,
                                    "Critique"      = PALETTE_SANTE$critique), name = NULL) +
      scale_x_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Taux de rupture de stock", y = NULL) +
      theme_sante()
    ggplotly(p, tooltip = "text") |> appliquer_theme_plotly()
  })

  output$graphe_assurance_region <- renderPlotly({
    d <- donnees_filtrees() |> group_by(region) |>
      summarise(taux = 100 * mean(est_assure), .groups = "drop") |> mutate(region = fct_reorder(region, taux))
    validate(need(nrow(d) > 0, "Aucune donnée pour cette sélection."))
    p <- ggplot(d, aes(x = taux, y = region, text = paste0(region, " : ", round(taux, 1), "%"))) +
      geom_col(fill = PALETTE_SANTE$secondaire, width = 0.65) +
      scale_x_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Taux de couverture", y = NULL) +
      theme_sante()
    ggplotly(p, tooltip = "text") |> appliquer_theme_plotly()
  })

  output$table_districts <- renderDT({
    d <- donnees_filtrees() |>
      group_by(region, district) |>
      summarise(
        consultations = n(),
        cout_moyen = round(mean(treatment_cost, na.rm = TRUE), 2),
        taux_rupture_pct = round(100 * mean(en_rupture_stock), 1),
        taux_assurance_pct = round(100 * mean(est_assure), 1),
        .groups = "drop"
      ) |>
      arrange(desc(consultations))
    datatable(d, rownames = FALSE, options = list(pageLength = 8, dom = "tip"),
              colnames = c("Région", "District", "Consultations", "Coût moyen ($)",
                           "Rupture de stock (%)", "Assurance (%)"))
  })

  # ---------------------------------------------------------------- Clinique
  output$graphe_cout_diagnostic <- renderPlotly({
    d <- donnees_filtrees() |> filter(diagnosis != "Non renseigné", !is.na(treatment_cost)) |>
      group_by(diagnosis) |> summarise(cout_moyen = mean(treatment_cost), .groups = "drop") |>
      mutate(diagnosis = fct_reorder(diagnosis, cout_moyen))
    validate(need(nrow(d) > 0, "Aucune donnée pour cette sélection."))
    p <- ggplot(d, aes(x = cout_moyen, y = diagnosis, text = paste0(diagnosis, " : ", round(cout_moyen, 2), " $"))) +
      geom_col(fill = PALETTE_SANTE$primaire_fonce, width = 0.65) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Coût moyen (USD)", y = NULL) +
      theme_sante()
    ggplotly(p, tooltip = "text") |> appliquer_theme_plotly()
  })

  output$graphe_genre_diagnostic <- renderPlotly({
    d <- donnees_filtrees() |> filter(diagnosis != "Non renseigné", gender != "Non renseigné") |>
      count(diagnosis, gender)
    validate(need(nrow(d) > 0, "Aucune donnée pour cette sélection."))
    p <- ggplot(d, aes(x = n, y = diagnosis, fill = gender, text = paste0(diagnosis, " · ", gender, " : ", comma(n)))) +
      geom_col(position = "stack", width = 0.65) +
      scale_fill_manual(values = c("Homme" = PALETTE_SANTE$primaire, "Femme" = PALETTE_SANTE$accent_magenta), name = NULL) +
      scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Consultations", y = NULL) +
      theme_sante()
    ggplotly(p, tooltip = "text") |> appliquer_theme_plotly()
  })

  output$graphe_heatmap_age <- renderPlotly({
    d <- donnees_filtrees() |> filter(diagnosis != "Non renseigné", !is.na(tranche_age)) |>
      count(diagnosis, tranche_age) |> group_by(diagnosis) |> mutate(part_pct = 100 * n / sum(n)) |> ungroup()
    validate(need(nrow(d) > 0, "Aucune donnée pour cette sélection."))
    p <- ggplot(d, aes(x = tranche_age, y = diagnosis, fill = part_pct,
                        text = paste0(diagnosis, " · ", tranche_age, " : ", round(part_pct, 1), "%"))) +
      geom_tile(colour = "white", linewidth = 1.2) +
      scale_fill_gradient(low = "#cde2fb", high = PALETTE_SANTE$primaire_fonce, name = "% des cas") +
      labs(x = NULL, y = NULL) +
      theme_sante() + theme(panel.grid = element_blank())
    ggplotly(p, tooltip = "text") |> appliquer_theme_plotly()
  })

  # ---------------------------------------------------------------- Financiere
  output$kpi_cout_moyen <- renderUI({
    d <- donnees_filtrees()
    carte_kpi("Coût moyen global", paste0(round(mean(d$treatment_cost, na.rm = TRUE), 2), " $"),
               "cash-stack", PALETTE_SANTE$primaire, "toutes consultations filtrées")
  })
  output$kpi_cout_assures <- renderUI({
    d <- donnees_filtrees() |> filter(insurance_status == "Insured")
    carte_kpi("Coût moyen — assurés", paste0(round(mean(d$treatment_cost, na.rm = TRUE), 2), " $"),
               "shield-check", PALETTE_SANTE$secondaire, paste(comma(nrow(d)), "consultations"))
  })
  output$kpi_cout_non_assures <- renderUI({
    d <- donnees_filtrees() |> filter(insurance_status == "Uninsured")
    carte_kpi("Coût moyen — non assurés", paste0(round(mean(d$treatment_cost, na.rm = TRUE), 2), " $"),
               "exclamation-circle", PALETTE_SANTE$accent_orange, paste(comma(nrow(d)), "consultations"))
  })

  output$graphe_cout_assurance <- renderPlotly({
    d <- donnees_filtrees() |> filter(!is.na(treatment_cost), insurance_status != "Non renseigné")
    validate(need(nrow(d) > 0, "Aucune donnée pour cette sélection."))
    p <- ggplot(d, aes(x = insurance_status, y = treatment_cost, fill = insurance_status)) +
      geom_boxplot(width = 0.45, outlier.size = 0.7, outlier.colour = PALETTE_SANTE$neutre_clair) +
      scale_fill_manual(values = c("Insured" = PALETTE_SANTE$primaire, "Uninsured" = PALETTE_SANTE$accent_orange), guide = "none") +
      labs(x = NULL, y = "Coût (USD)") +
      theme_sante()
    ggplotly(p) |> appliquer_theme_plotly()
  })

  output$graphe_cout_type <- renderPlotly({
    d <- donnees_filtrees() |> filter(!is.na(treatment_cost)) |>
      group_by(consultation_type) |> summarise(cout_moyen = mean(treatment_cost), .groups = "drop") |>
      mutate(consultation_type = fct_reorder(consultation_type, cout_moyen))
    validate(need(nrow(d) > 0, "Aucune donnée pour cette sélection."))
    p <- ggplot(d, aes(x = cout_moyen, y = consultation_type, text = paste0(consultation_type, " : ", round(cout_moyen, 2), " $"))) +
      geom_col(fill = PALETTE_SANTE$primaire, width = 0.65) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Coût moyen (USD)", y = NULL) +
      theme_sante()
    ggplotly(p, tooltip = "text") |> appliquer_theme_plotly()
  })

  # ---------------------------------------------------------------- Qualite des donnees
  output$table_qualite <- renderTable({
    if (is.null(rapport_qualite)) return(data.frame(Info = "Rapport de qualité non disponible."))
    rapport_qualite |>
      mutate(valeur = comma(valeur, accuracy = 1)) |>
      rename(Étape = etape, Valeur = valeur)
  }, striped = TRUE, bordered = FALSE, spacing = "s", align = "lr")

  output$graphe_completude <- renderPlotly({
    d <- donnees_completes |>
      summarise(
        Région = 100 * mean(region != "Non renseigné"),
        Genre = 100 * mean(gender != "Non renseigné"),
        Diagnostic = 100 * mean(diagnosis != "Non renseigné"),
        `Statut assurance` = 100 * mean(insurance_status != "Non renseigné"),
        Âge = 100 * mean(!is.na(patient_age)),
        `Coût traitement` = 100 * mean(!is.na(treatment_cost))
      ) |>
      pivot_longer(everything(), names_to = "champ", values_to = "pct") |>
      mutate(champ = fct_reorder(champ, pct))
    p <- ggplot(d, aes(x = pct, y = champ, text = paste0(champ, " : ", round(pct, 1), "%"))) +
      geom_col(fill = PALETTE_SANTE$primaire, width = 0.6) +
      scale_x_continuous(limits = c(0, 100), labels = label_percent(scale = 1),
                         expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Complétude", y = NULL) +
      theme_sante()
    ggplotly(p, tooltip = "text") |> appliquer_theme_plotly()
  })

  # ---------------------------------------------------------------- Donnees detaillees
  output$table_donnees <- renderDT({
    d <- donnees_filtrees() |>
      select(patient_id, consultation_date, region, district, facility_name,
             patient_age, gender, diagnosis, treatment_cost, medication_available,
             consultation_type, insurance_status)
    datatable(d, rownames = FALSE, filter = "top",
              options = list(pageLength = 12, scrollX = TRUE),
              colnames = c("ID Patient", "Date", "Région", "District", "Établissement",
                           "Âge", "Genre", "Diagnostic", "Coût ($)", "Médicaments",
                           "Type consultation", "Assurance"))
  })

  output$telecharger_donnees <- downloadHandler(
    filename = function() paste0("consultations_filtrees_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) write_csv(donnees_filtrees(), file)
  )
}

shinyApp(ui, server)
