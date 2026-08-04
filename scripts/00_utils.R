# ==============================================================================
# 00_utils.R
# Fonctions et constantes partagees par tous les scripts du projet
# (nettoyage, transformation, analyse, visualisation, dashboard Shiny)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(lubridate)
})

# ------------------------------------------------------------------------------
# Chemins du projet (chemins relatifs -- executer les scripts depuis la racine
# du projet, ou ouvrir "Sante Cameroun.Rproj" dans RStudio)
# ------------------------------------------------------------------------------
CHEMIN_BRUT      <- "data/consultations_cameroun.csv"
CHEMIN_CLEAN     <- "data/clean/consultations_clean.csv"
CHEMIN_FINAL     <- "data/clean/consultations_final.csv"
CHEMIN_QUALITE   <- "data/clean/rapport_qualite.csv"

# ------------------------------------------------------------------------------
# Charte graphique "sante" -- utilisee par ggplot2 (scripts 05, 06) et Shiny
# Palette validee (bleu clinique en couleur primaire, statuts cliniques
# good/warning/serious/critical reserves aux indicateurs d'etat).
# ------------------------------------------------------------------------------
PALETTE_SANTE <- list(
  primaire      = "#2a78d6",  # bleu clinique -- identite de marque
  primaire_fonce= "#184f95",
  secondaire    = "#1baf7a",  # aqua -- categorique slot 2
  accent_violet = "#4a3aa7",
  accent_orange = "#eb6834",
  accent_jaune  = "#eda100",
  accent_magenta= "#e87ba4",
  neutre_fonce  = "#0b0b0b",
  neutre_moyen  = "#52514e",
  neutre_clair  = "#898781",
  grille        = "#e1e0d9",
  surface       = "#fcfcfb",
  fond_page     = "#f9f9f7",
  bon           = "#0ca30c",  # statut : disponible / bon niveau
  avertissement = "#fab219",  # statut : a surveiller -- UNIQUEMENT en fond (icone/badge/barre),
                               # jamais en texte : contraste 1.79 sur fond clair (cf. palette.md)
  avertissement_texte = "#9a6700",  # variante foncee, accessible en texte sur fond clair (~4.6:1)
  serieux       = "#ec835a",  # statut : degrade
  critique      = "#d03b3b",  # statut : rupture / critique
  emphase_muet  = "#d5d3cc"   # gris clair pour les series "hors sujet" d'un graphique en emphase
)

# Palette categorique ordonnee (ne jamais permuter l'ordre -- securite CVD)
PALETTE_CATEGORIELLE <- c(
  "#2a78d6", "#eb6834", "#1baf7a", "#eda100",
  "#e87ba4", "#008300", "#4a3aa7", "#e34948"
)

# "" = police sans-serif par defaut du peripherique graphique -- portable sur
# toute installation R (Windows/Mac/Linux), evite les avertissements de police
# non enregistree que "Segoe UI" declenche avec certains peripheriques PNG.
THEME_SANTE_FONT <- ""

# ------------------------------------------------------------------------------
# Referentiels de standardisation (issus de l'exploration du dataset brut)
# ------------------------------------------------------------------------------
REGIONS_CAMEROUN <- c(
  "Adamaoua", "Centre", "Est", "Extrême-Nord", "Littoral",
  "Nord", "Nord-Ouest", "Ouest", "Sud", "Sud-Ouest"
)

standardiser_region <- function(x) {
  x_norm <- str_to_lower(str_trim(x))
  case_when(
    x_norm %in% c("center", "centre", "ctr")                    ~ "Centre",
    x_norm %in% c("littoral", "litoral")                        ~ "Littoral",
    x_norm %in% c("sw", "sud-ouest", "sud ouest")                ~ "Sud-Ouest",
    x_norm == "adamaoua"                                          ~ "Adamaoua",
    x_norm == "est"                                               ~ "Est",
    x_norm %in% c("extreme-nord", "extrême-nord", "extreme nord")~ "Extrême-Nord",
    x_norm == "nord"                                              ~ "Nord",
    x_norm %in% c("nord-ouest", "nord ouest")                    ~ "Nord-Ouest",
    x_norm == "ouest"                                             ~ "Ouest",
    x_norm == "sud"                                               ~ "Sud",
    is.na(x)                                                      ~ NA_character_,
    TRUE                                                          ~ str_to_title(x)
  )
}

standardiser_genre <- function(x) {
  x_norm <- str_to_lower(str_trim(x))
  case_when(
    x_norm %in% c("male", "m", "masculin")   ~ "Homme",
    x_norm %in% c("female", "f", "feminin", "féminin") ~ "Femme",
    is.na(x)                                  ~ NA_character_,
    TRUE                                       ~ x
  )
}

# Dates arrivent sous 2 formats : "AAAA-MM-JJ" et "JJ/MM/AAAA"
parser_date_mixte <- function(x) {
  d_iso <- suppressWarnings(ymd(x, quiet = TRUE))
  d_fr  <- suppressWarnings(dmy(x, quiet = TRUE))
  coalesce(d_iso, d_fr)
}

# ------------------------------------------------------------------------------
# Bornes plausibles utilisees pour detecter les valeurs aberrantes
# ------------------------------------------------------------------------------
AGE_MIN_PLAUSIBLE  <- 0
AGE_MAX_PLAUSIBLE  <- 100
COUT_MIN_PLAUSIBLE <- 0
COUT_MAX_PLAUSIBLE <- 500

# Tranches d'age utilisees dans les analyses et le dashboard
BORNES_AGE  <- c(-Inf, 4, 14, 24, 44, 64, Inf)
LABELS_AGE  <- c("0-4 ans", "5-14 ans", "15-24 ans", "25-44 ans", "45-64 ans", "65 ans et +")

# Libelles temporels en francais, independants de la locale systeme
MOIS_FR  <- c("Jan", "Fév", "Mar", "Avr", "Mai", "Jun", "Jul", "Aoû", "Sep", "Oct", "Nov", "Déc")
JOURS_FR <- c("Dim", "Lun", "Mar", "Mer", "Jeu", "Ven", "Sam")

CATEGORIES_COUT <- c("Faible (< 10)", "Modéré (10-25)", "Élevé (> 25)")

# ------------------------------------------------------------------------------
# read_csv() ne conserve pas l'ordre des facteurs (tranche_age, mois_label,
# categorie_cout redeviennent de simples chaines de caracteres, triees par
# ordre alphabetique par defaut dans les graphiques). Cette fonction restaure
# l'ordre logique -- a appeler juste apres la lecture de CHEMIN_FINAL.
# ------------------------------------------------------------------------------
restaurer_facteurs <- function(df) {
  df |>
    mutate(
      tranche_age    = factor(tranche_age, levels = LABELS_AGE),
      mois_label     = factor(mois_label, levels = MOIS_FR),
      jour_semaine   = factor(jour_semaine, levels = JOURS_FR),
      categorie_cout = factor(categorie_cout, levels = CATEGORIES_COUT)
    )
}

# ------------------------------------------------------------------------------
# Petite fonction d'affichage pour separer les sections dans la console
# ------------------------------------------------------------------------------
titre_section <- function(titre) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat(titre, "\n")
  cat(strrep("=", 70), "\n", sep = "")
}

# ------------------------------------------------------------------------------
# Theme ggplot2 partage -- traits fins, grille discrete, look clinique/moderne
# Utilise par scripts/05_visualisation.R ET par le dashboard Shiny (app/app.R)
# ------------------------------------------------------------------------------
theme_sante <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size, base_family = THEME_SANTE_FONT) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.15),
                                                   colour = PALETTE_SANTE$neutre_fonce,
                                                   margin = ggplot2::margin(b = 10)),
      plot.subtitle      = ggplot2::element_text(colour = PALETTE_SANTE$neutre_moyen,
                                                   margin = ggplot2::margin(b = 12)),
      plot.caption       = ggplot2::element_text(colour = PALETTE_SANTE$neutre_clair, size = ggplot2::rel(0.75)),
      axis.title         = ggplot2::element_text(colour = PALETTE_SANTE$neutre_moyen, size = ggplot2::rel(0.9)),
      axis.text          = ggplot2::element_text(colour = PALETTE_SANTE$neutre_moyen),
      axis.line          = ggplot2::element_line(colour = PALETTE_SANTE$neutre_clair, linewidth = 0.3),
      panel.grid.major   = ggplot2::element_line(colour = PALETTE_SANTE$grille, linewidth = 0.35),
      panel.grid.minor   = ggplot2::element_blank(),
      legend.title       = ggplot2::element_text(colour = PALETTE_SANTE$neutre_moyen, size = ggplot2::rel(0.85)),
      legend.text        = ggplot2::element_text(colour = PALETTE_SANTE$neutre_moyen),
      legend.position    = "top",
      legend.justification = "left",
      strip.text         = ggplot2::element_text(face = "bold", colour = PALETTE_SANTE$neutre_fonce),
      strip.background   = ggplot2::element_rect(fill = "#eef2f8", colour = NA),
      plot.background    = ggplot2::element_rect(fill = PALETTE_SANTE$surface, colour = NA),
      panel.background   = ggplot2::element_rect(fill = PALETTE_SANTE$surface, colour = NA)
    )
}
