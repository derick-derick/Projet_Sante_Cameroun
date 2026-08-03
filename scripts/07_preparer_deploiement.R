# ==============================================================================
# 07_preparer_deploiement.R
#
# Rend le dossier app/ totalement autonome (auto-suffisant) en y copiant :
#   - une copie de scripts/00_utils.R
#   - les donnees nettoyees et transformees (data/clean/*.csv)
#
# A executer depuis la racine du projet AVANT de generer le manifest.json ou
# de deployer sur Posit Cloud / Posit Connect / shinyapps.io -- ces plateformes
# deploient uniquement le dossier app/, sans acces aux dossiers scripts/ et
# data/ du depot.
# ==============================================================================

source("scripts/00_utils.R")

if (!file.exists(CHEMIN_FINAL)) {
  message("Donnees nettoyees introuvables -- execution du pipeline complet...")
  source("scripts/02_nettoyage.R")
  source("scripts/03_transformation.R")
}

dir.create("app/data/clean", showWarnings = FALSE, recursive = TRUE)

invisible(file.copy("scripts/00_utils.R", "app/00_utils.R", overwrite = TRUE))
invisible(file.copy(CHEMIN_FINAL, file.path("app", CHEMIN_FINAL), overwrite = TRUE))
invisible(file.copy(CHEMIN_QUALITE, file.path("app", CHEMIN_QUALITE), overwrite = TRUE))

cat("Dossier app/ pret pour le deploiement autonome :\n")
cat(" -", "app/00_utils.R\n")
cat(" -", file.path("app", CHEMIN_FINAL), "\n")
cat(" -", file.path("app", CHEMIN_QUALITE), "\n")
cat("\nProchaine etape : rsconnect::writeManifest(appDir = \"app\")\n")
