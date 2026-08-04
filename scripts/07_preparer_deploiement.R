# ==============================================================================
# 07_preparer_deploiement.R
#
# Rend le dossier app/ totalement autonome (auto-suffisant) en y copiant :
#   - une copie de scripts/00_utils.R
#   - les donnees nettoyees et transformees (data/clean/*.csv)
# puis genere app/manifest.json (rsconnect::writeManifest) pour le deploiement
# sur Posit Cloud / Posit Connect / shinyapps.io.
#
# A executer depuis la racine du projet avant tout (re)deploiement -- ces
# plateformes ne deploient que le dossier app/, sans acces aux dossiers
# scripts/ et data/ du depot.
#
# IMPORTANT -- correctif de locale :
# rsconnect::writeManifest() enregistre la locale de la session R LOCALE dans
# manifest.json (champ "locale"), et Posit Connect/Cloud tente de reproduire
# cette locale sur le serveur au demarrage de l'app. Sur une machine dont la
# locale systeme est mal configuree (observe ici : "French_Togo.utf8" / "fr_TG"
# -- une locale quasi jamais installee sur un serveur Linux), cela provoque un
# echec silencieux de Sys.setlocale() sur le serveur, un repli sur la locale
# "C" (ASCII), puis un crash au sourcing de app.R des qu'un caractere accentue
# est rencontre ("Error sourcing app.R"). On force donc systematiquement une
# locale UTF-8 universellement disponible ("en_US") apres generation du
# manifeste, quelle que soit la locale de la machine qui l'a genere.
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
cat(" -", file.path("app", CHEMIN_QUALITE), "\n\n")

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Le package 'rsconnect' est requis : install.packages('rsconnect')")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Le package 'jsonlite' est requis : install.packages('jsonlite')")
}

cat("Generation de app/manifest.json...\n")
rsconnect::writeManifest(appDir = "app")

manifest_path <- "app/manifest.json"
manifeste <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
locale_avant <- manifeste$locale
manifeste$locale <- "en_US"
jsonlite::write_json(manifeste, manifest_path, auto_unbox = TRUE, pretty = TRUE,
                      null = "null", na = "null")

cat("Locale du manifeste : \"", locale_avant, "\" -> \"en_US\" (correctif applique)\n", sep = "")
cat("\napp/manifest.json est pret pour le deploiement (rsconnect::deployApp(\"app\")).\n")
