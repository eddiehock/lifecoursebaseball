# =============================================================================
# Adds an ID field to mlb_1968_2025_with_bio.json:
#
#   id1  — Retrosheet-style player ID matching the format used in
#           relatives.csv: [4 chars last name][1 char first name][3-digit num]
#           e.g. "Barry Bonds" -> "bondb101"
#
#           For ambiguous slugs (father/son same initials) players are sorted
#           by birth_date and paired with IDs sorted by number, so earlier-born
#           players get lower numbers — matching Retrosheet convention.
#
#           Players whose slug does not appear in relatives.csv receive id1 = NA.
#
# Output: mlb_1968_2025_relatives.json
#
# Load in RStudio:
#   players <- jsonlite::fromJSON("mlb_1968_2025_relatives.json",
#                                  simplifyVector    = FALSE,
#                                  simplifyDataFrame = FALSE,
#                                  simplifyMatrix    = FALSE)
# =============================================================================

library(jsonlite)

JSON_IN       <- "mlb_1968_2025_with_bio.json"
RELATIVES_CSV <- "relatives.csv"
JSON_OUT      <- "mlb_1968_2025_relatives.json"

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── Load ──────────────────────────────────────────────────────────────────────
cat("Loading files...\n")
players <- fromJSON(JSON_IN,
                    simplifyVector    = FALSE,
                    simplifyDataFrame = FALSE,
                    simplifyMatrix    = FALSE)
rel_df  <- read.csv(RELATIVES_CSV, stringsAsFactors = FALSE)

cat("  Players      :", length(players), "\n")
cat("  Relative rows:", nrow(rel_df),    "\n")

# ── Slug helpers ──────────────────────────────────────────────────────────────

retrosheet_slug <- function(lid) {
  sub("\\d{3}$", "", lid)
}

name_to_slug <- function(nm) {
  nm <- trimws(gsub("\\s+(Jr\\.|Sr\\.|II|III|IV)$", "", nm,
                    ignore.case = TRUE))
  nm <- iconv(nm, from = "UTF-8", to = "ASCII//TRANSLIT")
  if (is.na(nm)) return(NA_character_)
  nm    <- gsub("\\s+", " ", tolower(trimws(nm)))
  parts <- strsplit(nm, " ", fixed = TRUE)[[1]]
  parts <- parts[nchar(parts) > 0L]
  if (length(parts) == 0L) return(NA_character_)
  first_slug <- substr(gsub("[^a-z]",  "", parts[1],             perl = TRUE), 1L, 1L)
  last_slug  <- substr(gsub("[^a-z-]", "", parts[length(parts)], perl = TRUE), 1L, 4L)
  paste0(last_slug, first_slug)
}

# ── Build slug -> sorted Retrosheet IDs from relatives.csv ───────────────────
cat("Building slug -> ID lookup from relatives.csv...\n")

all_rel_ids <- unique(c(rel_df$id1, rel_df$id2))

slug_to_lids <- list()
for (lid in all_rel_ids) {
  slug <- retrosheet_slug(lid)
  if (nchar(slug) == 0L) next
  num  <- as.integer(regmatches(lid, regexpr("\\d{3}$", lid)))
  slug_to_lids[[slug]] <- c(slug_to_lids[[slug]],
                            list(list(num = num, lid = lid)))
}
slug_to_lids <- lapply(slug_to_lids, function(entries) {
  entries[order(vapply(entries, function(e) e$num, integer(1L)))]
})

cat("  Unique slugs in relatives.csv:", length(slug_to_lids), "\n")

# ── Group players by slug, sorted by birth date ───────────────────────────────
player_names_vec <- names(players)
player_slugs_vec <- vapply(player_names_vec, name_to_slug, character(1L))

slug_to_players <- list()
for (i in seq_along(player_names_vec)) {
  nm   <- player_names_vec[i]
  slug <- player_slugs_vec[i]
  if (is.na(slug) || is.null(slug_to_lids[[slug]])) next
  birth <- players[[i]]$bio$birth_date %||% "9999-99-99"
  slug_to_players[[slug]] <- c(
    slug_to_players[[slug]],
    list(list(name = nm, birth = birth))
  )
}
slug_to_players <- lapply(slug_to_players, function(plist) {
  plist[order(vapply(plist, function(p) p$birth, character(1L)))]
})

# ── Pair sorted players with sorted IDs ──────────────────────────────────────
cat("Assigning Retrosheet IDs...\n")

id_lookup <- stats::setNames(
  rep(NA_character_, length(player_names_vec)),
  player_names_vec
)

for (slug in names(slug_to_players)) {
  plist <- slug_to_players[[slug]]
  lids  <- vapply(slug_to_lids[[slug]], function(e) e$lid, character(1L))
  for (k in seq_along(plist)) {
    if (k > length(lids)) break
    id_lookup[plist[[k]]$name] <- lids[k]
  }
}

assigned <- sum(!is.na(id_lookup))
cat("  IDs assigned:", assigned, "/", length(players), "\n")

# ── Build output: only players with a matched ID ──────────────────────────────
cat("Filtering to matched players only...\n")

matched_players <- list()
for (nm in player_names_vec) {
  lid <- id_lookup[nm]
  if (is.na(lid)) next
  p     <- players[[nm]]
  p$id1 <- unname(lid)
  
  # Ensure career_war is present in career_summary.
  # If the source JSON already has it, pass it through;
  # if not (older JSON), set to NA so the field is always present.
  if (!is.null(p$career_summary)) {
    p$career_summary$career_war <- p$career_summary$career_war %||% NA_real_
  }
  
  matched_players[[nm]] <- p
}

cat("  Matched players in output:", length(matched_players), "\n")

# ── Sanity checks ─────────────────────────────────────────────────────────────
cat("\nSanity checks:\n")
checks <- list(
  list(name = "Barry Bonds",     expected = "bondb101"),
  list(name = "Bobby Bonds",     expected = "bondb001"),
  list(name = "Felipe Alou",     expected = "alouf101"),
  list(name = "Moises Alou",     expected = "aloum101"),
  list(name = "Cal Ripken",      expected = "ripkc001"),
  list(name = "Ken Griffey",     expected = "grifk001"),
  list(name = "Ken Griffey Jr.", expected = "grifk002")
)
for (chk in checks) {
  p   <- matched_players[[chk$name]]
  got <- if (!is.null(p)) (p$id1 %||% "NA") else "NOT IN OUTPUT"
  ok  <- identical(got, chk$expected)
  cw  <- if (!is.null(p)) p$career_summary$career_war %||% "NA" else "NA"
  cat(sprintf("  %-25s  expected=%-12s  got=%-12s  career_war=%-8s  [%s]\n",
              chk$name, chk$expected, got, cw,
              if (ok) "OK" else "WARN"))
}

# ── Write ─────────────────────────────────────────────────────────────────────
cat("\nWriting", JSON_OUT, "...\n")
write(
  toJSON(matched_players, auto_unbox = TRUE, pretty = TRUE,
         na = "null", digits = NA),
  JSON_OUT
)
cat("  Done —", length(matched_players), "players —",
    round(file.size(JSON_OUT) / 1e6, 1), "MB\n")
