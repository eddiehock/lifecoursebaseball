# =============================================================================
# Builds a player-keyed relatives network from:
#   - mlb_1968_2025_relatives.json  (output of script retro_id.R)
#   - relatives.csv                (Lahman id1 / relation / id2)
#
#
# Output: relatives_network.json
#
# Structure per player (seasons dropped):
#   {
#     "Barry Bonds": {
#       "id1":            "bondb101",
#       "bio":            { ... },
#       "career_summary": { ... },
#       "ties": [
#         { "lahman_id": "bondb001", "tie_name": "Bobby Bonds", "relation": "Father" },
#         ...
#       ]
#     }
#   }
#
# Load in RStudio:
#   network <- jsonlite::fromJSON("relatives_network.json",
#                                  simplifyVector    = FALSE,
#                                  simplifyDataFrame = FALSE,
#                                  simplifyMatrix    = FALSE)
# =============================================================================

library(jsonlite)

PLAYERS_JSON  <- "mlb_1968_2025_relatives.json"
RELATIVES_CSV <- "relatives.csv"
NETWORK_OUT   <- "relatives_network.json"

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── Relation inversion table ──────────────────────────────────────────────────
INVERT_RELATION <- c(
  "Father"              = "Son",
  "Son"                 = "Father",
  "Brother"             = "Brother",
  "Half Brother"        = "Half Brother",
  "Step Brother"        = "Step Brother",
  "Brother-in-Law"      = "Brother-in-Law",
  "Grandfather"         = "Grandson",
  "Grandson"            = "Grandfather",
  "Great Uncle"         = "Great Nephew",
  "Great Nephew"        = "Great Uncle",
  "Great Grandson"      = "Great Grandfather",
  "Great Grandfather"   = "Great Grandson",
  "Uncle"               = "Nephew",
  "Nephew"              = "Uncle",
  "Father-in-Law"       = "Son-in-Law",
  "Son-in-Law"          = "Father-in-Law",
  "Step Father"         = "Step Son",
  "Step Son"            = "Step Father",
  "Uncle and Stepfather"= "Nephew and Stepson",
  "Cousin"              = "Cousin",
  "Related To"          = "Related To"
)

invert_relation <- function(relation) {
  inv <- INVERT_RELATION[relation]
  ifelse(is.na(inv), relation, inv)   # fall back to original if not in table
}

# ── Load ----------------------------------------------------------------------
cat("Loading files...\n")
players <- fromJSON(PLAYERS_JSON,
                    simplifyVector    = FALSE,
                    simplifyDataFrame = FALSE,
                    simplifyMatrix    = FALSE)
rel_df  <- read.csv(RELATIVES_CSV, stringsAsFactors = FALSE)
cat("  Players       :", length(players),  "\n")
cat("  Relative rows :", nrow(rel_df),     "\n")

# ── Build id1 -> player name lookup ------------------------------------------
# Direct reverse map: Lahman ID -> display name
# Used to resolve a tie's Lahman ID back to a readable name
id_to_name <- list()
for (nm in names(players)) {
  pid <- players[[nm]]$id1
  if (!is.null(pid) && !is.na(pid)) {
    id_to_name[[pid]] <- nm
  }
}
cat("  id1 -> name mappings:", length(id_to_name), "\n")

# ── search_player -------------------------------------------------------------

#' Find player(s) by name fragment or Lahman ID.
#' Returns matching display names from the network (run after network is built).
#'
#' Examples:
#'   search_player("bonds")
#'   search_player("bondb101")
search_player <- function(query) {
  q <- tolower(trimws(query))
  
  # Exact Lahman ID match
  nm <- id_to_name[[q]]
  if (!is.null(nm)) return(nm)
  
  # Name fragment match
  nms <- names(network)
  nms[grepl(q, tolower(nms), fixed = TRUE)]
}

# ── add_relative --------------------------------------------------------------

#' Add a relationship row to rel_df; prevents exact duplicates.
#' Assign result back: rel_df <- add_relative(rel_df, ...)
#'
#' Example:
#'   rel_df <- add_relative(rel_df, "bondb101", "Son", "bondb201")
add_relative <- function(df, id1, relation, id2) {
  dupe <- any(df$id1 == id1 & df$relation == relation & df$id2 == id2)
  if (dupe) {
    message("Already exists: ", id1, " -[", relation, "]-> ", id2)
    return(df)
  }
  rbind(df, data.frame(id1 = id1, relation = relation, id2 = id2,
                       stringsAsFactors = FALSE))
}

# ── Build network -------------------------------------------------------------
cat("Building network...\n")

network     <- vector("list", length(players))
names(network) <- names(players)
n_with_ties <- 0L

for (i in seq_along(players)) {
  p      <- players[[i]]
  nm     <- names(players)[i]
  own_id <- p$id1
  
  if (is.null(own_id) || is.na(own_id)) next
  
  rows_id1 <- rel_df[rel_df$id1 == own_id, , drop = FALSE]
  rows_id2 <- rel_df[rel_df$id2 == own_id, , drop = FALSE]
  
  total_ties <- nrow(rows_id1) + nrow(rows_id2)
  if (total_ties == 0L) next
  
  ties      <- vector("list", total_ties)
  tie_idx   <- 1L
  seen_ids  <- character(0)
  
  for (j in seq_len(nrow(rows_id1))) {
    tie_id <- rows_id1$id2[j]
    if (tie_id %in% seen_ids) next
    seen_ids        <- c(seen_ids, tie_id)
    tie_name        <- id_to_name[[tie_id]] %||% NA_character_
    ties[[tie_idx]] <- list(lahman_id = tie_id,
                            tie_name  = tie_name,
                            relation  = rows_id1$relation[j])   
    tie_idx <- tie_idx + 1L
  }
  for (j in seq_len(nrow(rows_id2))) {
    tie_id <- rows_id2$id1[j]
    if (tie_id %in% seen_ids) next
    seen_ids        <- c(seen_ids, tie_id)
    tie_name        <- id_to_name[[tie_id]] %||% NA_character_
    ties[[tie_idx]] <- list(lahman_id = tie_id,
                            tie_name  = tie_name,
                            relation  = invert_relation(rows_id2$relation[j]))   
    tie_idx <- tie_idx + 1L
  }
  
  # Trim unused pre-allocated slots
  ties <- ties[!vapply(ties, is.null, logical(1L))]
  if (length(ties) == 0L) next
  
  network[[nm]] <- list(
    id1            = own_id,
    bio            = p$bio,
    career_summary = p$career_summary,
    ties           = ties
  )
  n_with_ties <- n_with_ties + 1L
}

network <- Filter(Negate(is.null), network)
cat("  Players with relatives found:", n_with_ties, "\n")

# ── Write ---------------------------------------------------------------------
cat("\nWriting", NETWORK_OUT, "...\n")
write(
  toJSON(network, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = NA),
  NETWORK_OUT
)
cat("  Done —", round(file.size(NETWORK_OUT) / 1e6, 1), "MB\n")

# ── Sanity check --------------------------------------------------------------
cat("\nSanity checks:\n")
for (nm in c("Barry Bonds", "Bobby Bonds", "Ken Griffey", "Ken Griffey Jr.")) {
  entry <- network[[nm]]
  if (!is.null(entry)) {
    cat(sprintf("  %-20s  id1=%-12s  ties=%d\n",
                nm, entry$id1, length(entry$ties)))
    for (t in entry$ties) {
      cat(sprintf("    -> %-12s  %-20s  (%s)\n",
                  t$lahman_id,
                  t$tie_name %||% "unknown",
                  t$relation))
    }
  } else {
    cat(sprintf("  %-20s  not in network\n", nm))
  }
}

# ── Read back as nested list --------------------------------------------------
cat("\nReading", NETWORK_OUT, "as nested list...\n")
network <- jsonlite::fromJSON(
  NETWORK_OUT,
  simplifyVector    = FALSE,
  simplifyDataFrame = FALSE,
  simplifyMatrix    = FALSE
)
cat("  network loaded —", length(network), "players\n")
cat("  class:", class(network), "\n")
