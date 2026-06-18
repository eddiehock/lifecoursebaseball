# =============================================================================
# build_csv.R
# =============================================================================
#   Filter uses Lahman Appearances (through 2021); seasons 2022+ retain
#   all batting rows since the universal DH rule means pitchers rarely bat.
#
# Output columns:
#   Identity       : name
#   Bio            : birth_date, birth_city, birth_state, birth_country,
#                    bats, throws, height_in, weight_lbs, hall_of_fame, foreign_born
#   Career summary : seasons_played, career_war, career_ops, career_g,
#                    career_ab, career_h, career_hr, career_rbi, career_bb
#   Season         : season, age, multi_team, team, lg, row_type, primary_pos
#   Stats          : G, AB, PA, H, 2B, 3B, HR, R, RBI, BB, SO,
#                    BA, OBP, SLG, OPS, WAR
#
# row_type values:
#   'single' — player spent full season with one team
#   'stint'  — one leg of a multi-team season
#   'total'  — combined season totals for a multi-team season
#
# Install requirements:
#   install.packages(c("Lahman", "dplyr", "tidyr", "readr"))
# =============================================================================

library(Lahman)
library(dplyr)
library(tidyr)
library(readr)

START_YEAR <- 1968
END_YEAR   <- 2025
OUTPUT_CSV <- "mlb_1968_2025_with_bio.csv"

# ── 1. Load Lahman tables ─────────────────────────────────────────────────────
cat("Loading Lahman tables...\n")

batting     <- Lahman::Batting     %>% filter(yearID >= START_YEAR, yearID <= END_YEAR)
people      <- Lahman::People
hof         <- Lahman::HallOfFame
appearances <- Lahman::Appearances %>% filter(yearID >= START_YEAR)

cat("  Batting rows     :", nrow(batting),     "\n")
cat("  People rows      :", nrow(people),      "\n")
cat("  Appearances rows :", nrow(appearances), "\n")

# ── 2. Build non-pitcher player-season keys ──────────────────────────────────
# Strategy: a player-season is kept if games at non-pitching positions
# exceed games pitched. This correctly handles two-way players and
# pitchers who occasionally bat.
#
# Position columns retained: C, 1B, 2B, 3B, SS, LF, CF, RF, OF, DH
# (covers catchers, infielders, outfielders, designated hitters)
#
# Coverage: Lahman Appearances runs through 2021.
# For 2022+ the universal DH rule means pitchers almost never bat,
# so any batting row from those seasons is treated as a position player.
cat("Building non-pitcher filter...\n")

NON_PITCH_COLS <- c("G_c", "G_1b", "G_2b", "G_3b", "G_ss",
                    "G_lf", "G_cf", "G_rf", "G_of", "G_dh")

appearances_filtered <- appearances %>%
  mutate(
    G_nonpitch = rowSums(across(all_of(NON_PITCH_COLS), ~ replace_na(.x, 0))),
    G_p_filled = replace_na(G_p, 0),
    keep       = G_p_filled <= G_nonpitch   # keep if NOT primarily a pitcher
  )

# Set of (playerID, yearID) pairs to retain
nonpitch_keys <- appearances_filtered %>%
  filter(keep) %>%
  select(playerID, yearID) %>%
  distinct()

app_max_year <- max(appearances$yearID)
cat("  Appearances coverage up to:", app_max_year, "\n")
cat("  Non-pitcher player-seasons :", nrow(nonpitch_keys), "\n")

# ── Derive primary position per player-season ─────────────────────────────────
# primary_pos = the position with the most games played that season.
# Labels: C, 1B, 2B, 3B, SS, OF, DH
# Note: G_lf/G_cf/G_rf are collapsed into OF since Lahman's G_of
# already captures total outfield games; we use G_of as the OF total.
position_keys <- appearances_filtered %>%
  filter(keep) %>%
  mutate(
    pos_games = list(
      c(C   = replace_na(G_c,   0),
        `1B` = replace_na(G_1b, 0),
        `2B` = replace_na(G_2b, 0),
        `3B` = replace_na(G_3b, 0),
        SS  = replace_na(G_ss,  0),
        OF  = replace_na(G_of,  0),
        DH  = replace_na(G_dh,  0))
    )
  )

# Vectorised primary position: find column with max games per row
pos_df <- appearances_filtered %>%
  filter(keep) %>%
  transmute(
    playerID,
    yearID,
    primary_pos = case_when(
      replace_na(G_c,   0) >= replace_na(G_1b, 0) &
        replace_na(G_c,   0) >= replace_na(G_2b, 0) &
        replace_na(G_c,   0) >= replace_na(G_3b, 0) &
        replace_na(G_c,   0) >= replace_na(G_ss, 0) &
        replace_na(G_c,   0) >= replace_na(G_of, 0) &
        replace_na(G_c,   0) >= replace_na(G_dh, 0) &
        replace_na(G_c,   0) > 0  ~ "C",
      
      replace_na(G_1b,  0) >= replace_na(G_2b, 0) &
        replace_na(G_1b,  0) >= replace_na(G_3b, 0) &
        replace_na(G_1b,  0) >= replace_na(G_ss, 0) &
        replace_na(G_1b,  0) >= replace_na(G_of, 0) &
        replace_na(G_1b,  0) >= replace_na(G_dh, 0) &
        replace_na(G_1b,  0) > 0  ~ "1B",
      
      replace_na(G_2b,  0) >= replace_na(G_3b, 0) &
        replace_na(G_2b,  0) >= replace_na(G_ss, 0) &
        replace_na(G_2b,  0) >= replace_na(G_of, 0) &
        replace_na(G_2b,  0) >= replace_na(G_dh, 0) &
        replace_na(G_2b,  0) > 0  ~ "2B",
      
      replace_na(G_3b,  0) >= replace_na(G_ss, 0) &
        replace_na(G_3b,  0) >= replace_na(G_of, 0) &
        replace_na(G_3b,  0) >= replace_na(G_dh, 0) &
        replace_na(G_3b,  0) > 0  ~ "3B",
      
      replace_na(G_ss,  0) >= replace_na(G_of, 0) &
        replace_na(G_ss,  0) >= replace_na(G_dh, 0) &
        replace_na(G_ss,  0) > 0  ~ "SS",
      
      replace_na(G_of,  0) >= replace_na(G_dh, 0) &
        replace_na(G_of,  0) > 0  ~ "OF",
      
      replace_na(G_dh,  0) > 0  ~ "DH",
      
      TRUE ~ NA_character_
    )
  ) %>% 
  distinct(playerID, yearID, .keep_all = TRUE)

cat("  Primary position distribution:\n")
print(table(pos_df$primary_pos, useNA = "ifany"))

# ── 2. Fetch WAR from Baseball Reference ─────────────────────────────────────
# BRef publishes two WAR flat files:
#   war_daily_bat.txt   — batting WAR (position players + pitcher batting)
#   war_daily_pitch.txt — pitching WAR
# Both use bbrefID as the player key, NOT Lahman playerID.
# We bridge via the bbrefID column in Lahman People.
#
# For players appearing in both files (two-way players, pitchers who bat),
# WAR values are summed so their total contribution is captured.
cat("Fetching WAR from Baseball Reference...\n")

fetch_war_file <- function(url, label) {
  tryCatch({
    raw <- readr::read_csv(url, show_col_types = FALSE)
    raw %>%
      select(player_ID, year_ID, WAR) %>%
      rename(bbrefID = player_ID, season = year_ID) %>%
      mutate(season = as.integer(season),
             WAR    = as.numeric(WAR)) %>%
      filter(season >= START_YEAR, season <= END_YEAR)
  }, error = function(e) {
    cat("  WARNING: could not fetch", label, "—", conditionMessage(e), "\n")
    NULL
  })
}

bat_war   <- fetch_war_file(
  "https://www.baseball-reference.com/data/war_daily_bat.txt",
  "batting WAR"
)
pitch_war <- fetch_war_file(
  "https://www.baseball-reference.com/data/war_daily_pitch.txt",
  "pitching WAR"
)

# Combine batting and pitching WAR, sum per player-season
all_war_raw <- bind_rows(bat_war, pitch_war)

if (nrow(all_war_raw) > 0) {
  # Build bbrefID -> playerID bridge from Lahman People
  id_bridge <- people %>%
    filter(!is.na(bbrefID), !is.na(playerID)) %>%
    select(playerID, bbrefID) %>%
    distinct()
  
  war_df <- all_war_raw %>%
    left_join(id_bridge, by = "bbrefID", relationship = "many-to-many") %>%
    filter(!is.na(playerID)) %>%
    group_by(playerID, season) %>%
    summarise(WAR = round(sum(WAR, na.rm = TRUE), 2), .groups = "drop")
  
  cat("  WAR rows fetched:", nrow(war_df), "\n")
  cat("  Season range    :", min(war_df$season), "-", max(war_df$season), "\n")
} else {
  cat("  WARNING: no WAR data fetched — WAR columns will be NA.\n")
  war_df <- data.frame(playerID = character(), season = integer(),
                       WAR = numeric())
}

# ── 3. Suffix display name lookup ─────────────────────────────────────────────
# Lahman People does not store suffixes (Jr., Sr., II, III, IV).
# This lookup maps playerID -> correct display name for all affected players.
SUFFIX_NAMES <- c(
  "rivasal01" = "Alfonso Rivas III",
  "wittbo02"  = "Bobby Witt Jr.",
  "edwarca01" = "Carl Edwards Jr.",
  "underdu01" = "Duane Underwood Jr.",
  "smithdw02" = "Dwight Smith Jr.",
  "younger03" = "Eric Young Jr.",
  "rodrife02" = "Fernando Rodriguez Jr.",
  "tatisfe02" = "Fernando Tatis Jr.",
  "alvarhe01" = "Henderson Alvarez III",
  "bradlja02" = "Jackie Bradley Jr.",
  "chishja01" = "Jazz Chisholm Jr.",
  "griffke02" = "Ken Griffey Jr.",
  "wadela01"  = "LaMonte Wade Jr.",
  "mcculla02" = "Lance McCullers Jr.",
  "gurrilo01" = "Lourdes Gurriel Jr.",
  "garcilu04" = "Luis García Jr.",
  "roberlu01" = "Luis Robert Jr.",
  "leitema02" = "Mark Leiter Jr.",
  "harrimi04" = "Michael Harris II",
  "wrighmi01" = "Mike Wright Jr.",
  "acunaro01" = "Ronald Acuña Jr.",
  "kazmase01" = "Sean Kazmar Jr.",
  "longsh01"  = "Shed Long Jr.",
  "souzast01" = "Steven Souza Jr.",
  "lakintr01" = "Travis Lakins Sr.",
  "stoketr01" = "Troy Stokes Jr.",
  "nunovi01"  = "Vidal Nuño III",
  "guerrvl02" = "Vladimir Guerrero Jr."
)

# ── 3. Build bio from People ──────────────────────────────────────────────────
cat("Building bio from People...\n")

hof_inducted <- hof %>%
  filter(inducted == "Y", category == "Player") %>%
  distinct(playerID) %>%
  mutate(hall_of_fame = "HOF")

bio <- people %>%
  select(playerID, nameFirst, nameLast,
         birthYear, birthMonth, birthDay,
         birthCity, birthState, birthCountry,
         bats, throws, height, weight) %>%
  mutate(
    name = ifelse(
      playerID %in% names(SUFFIX_NAMES),
      SUFFIX_NAMES[playerID],
      trimws(paste(
        ifelse(is.na(nameFirst), "", nameFirst),
        ifelse(is.na(nameLast),  "", nameLast)
      ))
    ),
    birth_date = case_when(
      !is.na(birthYear) & !is.na(birthMonth) & !is.na(birthDay) ~
        sprintf("%04d-%02d-%02d", as.integer(birthYear),
                as.integer(birthMonth),
                as.integer(birthDay)),
      !is.na(birthYear) & !is.na(birthMonth) ~
        sprintf("%04d-%02d", as.integer(birthYear),
                as.integer(birthMonth)),
      !is.na(birthYear) ~ as.character(as.integer(birthYear)),
      TRUE ~ NA_character_
    ),
    birth_city    = birthCity,
    birth_state   = birthState,
    birth_country = birthCountry,
    height_in     = as.integer(height),
    weight_lbs    = as.integer(weight),
    # 0 = born in USA, 1 = born abroad, NA = birth country unknown
    foreign_born  = case_when(
      is.na(birthCountry)      ~ NA_integer_,
      birthCountry == "USA"    ~ 0L,
      TRUE                     ~ 1L
    )
  ) %>%
  left_join(hof_inducted, by = "playerID") %>%
  select(playerID, name, birth_date, birth_city, birth_state,
         birth_country, foreign_born, bats, throws,
         height_in, weight_lbs, hall_of_fame)

# ── 4. Compute season-level stats ─────────────────────────────────────────────
cat("Computing season stats...\n")

# Apply position filter:
# For seasons covered by Appearances, keep only non-pitcher player-seasons.
# For seasons beyond Appearances coverage (2022+), keep all batting rows
# since the universal DH rule makes pitcher batting essentially non-existent.
batting_filtered <- batting %>%
  left_join(nonpitch_keys %>% rename(yearID = yearID),
            by = c("playerID", "yearID")) %>%
  filter(
    yearID > app_max_year |                              # beyond Appearances: keep all
      playerID %in% nonpitch_keys$playerID[              # within Appearances: non-pitchers
        nonpitch_keys$yearID == yearID                   # for that specific season
      ]
  )

# Cleaner approach: semi_join for covered years, bind with uncovered years
batting_covered   <- batting %>%
  filter(yearID <= app_max_year) %>%
  semi_join(nonpitch_keys, by = c("playerID", "yearID"))

batting_uncovered <- batting %>%
  filter(yearID > app_max_year)

batting_filtered  <- bind_rows(batting_covered, batting_uncovered)

cat("  Batting rows before filter:", nrow(batting), "\n")
cat("  Batting rows after filter :", nrow(batting_filtered), "\n")

stats <- batting_filtered %>%
  mutate(
    PA  = AB + BB + coalesce(HBP, 0L) + coalesce(SF, 0L),
    TB  = H  + coalesce(X2B, 0L) +
      2L * coalesce(X3B, 0L) +
      3L * coalesce(HR,  0L),
    BA  = ifelse(AB > 0, round(H / AB, 3), NA_real_),
    OBP = ifelse(
      (AB + BB + coalesce(HBP, 0L) + coalesce(SF, 0L)) > 0,
      round(
        (H + BB + coalesce(HBP, 0L)) /
          (AB + BB + coalesce(HBP, 0L) + coalesce(SF, 0L)), 3),
      NA_real_
    ),
    SLG = ifelse(AB > 0, round(TB / AB, 3), NA_real_),
    OPS = ifelse(!is.na(OBP) & !is.na(SLG),
                 round(OBP + SLG, 3), NA_real_)
  ) %>%
  rename(
    season = yearID,
    team   = teamID,
    lg     = lgID,
    `2B`   = X2B,
    `3B`   = X3B
  ) %>%
  left_join(bio, by = "playerID") %>%
  left_join(people %>% select(playerID, birthYear), by = "playerID") %>%
  mutate(
    age = ifelse(!is.na(birthYear),
                 as.integer(season - birthYear),
                 NA_integer_)
  )

# ── 5. Join season WAR onto stats ────────────────────────────────────────────
cat("Joining season WAR...\n")

stats <- stats %>%
  left_join(war_df, by = c("playerID", "season"))

if (nrow(war_df) > 0) {
  matched <- sum(!is.na(stats$WAR))
  cat("  WAR matched:", matched, "of", nrow(stats), "stint rows\n")
} else {
  cat("  WAR not available — column set to NA\n")
}

# ── 6. Join primary position onto stats ──────────────────────────────────────
cat("Joining primary position...\n")

stats <- stats %>%
  left_join(
    pos_df %>% rename(season = yearID),
    by = c("playerID", "season")
  )

# For seasons beyond Appearances coverage, set primary_pos to NA
# (position data not available from Lahman for those years)
stats <- stats %>%
  mutate(primary_pos = ifelse(season > app_max_year, NA_character_, primary_pos))

cat("  primary_pos NA count (beyond Appearances):",
    sum(is.na(stats$primary_pos)), "\n")

# ── 7. Build row_type and multi-team flags ────────────────────────────────────
cat("Building row types...\n")

stint_counts <- stats %>%
  group_by(playerID, season) %>%
  summarise(n_stints = n(), .groups = "drop")

stats <- stats %>%
  left_join(stint_counts, by = c("playerID", "season")) %>%
  mutate(multi_team = n_stints > 1)

single_rows <- stats %>%
  filter(!multi_team) %>%
  mutate(row_type = "single") %>%
  select(-n_stints)

stint_rows <- stats %>%
  filter(multi_team) %>%
  mutate(row_type = "stint") %>%
  select(-n_stints)

total_rows <- stint_rows %>%
  group_by(
    playerID, name, season,
    birth_date, birth_city, birth_state, birth_country, foreign_born,
    bats, throws, height_in, weight_lbs, hall_of_fame, birthYear,
    primary_pos
  ) %>%
  summarise(
    age   = first(age),
    team  = "TOTAL",
    lg    = NA_character_,
    G     = sum(G,    na.rm = TRUE),
    AB    = sum(AB,   na.rm = TRUE),
    PA    = sum(PA,   na.rm = TRUE),
    H     = sum(H,    na.rm = TRUE),
    `2B`  = sum(`2B`, na.rm = TRUE),
    `3B`  = sum(`3B`, na.rm = TRUE),
    HR    = sum(HR,   na.rm = TRUE),
    R     = sum(R,    na.rm = TRUE),
    RBI   = sum(RBI,  na.rm = TRUE),
    BB    = sum(BB,   na.rm = TRUE),
    SO    = sum(SO,   na.rm = TRUE),
    WAR   = ifelse(all(is.na(WAR)), NA_real_,
                   round(sum(WAR, na.rm = TRUE), 2)),
    OBP   = ifelse(
      sum(PA, na.rm = TRUE) > 0,
      round(sum(OBP * PA, na.rm = TRUE) /
              sum(PA,       na.rm = TRUE), 3),
      NA_real_),
    SLG   = ifelse(
      sum(AB, na.rm = TRUE) > 0,
      round(sum(SLG * AB, na.rm = TRUE) /
              sum(AB,       na.rm = TRUE), 3),
      NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    BA         = ifelse(AB > 0, round(H / AB, 3), NA_real_),
    OPS        = ifelse(!is.na(OBP) & !is.na(SLG),
                        round(OBP + SLG, 3), NA_real_),
    multi_team = TRUE,
    row_type   = "total"
  )

cat("  Single rows:", nrow(single_rows), "\n")
cat("  Stint rows :", nrow(stint_rows),  "\n")
cat("  Total rows :", nrow(total_rows),  "\n")

# ── 7. Combine all rows ───────────────────────────────────────────────────────
all_rows <- bind_rows(single_rows, stint_rows, total_rows) %>%
  arrange(name, season, row_type)

# ── 8. Career summaries ───────────────────────────────────────────────────────
cat("Computing career summaries...\n")

for_career <- all_rows %>% filter(row_type %in% c("single", "total"))

career_ops_df <- for_career %>%
  group_by(playerID) %>%
  summarise(
    career_ops = {
      obp_num <- sum(OBP * PA, na.rm = TRUE)
      obp_den <- sum(ifelse(!is.na(OBP), PA, 0L), na.rm = TRUE)
      slg_num <- sum(SLG * AB, na.rm = TRUE)
      slg_den <- sum(ifelse(!is.na(SLG), AB, 0L), na.rm = TRUE)
      if (obp_den > 0 && slg_den > 0)
        round((obp_num / obp_den) + (slg_num / slg_den), 3)
      else NA_real_
    },
    .groups = "drop"
  )

career_summary <- for_career %>%
  group_by(playerID) %>%
  summarise(
    seasons_played = n_distinct(season),
    career_war     = ifelse(all(is.na(WAR)), NA_real_,
                            round(sum(WAR, na.rm = TRUE), 2)),
    career_g       = sum(G,   na.rm = TRUE),
    career_ab      = sum(AB,  na.rm = TRUE),
    career_h       = sum(H,   na.rm = TRUE),
    career_hr      = sum(HR,  na.rm = TRUE),
    career_rbi     = sum(RBI, na.rm = TRUE),
    career_bb      = sum(BB,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(career_ops_df, by = "playerID")

# ── 9. Final join and column order ────────────────────────────────────────────
cat("Building final output...\n")

final <- all_rows %>%
  left_join(career_summary, by = "playerID") %>%
  select(
    name,
    birth_date, birth_city, birth_state, birth_country, foreign_born,
    bats, throws, height_in, weight_lbs, hall_of_fame,
    seasons_played, career_war, career_ops,
    career_g, career_ab, career_h, career_hr, career_rbi, career_bb,
    season, age, multi_team, team, lg, row_type, primary_pos,
    G, AB, PA, H, `2B`, `3B`, HR, R, RBI, BB, SO,
    BA, OBP, SLG, OPS, WAR
  ) %>%
  arrange(name, season, row_type)

# ── 10. Sanity checks ─────────────────────────────────────────────────────────
cat("\nSanity checks:\n")

cat("  Row type counts:\n")
print(table(final$row_type))

cat("\n  Season range  :", min(final$season), "-", max(final$season), "\n")
cat("  Unique players:", n_distinct(final$name), "\n")

cat("\n  Barry Bonds:\n")
bb <- final %>% filter(name == "Barry Bonds") %>% slice(1)
cat("    seasons_played:", bb$seasons_played, "\n")
cat("    career_war    :", bb$career_war,     "\n")
cat("    career_ops    :", bb$career_ops,     "\n")
cat("    career_hr     :", bb$career_hr,      "\n")
cat("    birth_country :", bb$birth_country,  "\n")
cat("    foreign_born  :", bb$foreign_born,   "\n")

cat("\n  WAR check (Barry Bonds 2004):\n")
bb04 <- final %>% filter(name == "Barry Bonds", season == 2004)
cat("    WAR:", bb04$WAR, "\n")

cat("\n  Foreign born distribution:\n")
print(table(final %>% distinct(name, foreign_born) %>% pull(foreign_born),
            useNA = "ifany"))

cat("\n  Position filter check:\n")
cat("    A.J. Burnett (pitcher) present:",
    any(final$name == "A. J. Burnett"), "\n")
cat("    A.J. Pierzynski (catcher) present:",
    any(final$name == "A. J. Pierzynski"), "\n")
cat("    Cal Ripken (SS) present:",
    any(final$name == "Cal Ripken"), "\n")

cat("\n  Primary position distribution (single rows only):\n")
print(table(
  final %>% filter(row_type == "single") %>% pull(primary_pos),
  useNA = "ifany"
))

cat("\n  Suffix players present:\n")
for (nm in c("Ken Griffey Jr.", "Vladimir Guerrero Jr.", "Fernando Tatis Jr.")) {
  n <- sum(final$name == nm, na.rm = TRUE)
  cat("   ", nm, ":", n, "rows\n")
}

cat("\n  Multi-team example (Aaron Boone 2003):\n")
print(
  final %>%
    filter(name == "Aaron Boone", season == 2003) %>%
    select(name, season, team, row_type, G, AB, H, OPS)
)

# ── 11. Write ─────────────────────────────────────────────────────────────────
cat("\nWriting", OUTPUT_CSV, "...\n")
write_csv(final, OUTPUT_CSV, na = "")
cat("  Done —", nrow(final), "rows x", ncol(final), "columns\n")
cat("  File size:", round(file.size(OUTPUT_CSV) / 1e6, 1), "MB\n")
