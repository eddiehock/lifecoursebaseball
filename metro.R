# ============================================================
# metro.R
#
# Builds a standalone dataset of MLB team metro-area population,
# using a 3-year LAGGED average (mean of t-1, t-2, t-3) to smooth
# year-to-year ACS estimate noise.
#
# Sources:
#   - US metros: Census Bureau ACS 5-Year estimates via {tidycensus}
#   - Canadian metro (Toronto): Statistics Canada CMA population
#     estimates via {cansim}. Montreal (Expos, 1998-2004) is excluded --
#     US metro data has a hard floor at 2009 (see note below), so there
#     is no year where Montreal would ever line up with the rest of the
#     dataset anyway.
#
# NOTE: ACS 5-year estimates only go back to 2009 (year param = the
# *last* year of the 5-year window). Going back further would need a
# 2000 decennial anchor rolled up from county-level data via a
# county->CBSA FIPS crosswalk.
#
# Output: metro_population.csv
#   Columns: teamID, city, country, year, metro_population,
#            metro_population_lag3avg
# ============================================================

library(tidyverse)
library(tidycensus)
library(cansim)

# ------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------

YEARS <- 2009:2024

OUTPUT_PATH <- "metro_population.csv"

census_key <- Sys.getenv("CENSUS_API_KEY")
if (nchar(census_key) > 0) {
  census_api_key(census_key, overwrite = FALSE, install = FALSE)
}

# ------------------------------------------------------------
# TEAM-TO-METRO CROSSWALK
# ------------------------------------------------------------
# teamID follows Lahman::Teams conventions (matches your build_csv.R).
# metro_pattern is a stable substring matched against tidycensus's NAME
# column. country: "US" -> tidycensus, "CA" -> cansim.
# valid_from / valid_to bound the years a team belongs to that metro.

team_crosswalk <- tribble(
  ~teamID, ~city,           ~country, ~metro_pattern,             ~valid_from, ~valid_to,
  "NYA",   "New York",      "US",     "New York-Newark",          1998,        NA,
  "NYN",   "New York",      "US",     "New York-Newark",          1998,        NA,
  "BOS",   "Boston",        "US",     "Boston-Cambridge",         1998,        NA,
  "TBA",   "Tampa Bay",     "US",     "Tampa-St. Petersburg",     1998,        NA,
  "BAL",   "Baltimore",     "US",     "Baltimore-Columbia",       1998,        NA,
  "TOR",   "Toronto",       "CA",     "Toronto",                  1998,        NA,
  "CHA",   "Chicago",       "US",     "Chicago-Naperville",       1998,        NA,
  "CHN",   "Chicago",       "US",     "Chicago-Naperville",       1998,        NA,
  "CLE",   "Cleveland",     "US",     "Cleveland-Elyria",         1998,        NA,
  "DET",   "Detroit",       "US",     "Detroit-Warren",           1998,        NA,
  "KCA",   "Kansas City",   "US",     "Kansas City, MO-KS",       1998,        NA,
  "MIN",   "Minneapolis",   "US",     "Minneapolis-St. Paul",     1998,        NA,
  "HOU",   "Houston",       "US",     "Houston-The Woodlands",    1998,        NA,
  "LAA",   "Los Angeles",   "US",     "Los Angeles-Long Beach",   1998,        NA,
  "LAN",   "Los Angeles",   "US",     "Los Angeles-Long Beach",   1998,        NA,
  "OAK",   "Oakland",       "US",     "San Francisco-Oakland",    1998,        2024,
  "SFN",   "San Francisco", "US",     "San Francisco-Oakland",    1998,        NA,
  "SEA",   "Seattle",       "US",     "Seattle-Tacoma",           1998,        NA,
  "TEX",   "Texas",         "US",     "Dallas-Fort Worth",        1998,        NA,
  "ATL",   "Atlanta",       "US",     "Atlanta-Sandy Springs",    1998,        NA,
  "MIA",   "Miami",         "US",     "Miami-Fort Lauderdale",    2012,        NA,
  "FLO",   "Miami",         "US",     "Miami-Fort Lauderdale",    1998,        2011,
  "PHI",   "Philadelphia",  "US",     "Philadelphia-Camden",      1998,        NA,
  "WAS",   "Washington",    "US",     "Washington-Arlington",     2005,        NA,
  # MON kept for documentation only -- inert while YEARS starts at 2009,
  # since valid_to = 2004 means it never falls inside the current range.
  "MON",   "Montreal",      "CA",     "Montreal",                 1998,        2004,
  "CIN",   "Cincinnati",    "US",     "Cincinnati",                1998,        NA,
  "MIL",   "Milwaukee",     "US",     "Milwaukee-Waukesha",       1998,        NA,
  "PIT",   "Pittsburgh",    "US",     "Pittsburgh",                1998,        NA,
  "SLN",   "St. Louis",     "US",     "St. Louis",                 1998,        NA,
  "ARI",   "Phoenix",       "US",     "Phoenix-Mesa",              1998,        NA,
  "COL",   "Denver",        "US",     "Denver-Aurora",            1998,        NA,
  "SDN",   "San Diego",     "US",     "San Diego-Chula Vista",    1998,        NA
)

# ------------------------------------------------------------
# 1. PULL US METRO POPULATION (ACS 5-Year, via tidycensus)
# ------------------------------------------------------------

us_patterns <- team_crosswalk %>%
  filter(country == "US") %>%
  distinct(metro_pattern) %>%
  pull(metro_pattern)

# The OMB revised CBSA delineations in Feb 2013 based on 2010 Census
# results, and the Census Bureau applied the new names starting with
# the 2013 5-year ACS vintage. ACS releases for years 2009-2012 still
# use the OLD names below for these four metros. Everything else in
# the crosswalk above only matches on the first two words of the CBSA
# name, which happen to be stable across the 2013 (and 2018) renames --
# these four are the exceptions where the first word pair itself changed.
legacy_metro_patterns <- tribble(
  ~metro_pattern,          ~name_variant,
  "New York-Newark",       "New York-Northern New Jersey-Long Island",
  "Chicago-Naperville",    "Chicago-Joliet-Naperville",
  "Baltimore-Columbia",    "Baltimore-Towson",
  "Houston-The Woodlands", "Houston-Sugar Land-Baytown"
)

metro_name_variants <- bind_rows(
  tibble(metro_pattern = us_patterns, name_variant = us_patterns),
  legacy_metro_patterns
)

match_us_metro <- function(nm) {
  hit <- metro_name_variants$metro_pattern[str_detect(nm, fixed(metro_name_variants$name_variant))]
  if (length(hit) == 0) NA_character_ else hit[1]
}

fetch_acs_year <- function(yr) {
  message("Fetching ACS 5-year CBSA population for ", yr, "...")
  acs_yr <- get_acs(
    geography = "cbsa",
    variables = "B01003_001",  # total population
    year = yr,
    survey = "acs5"
  )
  acs_yr %>%
    filter(str_detect(NAME, str_c(metro_name_variants$name_variant, collapse = "|"))) %>%
    transmute(year = yr, cbsa_name = NAME, metro_population = estimate)
}

us_metro_pop <- map_dfr(YEARS, fetch_acs_year) %>%
  mutate(metro_pattern = map_chr(cbsa_name, match_us_metro)) %>%
  filter(!is.na(metro_pattern)) %>%
  select(year, metro_pattern, metro_population)

# ------------------------------------------------------------
# 2. PULL CANADIAN METRO POPULATION (StatCan, via cansim)
# ------------------------------------------------------------
# Table 17-10-0148-01: Population estimates, July 1, by census
# metropolitan area and census agglomeration (2021 boundaries).

# Using EXACT GEO string matches here rather than a substring/regex match,
# now that we know the real format from inspecting `distinct(ca_raw, GEO)`:
# StatCan labels these as "<City> (CMA), <Province>" -- e.g.
# "Toronto (CMA), Ontario" -- not the "<City>, <Province>" format I'd
# assumed earlier.
ca_geo_lookup <- tribble(
  ~metro_pattern, ~geo_name,
  "Toronto",      "Toronto (CMA), Ontario"
  # Montreal deliberately omitted -- see note at top of file.
)

message("Fetching StatCan CMA population estimates...")
ca_raw <- get_cansim("17-10-0148-01")

ca_metro_pop <- ca_raw %>%
  filter(
    GEO %in% ca_geo_lookup$geo_name,
    Gender == "Total - gender",
    `Age group` == "All ages"
  ) %>%
  left_join(ca_geo_lookup, by = c("GEO" = "geo_name")) %>%
  transmute(
    year = lubridate::year(Date),
    metro_pattern,
    metro_population = val_norm
  ) %>%
  filter(year %in% YEARS)

if (nrow(ca_metro_pop) == 0) {
  warning("No rows matched for Toronto in 17-10-0148-01 -- check GEO column values with `distinct(ca_raw, GEO)`.")
}

# Guard: each (metro_pattern, year) should resolve to exactly one row. If
# this table carries an extra dimension we haven't accounted for (a
# boundary vintage, a revised-vs-preliminary estimate flag, etc.), the
# same GEO + Date combo can appear more than once with different
# val_norm values -- silently picking one would be a real correctness risk for
# research data, so this fails loudly instead with debugging pointers.
ca_dupes <- ca_metro_pop %>% count(metro_pattern, year) %>% filter(n > 1)
if (nrow(ca_dupes) > 0) {
  print(ca_dupes)
  stop(
    "Duplicate (metro_pattern, year) rows in Canadian population data -- ",
    "this table has another dimension varying underneath GEO/Date. Run:\n",
    "  ca_raw %>% filter(GEO %in% ca_geo_lookup$geo_name) %>% ",
    "distinct(across(-c(Date, val_norm))) %>% View()\n",
    "to see which extra column is producing multiple rows, then add a ",
    "filter on it above (e.g. selecting only the current/final estimate)."
  )
}

# ------------------------------------------------------------
# 3. COMBINE US + CANADA, JOIN BACK TO TEAM CROSSWALK
# ------------------------------------------------------------

all_metro_pop <- bind_rows(us_metro_pop, ca_metro_pop)

team_year_pop <- team_crosswalk %>%
  crossing(year = YEARS) %>%
  filter(year >= valid_from, is.na(valid_to) | year <= valid_to) %>%
  left_join(all_metro_pop, by = c("metro_pattern", "year")) %>%
  select(teamID, city, country, year, metro_population) %>%
  arrange(teamID, year)

# Safeguard: Montreal (MON) must never appear past 2004 -- the Expos
# left after the 2004 season.
stopifnot(
  "Montreal (MON) rows found after 2004 -- Expos left after 2004 season" =
    !any(team_year_pop$teamID == "MON" & team_year_pop$year > 2004)
)

# ------------------------------------------------------------
# 4. COMPUTE 3-YEAR LAGGED AVERAGE
# ------------------------------------------------------------
# For season `year`, this averages metro_population from year-1,
# year-2, and year-3 -- never the current or future season's
# estimate. Requires all 3 prior years present; otherwise NA.

team_year_pop <- team_year_pop %>%
  group_by(teamID) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    metro_population_lag3avg = (
      lag(metro_population, 1) +
        lag(metro_population, 2) +
        lag(metro_population, 3)
    ) / 3
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 5. WRITE OUTPUT CSV
# ------------------------------------------------------------

write_csv(team_year_pop, OUTPUT_PATH)
message("Wrote ", nrow(team_year_pop), " rows to ", OUTPUT_PATH)