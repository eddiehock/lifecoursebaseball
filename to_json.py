"""
MLB CSV -> Nested JSON Converter
---------------------------------
Converts mlb_1968_2025_with_bio.csv to a
nested JSON object keyed by player name.


Output structure per player:
{
  "Barry Bonds": {
    "bio": {
      "birth_date":    "1964-07-24",
      "birth_city":    "Riverside",
      "birth_state":   "California",
      "birth_country": "USA",
      "bats":          "L",
      "throws":        "L",
      "height_in":     73,
      "weight_lbs":    185,
      "hall_of_fame":  null
    },
    "career_summary": {
      "seasons_played": 22,
      "teams":          ["PIT", "SFN"],
      "career_ops":     1.051,
      "career_g":       2986,
      "career_ab":      9847,
      "career_h":       2935,
      "career_hr":      762,
      "career_rbi":     1996,
      "career_bb":      2558
    },
    "seasons": [
      {
        "season":     1986,
        "age":        21,
        "multi_team": false,
        "team":       "PIT",
        "lg":         "NL",
        "stats":      { "G": 113, "AB": 413, ... }
      }
    ]
  }
}
"""

import os
import math
import json
import pandas as pd

# ── config ────────────────────────────────────────────────────────────────────

INPUT_CSV   = "mlb_1968_2025_with_bio.csv"
OUTPUT_JSON = "mlb_1968_2025_with_bio.json"

STAT_COLS = ["G", "AB", "PA", "H", "2B", "3B", "HR",
             "R", "RBI", "BB", "SO", "BA", "OBP", "SLG", "OPS", "WAR"]

BIO_COLS = ["birth_date", "birth_city", "birth_state", "birth_country",
            "bats", "throws", "height_in", "weight_lbs", "hall_of_fame"]

CAREER_COLS = ["seasons_played", "career_ops", "career_g",
               "career_ab", "career_h", "career_hr", "career_rbi", "career_bb", "career_war"]

TOTALS_TOKENS = {"TOTAL", "TOT", "2TM", "3TM", "4TM"}

# ── helpers ───────────────────────────────────────────────────────────────────

def clean(value):
    """Convert numpy/pandas scalars to plain Python; NaN -> None."""
    if isinstance(value, float) and math.isnan(value):
        return None
    if hasattr(value, "item"):
        return value.item()
    return value


def row_to_stats(row: pd.Series) -> dict:
    return {col: clean(row[col]) for col in STAT_COLS if col in row.index}


# ── load ──────────────────────────────────────────────────────────────────────

print(f"Loading {INPUT_CSV} ...")
df = pd.read_csv(INPUT_CSV)

# Normalise column names — CSV from R has lowercase, ensure stat cols exist
print(f"  {len(df):,} rows | {df['name'].nunique():,} players "
      f"| seasons {df['season'].min()}-{df['season'].max()}")

# Validate expected columns are present
missing = [c for c in BIO_COLS + CAREER_COLS + STAT_COLS + ["row_type"]
           if c not in df.columns]
if missing:
    raise ValueError(f"Missing expected columns: {missing}")

# ── build player dictionary ───────────────────────────────────────────────────

print("Building nested player dictionary ...")
players: dict = {}

for name, player_df in df.groupby("name", sort=True):
    player_df = player_df.sort_values("season")

    # ── bio — read from first row (same for all rows of this player) ──────────
    first_row = player_df.iloc[0]
    bio = {}
    for col in BIO_COLS:
        val = clean(first_row[col]) if col in player_df.columns else None
        # Convert height/weight to int if possible
        if col in ("height_in", "weight_lbs") and val is not None:
            try:
                val = int(val)
            except (ValueError, TypeError):
                pass
        bio[col] = val

    # ── career summary — read from first row (pre-computed in R) ─────────────
    career_summary = {}
    for col in CAREER_COLS:
        val = clean(first_row[col]) if col in player_df.columns else None
        if col in ("seasons_played", "career_g", "career_ab", "career_h",
                   "career_hr", "career_rbi", "career_bb") and val is not None:
            try:
                val = int(val)
            except (ValueError, TypeError):
                pass
        career_summary[col] = val

    # Add teams list (all non-total teams this player appeared for)
    all_teams = sorted(set(
        t for t in player_df["team"].dropna().tolist()
        if t and t not in TOTALS_TOKENS
    ))
    career_summary["teams"] = all_teams

    # ── seasons — use row_type directly ──────────────────────────────────────
    seasons = []

    for season_num, season_df in player_df.groupby("season"):
        row_types = season_df["row_type"].tolist()

        if "single" in row_types:
            # Single-team season — one row
            row = season_df[season_df["row_type"] == "single"].iloc[0]
            seasons.append({
                "season":     int(season_num),
                "age":        clean(row["age"]),
                "multi_team": False,
                "team":       clean(row["team"]),
                "lg":         clean(row["lg"]) if pd.notna(row.get("lg")) else None,
                "stats":      row_to_stats(row),
            })

        elif "stint" in row_types or "total" in row_types:
            # Multi-team season — collect stints and total
            stint_rows  = season_df[season_df["row_type"] == "stint"]
            total_rows  = season_df[season_df["row_type"] == "total"]

            stints = [
                {
                    "team":  clean(r["team"]),
                    "lg":    clean(r["lg"]) if pd.notna(r.get("lg")) else None,
                    "stats": row_to_stats(r),
                }
                for _, r in stint_rows.iterrows()
            ]

            # Use total row stats if present, otherwise compute from stints
            if not total_rows.empty:
                season_stats = row_to_stats(total_rows.iloc[0])
            else:
                # Fallback: sum cumulative, weight rate stats by PA
                cumulative = {"G","AB","PA","H","2B","3B","HR","R","RBI","BB","SO"}
                season_stats = {}
                for col in STAT_COLS:
                    if col in cumulative:
                        season_stats[col] = clean(stint_rows[col].sum()) \
                            if col in stint_rows.columns else None
                    else:
                        pa = stint_rows["PA"].sum() if "PA" in stint_rows.columns else 0
                        if pa > 0 and col in stint_rows.columns:
                            season_stats[col] = clean(
                                (stint_rows[col] * stint_rows["PA"]).sum() / pa
                            )
                        else:
                            season_stats[col] = None

            seasons.append({
                "season":     int(season_num),
                "age":        clean(season_df["age"].iloc[0]),
                "multi_team": True,
                "stints":     stints,
                "stats":      season_stats,
            })

    players[name] = {
        "bio":            bio,
        "career_summary": career_summary,
        "seasons":        seasons,
    }

print(f"  Built entries for {len(players):,} players")

# ── write ─────────────────────────────────────────────────────────────────────

print(f"Writing {OUTPUT_JSON} ...")
with open(OUTPUT_JSON, "w", encoding="utf-8") as f:
    json.dump(players, f, indent=2, ensure_ascii=False)

size_mb = os.path.getsize(OUTPUT_JSON) / 1_048_576
print(f"  Done — {size_mb:.1f} MB")

# ── sanity checks ─────────────────────────────────────────────────────────────

print("\nSanity checks:")
for nm in ["Barry Bonds", "Derek Jeter", "Ken Griffey Jr.", "Fernando Tatis Jr."]:
    p = players.get(nm)
    if p:
        cs = p["career_summary"]
        print(f"\n  {nm}")
        print(f"    seasons : {cs['seasons_played']}  "
              f"({p['seasons'][0]['season']}-{p['seasons'][-1]['season']})")
        print(f"    career_ops : {cs['career_ops']}")
        print(f"    career_hr  : {cs['career_hr']}")
        print(f"    birth_country: {p['bio']['birth_country']}")
        print(f"    career_war : {cs['career_war']}")
    else:
        print(f"\n  {nm}: NOT FOUND")

multi_example = next(
    (n for n, p in players.items()
     if any(s["multi_team"] for s in p["seasons"])), None
)
if multi_example:
    ms = next(s for s in players[multi_example]["seasons"] if s["multi_team"])
    print(f"\n  Multi-team example ({multi_example}, {ms['season']}):")
    print(json.dumps(ms, indent=4))
