"""
Relatives Network JSON -> CSV Flattener
-----------------------------------------
Converts relatives_network.json to a flat CSV
where each row represents one relationship tie.

Player bio and career summary fields are repeated on every row
for that player, making the CSV self-contained for analysis.

Output columns:
  Identity       : name, id1
  Bio            : birth_date, birth_city, birth_state, birth_country,
                   bats, throws, height_in, weight_lbs, hall_of_fame
  Career summary : seasons_played, career_ops, career_g, career_ab,
                   career_h, career_hr, career_rbi, career_bb
  Tie            : tie_lahman_id, tie_name, relation
"""

import json
import os
import math
import pandas as pd

# ── config ────────────────────────────────────────────────────────────────────

JSON_IN  = "relatives_network.json"
CSV_OUT  = "relatives_network.csv"

BIO_COLS = ["birth_date", "birth_city", "birth_state", "birth_country",
            "bats", "throws", "height_in", "weight_lbs", "hall_of_fame"]

CAREER_COLS = ["seasons_played", "career_ops", "career_g", "career_ab",
               "career_h", "career_hr", "career_rbi", "career_bb", "career_war"]

# ── helpers ───────────────────────────────────────────────────────────────────

def clean(value):
    """Convert numpy/pandas scalars to plain Python; NaN/empty dict -> None."""
    if value is None:
        return None
    if isinstance(value, dict) and len(value) == 0:
        return None
    if isinstance(value, float) and math.isnan(value):
        return None
    if hasattr(value, "item"):
        return value.item()
    return value

# ── load ──────────────────────────────────────────────────────────────────────

print(f"Loading {JSON_IN} ...")
with open(JSON_IN, encoding="utf-8") as f:
    data = json.load(f)
print(f"  {len(data):,} players loaded")

# ── flatten ───────────────────────────────────────────────────────────────────

print("Flattening to rows ...")
rows = []

for name, player in data.items():

    bio    = player.get("bio", {})
    career = player.get("career_summary", {})
    ties   = player.get("ties", [])
    id1    = player.get("id1")

    # Base fields repeated on every row for this player
    base = {"name": name, "id1": id1}

    for col in BIO_COLS:
        base[col] = clean(bio.get(col))

    for col in CAREER_COLS:
        base[col] = clean(career.get(col))

    # One row per tie
    for tie in ties:
        tie_name = tie.get("tie_name")
        # tie_name may be a list (when slug matched multiple players)
        if isinstance(tie_name, list):
            tie_name = ", ".join(str(t) for t in tie_name if t)

        row = {
            **base,
            "tie_lahman_id": tie.get("lahman_id"),
            "tie_name":      tie_name,
            "relation":      tie.get("relation"),
        }
        rows.append(row)

print(f"  {len(rows):,} tie rows built")

# ── build dataframe ───────────────────────────────────────────────────────────

print("Building DataFrame ...")
df = pd.DataFrame(rows)

col_order = (
    ["name", "id1"]
    + BIO_COLS
    + CAREER_COLS
    + ["tie_lahman_id", "tie_name", "relation"]
)
col_order = [c for c in col_order if c in df.columns]
df = df[col_order]

# Integer columns
int_cols = ["height_in", "weight_lbs", "seasons_played",
            "career_g", "career_ab", "career_h",
            "career_hr", "career_rbi", "career_bb"]
for col in int_cols:
    if col in df.columns:
        df[col] = pd.to_numeric(df[col], errors="coerce").astype("Int64")

# Float columns
float_cols = ["career_ops", "career_wars"]
for col in float_cols:
    if col in df.columns:
        df[col] = pd.to_numeric(df[col], errors="coerce")

# ── write ─────────────────────────────────────────────────────────────────────

print(f"Writing {CSV_OUT} ...")
df.to_csv(CSV_OUT, index=False)

size_mb = os.path.getsize(CSV_OUT) / 1_048_576
print(f"  Done — {len(df):,} rows x {len(df.columns)} columns — {size_mb:.1f} MB")

# ── sanity checks ─────────────────────────────────────────────────────────────

print("\nSanity checks:")

print("\n  Relation distribution:")
print(df["relation"].value_counts().to_string())

print("\n  Birth country distribution (top 8):")
print(df.drop_duplicates("name")["birth_country"].value_counts().head(8).to_string())

print("\n  Barry Bonds ties:")
bonds = df[df["name"] == "Barry Bonds"]
print(bonds[["name", "id1", "tie_name", "relation"]].to_string(index=False))

print("\n  Ken Griffey ties:")
griffey = df[df["name"] == "Ken Griffey"]
print(griffey[["name", "id1", "tie_name", "relation"]].to_string(index=False))

print("\n  Ken Griffey Jr. ties:")
griffey_jr = df[df["name"] == "Ken Griffey Jr."]
print(griffey_jr[["name", "id1", "tie_name", "relation"]].to_string(index=False))

print("\n  Most connected players:")
print(df.groupby("name").size().sort_values(ascending=False).head(10).to_string())
