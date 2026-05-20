"""
Baseball.py
-----------
Pulls MLB outfielder batting data from 1968-2025 using pybaseball.

Data source strategy (hybrid):
  - 1968-2007  ->  Lahman database CSVs fetched individually by raw URL
                   from the cbwinslow/baseballdatabank GitHub mirror.
  - 2008-2025  ->  batting_stats_bref() (Baseball Reference scrape)

Position filtering:
  - Lahman: G_lf + G_cf + G_rf > 0 in the Appearances table.
  - BRef: any token in 'Pos Summary' is OF, LF, CF, or RF.

Output:
  - mlb_outfielders_1968_2025.csv in the same directory as this script.
"""

"""
TO DO:
Solve BRef POS issue-> account for LF, RF, CF
Look more into Lahman WAR
Solve Nickname matching issue(JR. vs SR.)
"""


import io
import os
import sys
import time
import requests
import pandas as pd
from pybaseball import batting_stats_bref, bwar_bat

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

START_YEAR      = 1968
LAHMAN_END_YEAR = 2007
BREF_START_YEAR = 2008
END_YEAR        = 2025
REQUEST_DELAY   = 3     # seconds between Baseball Reference requests

# Raw CSV URLs from the cbwinslow mirror of the Lahman/Chadwick databank.
RAW_BASE = "https://raw.githubusercontent.com/cbwinslow/baseballdatabank/master/core"
LAHMAN_URLS = {
    "Batting":     f"{RAW_BASE}/Batting.csv",
    "Appearances": f"{RAW_BASE}/Appearances.csv",
    "People":      f"{RAW_BASE}/People.csv",
}

OUTPUT_FILE = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    "mlb_outfielders_1968_2025.csv"
)

FINAL_COLUMNS = [
    "Season", "Name", "Age", "Team", "Lg",
    "G", "AB", "PA", "H", "2B", "3B", "HR",
    "R", "RBI", "BB", "SO",
    "BA", "OBP", "SLG", "OPS", "WAR",
    "Pos",
]


# ---------------------------------------------------------------------------
# Lahman CSV fetcher
# ---------------------------------------------------------------------------

def fetch_csv(name: str, url: str) -> pd.DataFrame:
    """Download a single CSV from a raw URL and return as a DataFrame."""
    print(f"  Fetching {name}.csv ...", end=" ", flush=True)
    resp = requests.get(url, timeout=60)
    resp.raise_for_status()
    df = pd.read_csv(io.StringIO(resp.text), low_memory=False)
    print(f"{len(df):,} rows")
    return df


# ---------------------------------------------------------------------------
# Part 1: Lahman (1968-2007)
# ---------------------------------------------------------------------------

def fetch_lahman(start: int, end: int) -> pd.DataFrame:
    print(f"\n--- Lahman database ({start}-{end}) ---")

    try:
        bat = fetch_csv("Batting",     LAHMAN_URLS["Batting"])
        app = fetch_csv("Appearances", LAHMAN_URLS["Appearances"])
        ppl = fetch_csv("People",      LAHMAN_URLS["People"])
    except Exception as e:
        print(f"  ERROR fetching Lahman CSV: {e}")
        return pd.DataFrame()

    # Filter year range
    bat = bat[(bat["yearID"] >= start) & (bat["yearID"] <= end)].copy()
    app = app[(app["yearID"] >= start) & (app["yearID"] <= end)].copy()

    # Identify outfielders by appearances
    of_cols = [c for c in ["G_lf", "G_cf", "G_rf"] if c in app.columns]
    if not of_cols:
        print("  WARNING: OF appearance columns not found in Appearances.csv")
        return pd.DataFrame()

    app["OF_games"] = app[of_cols].fillna(0).sum(axis=1)
    of_keys = app[app["OF_games"] > 0][["yearID", "playerID"]].drop_duplicates()

    bat = bat.merge(of_keys, on=["yearID", "playerID"], how="inner")

    # Merge player bio for names and birth year
    ppl_slim = ppl[["playerID", "nameFirst", "nameLast", "birthYear"]].copy()
    bat = bat.merge(ppl_slim, on="playerID", how="left")

    # Build unified columns
    bat["Name"]   = (bat["nameFirst"].fillna("") + " " + bat["nameLast"].fillna("")).str.strip()
    bat["Season"] = bat["yearID"]
    bat["Age"]    = (bat["yearID"] - bat["birthYear"]).where(bat["birthYear"].notna())
    bat["Team"]   = bat["teamID"].fillna("")
    bat["Lg"]     = bat["lgID"].fillna("")
    bat["Pos"]    = "OF"
    bat["WAR"]    = float("nan")

    # Normalise column name variants
    for old, new in [("X2B", "2B"), ("X3B", "3B")]:
        if old in bat.columns and new not in bat.columns:
            bat[new] = bat[old]

    # Plate appearances (simplified: AB + BB)
    bat["PA"] = bat["AB"].fillna(0) + bat["BB"].fillna(0)

    # Batting average
    bat["BA"] = (bat["H"] / bat["AB"]).where(bat["AB"] > 0).round(3)

    # OBP
    if "HBP" in bat.columns and "SF" in bat.columns:
        bat["OBP"] = (
            (bat["H"].fillna(0) + bat["BB"].fillna(0) + bat["HBP"].fillna(0)) /
            (bat["AB"].fillna(0) + bat["BB"].fillna(0) +
             bat["HBP"].fillna(0) + bat["SF"].fillna(0))
        ).where(bat["AB"] > 0).round(3)
    else:
        bat["OBP"] = (
            (bat["H"].fillna(0) + bat["BB"].fillna(0)) /
            (bat["AB"].fillna(0) + bat["BB"].fillna(0))
        ).where(bat["AB"] > 0).round(3)

    # SLG and OPS
    tb = (bat["H"].fillna(0)
          + bat["2B"].fillna(0)
          + 2 * bat["3B"].fillna(0)
          + 3 * bat["HR"].fillna(0))
    bat["SLG"] = (tb / bat["AB"]).where(bat["AB"] > 0).round(3)
    bat["OPS"] = (bat["OBP"].fillna(0) + bat["SLG"].fillna(0)).round(3)

    available = [c for c in FINAL_COLUMNS if c in bat.columns]
    result = bat[available].copy()
    print(f"  Outfielder rows retained: {len(result):,}")
    return result


# ---------------------------------------------------------------------------
# Part 2: Baseball Reference (2008-2025)
# ---------------------------------------------------------------------------

def fetch_war() -> pd.DataFrame:
    """
    Pull Baseball Reference WAR for batters via bwar_bat().
    Returns a DataFrame keyed by (name_common, year_ID) with a WAR column.
    """
    print("\n--- Fetching WAR from Baseball Reference (bwar_bat) ---")
    try:
        war = bwar_bat(return_all=False)
        # Keep only the columns needed for the join
        war = war[["name_common", "year_ID", "WAR"]].copy()
        war = war.rename(columns={"name_common": "Name", "year_ID": "Season"})
        # Some player names in bwar have suffixes like " *" — strip them
        war["Name"] = war["Name"].str.strip()
        # If a player changed teams, bwar has one row per team; keep the max WAR
        # (the totals row, if present, will naturally be the highest)
        war = war.groupby(["Name", "Season"], as_index=False)["WAR"].sum()
        print(f"  WAR rows loaded: {len(war):,}")
        return war
    except Exception as e:
        print(f"  ERROR fetching WAR: {e}")
        return pd.DataFrame()


def is_outfielder_bref(pos_value) -> bool:
    if pd.isna(pos_value):
        return False
    cleaned = str(pos_value).replace("*", "").replace("-", "")
    parts = set(cleaned.replace("/", " ").split())
    return bool(parts & {"OF", "LF", "CF", "RF"})


def fetch_bref(start: int, end: int) -> pd.DataFrame:
    print(f"\n--- Baseball Reference ({start}-{end}) ---")
    print(f"  Pulling {end - start + 1} seasons with {REQUEST_DELAY}s delay each...")

    all_seasons = []
    pos_col_confirmed = None   # cache the confirmed column name after first year

    for year in range(start, end + 1):
        try:
            df = batting_stats_bref(year)
            if df is None or df.empty:
                print(f"  {year}: no data returned.")
                continue

            df.insert(0, "Season", year)

            # On the first successful year, print actual columns so we know
            # exactly what batting_stats_bref() is returning
            if pos_col_confirmed is None:
                pos_candidates = [c for c in df.columns
                                  if "pos" in c.lower() or "position" in c.lower()]
                print(f"  [First year columns containing 'pos': {pos_candidates}]")
                pos_col_confirmed = next(
                    (c for c in df.columns
                     if c.lower() in ["pos summary", "pos", "position",
                                      "pos_summary", "positions"]),
                    None
                )
                if pos_col_confirmed is None and pos_candidates:
                    pos_col_confirmed = pos_candidates[0]
                print(f"  [Using position column: '{pos_col_confirmed}']")

            pos_col = pos_col_confirmed

            if pos_col:
                df = df[df[pos_col].apply(is_outfielder_bref)].copy()
                df["Pos"] = df[pos_col].astype(str)
            else:
                print(f"  {year}: WARNING - no position column found, keeping all rows.")
                df["Pos"] = ""

            all_seasons.append(df)
            print(f"  {year}: {len(df):>4} outfielder rows.")

        except Exception as e:
            print(f"  {year}: ERROR - {e}")

        if year < end:
            time.sleep(REQUEST_DELAY)

    if not all_seasons:
        return pd.DataFrame()

    combined = pd.concat(all_seasons, ignore_index=True)
    combined = combined.rename(columns={"Tm": "Team"})
    # Drop WAR from bref output — we'll replace it with bwar_bat values in main()
    if "WAR" in combined.columns:
        combined = combined.drop(columns=["WAR"])
    available = [c for c in FINAL_COLUMNS if c in combined.columns]
    print(f"  Total BRef outfielder rows: {len(combined):,}")
    return combined[available].copy()


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    print("=" * 60)
    print(f"MLB Outfielder Data Pull: {START_YEAR}-{END_YEAR}")
    print("=" * 60)

    lahman_df = fetch_lahman(START_YEAR, LAHMAN_END_YEAR)
    bref_df   = fetch_bref(BREF_START_YEAR, END_YEAR)
    war_df    = fetch_war()

    frames = [df for df in [lahman_df, bref_df] if not df.empty]
    if not frames:
        print("\nERROR: No data retrieved.", file=sys.stderr)
        sys.exit(1)

    data = pd.concat(frames, ignore_index=True)

    # Drop any existing WAR column (NaN placeholders from Lahman half,
    # or unreliable values from bref) and replace with bwar_bat values
    if "WAR" in data.columns:
        data = data.drop(columns=["WAR"])

    if not war_df.empty:
        # Ensure join key types match
        data["Season"] = data["Season"].astype(int)
        war_df["Season"] = war_df["Season"].astype(int)
        data["Name"] = data["Name"].astype(str).str.strip()
        war_df["Name"] = war_df["Name"].astype(str).str.strip()
        data = data.merge(war_df, on=["Name", "Season"], how="left")
        matched = data["WAR"].notna().sum()
        print(f"\nWAR values matched: {matched:,} of {len(data):,} rows")
    else:
        data["WAR"] = float("nan")
        print("\nWARNING: WAR data unavailable — column will be empty.")

    # Enforce final column order, keeping only what exists
    final_cols = [c for c in FINAL_COLUMNS if c in data.columns]
    data = data[final_cols].copy()

    sort_cols = [c for c in ["Season", "Name"] if c in data.columns]
    if sort_cols:
        data = data.sort_values(sort_cols).reset_index(drop=True)

    data.to_csv(OUTPUT_FILE, index=False)
    data.to_json(OUTPUT_FILE.replace(".csv", ".json"), orient="records", indent=2)

    print("\n" + "=" * 60)
    print(f"Done. {len(data):,} rows written to:")
    print(f"  {OUTPUT_FILE}")
    print("=" * 60)


if __name__ == "__main__":
    main()
