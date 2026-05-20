# pyrefly: ignore [missing-import]
import duckdb
import json
import pandas as pd
import os

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
JSON_FILE = os.path.join(BASE_DIR, 'mlb_1968_2025_with_bio.json')

# ── Load and reshape JSON ─────────────────────────────────────────────
print("Loading and reshaping JSON...")
with open(JSON_FILE) as f:
    data = json.load(f)

records = []
for player_name, player_data in data.items():
    bio    = player_data.get('bio', {})
    career = player_data.get('career_summary', {})
    records.append({
        'player_name':    player_name,
        'birth_date':     bio.get('birth_date'),
        'birth_city':     bio.get('birth_city'),
        'birth_state':    bio.get('birth_state'),
        'birth_country':  bio.get('birth_country'),
        'bats':           bio.get('bats'),
        'throws':         bio.get('throws'),
        'height_in':      bio.get('height_in'),
        'weight_lbs':     bio.get('weight_lbs'),
        'hall_of_fame':   bio.get('hall_of_fame'),
        'seasons_played': career.get('seasons_played'),
        'career_war':     career.get('career_war'),
        'career_hr':      career.get('career_hr'),
        'career_h':       career.get('career_h'),
        'career_g':       career.get('career_g'),
        'career_ab':      career.get('career_ab'),
        'career_rbi':     career.get('career_rbi'),
        'career_bb':      career.get('career_bb'),
    })

# ── Build a Pandas DataFrame, then register it with DuckDB ────────────
df = pd.DataFrame(records)

conn = duckdb.connect()
conn.register('mlb_players', df)   # register DataFrame directly — no JSON serialization

# ── Run queries ───────────────────────────────────────────────────────
result = conn.execute("""
    SELECT player_name, career_war, seasons_played
    FROM mlb_players
    WHERE birth_country IS NULL AND seasons_played >= 5
    LIMIT 15
""").df()

print(result)