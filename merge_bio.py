"""
MLB Bio Merge Script
--------------------------
Joins biographical data from biofile0.csv into mlb_1968_2025.json.

Matching passes (applied in order, first match wins):
  Pass 1  — usename + lastname, normalised
  Pass 2  — fullname field, normalised
  Pass 3  — suffix-stripped (Jr., Sr., II, III, IV)
  Pass 4  — suffix-stripped fullname
  Pass 5  — initials collapsed (A.J. / B. J. / AJ → aj)
  Pass 6  — initials-collapsed fullname
  --- NEW ---
  Pass 7  — middle initial stripped (Josh A. Smith → Josh Smith)
  Pass 8  — nickname expansion, both directions
             short → long  : Gio → Giovanny, Manny → Manuel …
             long  → short : Joshua → Josh, Matthew → Matt …
  Pass 9  — manual alias dictionary for transliteration / edge cases
             (Hyun Jin Ryu → Hyun-Jin Ryu, Yunesky → Yulieski …)
"""

import json
import re
import unicodedata
import pandas as pd
from pathlib import Path

# ── config ────────────────────────────────────────────────────────────────────

JSON_IN    = "mlb_1968_2025.json"
BIO_CSV    = "biofile0.csv"
JSON_OUT   = "mlb_1968_2025_with_bio.json"
UNMATCHED  = "unmatched_players.txt"

BIO_FIELD_MAP = {
    "birthdate":    "birth_date",
    "birthcity":    "birth_city",
    "birthstate":   "birth_state",
    "birthcountry": "birth_country",
    "bats":         "bats",
    "throws":       "throws",
    "height":       "height_in",
    "weight":       "weight_lbs",
    "HOF":          "hall_of_fame",
}

# ── nickname tables ───────────────────────────────────────────────────────────
# Each entry maps a name that appears in the JSON → the usename in the bio file.
# Direction A: long formal name in JSON, short usename in bio file
LONG_TO_SHORT = {
    "joshua":      "josh",
    "matthew":     "matt",
    "michael":     "mike",
    "thomas":      "tom",
    "daniel":      "danny",     # Danny Coulombe stored as Daniel
    "christopher": "chris",
    "timothy":     "tim",
    "nathaniel":   "nate",
    "vincent":     "vince",
    "samuel":      "sam",
    "joseph":      "joe",
    "james":       "jim",
    "jeffrey":     "jeff",
    "phillip":     "phil",
    "stephen":     "steve",
    "steven":      "steve",
    "william":     "bill",
    "andrew":      "andy",
    "benjamin":    "ben",
    "zachary":     "zach",
    "nathaniel":   "nate",
    "jacob":       "jake",      # Jakob Junis stored as Jake
    "jakob":       "jake",
    "tobias":      "toby",
    "anthony":     "tony",
    "robert":      "bob",
    "richard":     "rich",
    "jonathan":    "jon",
    "alexander":   "alex",
    "cameron":     "cam",
}

# Direction B: short nickname in JSON, formal usename in bio file
SHORT_TO_LONG = {
    "gio":     "giovanny",
    "manny":   "manuel",
    "tommy":   "tom",
    "jake":    "jacob",
    "vince":   "vincent",
    "frankie": "frank",
    "dave":    "david",
    "danny":   "daniel",
    "mike":    "michael",
    "matt":    "matthew",
    "josh":    "joshua",
    "tom":     "thomas",
    "jim":     "james",
    "steve":   "steven",
    "bill":    "william",
    "andy":    "andrew",
    "joe":     "joseph",
    "jeff":    "jeffrey",
    "phil":    "phillip",
    "nate":    "nathan",
    "sam":     "samuel",
    "chris":   "christopher",
    "zach":    "zachary",
    "ben":     "benjamin",
    "tony":    "anthony",
    "bob":     "robert",
    "rich":    "richard",
    "jon":     "jonathan",
    "alex":    "alexander",
    "cam":     "cameron",
}

# ── manual alias dictionary (pass 9) ─────────────────────────────────────────
# Key   = normalised JSON player name
# Value = normalised bio file usename + lastname to look up
MANUAL_ALIASES = {
    # Asian name romanisation differences
    "hyun jin ryu":        "hyun-jin ryu",
    "hung-chih kuo":       "hong-chih kuo",
    "byungho park":        "byung-ho park",
    "jihwan bae":          "ji hwan bae",
    "jiman choi":          "ji-man choi",
    # Spelling / transliteration variants
    "yunesky maya":        "yuniesky maya",
    "yuli gurriel":        "yulieski gurriel",
    "juan carlos oviedo":  "johan oviedo",
    "geraldo perdomo":     "gerardo perdomo",
    "guillermo moscoso":   "edwin moscoso",
    # Initials stored differently in bio file
    "jb shuck":            "j.b. shuck",
    "jc ramirez":          "j.c. ramirez",
    "jt chargois":         "j.t. chargois",
    "jt riddle":           "j.t. riddle",
    "cj abrams":           "c. j. abrams",
    "cj kayfus":           "c. j. kayfus",
    "cj stubbs":           "c. j. stubbs",
    "aj reed":             "a.j. reed",
    "aj pollock":          "a.j. pollock",
    # Spaced initials (B. J. → B.J. as stored in bio file)
    "b j surhoff":         "b.j. surhoff",
    "b j upton":           "b.j. upton",
    "bj upton":            "b.j. upton",
    "d j dozier":          "d.j. dozier",
    "f p santangelo":      "f.p. santangelo",
    "j c martin":          "j.c. martin",
    "j d drew":            "j.d. drew",
    "j j davis":           "j.j. davis",
    "j j furmaniak":       "j.j. furmaniak",
    "j r phillips":        "j.r. phillips",
    "j t bruett":          "j.t. bruett",
    "r j reynolds":        "r.j. reynolds",
    "t j bohn":            "t.j. bohn",
    # Dotted initials with accent stripped by normalize
    "jc escarra":          "j. c. escarra",
    "jc gutierrez":        "j.c. gutierrez",
    "jc mejia":            "j.c. mejia",
    "jd hammer":           "jd hammer",
    "tj house":            "t.j. house",
    "tj hopkins":          "t.j. hopkins",
    # Three-part / compound names
    "hoy park":            "hoy jun park",
    "rey fuentes":         "reymond fuentes",
    "sam long":            "sammy long",
    # Nickname ↔ formal mismatches not caught by the table
    "jakob junis":         "jake junis",
    "james sherfy":        "jimmie sherfy",
    "jay schlueter":       "jayd schlueter",
    "jim steels":          "james steels",
    "joe thurston":        "joseph thurston",
    "jeff fiorentino":     "jeffrey fiorentino",
    "kam mickolio":        "kameron mickolio",
    "marv lane":           "marvin lane",        # attempt; may not exist
    "stevie wilkerson":    "steve wilkerson",
    "luke french":         "drew french",        # bio has Drew French for this id
    "dan winkler":         "daniel winkler",
    "dave bush":           "david bush",
    "drew carpenter":      "andrew carpenter",
    "mike morse":          "michael morse",
    "mike fiers":          "michael fiers",
    "mike dunn":           "michael dunn",
    "mike brosseau":       "michael brosseau",
    "phil ervin":          "phillip ervin",
    "sam deduno":          "samuel deduno",
    "steven baron":        "steve baron",
    "nate karns":          "nathan karns",
    "tommy milone":        "tom milone",
    "vince velasquez":     "vincent velasquez",
    "manny corpas":        "manuel corpas",
    "frankie de la rosa":  "frank de la rosa",
    "gio urshela":         "giovanny urshela",
}


# ── helpers ───────────────────────────────────────────────────────────────────

def normalize(s: str) -> str:
    if not isinstance(s, str):
        return ""
    s = unicodedata.normalize("NFD", s)
    s = "".join(c for c in s if unicodedata.category(c) != "Mn")
    return " ".join(s.lower().split())


def strip_suffix(s: str) -> str:
    return re.sub(r"\s+(jr\.?|sr\.?|ii|iii|iv)$", "", s.strip(), flags=re.I).strip()


def collapse_initials(s: str) -> str:
    # Remove dots after single letters and collapse spaces: B. J. / B.J. / BJ → bj
    s = re.sub(r"\b([A-Za-z])\.(\s*)", r"\1 ", s)   # A. → "A "
    s = re.sub(r"\b([A-Za-z])\s+([A-Za-z])\b(?!\w)", r"\1\2", s)  # B J → BJ
    return normalize(s)


def strip_middle_initial(s: str) -> str:
    """'Josh A. Smith' → 'Josh Smith'"""
    return re.sub(r"\s+[A-Z]\.\s+", " ", s).strip()


def apply_nickname(s: str, table: dict) -> str:
    """Replace the first word of s using the nickname table."""
    parts = s.split(" ", 1)
    if not parts:
        return s
    first = parts[0]
    rest  = parts[1] if len(parts) > 1 else ""
    replacement = table.get(first)
    if replacement:
        return (replacement + " " + rest).strip()
    return s


def clean_bio_value(col: str, val):
    if pd.isna(val):
        return None
    if col == "birthdate":
        try:
            s = str(int(val))
            if len(s) == 8:
                return f"{s[:4]}-{s[4:6]}-{s[6:]}"
        except (ValueError, TypeError):
            pass
        return str(val)
    if col in ("height", "weight"):
        try:
            return int(val)
        except (ValueError, TypeError):
            return float(val)
    if col == "HOF":
        return str(val) if val else None
    return str(val).strip() if isinstance(val, str) else val


# ── load data ─────────────────────────────────────────────────────────────────

print("Loading files …")
with open(JSON_IN, encoding="utf-8") as f:
    players = json.load(f)

bio_df = pd.read_csv(BIO_CSV, low_memory=False)
bio_df["_full"] = (
    bio_df["usename"].fillna("").str.strip()
    + " "
    + bio_df["lastname"].fillna("").str.strip()
).str.strip()

print(f"  JSON players : {len(players):,}")
print(f"  Bio records  : {len(bio_df):,}")


# ── build lookups ─────────────────────────────────────────────────────────────

def build_lookup(series: pd.Series) -> dict:
    lookup = {}
    for idx, val in series.items():
        key = normalize(str(val)) if isinstance(val, str) else ""
        if key:
            lookup[key] = idx
    return lookup

lookup_full        = build_lookup(bio_df["_full"])
lookup_fullname    = build_lookup(bio_df["fullname"])
lookup_full_ns     = {normalize(strip_suffix(k)): v for k, v in lookup_full.items()}
lookup_fullname_ns = {normalize(strip_suffix(k)): v for k, v in lookup_fullname.items()}
lookup_full_ci     = {collapse_initials(k): v        for k, v in lookup_full.items()}
lookup_fullname_ci = {collapse_initials(k): v        for k, v in lookup_fullname.items()}


def lookup_key(key: str, *lookups) -> int | None:
    """Try a normalised key against one or more lookup dicts."""
    for lkp in lookups:
        if key in lkp:
            return lkp[key]
    return None


def find_bio_row(name: str) -> int | None:
    n    = normalize(name)
    n_ns = normalize(strip_suffix(name))
    n_ci = collapse_initials(name)

    # Passes 1–6 (unchanged from v1)
    for key in (n, n_ns, n_ci):
        idx = lookup_key(key,
                         lookup_full, lookup_fullname,
                         lookup_full_ns, lookup_fullname_ns,
                         lookup_full_ci, lookup_fullname_ci)
        if idx is not None:
            return idx

    # Pass 7 — strip middle initial
    n_mi = normalize(strip_middle_initial(name))
    if n_mi != n:
        idx = lookup_key(n_mi,
                         lookup_full, lookup_fullname,
                         lookup_full_ci, lookup_fullname_ci)
        if idx is not None:
            return idx

    # Pass 8a — long → short nickname on normalised name
    n_l2s = normalize(apply_nickname(n, LONG_TO_SHORT))
    if n_l2s != n:
        idx = lookup_key(n_l2s,
                         lookup_full, lookup_fullname,
                         lookup_full_ci, lookup_fullname_ci)
        if idx is not None:
            return idx

    # Pass 8b — short → long nickname on normalised name
    n_s2l = normalize(apply_nickname(n, SHORT_TO_LONG))
    if n_s2l != n:
        idx = lookup_key(n_s2l,
                         lookup_full, lookup_fullname,
                         lookup_full_ci, lookup_fullname_ci)
        if idx is not None:
            return idx

    # Pass 9 — manual alias dictionary
    alias = MANUAL_ALIASES.get(n)
    if alias:
        idx = lookup_key(alias,
                         lookup_full, lookup_fullname,
                         lookup_full_ci, lookup_fullname_ci)
        if idx is not None:
            return idx

    return None


# ── merge ─────────────────────────────────────────────────────────────────────

print("\nMerging bio data …")

matched   = 0
unmatched = []

for player_name, player_data in players.items():
    row_idx = find_bio_row(player_name)

    if row_idx is None:
        unmatched.append(player_name)
        continue

    row = bio_df.loc[row_idx]
    bio_block = {}
    for csv_col, json_key in BIO_FIELD_MAP.items():
        bio_block[json_key] = clean_bio_value(csv_col, row.get(csv_col))

    player_data["bio"] = bio_block
    matched += 1


# ── report ────────────────────────────────────────────────────────────────────

total = len(players)
pct   = matched / total * 100

print(f"\n  Matched   : {matched:,} / {total:,}  ({pct:.1f}%)")
print(f"  Unmatched : {len(unmatched):,}")

if unmatched:
    with open(UNMATCHED, "w", encoding="utf-8") as f:
        f.write(f"# Unmatched players after all 9 passes ({len(unmatched)})\n")
        f.write("# These require manual bio lookup.\n\n")
        for name in sorted(unmatched):
            f.write(name + "\n")
    print(f"  Unmatched list saved → {UNMATCHED}")


# ── write output ──────────────────────────────────────────────────────────────

print(f"\nWriting {JSON_OUT} …")
with open(JSON_OUT, "w", encoding="utf-8") as f:
    json.dump(players, f, indent=2, ensure_ascii=False)

import os
size_mb = os.path.getsize(JSON_OUT) / 1_048_576
print(f"  Done — {size_mb:.1f} MB")


# ── sanity checks ─────────────────────────────────────────────────────────────

print("\nSanity checks (previously unmatched):")

checks = [
    "AJ Pollock", "B. J. Upton", "Hyun Jin Ryu", "Josh A. Smith",
    "Gio Urshela", "Joshua Fuentes", "Yuli Gurriel", "Manny Corpas",
    "Michael A. Taylor", "Tommy Milone", "Jakob Junis", "Jihwan Bae",
]
for name in checks:
    p = players.get(name)
    if p:
        bio = p["bio"]
        filled = {k: v for k, v in bio.items() if v is not None}
        status = f"✓  birth_country={filled.get('birth_country','—')}  bats={filled.get('bats','—')}"
    else:
        status = "not in JSON"
    print(f"  {name:<28} {status}")
