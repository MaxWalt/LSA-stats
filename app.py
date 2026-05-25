import streamlit as st
import pandas as pd
import os

# =============================================================================
# CONFIG
# =============================================================================

st.set_page_config(
    page_title="LSA — Statistiques du club",
    page_icon="🏃",
    layout="wide"
)

# =============================================================================
# EVENT SORT ORDER
# Maps exact event name → integer sort key.
# Events not listed fall back to 9999 (displayed last).
# Groups: Course < Haies < Sauts < Lancers < Combiné < Autre
# =============================================================================

EVENT_SORT = {
    # --- Course ---
    "50 m.": 10,  "60 m.": 20,  "80 m.": 30,  "100 m.": 40,  "150 m.": 50,
    "200 m.": 60, "300 m.": 70, "400 m.": 80,  "500 m.": 90,  "600 m.": 100,
    "800 m.": 110, "1000 m.": 120, "1500 m.": 130, "2000 m.": 140,
    "3000 m.": 150, "5000 m.": 160, "10000 m.": 170,
    "Mile": 180, "1/2 marathon": 190, "Marathon": 200, "100 km.": 210, "Heure": 220,
    # --- Haies ---
    "50 m. haies": 310,
    "50 m. haies (76.2)": 311,  "50 m. haies (84.0)": 312,
    "50 m. haies (91.4)": 313,  "50 m. haies (99.1)": 314,  "50 m. haies (106.7)": 315,
    "60 m. haies": 320,
    "60 m. haies (76.2)": 321,  "60 m. haies (84.0)": 322,
    "60 m. haies (91.4)": 323,  "60 m. haies (99.1)": 324,  "60 m. haies (106.7)": 325,
    "80 m. haies (76.2)": 330,
    "100 m. haies (76.2)": 340, "100 m. haies (84.0)": 341,
    "110 m. haies (91.4)": 350, "110 m. haies (99.1)": 351, "110 m. haies (106.7)": 352,
    "200 m. haies": 360, "300 m. haies": 370,
    "400 m. haies (76.2)": 380, "400 m. haies (91.4)": 381,
    "1500 m. steeple": 390, "2000 m. steeple": 391, "3000 m. steeple": 392,
    # --- Sauts ---
    "Longueur": 410, "Longueur (zone)": 411,
    "Triple saut": 420, "Hauteur": 430, "Perche": 440,
    # --- Lancers ---
    "poids": 500,
    "Poids (2.5 kg)": 501, "Poids (3 kg)": 502, "Poids (4 kg)": 503,
    "Poids (5 kg)": 504,   "Poids (6 kg)": 505, "Poids (7.26 kg)": 506,
    "Disque (0.75 kg)": 510, "Disque (1 kg)": 511, "Disque (1.5 kg)": 512,
    "Disque (1.75 kg)": 513, "Disque (2 kg)": 514,
    "Javelot (400 g)": 520, "Javelot (500 g)": 521, "Javelot (600 g)": 522,
    "Javelot (700 g)": 523, "Javelot (800 g)": 524,
    "Marteau (3 kg)": 530, "Marteau (4 kg)": 531, "Marteau (5 kg)": 532,
    "Marteau (6 kg)": 533, "Marteau (7.26 kg)": 534,
    "Balle (200 g)": 540,
    # --- Combiné ---
    "Pentathlon": 610, "Hexathlon": 620, "Heptathlon femmes": 630, "Décathlon": 640,
    # --- Autre ---
    "UBS Kids Cup": 710, "Triathlon sprint (60-100-200 m.)": 720,
}

def event_sort_key(event_name):
    return EVENT_SORT.get(str(event_name).strip(), 9999)

def sort_events(events):
    return sorted(events, key=event_sort_key)

# =============================================================================
# LOAD DATA
# =============================================================================

@st.cache_data
def load_master():
    df = pd.read_excel("data/ls_master/ls_master_2025.xlsx")
    df["indoor"] = df["indoor"].astype(bool)
    df["season"] = pd.to_numeric(df["season"], errors="coerce")
    df["yob"]    = pd.to_numeric(df["yob"],    errors="coerce")
    df["mark"]   = pd.to_numeric(df["mark"],   errors="coerce")
    return df

@st.cache_data
def compute_records(master_df):
    """Derive club records from the master dataset.

    For each (event, gender, indoor, club_cat) group the single best
    performance is kept.  An extra synthetic 'ALL' category holds the
    absolute club record (best across all age categories).
    """
    df = master_df[master_df["mark"].notna()].copy()
    if df.empty:
        return pd.DataFrame(columns=master_df.columns)

    # Per-event sort direction (True = lower is better, e.g. times)
    if "better_is_lower" in df.columns:
        dir_map = df.groupby("event")["better_is_lower"].first().to_dict()
    else:
        dir_map = {}

    def pick_best(group, event):
        asc = bool(dir_map.get(event, True))
        idx = group["mark"].idxmin() if asc else group["mark"].idxmax()
        return group.loc[idx]

    # Best per event + gender + indoor + club_cat
    per_cat = [
        pick_best(grp, event)
        for (event, _gender, _indoor, _club_cat), grp
        in df.groupby(["event", "gender", "indoor", "club_cat"])
    ]

    # ALL = absolute club record regardless of age category
    all_rows = []
    for (event, _gender, _indoor), grp in df.groupby(["event", "gender", "indoor"]):
        row = pick_best(grp, event).copy()
        row["club_cat"] = "ALL"
        all_rows.append(row)

    return pd.concat(
        [pd.DataFrame(per_cat), pd.DataFrame(all_rows)],
        ignore_index=True,
    )

master  = load_master()
records = compute_records(master)

# Fallback: use "athlete" if "athlete_display" column is absent
_ACOL = "athlete_display" if "athlete_display" in master.columns else "athlete"

# =============================================================================
# LABELS
# =============================================================================

GENDER_LABELS = {"female": "Femmes", "male": "Hommes"}
INDOOR_LABELS = {True: "Salle", False: "Plein air"}

CAT_ORDER  = ["ALL", "SEN", "U23", "U20", "U18", "U16", "U14", "U12", "U10"]
CAT_LABELS = {
    "ALL":  "Tous (record du club)",
    "SEN":  "Seniors",
    "U23":  "U23", "U20": "U20", "U18": "U18",
    "U16":  "U16", "U14": "U14", "U12": "U12", "U10": "U10",
    "TOUS": "Toutes catégories",
}

def format_date(d):
    try:
        return pd.to_datetime(d).strftime("%d.%m.%Y")
    except Exception:
        return "—"

def format_season(s):
    try:
        return str(int(s))
    except Exception:
        return "—"

def is_lower_better(df_subset):
    if "better_is_lower" in df_subset.columns:
        vals = df_subset["better_is_lower"].dropna()
        if len(vals):
            return bool(vals.iloc[0])
    return True  # default: times → lower is better

def best_per_athlete(df, n):
    asc = is_lower_better(df)
    return (
        df.sort_values("mark", ascending=asc)
          .drop_duplicates(subset=["athlete"])
          .head(n)
          .reset_index(drop=True)
    )

# =============================================================================
# SIDEBAR
# =============================================================================

with st.sidebar:
    logo_path = "assets/lsa_logo.png"
    if os.path.exists(logo_path):
        st.image(logo_path, width=150)

    st.title("Filtres")

    gender_choice = st.radio(
        "Genre",
        options=["female", "male"],
        format_func=lambda x: GENDER_LABELS[x],
    )

    indoor_choice = st.radio(
        "Piste",
        options=[False, True],
        format_func=lambda x: INDOOR_LABELS[x],
    )

    view = st.radio(
        "Vue",
        options=["Records", "Top 10", "Top 50"],
    )

# =============================================================================
# PAGE HEADER
# =============================================================================

st.title("Lausanne-Sports Athlétisme")
st.caption(
    f"{'Femmes' if gender_choice == 'female' else 'Hommes'} · "
    f"{'Salle' if indoor_choice else 'Plein air'}"
)

# =============================================================================
# VIEW: RECORDS
# =============================================================================

if view == "Records":
    st.header("Records du club")

    available_cats = [c for c in CAT_ORDER if c in records["club_cat"].unique()]
    cat_choice = st.selectbox(
        "Catégorie",
        options=available_cats,
        format_func=lambda x: CAT_LABELS.get(x, x),
    )

    df = records[
        (records["gender"]   == gender_choice) &
        (records["indoor"]   == indoor_choice) &
        (records["club_cat"] == cat_choice)
    ].copy()

    if df.empty:
        st.info("Aucun record trouvé pour cette sélection.")
    else:
        df["_sort"] = df["event"].apply(event_sort_key)
        df = df.sort_values("_sort")
        df["Année"] = df["season"].apply(format_season)
        df["Date"]  = df["date"].apply(format_date)

        display = df[[
            "event", "resultat", _ACOL, "Année", "lieu"
        ]].rename(columns={
            "event":    "Épreuve",
            "resultat": "Performance",
            _ACOL:  "Athlète",
            "lieu":     "Lieu",
        }).reset_index(drop=True)

        st.dataframe(display, use_container_width=True, hide_index=True)
        st.caption(f"{len(display)} records")

# =============================================================================
# VIEW: TOP 10
# =============================================================================

elif view == "Top 10":
    st.header("Top 10 par catégorie")

    base = master[
        (master["gender"] == gender_choice) &
        (master["indoor"] == indoor_choice)
    ]

    events_available = sort_events(base["event"].dropna().unique())
    if not events_available:
        st.info("Aucune épreuve disponible.")
        st.stop()

    event_choice = st.selectbox("Épreuve", options=events_available)

    # "TOUS" = no category filter (best 10 athletes across all categories)
    cats_in_data = [c for c in CAT_ORDER if c != "ALL" and c in master["club_cat"].unique()]
    cat_choice = st.selectbox(
        "Catégorie",
        options=["TOUS"] + cats_in_data,
        format_func=lambda x: CAT_LABELS.get(x, x),
    )

    df = base[
        (base["event"] == event_choice) &
        (base["mark"].notna())
    ].copy()

    if cat_choice != "TOUS":
        df = df[df["club_cat"] == cat_choice]

    if df.empty:
        st.info("Aucun résultat trouvé pour cette sélection.")
    else:
        df = best_per_athlete(df, 10)
        df.index += 1

        df["Année"] = df["season"].apply(format_season)
        df["Date"]  = df["date"].apply(format_date)

        display = df[[
            "resultat", _ACOL, "club_cat", "Année", "lieu"
        ]].rename(columns={
            "resultat": "Performance",
            _ACOL:      "Athlète",
            "club_cat": "Catégorie",
            "lieu":     "Lieu",
        })
        display.index.name = "Rang"

        st.dataframe(display, use_container_width=True)
        st.caption(f"{len(display)} athlètes")

# =============================================================================
# VIEW: TOP 50
# =============================================================================

elif view == "Top 50":
    st.header("Top 50 toutes catégories")

    base = master[
        (master["gender"] == gender_choice) &
        (master["indoor"] == indoor_choice)
    ]

    events_available = sort_events(base["event"].dropna().unique())
    if not events_available:
        st.info("Aucune épreuve disponible.")
        st.stop()

    event_choice = st.selectbox("Épreuve", options=events_available)

    df = base[
        (base["event"] == event_choice) &
        (base["mark"].notna())
    ].copy()

    if df.empty:
        st.info("Aucun résultat trouvé pour cette sélection.")
    else:
        df = best_per_athlete(df, 50)
        df.index += 1

        df["Année"] = df["season"].apply(format_season)
        df["Date"]  = df["date"].apply(format_date)
        df["Né(e)"] = df["yob"].apply(
            lambda x: str(int(x)) if pd.notna(x) else "—"
        )

        display = df[[
            "resultat", _ACOL, "Né(e)", "club_cat", "Année", "lieu"
        ]].rename(columns={
            "resultat": "Performance",
            _ACOL:      "Athlète",
            "club_cat": "Catégorie",
            "lieu":     "Lieu",
        })
        display.index.name = "Rang"

        st.dataframe(display, use_container_width=True)
        st.caption(f"{len(display)} athlètes")

# =============================================================================
# FOOTER
# =============================================================================

st.divider()
st.caption("Lausanne-Sports Athlétisme · Données Swiss Athletics (alabus) · 2006–2025")
