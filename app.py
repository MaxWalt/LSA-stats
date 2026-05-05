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
# EVENT DISPLAY CONFIG
# Maps raw event key → (display label, group, sort key)
# Groups/sort: Course < Haies < Sauts < Lancers < Route < Combiné
# =============================================================================

EVENT_CONFIG = {
    # --- Course (flat, by distance) ---
    "50":          ("50m",              "Course",   10),
    "60":          ("60m",              "Course",   20),
    "80":          ("80m",              "Course",   30),
    "100":         ("100m",             "Course",   40),
    "150":         ("150m",             "Course",   50),
    "200":         ("200m",             "Course",   60),
    "300":         ("300m",             "Course",   70),
    "400":         ("400m",             "Course",   80),
    "600":         ("600m",             "Course",   90),
    "800":         ("800m",             "Course",  100),
    "1000":        ("1000m",            "Course",  110),
    "1500":        ("1500m",            "Course",  120),
    "1609":        ("1 mile",           "Course",  130),
    "2000":        ("2000m",            "Course",  140),
    "3000":        ("3000m",            "Course",  150),
    "5000":        ("5000m",            "Course",  160),
    "10000":       ("10000m",           "Course",  170),
    # --- Haies ---
    "50H":         ("50m haies",        "Haies",   210),
    "60H":         ("60m haies",        "Haies",   220),
    "80H":         ("80m haies",        "Haies",   230),
    "100H":        ("100m haies",       "Haies",   240),
    "110H":        ("110m haies",       "Haies",   250),
    "300H":        ("300m haies",       "Haies",   260),
    "400H":        ("400m haies",       "Haies",   270),
    "1500SC":      ("1500m steeple",    "Haies",   280),
    "2000SC":      ("2000m steeple",    "Haies",   290),
    "3000SC":      ("3000m steeple",    "Haies",   300),
    # --- Sauts ---
    "long":        ("Longueur",         "Sauts",   410),
    "triple":      ("Triple saut",      "Sauts",   420),
    "high":        ("Hauteur",          "Sauts",   430),
    "pole":        ("Perche",           "Sauts",   440),
    # --- Lancers ---
    "shot":        ("Poids",            "Lancers", 510),
    "javelin":     ("Javelot",          "Lancers", 520),
    "disc":        ("Disque",           "Lancers", 530),
    "hammer":      ("Marteau",          "Lancers", 540),
    "weight":      ("Poids (salle)",    "Lancers", 550),
    # --- Route ---
    "5km":         ("5km route",        "Route",   610),
    "10km":        ("10km route",       "Route",   620),
    "15km":        ("15km route",       "Route",   630),
    "half":        ("Semi-marathon",    "Route",   640),
    "marathon":    ("Marathon",         "Route",   650),
    # --- Combiné ---
    "pentathlon":  ("Pentathlon",       "Combiné", 710),
    "heptathlon":  ("Heptathlon",       "Combiné", 720),
    "decathlon":   ("Décathlon",        "Combiné", 730),
}

def event_label(event_name):
    cfg = EVENT_CONFIG.get(str(event_name).strip())
    return cfg[0] if cfg else str(event_name)

def event_sort_key(event_name):
    cfg = EVENT_CONFIG.get(str(event_name).strip())
    return cfg[2] if cfg else 999

def sort_events(events):
    return sorted(events, key=event_sort_key)

# =============================================================================
# LOAD DATA
# =============================================================================

@st.cache_data
def load_records():
    df = pd.read_excel("data/ls_master/ls_records_2025.xlsx")
    df["indoor"] = df["indoor"].astype(bool)
    df["season"] = pd.to_numeric(df["season"], errors="coerce")
    df["yob"]    = pd.to_numeric(df["yob"],    errors="coerce")
    df["mark"]   = pd.to_numeric(df["mark"],   errors="coerce")
    return df

@st.cache_data
def load_master():
    df = pd.read_excel("data/ls_master/ls_master_2025.xlsx")
    df["indoor"] = df["indoor"].astype(bool)
    df["season"] = pd.to_numeric(df["season"], errors="coerce")
    df["yob"]    = pd.to_numeric(df["yob"],    errors="coerce")
    df["mark"]   = pd.to_numeric(df["mark"],   errors="coerce")
    return df

records = load_records()
master  = load_master()

# Fallback: use "athlete" if "athlete_display" column is absent
_ACOL     = "athlete_display" if "athlete_display" in master.columns  else "athlete"
_ACOL_REC = "athlete_display" if "athlete_display" in records.columns else "athlete"

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

        df["Épreuve"] = df["event"].apply(event_label)
        df["Année"]   = df["season"].apply(format_season)
        df["Date"]    = df["date"].apply(format_date)

        display = df[[
            "Épreuve", "resultat", _ACOL_REC, "Année", "lieu"
        ]].rename(columns={
            "resultat": "Performance",
            _ACOL_REC:  "Athlète",
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

    event_choice = st.selectbox(
        "Épreuve",
        options=events_available,
        format_func=event_label,
    )

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

    event_choice = st.selectbox(
        "Épreuve",
        options=events_available,
        format_func=event_label,
    )

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
