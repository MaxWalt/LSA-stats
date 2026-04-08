import streamlit as st
import pandas as pd

# =============================================================================
# CONFIG
# =============================================================================

st.set_page_config(
    page_title="LSA — Statistiques du club",
    page_icon="🏃",
    layout="wide"
)

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

# =============================================================================
# LABELS (French)
# =============================================================================

GENDER_LABELS  = {"female": "Femmes", "male": "Hommes"}
INDOOR_LABELS  = {True: "Salle", False: "Plein air"}
CAT_ORDER      = ["ALL", "U23", "U20", "U18", "U16", "U14", "U12", "U10"]
CAT_LABELS     = {
    "ALL": "Tous (record du club)",
    "U23": "U23", "U20": "U20", "U18": "U18",
    "U16": "U16", "U14": "U14", "U12": "U12", "U10": "U10"
}

def format_date(d):
    try:
        return pd.to_datetime(d).strftime("%d.%m.%Y")
    except:
        return "—"

def format_season(s):
    try:
        return str(int(s))
    except:
        return "—"

# =============================================================================
# SIDEBAR FILTERS
# =============================================================================

st.sidebar.image(
    "https://upload.wikimedia.org/wikipedia/fr/thumb/8/85/Lausanne-Sports_logo.svg/200px-Lausanne-Sports_logo.svg.png",
    width=120
)
st.sidebar.title("Filtres")

# Gender
gender_choice = st.sidebar.radio(
    "Genre",
    options=["female", "male"],
    format_func=lambda x: GENDER_LABELS[x]
)

# Indoor / outdoor
indoor_choice = st.sidebar.radio(
    "Piste",
    options=[False, True],
    format_func=lambda x: INDOOR_LABELS[x]
)

# View
view = st.sidebar.radio(
    "Vue",
    options=["Records", "Top 10", "Top 50", "Saison"]
)

# =============================================================================
# MAIN CONTENT
# =============================================================================

st.title("Lausanne-Sports Athlétisme")
st.caption(
    f"{'Femmes' if gender_choice == 'female' else 'Hommes'} · "
    f"{'Salle' if indoor_choice else 'Plein air'}"
)

# -----------------------------------------------------------------------------
# VIEW: RECORDS
# -----------------------------------------------------------------------------

if view == "Records":
    st.header("Records du club")

    # Category filter
    available_cats = [c for c in CAT_ORDER if c in records["club_cat"].unique()]
    cat_choice = st.selectbox(
        "Catégorie",
        options=available_cats,
        format_func=lambda x: CAT_LABELS.get(x, x)
    )

    df = records[
        (records["gender"] == gender_choice) &
        (records["indoor"] == indoor_choice) &
        (records["club_cat"] == cat_choice)
    ].copy()

    if df.empty:
        st.info("Aucun record trouvé pour cette sélection.")
    else:
        df = df.sort_values("event")
        df["Année"] = df["season"].apply(format_season)
        df["Date"]  = df["date"].apply(format_date)

        display = df[[
            "event", "resultat", "athlete_display", "Année", "lieu"
        ]].rename(columns={
            "event":           "Épreuve",
            "resultat":        "Performance",
            "athlete_display": "Athlète",
            "lieu":            "Lieu"
        }).reset_index(drop=True)

        st.dataframe(display, use_container_width=True, hide_index=True)
        st.caption(f"{len(display)} records")

# -----------------------------------------------------------------------------
# VIEW: TOP 10
# -----------------------------------------------------------------------------

elif view == "Top 10":
    st.header("Top 10 par catégorie")

    # Event filter
    events_available = sorted(master[
        (master["gender"] == gender_choice) &
        (master["indoor"] == indoor_choice)
    ]["event"].dropna().unique())

    if not events_available:
        st.info("Aucune épreuve disponible.")
        st.stop()

    event_choice = st.selectbox("Épreuve", options=events_available)

    # Category filter (exclude ALL)
    cats_available = [c for c in CAT_ORDER if c != "ALL" and c in master["club_cat"].unique()]
    cat_choice = st.selectbox(
        "Catégorie",
        options=cats_available,
        format_func=lambda x: CAT_LABELS.get(x, x)
    )

    df = master[
        (master["gender"]   == gender_choice) &
        (master["indoor"]   == indoor_choice) &
        (master["event"]    == event_choice) &
        (master["club_cat"] == cat_choice) &
        (master["mark"].notna())
    ].copy()

    if df.empty:
        st.info("Aucun résultat trouvé pour cette sélection.")
    else:
        # One best performance per athlete
        better_is_lower = df["event"].iloc[0] if "better_is_lower" in df.columns else None
        if "better_is_lower" in df.columns:
            asc = bool(df["better_is_lower"].iloc[0])
        else:
            asc = True  # default: lower is better (times)

        df = (
            df.sort_values("mark", ascending=asc)
              .drop_duplicates(subset=["athlete"])
              .head(10)
              .reset_index(drop=True)
        )
        df.index += 1  # rank starts at 1

        df["Année"] = df["season"].apply(format_season)
        df["Date"]  = df["date"].apply(format_date)

        display = df[[
            "resultat", "athlete_display", "Année", "lieu"
        ]].rename(columns={
            "resultat":        "Performance",
            "athlete_display": "Athlète",
            "lieu":            "Lieu"
        })
        display.index.name = "Rang"

        st.dataframe(display, use_container_width=True)
        st.caption(f"{len(display)} athlètes")

# -----------------------------------------------------------------------------
# VIEW: TOP 50
# -----------------------------------------------------------------------------

elif view == "Top 50":
    st.header("Top 50 toutes catégories")

    events_available = sorted(master[
        (master["gender"] == gender_choice) &
        (master["indoor"] == indoor_choice)
    ]["event"].dropna().unique())

    if not events_available:
        st.info("Aucune épreuve disponible.")
        st.stop()

    event_choice = st.selectbox("Épreuve", options=events_available)

    df = master[
        (master["gender"] == gender_choice) &
        (master["indoor"] == indoor_choice) &
        (master["event"]  == event_choice) &
        (master["mark"].notna())
    ].copy()

    if df.empty:
        st.info("Aucun résultat trouvé pour cette sélection.")
    else:
        if "better_is_lower" in df.columns:
            asc = bool(df["better_is_lower"].iloc[0])
        else:
            asc = True

        df = (
            df.sort_values("mark", ascending=asc)
              .drop_duplicates(subset=["athlete"])
              .head(50)
              .reset_index(drop=True)
        )
        df.index += 1

        df["Année"] = df["season"].apply(format_season)
        df["Date"]  = df["date"].apply(format_date)

        display = df[[
            "resultat", "athlete_display", "yob", "Année", "lieu"
        ]].rename(columns={
            "resultat":        "Performance",
            "athlete_display": "Athlète",
            "yob":             "Né(e)",
            "lieu":            "Lieu"
        })
        display["Né(e)"] = display["Né(e)"].apply(
            lambda x: str(int(x)) if pd.notna(x) else "—"
        )
        display.index.name = "Rang"

        st.dataframe(display, use_container_width=True)
        st.caption(f"{len(display)} athlètes")

# -----------------------------------------------------------------------------
# VIEW: SAISON
# -----------------------------------------------------------------------------

elif view == "Saison":
    st.header("Performances par saison")

    # Season filter
    seasons_available = sorted(
        master["season"].dropna().unique().astype(int),
        reverse=True
    )

    if not seasons_available:
        st.info("Aucune saison disponible.")
        st.stop()

    col1, col2 = st.columns(2)
    with col1:
        season_choice = st.selectbox(
            "Saison",
            options=["Toutes"] + [str(s) for s in seasons_available]
        )
    with col2:
        events_available = sorted(master[
            (master["gender"] == gender_choice) &
            (master["indoor"] == indoor_choice)
        ]["event"].dropna().unique())
        event_choice = st.selectbox(
            "Épreuve",
            options=["Toutes"] + list(events_available)
        )

    df = master[
        (master["gender"] == gender_choice) &
        (master["indoor"] == indoor_choice) &
        (master["mark"].notna())
    ].copy()

    if season_choice != "Toutes":
        df = df[df["season"] == int(season_choice)]
    if event_choice != "Toutes":
        df = df[df["event"] == event_choice]

    if df.empty:
        st.info("Aucun résultat trouvé pour cette sélection.")
    else:
        if "better_is_lower" in df.columns:
            asc = bool(df["better_is_lower"].iloc[0]) if event_choice != "Toutes" else True
        else:
            asc = True

        df = df.sort_values(["event", "mark"], ascending=[True, asc])

        df["Année"] = df["season"].apply(format_season)
        df["Date"]  = df["date"].apply(format_date)

        display = df[[
            "event", "resultat", "athlete_display", "club_cat", "Année", "lieu"
        ]].rename(columns={
            "event":           "Épreuve",
            "resultat":        "Performance",
            "athlete_display": "Athlète",
            "club_cat":        "Catégorie",
            "lieu":            "Lieu"
        }).reset_index(drop=True)

        st.dataframe(display, use_container_width=True, hide_index=True)
        st.caption(f"{len(display)} performances")

# =============================================================================
# FOOTER
# =============================================================================

st.divider()
st.caption("Lausanne-Sports Athlétisme · Données Swiss Athletics (alabus) · 2006–2025")
