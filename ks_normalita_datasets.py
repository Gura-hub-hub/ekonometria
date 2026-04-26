import sys
try:
    sys.stdout.reconfigure(encoding="utf-8")
except AttributeError:
    pass

import io
import os
import zipfile
import requests
import pandas as pd
import numpy as np
from scipy import stats

ALPHA = 0.05


def ks_report(df, nazov):
    numericke = df.select_dtypes(include=[np.number]).columns
    normalny, nenormalny = [], []
    for col in numericke:
        data = df[col].replace([np.inf, -np.inf], np.nan).dropna()
        if len(data) < 8 or data.std() == 0:
            continue
        z = (data - data.mean()) / data.std(ddof=1)
        stat, p = stats.kstest(z, "norm")
        (normalny if p >= ALPHA else nenormalny).append((col, stat, p))

    print(f"\n{'='*62}")
    print(f"  {nazov}")
    print(f"  Pocet riadkov: {len(df)} | Numericke stlpce: {len(numericke)}")
    print(f"{'='*62}")

    print(f"\n  NORMALNE rozdelenie (p >= {ALPHA}):")
    if normalny:
        print(f"  {'Stlpec':<28} {'KS stat':>9} {'p-hodnota':>11}")
        print(f"  {'-'*50}")
        for n, s, p in normalny:
            print(f"  {str(n):<28} {s:>9.4f} {p:>11.4f}")
    else:
        print("  --> Ziadny stlpec nie je normalne rozdeleny.")

    print(f"\n  NENORMALNE rozdelenie (p < {ALPHA}):")
    if nenormalny:
        print(f"  {'Stlpec':<28} {'KS stat':>9} {'p-hodnota':>11}")
        print(f"  {'-'*50}")
        for n, s, p in nenormalny:
            print(f"  {str(n):<28} {s:>9.4f} {p:>11.4f}")
    else:
        print("  --> Vsetky stlpce su normalne rozdelene.")

    return normalny, nenormalny


def get(url, timeout=30):
    r = requests.get(url, timeout=timeout)
    r.raise_for_status()
    return r


# ---------------------------------------------------------------------------
# 1. Wine Quality (cervene vino)
# ---------------------------------------------------------------------------
def ds1():
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
    df = pd.read_csv(io.StringIO(get(url).text), sep=";")
    ks_report(df, "1. Wine Quality (cervene vino) | N=1599")


# ---------------------------------------------------------------------------
# 2. Boston Housing
# ---------------------------------------------------------------------------
def ds2():
    url = "https://raw.githubusercontent.com/selva86/datasets/master/BostonHousing.csv"
    df = pd.read_csv(io.StringIO(get(url).text))
    ks_report(df, "2. Boston Housing | N=506")


# ---------------------------------------------------------------------------
# 3. Auto MPG
# ---------------------------------------------------------------------------
def ds3():
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"
    cols = ["mpg","cylinders","displacement","horsepower","weight",
            "acceleration","model_year","origin","car_name"]
    df = pd.read_csv(io.StringIO(get(url).text), sep=r"\s+",
                     names=cols, na_values="?")
    df = df.drop(columns=["car_name"])
    ks_report(df, "3. Auto MPG | N=398")


# ---------------------------------------------------------------------------
# 4. Student Performance (matematika)
# ---------------------------------------------------------------------------
def ds4():
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip"
    r = get(url)
    with zipfile.ZipFile(io.BytesIO(r.content)) as z:
        with z.open("student-mat.csv") as f:
            df = pd.read_csv(f, sep=";")
    ks_report(df, "4. Student Performance (matematika) | N=395")


# ---------------------------------------------------------------------------
# 5. Concrete Compressive Strength
# ---------------------------------------------------------------------------
def ds5():
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/concrete/compressive/Concrete_Data.xls"
    r = get(url)
    df = pd.read_excel(io.BytesIO(r.content))
    ks_report(df, "5. Concrete Compressive Strength | N=1030")


# ---------------------------------------------------------------------------
# 6. Forest Fires
# ---------------------------------------------------------------------------
def ds6():
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"
    df = pd.read_csv(io.StringIO(get(url).text))
    df["log_area"] = np.log1p(df["area"])
    ks_report(df, "6. Forest Fires | N=517")


# ---------------------------------------------------------------------------
# 7. Real Estate Valuation (Taiwan)
# ---------------------------------------------------------------------------
def ds7():
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00477/Real%20estate%20valuation%20data%20set.xlsx"
    r = get(url)
    df = pd.read_excel(io.BytesIO(r.content))
    ks_report(df, "7. Real Estate Valuation (Taiwan) | N=414")


# ---------------------------------------------------------------------------
# 8. Bike Sharing (denne data)
# ---------------------------------------------------------------------------
def ds8():
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip"
    r = get(url)
    with zipfile.ZipFile(io.BytesIO(r.content)) as z:
        with z.open("day.csv") as f:
            df = pd.read_csv(f)
    ks_report(df, "8. Bike Sharing (denne) | N=731")


# ---------------------------------------------------------------------------
# 9. Fish Market
# ---------------------------------------------------------------------------
def ds9():
    url = "https://raw.githubusercontent.com/dsrscientist/dataset1/master/Fish.csv"
    df = pd.read_csv(io.StringIO(get(url).text))
    ks_report(df, "9. Fish Market | N=159")


# ---------------------------------------------------------------------------
# 10. Diabetes (Pima Indians)
# ---------------------------------------------------------------------------
def ds10():
    url = "https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.data.csv"
    cols = ["pregnancies","glucose","blood_pressure","skin_thickness",
            "insulin","bmi","diabetes_pedigree","age","outcome"]
    df = pd.read_csv(io.StringIO(get(url).text), names=cols)
    # nulove hodnoty su chybajuce hodnoty pre niekt. stlpce
    for c in ["glucose","blood_pressure","skin_thickness","insulin","bmi"]:
        df[c] = df[c].replace(0, np.nan)
    ks_report(df, "10. Diabetes (Pima Indians) | N=768")


# ---------------------------------------------------------------------------
# 11. Air Quality (UCI)
# ---------------------------------------------------------------------------
def ds11():
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00360/AirQualityUCI.zip"
    r = get(url, timeout=60)
    with zipfile.ZipFile(io.BytesIO(r.content)) as z:
        fname = [n for n in z.namelist() if n.endswith(".csv") or n.endswith(".xlsx")][0]
        with z.open(fname) as f:
            if fname.endswith(".xlsx"):
                df = pd.read_excel(f)
            else:
                df = pd.read_csv(f, sep=";", decimal=",", na_values=-200)
    df = df.dropna(axis=1, how="all").dropna(how="all")
    ks_report(df, "11. Air Quality (UCI) | N~9358")


# ---------------------------------------------------------------------------
# 12. Beijing PM2.5
# ---------------------------------------------------------------------------
def ds12():
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00381/PRSA_data_2010.1.1-2014.12.31.csv"
    df = pd.read_csv(io.StringIO(get(url, timeout=60).text))
    df = df.replace("cv", np.nan)
    ks_report(df, "12. Beijing PM2.5 | N~43800")


# ---------------------------------------------------------------------------
# 13. Energy Appliances Consumption
# ---------------------------------------------------------------------------
def ds13():
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00374/energydata_complete.csv"
    df = pd.read_csv(io.StringIO(get(url, timeout=60).text))
    ks_report(df, "13. Energy Appliances Consumption | N~19735")


# ---------------------------------------------------------------------------
# 14. Daily Demand Forecasting Orders
# ---------------------------------------------------------------------------
def ds14():
    url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00409/Daily_Demand_Forecasting_Orders.csv"
    df = pd.read_csv(io.StringIO(get(url).text), sep=";")
    ks_report(df, "14. Daily Demand Forecasting Orders | N=60")


# ---------------------------------------------------------------------------
# 15. FRED makroekonomicke data USA
# ---------------------------------------------------------------------------
def ds15():
    series = {
        "GDP":    "https://fred.stlouisfed.org/graph/fredgraph.csv?id=GDP",
        "UNRATE": "https://fred.stlouisfed.org/graph/fredgraph.csv?id=UNRATE",
        "CPIAUCSL": "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCSL",
        "FEDFUNDS": "https://fred.stlouisfed.org/graph/fredgraph.csv?id=FEDFUNDS",
    }
    frames = []
    for name, url in series.items():
        tmp = pd.read_csv(io.StringIO(get(url).text))
        tmp.columns = ["DATE", name]
        tmp[name] = pd.to_numeric(tmp[name], errors="coerce")
        tmp = tmp.set_index("DATE")
        frames.append(tmp)
    df = pd.concat(frames, axis=1).dropna(how="all")
    ks_report(df, "15. FRED makroekonomika USA | GDP, UNRATE, CPI, FEDFUNDS")


# ---------------------------------------------------------------------------
DATASETY = [
    ("1.  Wine Quality",             ds1),
    ("2.  Boston Housing",           ds2),
    ("3.  Auto MPG",                 ds3),
    ("4.  Student Performance",      ds4),
    ("5.  Concrete Strength",        ds5),
    ("6.  Forest Fires",             ds6),
    ("7.  Real Estate Taiwan",       ds7),
    ("8.  Bike Sharing",             ds8),
    ("9.  Fish Market",              ds9),
    ("10. Diabetes Pima Indians",    ds10),
    ("11. Air Quality UCI",          ds11),
    ("12. Beijing PM2.5",            ds12),
    ("13. Energy Appliances",        ds13),
    ("14. Daily Demand Orders",      ds14),
    ("15. FRED makrodata USA",       ds15),
]

if __name__ == "__main__":
    print("Kolmogorov-Smirnovov test normality — 15 datasetov")
    print(f"Hladina vyznamnosti: alfa = {ALPHA}\n")

    zhrnutie = {}
    for nazov, fn in DATASETY:
        print(f"\n>>> Spracuvam: {nazov} ...")
        try:
            fn()
        except Exception as e:
            print(f"  [CHYBA] {e}")
            zhrnutie[nazov] = "CHYBA"
            continue

    print("\n\n" + "="*62)
    print("  HOTOVO")
    print("="*62)
