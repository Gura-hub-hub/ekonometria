# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

Econometrics course project (LS 2025/2026) analysing **Concrete Compressive Strength** (UCI dataset, n=1030). The main deliverable is a LaTeX report (`hypotezy.txt`) with OLS regression, hypothesis tests, prediction intervals, and heteroskedasticity tests. Analysis is done in R (Jupyter notebook with R kernel or standalone `.R` scripts).

## Running the analysis

```r
# Load the dataset (use the clean CSV, not the raw XLS)
df <- read.csv("Data/concrete_data.csv")
# Columns: cement, slag, fly_ash, water, superplasticizer, coarse_agg, fine_agg, age, strength
```

Run the KS normality test across all 15 UCI datasets:
```bash
python ks_normalita_datasets.py
```

## Repository structure

| Path | Contents |
|------|----------|
| `Data/concrete_data.csv` | Clean dataset for R (rename of Concrete_Data.xls with short column names) |
| `hypotezy.txt` | LaTeX source of the full project report |
| `Ekonometria.ipynb` | Jupyter notebook with R kernel — main analysis workspace |
| `Recources/SomogyRko/` | Lecture R scripts (02–09) and accompanying datasets |
| `Recources/Prezentacie/` | Lecture PDFs (EM_01–EM_06) |
| `zadanie/info_projekt_2026.pdf` | Official project assignment |
| `r_style_memory.md` | R coding conventions used throughout the course |

## Regression model

The project fits:

```
strength ~ cement + slag + fly_ash + water + superplasticizer + coarse_agg + fine_agg + log(age)
```

`age` is log-transformed (KS test confirms strongly non-normal distribution, p ≈ 0).

## R coding conventions

Follow the patterns in `r_style_memory.md`. Key points:
- Always build `X` matrix with an explicit intercept column: `X <- cbind(1, x1, x2, ...)`
- Use `k <- ncol(X)` for number of parameters (including intercept)
- Variable naming: `betaHAT`, `epsilonHAT`, `Y_hat`, `s2`, `RSS`, `ESS`, `TSS`
- Manual OLS: `solve(t(X) %*% X) %*% t(X) %*% Y`
- Prefer `anova(subMODEL, model)` for F-tests comparing nested models
- Comments and variable names in Slovak or English; section separators with `###...###`

## Git workflow

`.claude/` and `*.zip` are gitignored. Use `/cmt` for a quick commit+push, `/commit` for a guided commit, `/pull` to sync.
