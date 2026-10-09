# Code documentation

## Data source
All variables were generated from the raw data files located in the `data/raw` directory. This data is available in Stata `.dta` format and includes individual and household response files for multiple waves. The data is available at UK Data Service by reasonable request.

## Data merging procedure
The data merging process starts by sourcing the `Stata` code from the `data/raw/code` directory. This code performs the following steps:
- Rename variables in individual and household response files to include wave numbers instead of letters.
- Drop BHPS cohort identifiers.
- Save the cleaned files back to the `data/raw` directory.

Variables of interest for this study

- `nchild_dv`: Number of own children in household from
- `gor_dv`: Government Office Region
- `age_dv`: Age (derived from date of birth and interview date)
- `sex_dv`: Sex
- `hiqual_dv`: Highest qualification ever reported
- `mastat_dv`: Marital status
- `tenure_dv`: Housing tenure
- `finnow`: Subjective financial situation - current -> Used to create financial distress variable
- `sf12mcs_dv`: SF-12 mental component summary
- `sf12pcs_dv`: SF-12 physical component summary

### Special variables

> [!NOTE]
> **Gross personal non-benefit income**
> Constructed to mirror UKMOD market income. Composed of:
> - **`fimnlabgrs_dv`** — total monthly gross labour income
> - **`fimnpen_dv`** — net pension income (employer pensions only: `ficode` 2–3)
> - **`inc_pp`** — private pension/annuity (`ficode` 4)
> - **`inc_tu`** — Trade Union/Friendly Society payment (`ficode` 25)
> - **`inc_ma`** — maintenance or alimony (`ficode` 26)
> - **`inc_fm`** — payments from a family member not living in household (`ficode` 27)
> - **`inc_oth`** — any other regular payment (`ficode` 38; not available in Wave 1)
>
> **Exclusions:** State/NI retirement pension (`ficode` 1) and educational grants (`ficode` 24) are excluded as they are transfer income, not market income.
>
> All components are sourced from `frmnthimp_dv` in the income file and collapsed to person-wave level in `01_merge.do`.
>
> Gross individual income is created for individuals and partners. These are the sources for household income variable, which is the sum of individual income and partner's income if coupled. If single, household income = individual income.

> [!NOTE]
> **Economic benefits**
> Derived from `fihhmnsben_dv`. A binary variable indicating whether a person receives universal credit.

> [!NOTE]
> **Financial Distress**
> Derived from `finnow` — a 5-point subjective measure of current financial situation. The binary variable flags respondents reporting financial difficulty, corresponding to:
> - `4` — Finding it quite difficult
> - `5` — Finding it very difficult
>
> The full scale ranges from 1 (Living comfortably) to 5 (Finding it very difficult).

