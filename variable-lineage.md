# Variable lineage

One entry per variable that reaches the paper, whether through a table, a
figure, a summary statistic, an estimate or a calibration. Each entry gives the
raw file and field the variable starts from, each transformation in order with
the script that does it, its level and units in final form, and the code files
it appears in. Scripts are named without line numbers; functions or SAS steps
are named where the work happens inside one. Variables built by one rule from a
set of inputs (the timing windows, the 31 comorbidities) are documented as a
family.

The pipeline has two stages. The SAS build runs inside the CMS VRDC
(`code/data-build/sas/`), reads the Medicare RIF, and exports one
cardiologist-year file plus a hospital-year cath-rate file. The R build
(`code/data-build/`, with crosswalks in `code/data-build/crosswalks/`, driven by
`_data-build.R`) turns that export plus the crosswalks into
`data/output/analysis_panel.csv`. The analysis scripts (`code/analysis/*.R`,
driven by `_analysis.R`) read the panel and produce `results/`.

Two structural facts recur below. First, `train_cath_lab` and its AHA cousins
are never materialized to `data/output/`; each analysis script that needs them
rebuilds them inline from `data/input/aha_hospital.csv` with the same recipe, so
ten scripts carry their own copy. Second, two matriculation conventions coexist:
the medical-school imprint uses `grad_year - 3` (start of medical school), while
the residency and fellowship exposures in `4_training_exposure.R` use `grad_year`
and `grad_year + 3` (start of residency and fellowship).

Raw inputs referred to below:

| File | Contents |
|---|---|
| Medicare RIF (VRDC-internal): `INPATIENT_CLAIMS_*`, `BCARRIER_LINE_*`, `MBSF_ABCD_*` | inpatient claims, carrier lines, beneficiary summary, 2008-2018; never leave the VRDC |
| MDPPAS (`data/input/mdppas/`) | physician practice ZIP (`phy_zip_perf1`) and primary specialty (`spec_prim_1_name`), by NPI-year |
| Physician Compare (`data/input/physician-compare/`) | medical school name, graduation year, gender, by NPI |
| `data/input/aha_hospital.csv` | AHA Annual Survey (symlink to `aha-data` 1980-2024); `CCLABHOS`, `OHSRGHOS`, `CICHOS`, teaching flags, `HRRCODE`, `ID`, `SYSID`, `MCRNUM`, by hospital-year |
| `data/input/hospital_year_cath.csv` | hospital-year NSTEMI cath-within-2-days rate (`rate_cath_d2`), written by `7_hospital_cath_rate.sas` |
| `data/input/med-school-nih.csv`, `cardio-school-to-nih.csv` | NIH ExPORTER funding rank by medical school and fiscal year, plus the school-name crosswalk |
| `data/input/med-school-xw.csv`, `LCME accreditation.xlsx` | hand-curated medical-school-to-ZIP inputs (Shirley Cai) |
| Dartmouth `ZipHsaHrr15.xls`, `HRR_Bdry.SHP` | ZIP-to-HRR crosswalk and HRR boundaries |
| Doximity `doximity_profiles.csv` (`D:/research-data/doximity/`) | residency and fellowship program strings and years, by physician |

---

## 1. Identifiers, sample membership, weights

### `BENE_ID` (patient key, VRDC-internal)
- Paper: none directly; the episode key behind the residualized outcome.
- Raw: `BENE_ID` on `INPATIENT_CLAIMS_*`, `BCARRIER_LINE_*`, `MBSF_ABCD_*`.
- Chain: `1_nstemi_episodes.sas` keeps the first NSTEMI admission per `BENE_ID` (`FIRST.BENE_ID` after sorting by `BENE_ID CLM_ADMSN_DT`). Joined through `2_cath_procedures.sas`, `3_beneficiary.sas`, `4_carrier_cardiologist.sas`, `5_residualize.sas`; collapsed away in `6_aggregate_export.sas`.
- Level: patient (episode); never leaves the VRDC, in no exported CSV.
- Files: `1_nstemi_episodes.sas`, `2_cath_procedures.sas`, `3_beneficiary.sas`, `4_carrier_cardiologist.sas`, `5_residualize.sas`, `6_aggregate_export.sas`, `7_hospital_cath_rate.sas`.

### `npi` (cardiologist key, panel identifier)
- Paper: the unit behind every estimate; the panel is a cardiologist-year file.
- Raw: `PRF_PHYSN_NPI` (carrier line, performing physician) and `NPI` (MDPPAS).
- Chain: `4_carrier_cardiologist.sas` restricts carrier `PRF_PHYSN_NPI` to the MDPPAS cardiologist list and resolves one per episode as `NPI_Gatekeeper`. `6_aggregate_export.sas` sets `NPI = NPI_Gatekeeper` as the panel key. `2_intensity_measures.R` renames it `npi` at the boundary; it is the primary key of `physician_panel.csv`, `analysis_panel.csv`, and every crosswalk output. Read as character throughout (leading-zero safe).
- Level: cardiologist.
- Files: `4_carrier_cardiologist.sas`, `6_aggregate_export.sas`; all R data-build; all 13 analysis scripts.

### `year`
- Paper: every by-year figure and the year fixed effects.
- Raw: `YEAR(CLM_ADMSN_DT)` (inpatient admission date); MDPPAS `Year`.
- Chain: `1_nstemi_episodes.sas` sets `AMI_Year = YEAR(CLM_ADMSN_DT)`, carried to `6_aggregate_export.sas` as `Year`; MDPPAS `Year` becomes `year` in `1_physicians.R`. Joined on `(npi, year)` in `2_intensity_measures.R`.
- Level: calendar year, 2008-2018 (`0_config.sas` `year_start`/`year_end`).
- Files: SAS 1-7; all R.

### `n_nstemi` (NSTEMI volume: cell-size filter and regression weight)
- Paper: the volume weight on every regression; the "NSTEMI volume per year" summary-stats row.
- Raw: derived; one row per NSTEMI episode assigned to a cardiologist.
- Chain: `6_aggregate_export.sas` computes `COUNT(*) AS N_NSTEMI` grouped by `(NPI_Gatekeeper, AMI_Year)`. The export keeps only `N_NSTEMI >= 11` (`min_patients`, `0_config.sas`), which is the sample-membership gate for the whole study. `2_intensity_measures.R` renames it `n_nstemi`; it enters as `weights = ~n_nstemi` in essentially every `feols` call and as the volume weight inside the leave-one-out intensity construction.
- Level: cardiologist-year; count of NSTEMI episodes.
- Files: `6_aggregate_export.sas`; `2_intensity_measures.R`; and every analysis script that estimates or summarizes (`1`, `2`, `3`, `4`, `5`, `6`, `8`, `9`, `11`, `12`, `13`).

### `NPI_Gatekeeper` (cardiologist assignment: Molitor consult > care > first-seen)
- Paper: defines which cardiologist owns each NSTEMI episode, hence the panel cell.
- Raw: carrier `PRF_PHYSN_NPI`, `HCPCS_CD`, `CLM_THRU_DT`; MDPPAS `SPEC_PRIM_1_NAME`, `NPI`.
- Chain (`4_carrier_cardiologist.sas`): build the cardiologist NPI list from MDPPAS specialties (Cardiology / Interventional / Clinical Cardiac Electrophysiology / Advanced Heart Failure), union across 2008-2018; keep carrier lines where the bene is an NSTEMI bene, the NPI is a cardiologist, and the claim is within 90 days; flag consult codes (99251-99255) and care codes (99221-99223); take the earliest claim in each tier; `NPI_Gatekeeper = COALESCE(first consult, first care, first seen)`. Consult codes were eliminated by CMS in January 2010, so from 2010 the hierarchy collapses to care > first-seen. `6_aggregate_export.sas` drops episodes with a blank gatekeeper and groups on it.
- Level: one cardiologist NPI per NSTEMI episode; becomes the `(npi, year)` cell.
- Files: `4_carrier_cardiologist.sas`, `6_aggregate_export.sas`.

---

## 2. The outcome and its residualization

### `mean_resid_cath` (risk-adjusted catheterization intensity, the dependent variable)
- Paper: the outcome in every regression, figure and summary statistic; SD ~0.16 in summary-stats.tex.
- Raw: inpatient `PRNCPAL_DGNS_CD` (NSTEMI = ICD-9 `41071` / ICD-10 `I214`), procedure codes `ICD_PRCDR_CD1-25` and dates `PRCDR_DT1-25`; MBSF `AGE_AT_END_REF_YR`, `BENE_RACE_CD`, `SEX_IDENT_CD`, `DUAL_ELGBL_MONS`; inpatient `ICD_DGNS_CD1-25` for the comorbidity flags.
- Chain: `1_nstemi_episodes.sas` takes the first NSTEMI admission and builds the 31 comorbidity flags. `2_cath_procedures.sas` finds the earliest invasive procedure within 90 days and builds `D_Cath_D2` (within two days). `3_beneficiary.sas` applies the FFS filter and recodes demographics, merging cath and comorbidities into `NSTEMI_Patients`. `5_residualize.sas` runs the patient-level LPM (`PROC GLM`) of `D_Cath_D2` on age, age-squared, the demographic indicators, `Dual_Elgbl_Mons`, the 31 Elixhauser flags and `AMI_Year` fixed effects, and outputs `Resid_Cath`. `6_aggregate_export.sas` takes `MEAN(Resid_Cath)` by `(NPI_Gatekeeper, AMI_Year)` for cells with at least 11 patients. `2_intensity_measures.R` renames it `mean_resid_cath`.
- Level: cardiologist-year; residual of a within-two-day catheterization probability, roughly centered on zero.
- Files: SAS `1`, `2`, `3`, `5`, `6`; `2_intensity_measures.R`; consumed as the LHS in `1`, `2`, `3`, `4`, `5`, `6`, `8`, `9`, `11`, `12`, `13`.

### `D_Cath_D2` (within-two-days catheterization indicator, the LPM outcome)
- Paper: the clinical margin behind the outcome; also the hospital-year rate feeding the recent-grad test.
- Raw: procedure codes and dates vs `AMI_Admsn_Dt`.
- Chain: built in `2_cath_procedures.sas`; the LHS of the residualization LPM (`5_residualize.sas`); aggregated to the hospital-year rate `rate_cath_d2 = MEAN(D_Cath_D2)` in `7_hospital_cath_rate.sas`.
- Level: patient 0/1 (SAS); hospital-year share (`7_hospital_cath_rate.sas`).
- Files: `2_cath_procedures.sas`, `3_beneficiary.sas`, `5_residualize.sas`, `6_aggregate_export.sas`, `7_hospital_cath_rate.sas`.

### Timing windows `D_Cath_D0/D1/D3/D7/D30/D90`, `Time_To_Cath` (robustness scaffolding)
- Paper: none; coded for robustness but not exported.
- Raw/chain: all built in the same `PROC SQL` in `2_cath_procedures.sas` from `Cath_Date - AMI_Admsn_Dt`; summed to counts in the unfiltered `Cardiologist_Year` panel (`6_aggregate_export.sas`) but not included in `Cardiologist_Year_Export`, so they never reach R.
- Level: patient 0/1 to cardiologist-year counts; VRDC-internal only.
- Files: `2_cath_procedures.sas`, `3_beneficiary.sas`, `5_residualize.sas`, `6_aggregate_export.sas`.

### The 31 Elixhauser comorbidities
- Paper: none directly; risk adjusters absorbed into `mean_resid_cath`.
- Raw: inpatient `ICD_DGNS_CD1-25`.
- Chain: `elix_lookup.sas` builds the ICD-version-by-code lookup to 31 categories (a code may map to several). `1_nstemi_episodes.sas` reshapes the 25 diagnosis codes long, joins on `(ICD_Code, ICD_Version)` with version chosen by admit date, and pivots to 31 0/1 columns. `3_beneficiary.sas` merges them; `5_residualize.sas` uses all 31 as LPM regressors.
- Level: patient 0/1; enter the outcome only through the residualization.
- Files: `elix_lookup.sas`, `1_nstemi_episodes.sas`, `3_beneficiary.sas`, `5_residualize.sas`, `6_aggregate_export.sas`.

### Patient demographics in the LPM (`Age`, `Age_Sq`, `D_Female`, `D_Black`, `D_Hisp`, `D_Asian`, `D_Race_Missing`, `D_Sex_Missing`, `D_Dual`, `Dual_Elgbl_Mons`)
- Paper: none directly; adjusters absorbed into `mean_resid_cath`.
- Raw: MBSF `AGE_AT_END_REF_YR`, `SEX_IDENT_CD`, `BENE_RACE_CD`, `DUAL_ELGBL_MONS`.
- Chain: recoded in `3_beneficiary.sas` (`D_Female = (SEX_IDENT_CD="2")`, `D_Black = (BENE_RACE_CD="2")`, `D_Dual = (DUAL_ELGBL_MONS>0)`); `Age_Sq = Age*Age` in `5_residualize.sas`; all enter the LPM.
- Level: patient; absorbed into the outcome.
- Files: `3_beneficiary.sas`, `5_residualize.sas`, `6_aggregate_export.sas`.

---

## 3. Training-environment regressors

### `train_cath_lab` (training-period cath-lab availability, the headline imprint regressor)
- Paper: the imprint coefficient throughout (0.058 within-origin in training-imprint.tex); summary-stats.tex "Training-HRR cath lab share"; the binscatter, subspecialty, rank horse-race, cohort-robustness and permutation results.
- Raw: `aha_hospital.csv` fields `CCLABHOS` (cath lab, service code "1"), `HRRCODE`, `year`; plus `grad_year` and `hrr_med_school`.
- Chain (canonical, `5_aha_training.R`): `has_cath_lab = as.integer(CCLABHOS == "1")`, then `cath_lab_share = mean(has_cath_lab)` by `(HRRCODE, year)`; infer matriculation `med_school_start = grad_year - 3`, clamp `aha_match_year = pmin(pmax(., 1980), 2003)`; join the HRR-year share onto each cardiologist by `(hrr_med_school, aha_match_year)`. Not materialized to disk; each consuming script rebuilds it with the identical recipe.
- Level: cardiologist (fixed given med-school HRR and cohort); share of hospitals in the medical-school HRR with a cath lab in the matriculation year, in [0,1].
- Files: built in `5_aha_training.R`; reconstructed inline in `1_descriptive.R`, `3_selection.R`, `4_rank.R`, `6_rank_x_cath.R`, `8_event_study.R`, `9_heterogeneity.R`, `10_mover_balance.R`, `13_permutation.R`; named `med_cath_lab` in `11_training_pipeline.R`.
- Note: no single source of truth. Any change to the recipe must be made in all ten scripts.

### `train_open_heart` (`OHSRGHOS`), `train_cardiac_icu` (`CICHOS`)
- Paper: the open-heart and cardiac-ICU columns of aha-training.tex.
- Raw/chain: built alongside `train_cath_lab` in `5_aha_training.R` (`open_heart_share`, `cardiac_icu_share`), same HRR-year aggregation and matriculation match.
- Level: cardiologist; HRR-year share of hospitals offering the service at matriculation.
- Files: `5_aha_training.R`.

### `train_cath_lab_teach` (teaching-hospital cath-lab share, clerkship mechanism)
- Paper: the teaching-vs-all-hospital columns of aha-mechanism-teaching.tex.
- Raw: `CCLABHOS`, plus `teach_major`/`teach_minor` in `aha_hospital.csv`.
- Chain: `5_aha_training.R` restricts the HRR-year aggregation to teaching hospitals, then `cath_lab_share_teach = mean(has_cath_lab)`, matched by the same matriculation year.
- Level: cardiologist; teaching-hospital-only HRR-year cath-lab share.
- Files: `5_aha_training.R`.

### `national_cath_share` (cohort-trend control)
- Paper: column 4 of cohort-robust.tex (the national-rollout control); the "0.14 in 1980 to 0.39 in 2003" statement.
- Raw/chain: `3_selection.R` computes `mean(CCLABHOS=="1")` by `year` nationally and joins it on the matriculation year.
- Level: national year-level share, attached per cardiologist by matriculation year.
- Files: `3_selection.R`.

### `med_school_start` / `aha_match_year` (inferred matriculation year)
- Paper: the timing behind every training-period measure; discussed in Section 2.1 and appendix A.4.
- Raw: `grad_year` (Physician Compare).
- Chain: `med_school_start = grad_year - 3`, clamped to the coverage window ([1980, 2003] for AHA cath-lab, [1985, 2025] for NIH rank). Recomputed identically wherever a training-period measure is built.
- Level: cardiologist; calendar year.
- Files: `5_aha_training.R`, `4_rank.R`, `6_rank_x_cath.R`, `8_event_study.R`, `9_heterogeneity.R`, `10_mover_balance.R`, `3_selection.R`, `1_descriptive.R`, `11_training_pipeline.R`, `13_permutation.R`.

---

## 4. Mover and destination variables

### `hrr_med_school` (medical-school HRR, origin / training location)
- Paper: the origin fixed effect in within-origin specs; the cluster variable in nearly every regression; the reshuffle block in the permutation.
- Raw: Physician Compare medical school name; Dartmouth `ZipHsaHrr15.xls`; `med-school-xw.csv`; `LCME accreditation.xlsx`.
- Chain: `crosswalks/1_medschool_list.R` gives `npi -> med_school`; `crosswalks/0_zip_hrr.R` gives `zip -> hrrnum`; `crosswalks/2_medschool_hrr.R` resolves each school to a ZIP (LCME program ZIP, else a defunct-school fallback; non-MD/foreign forced to NA) and joins to HRR, writing `med-school-hrr-crosswalk.csv`. `1_physicians.R` joins `med_school -> hrr_med_school`; `2_intensity_measures.R` merges it onto the panel.
- Level: cardiologist; Dartmouth HRR integer.
- Files: crosswalks `0`, `1`, `2`; `1_physicians.R`, `2_intensity_measures.R`, `3_movers.R`; then `1`, `3`, `4`, `5`, `6`, `8`, `9`, `10`, `11`, `12`, `13`.

### `hrr_practice` (practice / destination HRR)
- Paper: the destination fixed effect in all mover specs; the HRR shaded in the choropleth; defines mid-career movers.
- Raw: MDPPAS `phy_zip_perf1` (primary practice ZIP by allowed dollars).
- Chain: `1_physicians.R` renames `phy_zip_perf1 -> zip5`, zero-pads to 5, joins `zip-hrr-crosswalk.csv` to `hrr_practice`; carried through `2_intensity_measures.R`.
- Level: cardiologist-year (can change across years); Dartmouth HRR integer.
- Files: `crosswalks/0_zip_hrr.R`, `1_physicians.R`, `2_intensity_measures.R`, `3_movers.R`; then `1`, `2`, `3`, `4`, `5`, `6`, `8`, `9`, `10`, `11`, `12`.

### `mover` (mover indicator)
- Paper: the mover-share statistic (~86% in the analytical sample); the mover-vs-full-sample comparison.
- Chain: `3_movers.R` sets `mover = as.integer(!is.na(hrr_med_school) & !is.na(hrr_practice) & hrr_med_school != hrr_practice)`, written back onto `analysis_panel.csv`. `3_selection.R` and `1_descriptive.R` collapse it to an ever-mover cardiologist-level flag. Distinct from the mid-career mover (>=2 distinct `hrr_practice`) used in `10`, `11`, `12`. A cardiologist with no medical-school HRR evaluates to 0 (stayer) rather than NA under this expression. That never reaches a reported number, since every analysis first conditions on a mapped school (non-missing `train_cath_lab` or `intensity_med_school`), but a full-panel mover share should not be read off this flag.
- Level: cardiologist-year 0/1.
- Files: `3_movers.R`; `1`, `3`, `5`.

### `intensity_med_school` (medical-school-HRR leave-one-out peer intensity)
- Paper: the peer-cath Level and Change specs (main-regressions.tex), the spline, the cohort-FE and career-stage specs.
- Chain: `2_intensity_measures.R` computes, over cardiologist-years with non-missing `hrr_med_school`, the volume-weighted mean residualized cath of same-origin peers excluding the focal NPI: `(hrr_total_resid - npi_total_resid) / (hrr_total_n - npi_total_n)`. Merged by `(npi, hrr_med_school)`.
- Level: cardiologist (constant across that NPI's years); leave-one-out residualized-cath rate.
- Files: `2_intensity_measures.R`; `3_selection.R`.

### `intensity_dest_loo` (destination-HRR leave-one-out peer intensity)
- Paper: the "current-HRR cath culture (LOO)" row in training-imprint.tex; the event-study jump; the heterogeneity interaction.
- Chain: `2_intensity_measures.R` computes, within `(hrr_practice, year)`, `(sum(mean_resid_cath*n_nstemi) - mean_resid_cath*n_nstemi) / (sum(n_nstemi) - n_nstemi)`. Solo-in-cell rows become NaN and are filtered downstream (this filter defines the within-origin regression N=10,729).
- Level: cardiologist-year; leave-one-out residualized-cath rate of same-destination peers.
- Files: `2_intensity_measures.R`; `1`, `3`, `5`, `8`, `9`, `10`.

### `intensity_change` (destination LOO minus medical-school HRR)
- Paper: the Change specification and its robustness (quartiles, positive/negative); the intensity-change histogram.
- Chain: `2_intensity_measures.R` sets `intensity_change = intensity_dest_loo - intensity_med_school`.
- Level: cardiologist-year; difference of two residualized-cath rates.
- Files: `2_intensity_measures.R`; `3_selection.R`.

### `delta_dest` (move-time jump in destination peer intensity, event study)
- Paper: the event-study interaction and the pooled 0.350 destination response; the mover-selection figure.
- Chain: `8_event_study.R` (recomputed in `10_mover_balance.R`) sets, for each mid-career mover, `intensity_dest_loo` at (destination HRR, move year) minus at (origin HRR, move year - 1).
- Level: cardiologist (per move); residualized-cath-rate difference.
- Files: `8_event_study.R`, `10_mover_balance.R`.

---

## 5. NIH rank, subspecialty, demographics, cohort

### NIH research rank: `nih_rank` (continuous), `nih_tier` and tier indicators, `rank_top25` (binary)
- Paper: rank.tex (continuous -log rank and tier indicators, with the training-cath horse race); rank-x-cath-stratified.tex and rank-x-cath-cells.tex.
- Raw: `med-school-nih.csv` (school-by-fiscal-year rank and tier, from NIH ExPORTER 1985-2025) and `cardio-school-to-nih.csv` (name crosswalk); these arrive as inputs, with no R build script in the data-build set.
- Chain (`4_rank.R`): join `med_school -> canonical_school`; `nih_match_year = clamp(grad_year - 3, 1985, 2025)`; join `med-school-nih.csv` on `(canonical_school, nih_match_year)` for `nih_rank` and `nih_tier`; derive the tier indicators and `I(-log(nih_rank))`, reference tier `05_unranked`. `6_rank_x_cath.R` rebuilds the join and forms `rank_top25 = nih_tier in {01_top10, 02_top11_25}`.
- Level: cardiologist; `nih_rank` an integer funding rank of the school in the matriculation year, tiers 0/1.
- Files: `4_rank.R`, `6_rank_x_cath.R`.

### `specialty` (subspecialty)
- Paper: the subspecialty heterogeneity (general vs interventional in aha-training-by-subspecialty.tex); summary and balance shares; a propensity-score covariate.
- Raw: MDPPAS `spec_prim_1_name`.
- Chain: `1_physicians.R` renames it `specialty`, filters to the four cardiology specialties, and uses it to set the GME-duration filter (Cardiology 6y / IC 7y / EP 8y / Adv HF 7y, `min_practice_year = grad_year + gme_yrs`); carried through `2_intensity_measures.R`.
- Level: cardiologist-year string (4 levels).
- Files: `1_physicians.R`, `2_intensity_measures.R`; `1`, `3`, `5`, `10`.

### `gender` -> `female`
- Paper: a physician control in the imprint, rank and subspecialty specs; a propensity covariate; balance rows.
- Raw: Physician Compare gender.
- Chain: `crosswalks/1_medschool_list.R` gives `cardiologist_pc.csv`; joined in `1_physicians.R`; each script forms `female = as.integer(gender == "F")` where it needs it.
- Level: cardiologist; binary control.
- Files: `crosswalks/1_medschool_list.R`, `crosswalks/3_doximity_residency.R` (match validation), `1_physicians.R`, `2_intensity_measures.R`; `1`, `3`, `4`, `5`, `6`, `10`.

### `grad_year` (graduation / cohort year)
- Paper: the cohort dimension; the graduation-year summary; the analysis window (grad 1983-2006); the matriculation inference.
- Raw: Physician Compare graduation year.
- Chain: `crosswalks/1_medschool_list.R` (modal per NPI); `1_physicians.R` uses it for the GME drop; `2_intensity_measures.R` carries it. Downstream it drives matriculation (`grad_year - 3`), `years_exp = year - grad_year`, cohort fixed effects, and the `grad_year in [1983, 2006]` window.
- Level: cardiologist; calendar year.
- Files: crosswalks `1`, `3`, `4`; `1_physicians.R`, `2_intensity_measures.R`, `3_movers.R`; then `1`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, `11`, `12`, `13`.

---

## 6. Training-pipeline and recent-grad variables

### Residency and fellowship exposure (`res_own_cath`, `res_sys_any_cath`, `res_sys_n_cath`, `res_sys_share_cath`, `res_sys_size`, `res_hosp_hrr`, and `fel_*` equivalents)
- Paper: the training-pipeline table (training-pipeline.tex): medical-school vs residency own-hospital and system-share imprints.
- Raw: Doximity `doximity_profiles.csv` (`residency_institution`, `fellowship_institution`); `aha_hospital.csv` (`ID`, `SYSID`, `HRRCODE`, `CCLABHOS`); `cardiologist_pc.csv` `grad_year`.
- Chain: `crosswalks/3_doximity_residency.R` matches NPIs to Doximity profiles (name block, grad-year/state/med-school tie-break, nickname passes) writing `cardiologist_doximity.csv`. `crosswalks/4_doximity_aha.R` maps each program string to an AHA `ID` (about 130 hand overrides, exact normalized match, guarded substring fuzzy) writing `training_aha_crosswalk.csv`. `4_training_exposure.R` attaches the AHA IDs and, using the residency convention (start `clamp(grad_year, 1980, 2001)`, fellowship `clamp(grad_year + 3, 1980, 2001)`, each expanded to a 3-year PGY window), computes own-hospital and same-HRR-system cath-lab exposures, writing `cardiologist_training_exposure.csv`.
- Level: cardiologist; `*_own_cath` and `*_sys_share_cath` shares in [0,1], `*_sys_size`/`*_sys_n_cath` counts, `*_sys_any_cath` 0/1, `res_hosp_hrr` an HRR integer.
- Files: `crosswalks/3_doximity_residency.R`, `crosswalks/4_doximity_aha.R`, `4_training_exposure.R`; consumed in `11_training_pipeline.R` and `12_recent_grad_residency.R`. `med_cath_lab` in `11` is the Section 3 `train_cath_lab` construction under another name.

### `res_cath_rate_pgy` (recent-grad residency-hospital NSTEMI cath rate, PGY1-3)
- Paper: the recent-grad direct-exposure test (training-recent-grad.tex, appendix E.4).
- Raw: `hospital_year_cath.csv` (`prvdr_num`, `year`, `rate_cath_d2` from `7_hospital_cath_rate.sas`); `aha_hospital.csv` `MCRNUM`/`ID`/`year` bridge; `ahaid_residency` from `cardiologist_training_exposure.csv`.
- Chain: `12_recent_grad_residency.R` bridges CCN to AHA ID via `MCRNUM` (zero-padded to 6), then for cardiologists with `grad_year >= 2006` and a mapped residency averages the residency hospital's contemporaneous `rate_cath_d2` over PGY1-3 (restricted to 2008-2018), yielding `res_cath_rate_pgy` and `n_pgy_years_obs`.
- Level: cardiologist; observed within-two-day catheterization rate at the residency hospital during training, in [0,1].
- Files: `7_hospital_cath_rate.sas`, `12_recent_grad_residency.R`.

---

## 7. The permutation

### `train_perm` (reshuffled training exposure, randomization-inference falsification)
- Paper: the permutation null in appendix C and the Section 3.5 pointer (perm-null.png, permutation-summary.csv); the true 0.058 beyond all but one of 1,000 placebo draws (two-sided p=0.005).
- Raw: identical inputs to `train_cath_lab` (`aha_hospital.csv` `CCLABHOS`/`HRRCODE`/`year`, `grad_year`, `hrr_med_school`).
- Chain (`13_permutation.R`, self-contained): rebuild `train_cath_lab` from source exactly as in Section 3; fix the estimation sample (grad 1983-2006, non-missing `train_cath_lab`, `mean_resid_cath`, `intensity_dest_loo`, `gender`, `specialty`, the N=10,729 within-origin sample); within each `hrr_med_school`, `train_perm = sample(train_cath_lab)` across cardiologists (reassigning each NPI a same-origin different-cohort value), rejoin by `npi`, and re-estimate `mean_resid_cath ~ train_perm | hrr_med_school + hrr_practice + year`, weighted by `n_nstemi`, 1,000 times (`set.seed(20260804)`).
- Level: cardiologist (mapped to cardiologist-years by the join); same units as `train_cath_lab`. Output is the null distribution of the imprint coefficient, not a panel column.
- Files: `13_permutation.R`.

---

## 8. What each analysis script writes

Descriptive numbers in the paper trace to these outputs. All `.tex` are bare
`tabular`; coefficient cells are `%.3f` with significance stars and a
standard-error row below. The persistence numbers, the pooled `delta_dest`, the
within-residency-hospital and placement specs, the destination-sorting diagnostic
and the national HRR-mean cath share now each write a CSV (added 2026-09-25), so
every reported number has an on-disk artifact. On 2026-09-26 the descriptive frame in
`1_descriptive.R` (`panel_summ`, which feeds `summary-stats.tex`, `balance.tex` and the
training binscatter) was restricted to the analytical sample, so Table 1 now reports the
10,736 cardiologist-years and 3,207 cardiologists the prose describes; the same pass added
`binscatter-slope.csv`, `recent-grad-sample.csv`, `rank-x-cath-panel-n.csv` and `unmapped-school-composition.csv` (the appendix A.3 exclusion counts), and persisted
the crosswalk resolution counts to `data/crosswalks/med-school-hrr-match-counts.csv`. One
residual coupling: `7_dynamic.R`
still hardcodes `beta_train = 0.058` and `beta_dest = 0.350` rather than reading
`training-imprint.tex` and `event-study-pooled.csv`.

| Script | Outputs |
|---|---|
| `1_descriptive.R` | `summary-stats.{tex,csv}`, `balance.{tex,csv}`, `origin-dispersion.csv`, `binscatter-slope.csv`, `unmapped-school-composition.csv`; figures `binscatter-training-vs-cath`, `panel-size-by-year`, `od-heatmap`, `od-hhi` |
| `2_hrr_map.R` | figure `hrr-cath-intensity` |
| `3_selection.R` | `balance-by-specialty.csv`, `selection.tex`, `selection-aha.tex`, `cohort-robust.tex`, `destination-sorting.csv`, `national-cath-share.csv` |
| `4_rank.R` | `rank.tex` |
| `5_aha_training.R` | `aha-training.tex`, `aha-training-by-subspecialty.tex`, `training-imprint.tex` (headline 0.058 within-origin), `aha-mechanism-teaching.tex`, `persistence.csv` |
| `6_rank_x_cath.R` | `rank-x-cath-stratified.tex`, `rank-x-cath-cells.tex`, `rank-x-cath-panel-n.csv` |
| `7_dynamic.R` | `dynamic-path.csv`, `place-variance-path.csv`; figures `dynamic-calibration`, `place-variance-path` (betas hardcoded) |
| `8_event_study.R` | `event-study-coefs.csv`, `event-study-pooled.csv`, `event-study-coefs-by-direction.csv`; figures `event-study`, `event-study-by-direction`, `two-peer-deviation` |
| `9_heterogeneity.R` | `heterogeneity-train-x-dest.tex` |
| `10_mover_balance.R` | `mover-balance.{tex,csv}`; figure `mover-selection` |
| `11_training_pipeline.R` | `training-pipeline.tex`, `training-pipeline-robust.csv`, `training-pipeline-selection.csv` |
| `12_recent_grad_residency.R` | `training-recent-grad.tex`, `recent-grad-sample.csv` |
| `13_permutation.R` | `permutation-summary.csv` (at `results/`, not `results/tables/`); figure `perm-null` |

Removed 2026-09-25 as stale and unused: `fgw-decomp-*.{csv,tex}` in
`results/tables/` (a decomposition step no current script produces) and Shirley's
original surgeon/PUF figures in `results/figures/` (`surg2013-2019.png`,
`spline-full/general/specialist.png`, `spline.tex`, `scatter-mover.tex`,
`surg-hrr.tex`, `intensity_corr_movers/stayers.png`). None was wired into a paper
claim.
