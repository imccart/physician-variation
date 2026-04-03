/* ------------------------------------------------------------ */
/* TITLE:         Aggregate to cardiologist-year and export       */
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  4/3/2026                                       */
/* CODE FILE ORDER: 6 of 6                                       */
/* INPUT:         PL027710.NSTEMI_Residualized,                  */
/*                PL027710.NSTEMI_Cardiologist                   */
/* OUTPUT:        PL027710.Cardiologist_Year                     */
/* ------------------------------------------------------------ */

/* Join patient-level data to assigned cardiologist, then         */
/* collapse to cardiologist-year panel. Apply minimum volume      */
/* threshold for both statistical and cell-size reasons.          */


/* ============================================================ */
/* Step 6a: Join patients to assigned cardiologist                */
/* ============================================================ */

PROC SQL;
    CREATE TABLE WORK.PatientCardio AS
    SELECT
        p.*,
        c.NPI_Gatekeeper
    FROM PL027710.NSTEMI_Residualized AS p
    LEFT JOIN PL027710.NSTEMI_Cardiologist AS c
        ON p.BENE_ID = c.BENE_ID;
QUIT;

/* Drop episodes with no assigned cardiologist */
DATA WORK.PatientCardio;
    SET WORK.PatientCardio;
    WHERE NPI_Gatekeeper NE '';
RUN;

/* Diagnostic: how many episodes have an assigned cardiologist? */
TITLE "Episodes with assigned cardiologist";
PROC SQL;
    SELECT
        COUNT(*) AS N_With_Cardio,
        COUNT(DISTINCT NPI_Gatekeeper) AS N_Cardiologists,
        (SELECT COUNT(*) FROM PL027710.NSTEMI_Residualized) AS N_Total,
        CALCULATED N_With_Cardio / CALCULATED N_Total AS Pct_Matched FORMAT=PERCENT8.1
    FROM WORK.PatientCardio;
QUIT;
TITLE;


/* ============================================================ */
/* Step 6b: Aggregate to cardiologist-year                       */
/* ============================================================ */

PROC SQL;
    CREATE TABLE WORK.CardioYear_Raw AS
    SELECT
        NPI_Gatekeeper AS NPI,
        AMI_Year AS Year,

        /* Volume */
        COUNT(*) AS N_NSTEMI,

        /* Treatment intensity */
        SUM(D_Cath_D0) AS N_Cath_D0,
        SUM(D_Cath_D1) AS N_Cath_D1,
        SUM(D_Cath_D2) AS N_Cath_D2,
        SUM(D_Cath_D3) AS N_Cath_D3,
        SUM(D_Cath_D7) AS N_Cath_D7,
        SUM(D_Cath_D30) AS N_Cath_D30,
        SUM(D_Cath_D90) AS N_Cath_D90,
        MEAN(Resid_Cath) AS Mean_Resid_Cath,

        /* Patient demographics (means) */
        MEAN(Age) AS Mean_Age,
        MEAN(D_Female) AS Prop_Female,
        MEAN(D_Black) AS Prop_Black,
        MEAN(D_Hisp) AS Prop_Hisp,
        MEAN(D_Asian) AS Prop_Asian,
        MEAN(D_Race_Missing) AS Prop_Race_Missing,
        MEAN(D_Sex_Missing) AS Prop_Sex_Missing,
        MEAN(D_Dual) AS Prop_Dual,
        MEAN(Dual_Elgbl_Mons) AS Mean_Dual_Months,

        /* Comorbidity prevalence (all 31 Elixhauser categories) */
        MEAN(chf) AS Prop_CHF,
        MEAN(car) AS Prop_CAR,
        MEAN(val) AS Prop_VAL,
        MEAN(pcd) AS Prop_PCD,
        MEAN(pvd) AS Prop_PVD,
        MEAN(htn_uncx) AS Prop_HTN_UNCX,
        MEAN(htn_cx) AS Prop_HTN_CX,
        MEAN(par) AS Prop_PAR,
        MEAN(neu) AS Prop_NEU,
        MEAN(cpd) AS Prop_CPD,
        MEAN(diab_uncx) AS Prop_DIAB_UNCX,
        MEAN(diab_cx) AS Prop_DIAB_CX,
        MEAN(thy_hypo) AS Prop_THY_HYPO,
        MEAN(renal_fl) AS Prop_RENAL_FL,
        MEAN(liver) AS Prop_LIVER,
        MEAN(pud) AS Prop_PUD,
        MEAN(aids) AS Prop_AIDS,
        MEAN(lymph) AS Prop_LYMPH,
        MEAN(mets) AS Prop_METS,
        MEAN(tumor) AS Prop_TUMOR,
        MEAN(rheu) AS Prop_RHEU,
        MEAN(coag) AS Prop_COAG,
        MEAN(obese) AS Prop_OBESE,
        MEAN(wgt_loss) AS Prop_WGT_LOSS,
        MEAN(fluid) AS Prop_FLUID,
        MEAN(anemia_blood) AS Prop_ANEMIA_BLOOD,
        MEAN(anemia_def) AS Prop_ANEMIA_DEF,
        MEAN(alcohol) AS Prop_ALCOHOL,
        MEAN(drug) AS Prop_DRUG,
        MEAN(psychoses) AS Prop_PSYCHOSES,
        MEAN(depression) AS Prop_DEPRESSION

    FROM WORK.PatientCardio
    GROUP BY NPI_Gatekeeper, AMI_Year;
QUIT;


/* ============================================================ */
/* Step 6c: Apply minimum volume filter                          */
/* ============================================================ */

DATA PL027710.Cardiologist_Year;
    SET WORK.CardioYear_Raw;
    WHERE N_NSTEMI >= &min_patients;
    /* Compute cath rates */
    Rate_Cath_D2 = N_Cath_D2 / N_NSTEMI;
RUN;

/* Diagnostic: how many cardiologist-years survive the filter? */
TITLE "Cardiologist-year panel after volume filter (>= &min_patients patients)";
PROC SQL;
    SELECT
        COUNT(*) AS N_CardioYears,
        COUNT(DISTINCT NPI) AS N_Cardiologists,
        MIN(Year) AS Min_Year,
        MAX(Year) AS Max_Year,
        MEAN(N_NSTEMI) AS Mean_Volume FORMAT=5.1,
        MEAN(Rate_Cath_D2) AS Mean_Cath_Rate FORMAT=PERCENT8.1,
        MEAN(Mean_Resid_Cath) AS Mean_Resid FORMAT=6.4
    FROM PL027710.Cardiologist_Year;
QUIT;
TITLE;

TITLE "Volume filter impact";
PROC SQL;
    SELECT
        (SELECT COUNT(*) FROM WORK.CardioYear_Raw) AS N_Before,
        (SELECT COUNT(*) FROM PL027710.Cardiologist_Year) AS N_After,
        (SELECT COUNT(DISTINCT NPI) FROM WORK.CardioYear_Raw) AS Cardio_Before,
        (SELECT COUNT(DISTINCT NPI) FROM PL027710.Cardiologist_Year) AS Cardio_After;
QUIT;
TITLE;

TITLE "Panel by year";
PROC SQL;
    SELECT Year,
           COUNT(*) AS N_CardioYears,
           COUNT(DISTINCT NPI) AS N_Cardiologists,
           MEAN(N_NSTEMI) AS Mean_Volume FORMAT=5.1,
           MEAN(Rate_Cath_D2) AS Mean_Cath_Rate FORMAT=PERCENT8.1
    FROM PL027710.Cardiologist_Year
    GROUP BY Year
    ORDER BY Year;
QUIT;
TITLE;


/* NOTE: Export is done manually via SAS EG within the VRDC.     */
/* Review diagnostics above before exporting. The volume filter  */
/* (>= min_patients) should satisfy CMS cell size requirements.  */


/* ============================================================ */
/* Clean up WORK                                                 */
/* ============================================================ */
PROC DELETE DATA=WORK.PatientCardio; RUN;
PROC DELETE DATA=WORK.CardioYear_Raw; RUN;
