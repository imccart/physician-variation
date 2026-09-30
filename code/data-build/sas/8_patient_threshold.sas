/* ------------------------------------------------------------ */
/* TITLE:         Patient-level file for the threshold analysis  */
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  9/29/2026                                      */
/* CODE FILE ORDER: 8                                            */
/* INPUT:         PL027710.NSTEMI_Benes                          */
/*                PL027710.NSTEMI_Patients                       */
/*                PL027710.NSTEMI_Residualized                   */
/*                PL027710.NSTEMI_Cardiologist                   */
/*                Inpatient RIF (100%, 2008-2018), MBSF ABCD     */
/* OUTPUT:        PL027710.NSTEMI_Threshold                      */
/* EXPORT:        Manual via SAS EG -> CSV into the R container's */
/*                data/input/nstemi_threshold.csv                */
/* ------------------------------------------------------------ */

/* One row per NSTEMI episode carrying the assigned cardiologist, the  */
/* admitting hospital, a baseline one-year mortality risk estimated     */
/* from patient characteristics alone, the cath-within-2-days outcome   */
/* split into episodes with and without revascularization within 90    */
/* days, and 30- and 365-day mortality. Read by                         */
/* code/analysis/vrdc/1_threshold.R inside the VRDC R container.        */

%include "0_config.sas";


/* ============================================================ */
/* Step 8a: Death dates from the beneficiary summary             */
/* ============================================================ */

%MACRO read_death(year);
    PROC SQL;
        CREATE TABLE WORK.Death_&year AS
        SELECT b.BENE_ID, b.BENE_DEATH_DT
        FROM MBSF.MBSF_ABCD_&year AS b
        INNER JOIN PL027710.NSTEMI_Benes AS p
            ON b.BENE_ID = p.BENE_ID
        WHERE b.BENE_DEATH_DT IS NOT NULL;
    QUIT;
%MEND read_death;

%MACRO read_all_death;
    %DO yr = &year_start %TO &year_end;
        %read_death(&yr);
    %END;
    DATA WORK.Death_All;
        SET
        %DO yr = &year_start %TO &year_end;
            WORK.Death_&yr
        %END;
        ;
    RUN;
%MEND read_all_death;
%read_all_death;

PROC SQL;
    CREATE TABLE WORK.Death AS
    SELECT BENE_ID,
           MIN(BENE_DEATH_DT) AS Death_Dt FORMAT=DATE9.
    FROM WORK.Death_All
    GROUP BY BENE_ID;
QUIT;


/* ============================================================ */
/* Step 8b: Revascularization (PCI/CABG) within 90 days          */
/* ============================================================ */

/* Same inpatient-claims search as 2_cath_procedures.sas, keeping only */
/* PCI, stent, and CABG codes so a cath can be classified as followed  */
/* by revascularization or diagnostic only.                             */

%MACRO extract_revasc(year);

    %stack_inpatient(&year);

    PROC SQL;
        CREATE TABLE WORK.RProcs_&year AS
        SELECT
            a.BENE_ID,
            b.AMI_Admsn_Dt,
            a.ICD_PRCDR_CD1,  a.PRCDR_DT1,
            a.ICD_PRCDR_CD2,  a.PRCDR_DT2,
            a.ICD_PRCDR_CD3,  a.PRCDR_DT3,
            a.ICD_PRCDR_CD4,  a.PRCDR_DT4,
            a.ICD_PRCDR_CD5,  a.PRCDR_DT5,
            a.ICD_PRCDR_CD6,  a.PRCDR_DT6,
            a.ICD_PRCDR_CD7,  a.PRCDR_DT7,
            a.ICD_PRCDR_CD8,  a.PRCDR_DT8,
            a.ICD_PRCDR_CD9,  a.PRCDR_DT9,
            a.ICD_PRCDR_CD10, a.PRCDR_DT10,
            a.ICD_PRCDR_CD11, a.PRCDR_DT11,
            a.ICD_PRCDR_CD12, a.PRCDR_DT12,
            a.ICD_PRCDR_CD13, a.PRCDR_DT13,
            a.ICD_PRCDR_CD14, a.PRCDR_DT14,
            a.ICD_PRCDR_CD15, a.PRCDR_DT15,
            a.ICD_PRCDR_CD16, a.PRCDR_DT16,
            a.ICD_PRCDR_CD17, a.PRCDR_DT17,
            a.ICD_PRCDR_CD18, a.PRCDR_DT18,
            a.ICD_PRCDR_CD19, a.PRCDR_DT19,
            a.ICD_PRCDR_CD20, a.PRCDR_DT20,
            a.ICD_PRCDR_CD21, a.PRCDR_DT21,
            a.ICD_PRCDR_CD22, a.PRCDR_DT22,
            a.ICD_PRCDR_CD23, a.PRCDR_DT23,
            a.ICD_PRCDR_CD24, a.PRCDR_DT24,
            a.ICD_PRCDR_CD25, a.PRCDR_DT25
        FROM WORK.InpatientStays_&year AS a
        INNER JOIN PL027710.NSTEMI_Benes AS b
            ON a.BENE_ID = b.BENE_ID
        WHERE a.CLM_ADMSN_DT >= b.AMI_Admsn_Dt
          AND a.CLM_ADMSN_DT - b.AMI_Admsn_Dt <= 90
          AND a.ICD_PRCDR_CD1 IS NOT NULL;
    QUIT;

    PROC DELETE DATA=WORK.InpatientStays_&year; RUN;

%MEND extract_revasc;

%MACRO extract_all_revasc;
    %DO yr = &year_start %TO &year_end;
        %extract_revasc(&yr);
    %END;
    DATA WORK.RProcs_All;
        SET
        %DO yr = &year_start %TO &year_end;
            WORK.RProcs_&yr
        %END;
        ;
    RUN;
%MEND extract_all_revasc;
%extract_all_revasc;

DATA WORK.RevascLong (KEEP = BENE_ID Proc_Date);
    SET WORK.RProcs_All;
    ARRAY PCodes{25} ICD_PRCDR_CD1-ICD_PRCDR_CD25;
    ARRAY PDates{25} PRCDR_DT1-PRCDR_DT25;
    DO i = 1 TO 25;
        IF PCodes{i} NE '' THEN DO;
            IF PDates{i} < &icd10_start_date THEN DO;
                IF PCodes{i} IN (&icd9_pci_codes,&icd9_stent_codes,&icd9_cabg_codes) THEN DO;
                    Proc_Date = PDates{i};
                    OUTPUT;
                END;
            END;
            ELSE DO;
                IF PCodes{i} IN (&icd10_pci_codes,&icd10_cabg_codes) THEN DO;
                    Proc_Date = PDates{i};
                    OUTPUT;
                END;
            END;
        END;
    END;
    FORMAT Proc_Date DATE9.;
RUN;

PROC SQL;
    CREATE TABLE WORK.Revasc AS
    SELECT BENE_ID,
           MIN(Proc_Date) AS Revasc_Date FORMAT=DATE9.
    FROM WORK.RevascLong
    GROUP BY BENE_ID;
QUIT;


/* ============================================================ */
/* Step 8c: Assemble the episode file                            */
/* ============================================================ */

/* BENE_ID is the episode key (one NSTEMI episode per beneficiary,     */
/* as in steps 3 and 6).                                                */

PROC SQL;
    CREATE TABLE WORK.Thr AS
    SELECT
        r.*,
        p.PRVDR_NUM,
        c.NPI_Gatekeeper,
        d.Death_Dt,
        v.Revasc_Date
    FROM PL027710.NSTEMI_Residualized AS r
    INNER JOIN PL027710.NSTEMI_Patients AS p
        ON r.BENE_ID = p.BENE_ID
    INNER JOIN PL027710.NSTEMI_Cardiologist AS c
        ON r.BENE_ID = c.BENE_ID
    LEFT JOIN WORK.Death AS d
        ON r.BENE_ID = d.BENE_ID
    LEFT JOIN WORK.Revasc AS v
        ON r.BENE_ID = v.BENE_ID
    WHERE c.NPI_Gatekeeper NE '';
QUIT;

DATA WORK.Thr;
    SET WORK.Thr;
    Age_Sq = Age * Age;

    D_Death_30  = (Death_Dt NE . AND Death_Dt - CLM_ADMSN_DT <= 30);
    D_Death_365 = (Death_Dt NE . AND Death_Dt - CLM_ADMSN_DT <= 365);

    /* Full-year follow-up is observable only for admissions through   */
    /* the year before the last MBSF year.                              */
    FU_365 = (CLM_ADMSN_DT <= MDY(12, 31, &year_end - 1));

    D_Revasc_D90     = (Revasc_Date NE . AND Revasc_Date - CLM_ADMSN_DT <= 90);
    D_Cath_D2_DxOnly = D_Cath_D2 * (1 - D_Revasc_D90);
    D_Cath_D2_Revasc = D_Cath_D2 * D_Revasc_D90;

    /* Response for the risk model: missing where follow-up is short,  */
    /* so those rows are scored but not fit.                            */
    IF FU_365 THEN D_Death_365_Fit = D_Death_365;
    ELSE D_Death_365_Fit = .;
RUN;


/* ============================================================ */
/* Step 8d: Baseline mortality risk from patient characteristics */
/* ============================================================ */

/* Same covariates as the cath residualization in 5_residualize.sas.   */
/* No year terms, so every episode (including 2018) receives a score   */
/* and the index ranks patients by severity alone.                      */

TITLE "Risk model: one-year mortality on patient characteristics";
PROC LOGISTIC DATA=WORK.Thr;
    MODEL D_Death_365_Fit(EVENT='1') =
        Age Age_Sq D_Female D_Black D_Hisp D_Asian
        D_Race_Missing D_Sex_Missing D_Dual Dual_Elgbl_Mons
        chf car val pcd pvd htn_uncx htn_cx par neu cpd
        diab_uncx diab_cx thy_hypo renal_fl liver pud aids
        lymph mets tumor rheu coag obese wgt_loss fluid
        anemia_blood anemia_def alcohol drug psychoses depression;
    OUTPUT OUT=WORK.ThrRisk PREDICTED=Pred_Death_365;
RUN;
TITLE;

PROC RANK DATA=WORK.ThrRisk GROUPS=5 OUT=WORK.ThrRisk;
    VAR Pred_Death_365;
    RANKS Risk_Q5;
RUN;


/* ============================================================ */
/* Step 8e: Save                                                 */
/* ============================================================ */

DATA PL027710.NSTEMI_Threshold (KEEP = BENE_ID AMI_Year CLM_ADMSN_DT
                                       PRVDR_NUM NPI_Gatekeeper
                                       D_Cath_D2 D_Cath_D2_DxOnly D_Cath_D2_Revasc
                                       D_Revasc_D90 D_Death_30 D_Death_365 FU_365
                                       Pred_Death_365 Risk_Q5 Pred_Cath Resid_Cath
                                       Age Age_Sq D_Female D_Black D_Hisp D_Asian
                                       D_Race_Missing D_Sex_Missing D_Dual
                                       Dual_Elgbl_Mons
                                       chf car val pcd pvd htn_uncx htn_cx par neu cpd
                                       diab_uncx diab_cx thy_hypo renal_fl liver pud aids
                                       lymph mets tumor rheu coag obese wgt_loss fluid
                                       anemia_blood anemia_def alcohol drug psychoses depression);
    SET WORK.ThrRisk;
    Risk_Q5 = Risk_Q5 + 1;
RUN;


/* ============================================================ */
/* Diagnostics                                                   */
/* ============================================================ */

TITLE "Episodes by risk quintile: cath, diagnostic-only cath, mortality";
PROC SQL;
    SELECT Risk_Q5,
           COUNT(*) AS N,
           MEAN(Pred_Death_365) AS Mean_Pred_Death FORMAT=6.3,
           MEAN(D_Cath_D2) AS Pct_Cath_D2 FORMAT=PERCENT8.1,
           MEAN(D_Cath_D2_DxOnly) AS Pct_Cath_D2_DxOnly FORMAT=PERCENT8.1,
           MEAN(D_Death_30) AS Pct_Death_30 FORMAT=PERCENT8.1,
           MEAN(CASE WHEN FU_365 THEN D_Death_365 ELSE . END) AS Pct_Death_365 FORMAT=PERCENT8.1
    FROM PL027710.NSTEMI_Threshold
    GROUP BY Risk_Q5
    ORDER BY Risk_Q5;
QUIT;
TITLE;


/* ============================================================ */
/* Clean up WORK                                                 */
/* ============================================================ */

%MACRO cleanup_threshold;
    %DO yr = &year_start %TO &year_end;
        PROC DELETE DATA=WORK.Death_&yr; RUN;
        PROC DELETE DATA=WORK.RProcs_&yr; RUN;
    %END;
    PROC DELETE DATA=WORK.Death_All; RUN;
    PROC DELETE DATA=WORK.Death; RUN;
    PROC DELETE DATA=WORK.RProcs_All; RUN;
    PROC DELETE DATA=WORK.RevascLong; RUN;
    PROC DELETE DATA=WORK.Revasc; RUN;
    PROC DELETE DATA=WORK.Thr; RUN;
    PROC DELETE DATA=WORK.ThrRisk; RUN;
%MEND cleanup_threshold;
%cleanup_threshold;
