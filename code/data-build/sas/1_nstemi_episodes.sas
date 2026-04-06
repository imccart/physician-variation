/* ------------------------------------------------------------ */
/* TITLE:         Identify NSTEMI episodes and comorbidities     */
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  4/3/2026                                       */
/* CODE FILE ORDER: 1 of 6                                       */
/* INPUT:         Inpatient RIF (100%, 2008-2018)                */
/* OUTPUT:        PL027710.NSTEMI_Episodes                       */
/*                PL027710.NSTEMI_Comorbidities                  */
/*                PL027710.NSTEMI_Benes (unique bene list)       */
/* ------------------------------------------------------------ */


/* ============================================================ */
/* Step 1a: Extract NSTEMI admissions from inpatient claims      */
/* ============================================================ */

/* NSTEMI defined by primary diagnosis code:                     */
/*   ICD-9-CM:  410.71 (initial episode of care)                 */
/*   ICD-10-CM: I214  (NSTEMI)                                  */
/* We keep initial episodes only (ICD-9 5th digit = 1).          */
/* Admits straddling Oct 2015 use the code on the claim.         */

%MACRO extract_nstemi(year);

    %stack_inpatient(&year);

    PROC SQL;
        CREATE TABLE WORK.NSTEMI_&year AS
        SELECT
            BENE_ID,
            CLM_ID,
            PRVDR_NUM,
            ORG_NPI_NUM,
            AT_PHYSN_NPI,
            OP_PHYSN_NPI,
            OT_PHYSN_NPI,
            ADMTG_DGNS_CD,
            PRNCPAL_DGNS_CD,
            /* Keep all 25 diagnosis codes for comorbidity flagging */
            ICD_DGNS_CD1,  ICD_DGNS_CD2,  ICD_DGNS_CD3,  ICD_DGNS_CD4,  ICD_DGNS_CD5,
            ICD_DGNS_CD6,  ICD_DGNS_CD7,  ICD_DGNS_CD8,  ICD_DGNS_CD9,  ICD_DGNS_CD10,
            ICD_DGNS_CD11, ICD_DGNS_CD12, ICD_DGNS_CD13, ICD_DGNS_CD14, ICD_DGNS_CD15,
            ICD_DGNS_CD16, ICD_DGNS_CD17, ICD_DGNS_CD18, ICD_DGNS_CD19, ICD_DGNS_CD20,
            ICD_DGNS_CD21, ICD_DGNS_CD22, ICD_DGNS_CD23, ICD_DGNS_CD24, ICD_DGNS_CD25,
            CLM_ADMSN_DT,
            NCH_BENE_DSCHRG_DT,
            PTNT_DSCHRG_STUS_CD
        FROM WORK.InpatientStays_&year
        WHERE (
            /* ICD-9: 410.71 (NSTEMI, initial episode) */
            (CLM_ADMSN_DT < &icd10_start_date
             AND SUBSTR(PRNCPAL_DGNS_CD, 1, 5) IN ("41071"))
            OR
            /* ICD-10: I21.4 (NSTEMI) */
            (CLM_ADMSN_DT >= &icd10_start_date
             AND SUBSTR(PRNCPAL_DGNS_CD, 1, 4) = "I214")
        )
        /* Basic validity checks */
        AND CLM_ADMSN_DT IS NOT NULL
        AND NCH_BENE_DSCHRG_DT IS NOT NULL
        AND CLM_ADMSN_DT <= NCH_BENE_DSCHRG_DT;
    QUIT;

    /* Clean up stacked inpatient */
    PROC DELETE DATA=WORK.InpatientStays_&year; RUN;

%MEND extract_nstemi;


/* Run for all years */
%MACRO extract_all_nstemi;
    %DO yr = &year_start %TO &year_end;
        %extract_nstemi(&yr);
    %END;
%MEND extract_all_nstemi;
%extract_all_nstemi;


/* ============================================================ */
/* Step 1b: Stack all years and keep new episodes only           */
/* ============================================================ */

/* Stack across years */
%MACRO stack_nstemi;
DATA WORK.NSTEMI_All;
    SET
    %DO yr = &year_start %TO &year_end;
        WORK.NSTEMI_&yr
    %END;
    ;
    AMI_Year = YEAR(CLM_ADMSN_DT);
RUN;
%MEND stack_nstemi;
%stack_nstemi;

/* New episode = first NSTEMI admission per beneficiary in data  */
/* Sort by bene and admit date, keep first */
PROC SORT DATA=WORK.NSTEMI_All;
    BY BENE_ID CLM_ADMSN_DT;
RUN;

DATA WORK.NSTEMI_New;
    SET WORK.NSTEMI_All;
    BY BENE_ID;
    IF FIRST.BENE_ID;
RUN;

/* Diagnostic: episode counts by year */
TITLE "NSTEMI new episodes by year";
PROC SQL;
    SELECT AMI_Year, COUNT(*) AS N_Episodes
    FROM WORK.NSTEMI_New
    GROUP BY AMI_Year
    ORDER BY AMI_Year;
QUIT;
TITLE;


/* ============================================================ */
/* Step 1c: Flag Elixhauser comorbidities from secondary dx      */
/* ============================================================ */

/* Pivot diagnosis codes to long format, then flag comorbidities */
/* using both ICD-9 and ICD-10 code sets.                        */
/* Comorbidity categories follow Elixhauser et al. as used in    */
/* Shirley's lookup file (elixhauser-icd10cm.csv).               */

/* First, reshape to long: one row per bene x diagnosis code     */
DATA WORK.DxLong (KEEP = BENE_ID CLM_ADMSN_DT ICD_Code);
    SET WORK.NSTEMI_New;
    ARRAY DxCodes{25} ICD_DGNS_CD1-ICD_DGNS_CD25;
    DO i = 1 TO 25;
        IF DxCodes{i} NE '' THEN DO;
            ICD_Code = DxCodes{i};
            OUTPUT;
        END;
    END;
RUN;

/* Flag comorbidities by joining to Elix_Lookup dataset.          */
/* A code can map to multiple categories (one row per match).     */
/* 31 categories, ~2,600 exact codes. See elix_lookup.sas.        */

PROC SQL;
    CREATE TABLE WORK.DxFlagged AS
    SELECT DISTINCT
        a.BENE_ID,
        b.Comorbidity
    FROM WORK.DxLong AS a
    INNER JOIN WORK.Elix_Lookup AS b
        ON a.ICD_Code = b.ICD_Code
        AND b.ICD_Version = CASE
            WHEN a.CLM_ADMSN_DT < &icd10_start_date THEN 9
            ELSE 10
        END
    WHERE b.Comorbidity NE '';
QUIT;

/* Pivot to wide: one row per bene, one 0/1 column per category  */
PROC SQL;
    CREATE TABLE PL027710.NSTEMI_Comorbidities AS
    SELECT
        a.BENE_ID,
        MAX(CASE WHEN b.Comorbidity = "chf" THEN 1 ELSE 0 END) AS chf,
        MAX(CASE WHEN b.Comorbidity = "car" THEN 1 ELSE 0 END) AS car,
        MAX(CASE WHEN b.Comorbidity = "val" THEN 1 ELSE 0 END) AS val,
        MAX(CASE WHEN b.Comorbidity = "pcd" THEN 1 ELSE 0 END) AS pcd,
        MAX(CASE WHEN b.Comorbidity = "pvd" THEN 1 ELSE 0 END) AS pvd,
        MAX(CASE WHEN b.Comorbidity = "htn_uncx" THEN 1 ELSE 0 END) AS htn_uncx,
        MAX(CASE WHEN b.Comorbidity = "htn_cx" THEN 1 ELSE 0 END) AS htn_cx,
        MAX(CASE WHEN b.Comorbidity = "par" THEN 1 ELSE 0 END) AS par,
        MAX(CASE WHEN b.Comorbidity = "neu" THEN 1 ELSE 0 END) AS neu,
        MAX(CASE WHEN b.Comorbidity = "cpd" THEN 1 ELSE 0 END) AS cpd,
        MAX(CASE WHEN b.Comorbidity = "diab_uncx" THEN 1 ELSE 0 END) AS diab_uncx,
        MAX(CASE WHEN b.Comorbidity = "diab_cx" THEN 1 ELSE 0 END) AS diab_cx,
        MAX(CASE WHEN b.Comorbidity = "thy_hypo" THEN 1 ELSE 0 END) AS thy_hypo,
        MAX(CASE WHEN b.Comorbidity = "renal_fl" THEN 1 ELSE 0 END) AS renal_fl,
        MAX(CASE WHEN b.Comorbidity = "liver" THEN 1 ELSE 0 END) AS liver,
        MAX(CASE WHEN b.Comorbidity = "pud" THEN 1 ELSE 0 END) AS pud,
        MAX(CASE WHEN b.Comorbidity = "aids" THEN 1 ELSE 0 END) AS aids,
        MAX(CASE WHEN b.Comorbidity = "lymph" THEN 1 ELSE 0 END) AS lymph,
        MAX(CASE WHEN b.Comorbidity = "mets" THEN 1 ELSE 0 END) AS mets,
        MAX(CASE WHEN b.Comorbidity = "tumor" THEN 1 ELSE 0 END) AS tumor,
        MAX(CASE WHEN b.Comorbidity = "rheu" THEN 1 ELSE 0 END) AS rheu,
        MAX(CASE WHEN b.Comorbidity = "coag" THEN 1 ELSE 0 END) AS coag,
        MAX(CASE WHEN b.Comorbidity = "obese" THEN 1 ELSE 0 END) AS obese,
        MAX(CASE WHEN b.Comorbidity = "wgt_loss" THEN 1 ELSE 0 END) AS wgt_loss,
        MAX(CASE WHEN b.Comorbidity = "fluid" THEN 1 ELSE 0 END) AS fluid,
        MAX(CASE WHEN b.Comorbidity = "anemia_blood" THEN 1 ELSE 0 END) AS anemia_blood,
        MAX(CASE WHEN b.Comorbidity = "anemia_def" THEN 1 ELSE 0 END) AS anemia_def,
        MAX(CASE WHEN b.Comorbidity = "alcohol" THEN 1 ELSE 0 END) AS alcohol,
        MAX(CASE WHEN b.Comorbidity = "drug" THEN 1 ELSE 0 END) AS drug,
        MAX(CASE WHEN b.Comorbidity = "psychoses" THEN 1 ELSE 0 END) AS psychoses,
        MAX(CASE WHEN b.Comorbidity = "depression" THEN 1 ELSE 0 END) AS depression
    FROM (SELECT DISTINCT BENE_ID FROM WORK.NSTEMI_New) AS a
    LEFT JOIN WORK.DxFlagged AS b
        ON a.BENE_ID = b.BENE_ID
    GROUP BY a.BENE_ID;
QUIT;


/* ============================================================ */
/* Save episodes and bene list                                   */
/* ============================================================ */

DATA PL027710.NSTEMI_Episodes;
    SET WORK.NSTEMI_New;
RUN;

PROC SQL;
    CREATE TABLE PL027710.NSTEMI_Benes AS
    SELECT DISTINCT BENE_ID, CLM_ADMSN_DT AS AMI_Admsn_Dt
    FROM WORK.NSTEMI_New;
QUIT;

/* Diagnostic: total episodes and comorbidity prevalence */
TITLE "NSTEMI episode count";
PROC SQL;
    SELECT COUNT(*) AS N_Episodes,
           COUNT(DISTINCT BENE_ID) AS N_Benes
    FROM PL027710.NSTEMI_Episodes;
QUIT;
TITLE;

TITLE "Comorbidity prevalence";
PROC MEANS DATA=PL027710.NSTEMI_Comorbidities MEAN;
    VAR chf car val pcd pvd htn_uncx htn_cx par neu cpd
        diab_uncx diab_cx thy_hypo renal_fl liver pud aids lymph mets tumor
        rheu coag obese wgt_loss fluid anemia_blood anemia_def
        alcohol drug psychoses depression;
RUN;
TITLE;


/* ============================================================ */
/* Clean up WORK                                                 */
/* ============================================================ */
%MACRO cleanup_nstemi;
    %DO yr = &year_start %TO &year_end;
        PROC DELETE DATA=WORK.NSTEMI_&yr; RUN;
    %END;
    PROC DELETE DATA=WORK.NSTEMI_All; RUN;
    PROC DELETE DATA=WORK.NSTEMI_New; RUN;
    PROC DELETE DATA=WORK.DxLong; RUN;
    PROC DELETE DATA=WORK.DxFlagged; RUN;
%MEND cleanup_nstemi;
%cleanup_nstemi;
