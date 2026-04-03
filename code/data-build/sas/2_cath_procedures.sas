/* ------------------------------------------------------------ */
/* TITLE:         Identify cardiac cath procedures and timing    */
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  4/3/2026                                       */
/* CODE FILE ORDER: 2 of 6                                       */
/* INPUT:         Inpatient RIF (100%, 2008-2018),               */
/*                PL027710.NSTEMI_Benes                          */
/* OUTPUT:        PL027710.NSTEMI_Cath                           */
/* ------------------------------------------------------------ */

/* For each NSTEMI patient, search all inpatient claims within   */
/* 90 days of AMI admission for cardiac cath, PCI, or CABG.      */
/* Compute time-to-cath and flag cath within various windows.    */


/* ============================================================ */
/* Step 2a: Extract procedure claims for NSTEMI patients         */
/* ============================================================ */

%MACRO extract_cath_procedures(year);

    %stack_inpatient(&year);

    /* Keep procedure codes and dates for NSTEMI benes only */
    PROC SQL;
        CREATE TABLE WORK.Procs_&year AS
        SELECT
            a.BENE_ID,
            a.CLM_ADMSN_DT,
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
            a.ICD_PRCDR_CD25, a.PRCDR_DT25,
            b.AMI_Admsn_Dt
        FROM WORK.InpatientStays_&year AS a
        INNER JOIN PL027710.NSTEMI_Benes AS b
            ON a.BENE_ID = b.BENE_ID
        WHERE a.CLM_ADMSN_DT >= b.AMI_Admsn_Dt
          AND a.CLM_ADMSN_DT - b.AMI_Admsn_Dt <= 90
          AND a.ICD_PRCDR_CD1 IS NOT NULL;
    QUIT;

    PROC DELETE DATA=WORK.InpatientStays_&year; RUN;

%MEND extract_cath_procedures;

%MACRO extract_all_cath;
    %DO yr = &year_start %TO &year_end;
        %extract_cath_procedures(&yr);
    %END;
%MEND extract_all_cath;
%extract_all_cath;


/* Stack all years */
DATA WORK.Procs_All;
    SET
    %DO yr = &year_start %TO &year_end;
        WORK.Procs_&yr
    %END;
    ;
RUN;


/* ============================================================ */
/* Step 2b: Reshape to long and flag invasive procedures         */
/* ============================================================ */

DATA WORK.ProcsLong (KEEP = BENE_ID AMI_Admsn_Dt Proc_Code Proc_Date);
    SET WORK.Procs_All;
    ARRAY PCodes{25} ICD_PRCDR_CD1-ICD_PRCDR_CD25;
    ARRAY PDates{25} PRCDR_DT1-PRCDR_DT25;
    DO i = 1 TO 25;
        IF PCodes{i} NE '' THEN DO;
            Proc_Code = PCodes{i};
            Proc_Date = PDates{i};
            OUTPUT;
        END;
    END;
    FORMAT Proc_Date DATE9.;
RUN;

/* Keep only invasive cardiac procedures */
DATA WORK.InvasiveProcs;
    SET WORK.ProcsLong;
    /* ICD-9 procedure codes (pre-Oct 2015) */
    IF Proc_Date < &icd10_start_date THEN DO;
        IF Proc_Code IN (&icd9_invasive_codes) THEN OUTPUT;
    END;
    /* ICD-10-PCS procedure codes (post-Oct 2015) */
    ELSE DO;
        IF Proc_Code IN (&icd10_invasive_codes) THEN OUTPUT;
    END;
RUN;


/* ============================================================ */
/* Step 2c: Earliest cath date per patient, timing indicators    */
/* ============================================================ */

/* Get earliest invasive procedure date per bene */
PROC SQL;
    CREATE TABLE WORK.EarliestCath AS
    SELECT BENE_ID,
           AMI_Admsn_Dt,
           MIN(Proc_Date) AS Cath_Date FORMAT=DATE9.
    FROM WORK.InvasiveProcs
    GROUP BY BENE_ID, AMI_Admsn_Dt;
QUIT;

/* Merge back to full NSTEMI bene list (left join — not all get cath) */
PROC SQL;
    CREATE TABLE PL027710.NSTEMI_Cath AS
    SELECT
        a.BENE_ID,
        a.AMI_Admsn_Dt,
        b.Cath_Date,
        CASE WHEN b.Cath_Date IS NOT NULL
             THEN b.Cath_Date - a.AMI_Admsn_Dt
             ELSE . END AS Time_To_Cath,
        CASE WHEN b.Cath_Date IS NOT NULL THEN 1 ELSE 0 END AS D_Cath_Any,
        CASE WHEN b.Cath_Date IS NOT NULL AND b.Cath_Date - a.AMI_Admsn_Dt <= 0 THEN 1 ELSE 0 END AS D_Cath_D0,
        CASE WHEN b.Cath_Date IS NOT NULL AND b.Cath_Date - a.AMI_Admsn_Dt <= 1 THEN 1 ELSE 0 END AS D_Cath_D1,
        CASE WHEN b.Cath_Date IS NOT NULL AND b.Cath_Date - a.AMI_Admsn_Dt <= 2 THEN 1 ELSE 0 END AS D_Cath_D2,
        CASE WHEN b.Cath_Date IS NOT NULL AND b.Cath_Date - a.AMI_Admsn_Dt <= 3 THEN 1 ELSE 0 END AS D_Cath_D3,
        CASE WHEN b.Cath_Date IS NOT NULL AND b.Cath_Date - a.AMI_Admsn_Dt <= 7 THEN 1 ELSE 0 END AS D_Cath_D7,
        CASE WHEN b.Cath_Date IS NOT NULL AND b.Cath_Date - a.AMI_Admsn_Dt <= 30 THEN 1 ELSE 0 END AS D_Cath_D30,
        CASE WHEN b.Cath_Date IS NOT NULL AND b.Cath_Date - a.AMI_Admsn_Dt <= 90 THEN 1 ELSE 0 END AS D_Cath_D90,
        YEAR(a.AMI_Admsn_Dt) AS AMI_Year
    FROM PL027710.NSTEMI_Benes AS a
    LEFT JOIN WORK.EarliestCath AS b
        ON a.BENE_ID = b.BENE_ID;
QUIT;

/* Diagnostics */
TITLE "Cath rates by year";
PROC SQL;
    SELECT AMI_Year,
           COUNT(*) AS N_Episodes,
           SUM(D_Cath_Any) AS N_Cath_Any,
           SUM(D_Cath_D2) AS N_Cath_D2,
           CALCULATED N_Cath_D2 / CALCULATED N_Episodes AS Pct_Cath_D2 FORMAT=PERCENT8.1
    FROM PL027710.NSTEMI_Cath
    GROUP BY AMI_Year
    ORDER BY AMI_Year;
QUIT;
TITLE;


/* ============================================================ */
/* Clean up WORK                                                 */
/* ============================================================ */
%MACRO cleanup_cath;
    %DO yr = &year_start %TO &year_end;
        PROC DELETE DATA=WORK.Procs_&yr; RUN;
    %END;
    PROC DELETE DATA=WORK.Procs_All; RUN;
    PROC DELETE DATA=WORK.ProcsLong; RUN;
    PROC DELETE DATA=WORK.InvasiveProcs; RUN;
    PROC DELETE DATA=WORK.EarliestCath; RUN;
%MEND cleanup_cath;
%cleanup_cath;
