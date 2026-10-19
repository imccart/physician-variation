/* ------------------------------------------------------------ */
/* TITLE:         Pre-admission utilization, threshold sample    */
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  10/16/2026                                     */
/* CODE FILE ORDER: 10                                           */
/* INPUT:         PL027710.NSTEMI_Threshold                      */
/*                Inpatient RIF (100%, 2008-2018)                */
/*                Carrier RIF line files (100%, 2008-2018)       */
/*                MBSF ABCD (2008-2018)                          */
/* OUTPUT:        PL027710.NSTEMI_Prior                          */
/* EXPORT:        Manual via SAS EG -> CSV into the R container's */
/*                data/input/nstemi_prior.csv                    */
/* ------------------------------------------------------------ */

/* One row per episode in the threshold file with utilization in the 365  */
/* days before the index admission: inpatient stays discharged in the     */
/* window (count, days, Medicare payment) and carrier claims with a        */
/* through date in the window (count, Medicare payment). These outcomes   */
/* are determined before the cardiologist sees the patient, so training   */
/* exposure should not predict them; the container R uses them as a       */
/* placebo for the mortality models. Prior_FFS_Full marks episodes whose  */
/* beneficiary had Part B in all 12 months and no HMO months in the        */
/* calendar year before admission (the step 3 rule applied one year        */
/* back), so the window is fully observed; it is 0 for 2008 admissions,   */
/* whose prior year is outside the data.                                   */

%include "0_config.sas";


/* ============================================================ */
/* Step 10a: Prior-year fee-for-service coverage                 */
/* ============================================================ */

%MACRO read_prior_cov(year);
    PROC SQL;
        CREATE TABLE WORK.PriorCov_&year AS
        SELECT t.BENE_ID,
               (b.BENE_HMO_CVRAGE_TOT_MONS = 0
                AND b.BENE_SMI_CVRAGE_TOT_MONS = 12) AS Prior_FFS_Full
        FROM PL027710.NSTEMI_Threshold AS t
        INNER JOIN MBSF.MBSF_ABCD_&year AS b
            ON t.BENE_ID = b.BENE_ID
        WHERE t.AMI_Year = &year + 1;
    QUIT;
%MEND read_prior_cov;

%MACRO read_all_prior_cov;
    %DO yr = &year_start %TO %EVAL(&year_end - 1);
        %read_prior_cov(&yr);
    %END;
    DATA WORK.PriorCov;
        SET
        %DO yr = &year_start %TO %EVAL(&year_end - 1);
            WORK.PriorCov_&yr
        %END;
        ;
    RUN;
%MEND read_all_prior_cov;
%read_all_prior_cov;


/* ============================================================ */
/* Step 10b: Inpatient stays discharged in the 365 days before   */
/* ============================================================ */

/* Discharge date in the window, so a stay that runs into the index       */
/* admission (a transfer) is not counted as prior use.                    */

%MACRO extract_prior_ip(year);

    %stack_inpatient(&year);

    PROC SQL;
        CREATE TABLE WORK.PriorIP_&year AS
        SELECT a.BENE_ID,
               COUNT(*) AS Prior_IP_Stays,
               SUM(a.NCH_BENE_DSCHRG_DT - a.CLM_ADMSN_DT) AS Prior_IP_Days,
               SUM(a.CLM_PMT_AMT) AS Prior_IP_Pmt
        FROM WORK.InpatientStays_&year AS a
        INNER JOIN PL027710.NSTEMI_Threshold AS t
            ON a.BENE_ID = t.BENE_ID
        WHERE a.NCH_BENE_DSCHRG_DT >= t.CLM_ADMSN_DT - 365
          AND a.NCH_BENE_DSCHRG_DT < t.CLM_ADMSN_DT
        GROUP BY a.BENE_ID;
    QUIT;

    PROC DELETE DATA=WORK.InpatientStays_&year; RUN;

%MEND extract_prior_ip;

%MACRO extract_all_prior_ip;
    %DO yr = &year_start %TO &year_end;
        %extract_prior_ip(&yr);
    %END;
    DATA WORK.PriorIP_All;
        SET
        %DO yr = &year_start %TO &year_end;
            WORK.PriorIP_&yr
        %END;
        ;
    RUN;
%MEND extract_all_prior_ip;
%extract_all_prior_ip;

/* A window spans two calendar years, so sum the yearly pieces. */
PROC SQL;
    CREATE TABLE WORK.PriorIP AS
    SELECT BENE_ID,
           SUM(Prior_IP_Stays) AS Prior_IP_Stays,
           SUM(Prior_IP_Days)  AS Prior_IP_Days,
           SUM(Prior_IP_Pmt)   AS Prior_IP_Pmt
    FROM WORK.PriorIP_All
    GROUP BY BENE_ID;
QUIT;


/* ============================================================ */
/* Step 10c: Carrier claims in the 365 days before               */
/* ============================================================ */

/* Aggregated to the beneficiary inside each monthly file so WORK holds   */
/* at most one row per beneficiary per month. A carrier claim sits in one */
/* monthly file, so distinct claim counts add across months.              */

%MACRO extract_prior_carrier(year);
    %DO m = 1 %TO 12;
        PROC SQL;
            CREATE TABLE WORK.PriorCar_m&m._&year AS
            SELECT a.BENE_ID,
                   COUNT(DISTINCT a.CLM_ID) AS Prior_Carrier_Clms,
                   SUM(a.LINE_NCH_PMT_AMT)  AS Prior_Carrier_Pmt
            FROM RIF&year..BCARRIER_LINE_%SYSFUNC(PUTN(&m, Z2.)) AS a
            INNER JOIN PL027710.NSTEMI_Threshold AS t
                ON a.BENE_ID = t.BENE_ID
            WHERE a.CLM_THRU_DT >= t.CLM_ADMSN_DT - 365
              AND a.CLM_THRU_DT < t.CLM_ADMSN_DT
            GROUP BY a.BENE_ID;
        QUIT;
    %END;

    DATA WORK.PriorCar_&year;
        SET
        %DO m = 1 %TO 12;
            WORK.PriorCar_m&m._&year
        %END;
        ;
    RUN;

    %DO m = 1 %TO 12;
        PROC DELETE DATA=WORK.PriorCar_m&m._&year; RUN;
    %END;
%MEND extract_prior_carrier;

%MACRO extract_all_prior_carrier;
    %DO yr = &year_start %TO &year_end;
        %extract_prior_carrier(&yr);
    %END;
    DATA WORK.PriorCar_All;
        SET
        %DO yr = &year_start %TO &year_end;
            WORK.PriorCar_&yr
        %END;
        ;
    RUN;
%MEND extract_all_prior_carrier;
%extract_all_prior_carrier;

PROC SQL;
    CREATE TABLE WORK.PriorCar AS
    SELECT BENE_ID,
           SUM(Prior_Carrier_Clms) AS Prior_Carrier_Clms,
           SUM(Prior_Carrier_Pmt)  AS Prior_Carrier_Pmt
    FROM WORK.PriorCar_All
    GROUP BY BENE_ID;
QUIT;


/* ============================================================ */
/* Step 10d: Save                                                */
/* ============================================================ */

PROC SQL;
    CREATE TABLE PL027710.NSTEMI_Prior AS
    SELECT t.BENE_ID,
           COALESCE(c.Prior_FFS_Full, 0)     AS Prior_FFS_Full,
           COALESCE(i.Prior_IP_Stays, 0)     AS Prior_IP_Stays,
           COALESCE(i.Prior_IP_Days, 0)      AS Prior_IP_Days,
           COALESCE(i.Prior_IP_Pmt, 0)       AS Prior_IP_Pmt,
           COALESCE(k.Prior_Carrier_Clms, 0) AS Prior_Carrier_Clms,
           COALESCE(k.Prior_Carrier_Pmt, 0)  AS Prior_Carrier_Pmt
    FROM PL027710.NSTEMI_Threshold AS t
    LEFT JOIN WORK.PriorCov AS c
        ON t.BENE_ID = c.BENE_ID
    LEFT JOIN WORK.PriorIP AS i
        ON t.BENE_ID = i.BENE_ID
    LEFT JOIN WORK.PriorCar AS k
        ON t.BENE_ID = k.BENE_ID;
QUIT;


/* ============================================================ */
/* Diagnostics                                                   */
/* ============================================================ */

TITLE "Pre-admission utilization by admission year (means over full prior-year FFS)";
PROC SQL;
    SELECT t.AMI_Year,
           COUNT(*) AS N_Episodes,
           COUNT(DISTINCT p.BENE_ID) AS N_Benes,
           MEAN(p.Prior_FFS_Full) AS Pct_Prior_FFS FORMAT=PERCENT8.1,
           MEAN(CASE WHEN p.Prior_FFS_Full THEN (p.Prior_IP_Stays > 0) ELSE . END)
               AS Pct_Any_IP FORMAT=PERCENT8.1,
           MEAN(CASE WHEN p.Prior_FFS_Full THEN p.Prior_IP_Stays ELSE . END)
               AS Mean_IP_Stays FORMAT=6.3,
           MEAN(CASE WHEN p.Prior_FFS_Full THEN p.Prior_IP_Pmt ELSE . END)
               AS Mean_IP_Pmt FORMAT=DOLLAR10.,
           MEAN(CASE WHEN p.Prior_FFS_Full THEN p.Prior_Carrier_Clms ELSE . END)
               AS Mean_Carrier_Clms FORMAT=6.1,
           MEAN(CASE WHEN p.Prior_FFS_Full THEN p.Prior_Carrier_Pmt ELSE . END)
               AS Mean_Carrier_Pmt FORMAT=DOLLAR10.
    FROM PL027710.NSTEMI_Prior AS p
    INNER JOIN PL027710.NSTEMI_Threshold AS t
        ON p.BENE_ID = t.BENE_ID
    GROUP BY t.AMI_Year
    ORDER BY t.AMI_Year;
QUIT;
TITLE;


/* ============================================================ */
/* Clean up WORK                                                 */
/* ============================================================ */

%MACRO cleanup_prior;
    %DO yr = &year_start %TO %EVAL(&year_end - 1);
        PROC DELETE DATA=WORK.PriorCov_&yr; RUN;
    %END;
    %DO yr = &year_start %TO &year_end;
        PROC DELETE DATA=WORK.PriorIP_&yr; RUN;
        PROC DELETE DATA=WORK.PriorCar_&yr; RUN;
    %END;
    PROC DELETE DATA=WORK.PriorCov; RUN;
    PROC DELETE DATA=WORK.PriorIP_All; RUN;
    PROC DELETE DATA=WORK.PriorIP; RUN;
    PROC DELETE DATA=WORK.PriorCar_All; RUN;
    PROC DELETE DATA=WORK.PriorCar; RUN;
%MEND cleanup_prior;
%cleanup_prior;
