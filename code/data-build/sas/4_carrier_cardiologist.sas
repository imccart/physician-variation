/* ------------------------------------------------------------ */
/* TITLE:         Link NSTEMI episodes to treating cardiologist  */
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  4/3/2026                                       */
/* CODE FILE ORDER: 4 of 6                                       */
/* INPUT:         Carrier RIF (100%, 2008-2018),                 */
/*                MD_PPAS (cardiologist filter),                  */
/*                PL027710.NSTEMI_Benes                          */
/* OUTPUT:        PL027710.NSTEMI_Cardiologist                   */
/* ------------------------------------------------------------ */

/* Identify the cardiologist "making the call" for each NSTEMI   */
/* episode using carrier claims, following Molitor (2018).        */
/*                                                               */
/* Hierarchy:                                                    */
/*   1. First cardiologist billed for inpatient consult           */
/*   2. First cardiologist billed for inpatient care              */
/*   3. First cardiologist seen (any carrier claim)              */
/*                                                               */
/* NOTE: CMS eliminated consult codes effective 1/1/2010.        */
/* For 2008-2009, the consult tier is populated. For 2010+,      */
/* consult billing shifted to initial hospital care codes, so    */
/* the hierarchy effectively becomes: first care > first seen.   */


/* ============================================================ */
/* Step 4a: Build cardiologist NPI list from MDPPAS               */
/* ============================================================ */

/* Use MDPPAS specialty name to identify cardiologists.           */
/* Stack across years to get the union of all cardiologist NPIs. */

%MACRO build_cardio_list;
    %DO yr = &year_start %TO &year_end;
        PROC SQL;
            CREATE TABLE WORK.Cardio_&yr AS
            SELECT DISTINCT NPI
            FROM MD_PPAS.MDPPAS_V24_&&mdppas_&yr
            WHERE SPEC_PRIM_1_NAME IN ("Cardiology",
                                       "Clinical Cardiac Electrophysiology",
                                       "Interventional Cardiology",
                                       "Advanced Heart Failure and Transplant Cardiology");
        QUIT;
    %END;

    /* Union across all years */
    DATA WORK.Cardiologist_NPIs;
        SET
        %DO yr = &year_start %TO &year_end;
            WORK.Cardio_&yr
        %END;
        ;
    RUN;

    PROC SORT DATA=WORK.Cardiologist_NPIs NODUPKEY;
        BY NPI;
    RUN;

    %DO yr = &year_start %TO &year_end;
        PROC DELETE DATA=WORK.Cardio_&yr; RUN;
    %END;
%MEND build_cardio_list;
%build_cardio_list;

TITLE "Cardiologist NPI count";
PROC SQL;
    SELECT COUNT(*) AS N_Cardiologists FROM WORK.Cardiologist_NPIs;
QUIT;
TITLE;


/* ============================================================ */
/* Step 4b: Extract carrier claims for NSTEMI patients           */
/* ============================================================ */

/* For each year, extract carrier line items where:              */
/*   - Patient is in the NSTEMI bene list                        */
/*   - Performing physician is a cardiologist (from MDPPAS)      */
/*   - Claim date is within 90 days of AMI admission             */

%MACRO extract_cardio_carrier(year);
    %DO m = 1 %TO 12;
        PROC SQL;
            CREATE TABLE WORK.CardCarrier_m&m._&year AS
            SELECT
                a.BENE_ID,
                a.PRF_PHYSN_NPI,
                a.CLM_THRU_DT,
                a.HCPCS_CD,
                b.AMI_Admsn_Dt,
                a.CLM_THRU_DT - b.AMI_Admsn_Dt AS Days_From_AMI
            FROM RIF&year..BCARRIER_LINE_%SYSFUNC(PUTN(&m, Z2.)) AS a
            INNER JOIN PL027710.NSTEMI_Benes AS b
                ON a.BENE_ID = b.BENE_ID
            INNER JOIN WORK.Cardiologist_NPIs AS c
                ON a.PRF_PHYSN_NPI = c.NPI
            WHERE a.CLM_THRU_DT >= b.AMI_Admsn_Dt
              AND a.CLM_THRU_DT - b.AMI_Admsn_Dt <= 90
              AND a.PRF_PHYSN_NPI NE ''
              AND a.PRF_PHYSN_NPI NE '0000000000';
        QUIT;
    %END;

    /* Stack monthly */
    DATA WORK.CardCarrier_&year;
        SET
        %DO m = 1 %TO 12;
            WORK.CardCarrier_m&m._&year
        %END;
        ;
    RUN;

    /* Clean up monthly tables */
    %DO m = 1 %TO 12;
        PROC DELETE DATA=WORK.CardCarrier_m&m._&year; RUN;
    %END;

%MEND extract_cardio_carrier;

%MACRO extract_all_cardio_carrier;
    %DO yr = &year_start %TO &year_end;
        %extract_cardio_carrier(&yr);
    %END;
%MEND extract_all_cardio_carrier;
%extract_all_cardio_carrier;

/* Stack all years */
%MACRO stack_cardcarrier;
DATA WORK.CardCarrier_All;
    SET
    %DO yr = &year_start %TO &year_end;
        WORK.CardCarrier_&yr
    %END;
    ;
RUN;
%MEND stack_cardcarrier;
%stack_cardcarrier;


/* ============================================================ */
/* Step 4c: Flag care vs. consult claims                         */
/* ============================================================ */

DATA WORK.CardCarrier_Flagged;
    SET WORK.CardCarrier_All;
    /* Consult: 99251-99255 (only valid pre-2010, but flag anyway) */
    D_Consult = (HCPCS_CD IN (&hcpcs_consult));
    /* Care: 99221-99223 (initial hospital care) */
    D_Care = (HCPCS_CD IN (&hcpcs_care));
RUN;


/* ============================================================ */
/* Step 4d: Molitor assignment — one cardiologist per episode    */
/* ============================================================ */

/* First, keep earliest claim per cardiologist per episode */
PROC SORT DATA=WORK.CardCarrier_Flagged;
    BY BENE_ID PRF_PHYSN_NPI CLM_THRU_DT;
RUN;

DATA WORK.CardCarrier_First;
    SET WORK.CardCarrier_Flagged;
    BY BENE_ID PRF_PHYSN_NPI;
    IF FIRST.PRF_PHYSN_NPI;
RUN;

/* Build each tier separately, then combine with COALESCE.       */
/* Tier 1: earliest consulting cardiologist per episode          */
PROC SORT DATA=WORK.CardCarrier_First OUT=WORK.Consult_Sort;
    WHERE D_Consult = 1;
    BY BENE_ID CLM_THRU_DT;
RUN;
DATA WORK.First_Consult (KEEP = BENE_ID NPI_First_Consult);
    SET WORK.Consult_Sort;
    BY BENE_ID;
    IF FIRST.BENE_ID;
    RENAME PRF_PHYSN_NPI = NPI_First_Consult;
RUN;

/* Tier 2: earliest care cardiologist per episode                */
PROC SORT DATA=WORK.CardCarrier_First OUT=WORK.Care_Sort;
    WHERE D_Care = 1;
    BY BENE_ID CLM_THRU_DT;
RUN;
DATA WORK.First_Care (KEEP = BENE_ID NPI_First_Care);
    SET WORK.Care_Sort;
    BY BENE_ID;
    IF FIRST.BENE_ID;
    RENAME PRF_PHYSN_NPI = NPI_First_Care;
RUN;

/* Tier 3: earliest seen cardiologist (any claim)                */
PROC SORT DATA=WORK.CardCarrier_First OUT=WORK.Seen_Sort;
    BY BENE_ID CLM_THRU_DT;
RUN;
DATA WORK.First_Seen (KEEP = BENE_ID NPI_First_Seen);
    SET WORK.Seen_Sort;
    BY BENE_ID;
    IF FIRST.BENE_ID;
    RENAME PRF_PHYSN_NPI = NPI_First_Seen;
RUN;

/* Combine: consult > care > first seen                          */
PROC SQL;
    CREATE TABLE PL027710.NSTEMI_Cardiologist AS
    SELECT
        s.BENE_ID,
        c.NPI_First_Consult,
        k.NPI_First_Care,
        COALESCE(c.NPI_First_Consult,
                 k.NPI_First_Care,
                 s.NPI_First_Seen) AS NPI_Gatekeeper
    FROM WORK.First_Seen AS s
    LEFT JOIN WORK.First_Consult AS c
        ON s.BENE_ID = c.BENE_ID
    LEFT JOIN WORK.First_Care AS k
        ON s.BENE_ID = k.BENE_ID;
QUIT;


/* ============================================================ */
/* Diagnostics                                                   */
/* ============================================================ */

TITLE "Cardiologist assignment summary";
PROC SQL;
    SELECT
        COUNT(*) AS N_Episodes_With_Cardio,
        COUNT(DISTINCT NPI_Gatekeeper) AS N_Unique_Cardiologists,
        SUM(CASE WHEN NPI_First_Consult IS NOT NULL THEN 1 ELSE 0 END) AS N_Consult,
        SUM(CASE WHEN NPI_First_Consult IS NULL AND NPI_First_Care IS NOT NULL THEN 1 ELSE 0 END) AS N_Care_Only,
        SUM(CASE WHEN NPI_First_Consult IS NULL AND NPI_First_Care IS NULL THEN 1 ELSE 0 END) AS N_First_Seen_Only
    FROM PL027710.NSTEMI_Cardiologist;
QUIT;
TITLE;


/* ============================================================ */
/* Clean up WORK                                                 */
/* ============================================================ */
%MACRO cleanup_carrier;
    %DO yr = &year_start %TO &year_end;
        PROC DELETE DATA=WORK.CardCarrier_&yr; RUN;
    %END;
    PROC DELETE DATA=WORK.CardCarrier_All; RUN;
    PROC DELETE DATA=WORK.CardCarrier_Flagged; RUN;
    PROC DELETE DATA=WORK.CardCarrier_First; RUN;
    PROC DELETE DATA=WORK.Consult_Sort; RUN;
    PROC DELETE DATA=WORK.Care_Sort; RUN;
    PROC DELETE DATA=WORK.Seen_Sort; RUN;
    PROC DELETE DATA=WORK.First_Consult; RUN;
    PROC DELETE DATA=WORK.First_Care; RUN;
    PROC DELETE DATA=WORK.First_Seen; RUN;
    PROC DELETE DATA=WORK.Cardiologist_NPIs; RUN;
%MEND cleanup_carrier;
%cleanup_carrier;
