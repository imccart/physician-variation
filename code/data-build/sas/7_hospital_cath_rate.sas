/* 7_hospital_cath_rate.sas
   Hospital-year NSTEMI cath-within-2-days rate panel for the residency-
   cath-rate analysis on recent-graduate cardiologists.

   For each (PRVDR_NUM, year) cell with sufficient volume, compute the
   share of NSTEMI admissions catheterized within two days. Recent-grad
   cardiologists (graduation year >= 2006) have residency periods that
   overlap our 2008-2018 claims panel, so this hospital-year panel lets us
   measure each trainee's actual exposure to procedural cardiology during
   training, rather than relying on the AHA binary cath-lab indicator.

   Inputs:  PL027710.NSTEMI_Patients (PRVDR_NUM, CLM_ADMSN_DT, D_Cath_D2)
   Outputs: PL027710.Hospital_Year_Cath  (full panel, stays in SAS)
            PL027710.Hospital_Year_Cath_Export  (volume-filtered for export)
   Export:  Manual via SAS EG -> CSV into the R container's data/input/
*/

%include "0_config.sas";


/* ------------------------------------------------------------------------
   1. Aggregate to hospital-year
   ------------------------------------------------------------------------ */

PROC SQL;
    CREATE TABLE PL027710.Hospital_Year_Cath AS
    SELECT
        PRVDR_NUM                  AS prvdr_num    LENGTH=6,
        YEAR(CLM_ADMSN_DT)         AS year         FORMAT=4.,
        COUNT(*)                   AS n_nstemi,
        SUM(D_Cath_D2)             AS n_cath_d2,
        MEAN(D_Cath_D2)            AS rate_cath_d2 FORMAT=6.4
    FROM PL027710.NSTEMI_Patients
    WHERE PRVDR_NUM IS NOT NULL
      AND YEAR(CLM_ADMSN_DT) BETWEEN 2008 AND 2018
    GROUP BY 1, 2
    ORDER BY 1, 2;
QUIT;


/* ------------------------------------------------------------------------
   2. Cell-size filter for export (CMS minimum cell size = 11)
   ------------------------------------------------------------------------ */

DATA PL027710.Hospital_Year_Cath_Export;
    SET PL027710.Hospital_Year_Cath;
    IF n_nstemi >= 11;
RUN;


/* ------------------------------------------------------------------------
   3. Diagnostics
   ------------------------------------------------------------------------ */

PROC SQL;
    SELECT  "All cells"  AS label,
            COUNT(*)     AS n_cells,
            COUNT(DISTINCT prvdr_num) AS n_hospitals,
            MEAN(rate_cath_d2)        AS mean_rate FORMAT=6.4
    FROM PL027710.Hospital_Year_Cath
    UNION
    SELECT  "Export (n_nstemi>=11)",
            COUNT(*),
            COUNT(DISTINCT prvdr_num),
            MEAN(rate_cath_d2) FORMAT=6.4
    FROM PL027710.Hospital_Year_Cath_Export;
QUIT;

PROC FREQ DATA=PL027710.Hospital_Year_Cath_Export;
    TABLES year / NOROW NOCOL NOPERCENT;
    TITLE "Hospital-year export rows by year";
RUN;
TITLE;


/*  Manual export step (right-click Hospital_Year_Cath_Export in SAS EG):
    -> Export -> CSV
    Save as hospital_year_cath.csv in the R container's data/input/.
    Columns: prvdr_num, year, n_nstemi, n_cath_d2, rate_cath_d2
*/
