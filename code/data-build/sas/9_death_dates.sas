/* ------------------------------------------------------------ */
/* TITLE:         Days from admission to death, threshold sample */
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  10/14/2026                                     */
/* CODE FILE ORDER: 9                                            */
/* INPUT:         PL027710.NSTEMI_Threshold                      */
/*                MBSF ABCD                                      */
/* OUTPUT:        PL027710.NSTEMI_Death                          */
/* EXPORT:        Manual via SAS EG -> CSV into the R container's */
/*                data/input/nstemi_death.csv                    */
/* ------------------------------------------------------------ */

/* One row per episode in the threshold file with the days from the    */
/* index admission to death (missing if no death is recorded). The     */
/* death date follows the rule in 8_patient_threshold.sas, the earliest */
/* non-missing BENE_DEATH_DT across the MBSF years, so the days line up */
/* with D_Death_30 and D_Death_365. Joined to the episode file by        */
/* BENE_ID in code/analysis/vrdc/1_threshold.R.                          */

%include "0_config.sas";


/* ============================================================ */
/* Step 9a: Death dates from the beneficiary summary             */
/* ============================================================ */

%MACRO read_death(year);
    PROC SQL;
        CREATE TABLE WORK.Death_&year AS
        SELECT b.BENE_ID, b.BENE_DEATH_DT
        FROM MBSF.MBSF_ABCD_&year AS b
        INNER JOIN PL027710.NSTEMI_Threshold AS t
            ON b.BENE_ID = t.BENE_ID
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
/* Step 9b: Days to death and save                               */
/* ============================================================ */

PROC SQL;
    CREATE TABLE PL027710.NSTEMI_Death AS
    SELECT t.BENE_ID,
           d.Death_Dt - t.CLM_ADMSN_DT AS Days_To_Death
    FROM PL027710.NSTEMI_Threshold AS t
    LEFT JOIN WORK.Death AS d
        ON t.BENE_ID = d.BENE_ID;
QUIT;


/* ============================================================ */
/* Diagnostics                                                   */
/* ============================================================ */

TITLE "Deaths by days from admission (0-2 days, and agreement with step 8 flags)";
PROC SQL;
    SELECT COUNT(*) AS N_Episodes,
           COUNT(DISTINCT n.BENE_ID) AS N_Benes,
           SUM(n.Days_To_Death NE . AND n.Days_To_Death <= 2) AS N_Death_D2,
           SUM((n.Days_To_Death NE . AND n.Days_To_Death <= 30) NE t.D_Death_30)
               AS N_Disagree_Death_30
    FROM PL027710.NSTEMI_Death AS n
    INNER JOIN PL027710.NSTEMI_Threshold AS t
        ON n.BENE_ID = t.BENE_ID;
QUIT;
TITLE;


/* ============================================================ */
/* Clean up WORK                                                 */
/* ============================================================ */

%MACRO cleanup_death;
    %DO yr = &year_start %TO &year_end;
        PROC DELETE DATA=WORK.Death_&yr; RUN;
    %END;
    PROC DELETE DATA=WORK.Death_All; RUN;
    PROC DELETE DATA=WORK.Death; RUN;
%MEND cleanup_death;
%cleanup_death;
