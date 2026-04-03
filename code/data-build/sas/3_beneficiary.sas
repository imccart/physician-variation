/* ------------------------------------------------------------ */
/* TITLE:         Merge beneficiary demographics and coverage    */
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  4/3/2026                                       */
/* CODE FILE ORDER: 3 of 6                                       */
/* INPUT:         Beneficiary Summary Files (2008-2018),         */
/*                PL027710.NSTEMI_Episodes,                      */
/*                PL027710.NSTEMI_Cath,                          */
/*                PL027710.NSTEMI_Comorbidities                  */
/* OUTPUT:        PL027710.NSTEMI_Patients                       */
/* ------------------------------------------------------------ */

/* Merge beneficiary demographics with NSTEMI episodes.          */
/* Filter to Part B FFS enrollees (12 months SMI, 0 months HMO). */
/* Join cath indicators and comorbidities into one patient file. */


/* ============================================================ */
/* Step 3a: Read and stack beneficiary summary files             */
/* ============================================================ */

%MACRO read_beneficiary(year);
    PROC SQL;
        CREATE TABLE WORK.Bene_&year AS
        SELECT
            BENE_ID,
            BENE_ENROLLMT_REF_YR,
            AGE_AT_END_REF_YR,
            BENE_RACE_CD,
            SEX_IDENT_CD,
            ZIP_CD,
            BENE_DEATH_DT,
            COVSTART,
            DUAL_ELGBL_MONS,
            BENE_HI_CVRAGE_TOT_MONS,
            BENE_SMI_CVRAGE_TOT_MONS,
            BENE_HMO_CVRAGE_TOT_MONS
        FROM RIF&year..BENEFICIARY_SUMMARY
        WHERE BENE_SMI_CVRAGE_TOT_MONS = 12  /* Full year Part B */
          AND BENE_HMO_CVRAGE_TOT_MONS = 0;  /* No HMO (FFS only) */
    QUIT;
%MEND read_beneficiary;

%MACRO read_all_bene;
    %DO yr = &year_start %TO &year_end;
        %read_beneficiary(&yr);
    %END;
%MEND read_all_bene;
%read_all_bene;

/* Stack all years */
DATA WORK.Bene_All;
    SET
    %DO yr = &year_start %TO &year_end;
        WORK.Bene_&yr
    %END;
    ;
RUN;


/* ============================================================ */
/* Step 3b: Merge episodes + beneficiary + cath + comorbidities  */
/* ============================================================ */

PROC SQL;
    CREATE TABLE PL027710.NSTEMI_Patients AS
    SELECT
        /* Episode identifiers */
        e.BENE_ID,
        e.CLM_ID,
        e.AMI_Year,
        e.CLM_ADMSN_DT,
        e.NCH_BENE_DSCHRG_DT,
        e.PRVDR_NUM,
        e.ORG_NPI_NUM,
        e.AT_PHYSN_NPI,
        e.OP_PHYSN_NPI,
        e.OT_PHYSN_NPI,
        e.PTNT_DSCHRG_STUS_CD,

        /* Beneficiary demographics */
        b.AGE_AT_END_REF_YR AS Age,
        b.BENE_RACE_CD,
        b.SEX_IDENT_CD,
        b.ZIP_CD AS Bene_Zip,
        b.DUAL_ELGBL_MONS,
        CASE WHEN b.BENE_RACE_CD = "2" THEN 1 ELSE 0 END AS D_Black,
        CASE WHEN b.BENE_RACE_CD = "5" THEN 1 ELSE 0 END AS D_Hisp,
        CASE WHEN b.BENE_RACE_CD = "4" THEN 1 ELSE 0 END AS D_Asian,
        CASE WHEN b.BENE_RACE_CD = "0" THEN 1 ELSE 0 END AS D_Race_Missing,
        CASE WHEN b.SEX_IDENT_CD = "2" THEN 1 ELSE 0 END AS D_Female,
        CASE WHEN b.SEX_IDENT_CD = "0" THEN 1 ELSE 0 END AS D_Sex_Missing,
        CASE WHEN b.DUAL_ELGBL_MONS > 0 THEN 1 ELSE 0 END AS D_Dual,

        /* Cath indicators */
        c.D_Cath_Any,
        c.D_Cath_D0,
        c.D_Cath_D1,
        c.D_Cath_D2,
        c.D_Cath_D3,
        c.D_Cath_D7,
        c.D_Cath_D30,
        c.D_Cath_D90,
        c.Time_To_Cath,

        /* Comorbidities (all 31 Elixhauser categories) */
        COALESCE(m.chf, 0) AS chf,
        COALESCE(m.car, 0) AS car,
        COALESCE(m.val, 0) AS val,
        COALESCE(m.pcd, 0) AS pcd,
        COALESCE(m.pvd, 0) AS pvd,
        COALESCE(m.htn_uncx, 0) AS htn_uncx,
        COALESCE(m.htn_cx, 0) AS htn_cx,
        COALESCE(m.par, 0) AS par,
        COALESCE(m.neu, 0) AS neu,
        COALESCE(m.cpd, 0) AS cpd,
        COALESCE(m.diab_uncx, 0) AS diab_uncx,
        COALESCE(m.diab_cx, 0) AS diab_cx,
        COALESCE(m.thy_hypo, 0) AS thy_hypo,
        COALESCE(m.renal_fl, 0) AS renal_fl,
        COALESCE(m.liver, 0) AS liver,
        COALESCE(m.pud, 0) AS pud,
        COALESCE(m.aids, 0) AS aids,
        COALESCE(m.lymph, 0) AS lymph,
        COALESCE(m.mets, 0) AS mets,
        COALESCE(m.tumor, 0) AS tumor,
        COALESCE(m.rheu, 0) AS rheu,
        COALESCE(m.coag, 0) AS coag,
        COALESCE(m.obese, 0) AS obese,
        COALESCE(m.wgt_loss, 0) AS wgt_loss,
        COALESCE(m.fluid, 0) AS fluid,
        COALESCE(m.anemia_blood, 0) AS anemia_blood,
        COALESCE(m.anemia_def, 0) AS anemia_def,
        COALESCE(m.alcohol, 0) AS alcohol,
        COALESCE(m.drug, 0) AS drug,
        COALESCE(m.psychoses, 0) AS psychoses,
        COALESCE(m.depression, 0) AS depression

    FROM PL027710.NSTEMI_Episodes AS e
    INNER JOIN WORK.Bene_All AS b
        ON e.BENE_ID = b.BENE_ID
        AND e.AMI_Year = b.BENE_ENROLLMT_REF_YR
    LEFT JOIN PL027710.NSTEMI_Cath AS c
        ON e.BENE_ID = c.BENE_ID
    LEFT JOIN PL027710.NSTEMI_Comorbidities AS m
        ON e.BENE_ID = m.BENE_ID;
QUIT;


/* ============================================================ */
/* Diagnostics                                                   */
/* ============================================================ */

TITLE "NSTEMI patients after beneficiary merge";
PROC SQL;
    SELECT
        COUNT(*) AS N_Patients,
        COUNT(DISTINCT BENE_ID) AS N_Benes,
        SUM(D_Cath_D2) AS N_Cath_D2,
        MEAN(Age) AS Mean_Age FORMAT=5.1,
        MEAN(D_Female) AS Pct_Female FORMAT=PERCENT8.1,
        MEAN(D_Black) AS Pct_Black FORMAT=PERCENT8.1,
        MEAN(D_Dual) AS Pct_Dual FORMAT=PERCENT8.1
    FROM PL027710.NSTEMI_Patients;
QUIT;

TITLE "Patient counts by year (post-merge)";
PROC SQL;
    SELECT AMI_Year,
           COUNT(*) AS N,
           MEAN(D_Cath_D2) AS Pct_Cath_D2 FORMAT=PERCENT8.1
    FROM PL027710.NSTEMI_Patients
    GROUP BY AMI_Year
    ORDER BY AMI_Year;
QUIT;
TITLE;


/* ============================================================ */
/* Clean up WORK                                                 */
/* ============================================================ */
%MACRO cleanup_bene;
    %DO yr = &year_start %TO &year_end;
        PROC DELETE DATA=WORK.Bene_&yr; RUN;
    %END;
    PROC DELETE DATA=WORK.Bene_All; RUN;
%MEND cleanup_bene;
%cleanup_bene;
