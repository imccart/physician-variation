/* ------------------------------------------------------------ */
/* TITLE:         Predict cath and compute residual intensity    */
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  4/3/2026                                       */
/* CODE FILE ORDER: 5 of 6                                       */
/* INPUT:         PL027710.NSTEMI_Patients                       */
/* OUTPUT:        PL027710.NSTEMI_Residualized                   */
/* ------------------------------------------------------------ */

/* Predict cardiac cath (within 2 days) from patient demographics, */
/* comorbidities, and year fixed effects. The residual captures    */
/* the component of treatment intensity not explained by patient   */
/* characteristics — i.e., the physician's contribution.           */


/* ============================================================ */
/* Step 5a: Prepare regression data                              */
/* ============================================================ */

DATA WORK.RegData;
    SET PL027710.NSTEMI_Patients;
    /* Ensure numeric types for regression */
    Age_Sq = Age * Age;
RUN;

/* Check completeness */
TITLE "Regression data: missingness check";
PROC MEANS DATA=WORK.RegData N NMISS;
    VAR D_Cath_D2 Age Age_Sq D_Female D_Black D_Hisp D_Asian
        D_Dual Dual_Elgbl_Mons
        chf car val pcd pvd htn_uncx htn_cx par neu cpd
        diab_uncx diab_cx thy_hypo renal_fl liver pud aids lymph mets tumor
        rheu coag obese wgt_loss fluid anemia_blood anemia_def
        alcohol drug psychoses depression;
RUN;
TITLE;


/* ============================================================ */
/* Step 5b: OLS prediction model                                 */
/* ============================================================ */

/* D_Cath_D2 = f(demographics, comorbidities) + year FE          */
/* This is a linear probability model (consistent with Shirley's  */
/* approach and standard in this literature).                     */

PROC GLM DATA=WORK.RegData;
    CLASS AMI_Year;
    MODEL D_Cath_D2 = Age Age_Sq D_Female D_Black D_Hisp D_Asian
                      D_Race_Missing D_Sex_Missing
                      D_Dual Dual_Elgbl_Mons
                      chf car val pcd pvd htn_uncx htn_cx par neu cpd
                      diab_uncx diab_cx thy_hypo renal_fl liver pud aids
                      lymph mets tumor rheu coag obese wgt_loss fluid
                      anemia_blood anemia_def alcohol drug psychoses depression
                      AMI_Year
                      / SOLUTION;
    OUTPUT OUT=WORK.RegOutput PREDICTED=Pred_Cath RESIDUAL=Resid_Cath;
RUN;
QUIT;


/* ============================================================ */
/* Step 5c: Save residualized data                               */
/* ============================================================ */

DATA PL027710.NSTEMI_Residualized;
    SET WORK.RegOutput (KEEP = BENE_ID AMI_Year CLM_ADMSN_DT
                               D_Cath_D2 Pred_Cath Resid_Cath
                               Age D_Female D_Black D_Hisp D_Asian
                               D_Race_Missing D_Sex_Missing D_Dual
                               Dual_Elgbl_Mons
                               chf car val pcd pvd htn_uncx htn_cx par neu cpd
                               diab_uncx diab_cx thy_hypo renal_fl liver pud aids
                               lymph mets tumor rheu coag obese wgt_loss fluid
                               anemia_blood anemia_def alcohol drug psychoses depression
                               D_Cath_D0 D_Cath_D1 D_Cath_D3
                               D_Cath_D7 D_Cath_D30 D_Cath_D90);
RUN;


/* ============================================================ */
/* Diagnostics                                                   */
/* ============================================================ */

TITLE "Residualization model: predicted vs actual cath rate";
PROC MEANS DATA=PL027710.NSTEMI_Residualized MEAN STD MIN MAX;
    VAR D_Cath_D2 Pred_Cath Resid_Cath;
RUN;
TITLE;

TITLE "Residual distribution by year";
PROC MEANS DATA=PL027710.NSTEMI_Residualized MEAN STD;
    CLASS AMI_Year;
    VAR Resid_Cath;
RUN;
TITLE;


/* ============================================================ */
/* Clean up WORK                                                 */
/* ============================================================ */
PROC DELETE DATA=WORK.RegData; RUN;
PROC DELETE DATA=WORK.RegOutput; RUN;
