/* ------------------------------------------------------------ */
/* TITLE:         Configuration and shared macros                */
/* AUTHOR:        Ian McCarthy                                   */
/*                Emory University                               */
/* DATE CREATED:  4/3/2026                                       */
/* CODE FILE ORDER: 0 of 6                                       */
/* PURPOSE:       Global parameters, library refs, utility macros*/
/* ------------------------------------------------------------ */


/* ============================================================ */
/* Library References (verify in VRDC before running)            */
/* ============================================================ */
LIBNAME PL027710 "/workspace/pl027710";


/* ============================================================ */
/* Year Range                                                    */
/* ============================================================ */
%LET year_start = 2008;
%LET year_end   = 2018;

/* ICD transition date: October 1, 2015                          */
/* Pre-transition years use ICD-9-CM diagnosis + procedure codes */
/* Post-transition years use ICD-10-CM diagnosis + ICD-10-PCS    */
%LET icd10_start_date = '01OCT2015'd;
%LET last_icd9_year   = 2015;  /* claims in 2015 straddle both */

/* MDPPAS year mapping (verify availability in VRDC)             */
/* MDPPAS not available before 2009; use 2009 for 2008           */
%LET mdppas_2008 = 2009;  /* fallback */
%LET mdppas_2009 = 2009;
%LET mdppas_2010 = 2010;
%LET mdppas_2011 = 2011;
%LET mdppas_2012 = 2012;
%LET mdppas_2013 = 2013;
%LET mdppas_2014 = 2014;
%LET mdppas_2015 = 2015;
%LET mdppas_2016 = 2016;
%LET mdppas_2017 = 2017;
%LET mdppas_2018 = 2017;  /* fallback — verify if 2018 available */


/* ============================================================ */
/* NSTEMI Diagnosis Codes                                        */
/* ============================================================ */

/* ICD-9-CM:  410.71 (NSTEMI, initial episode)                   */
/*            410.70 (NSTEMI, unspecified episode)                */
/*            410.72 (NSTEMI, subsequent episode) — exclude       */
/* ICD-10-CM: I21.4  (NSTEMI)                                    */
/*            I21.9  (AMI unspecified) — exclude                  */
/*            I22.*  (subsequent MI) — exclude                    */

/* For broader AMI cohort (used in carrier lookback):            */
/* ICD-9-CM:  410.x (AMI, any)                                  */
/* ICD-10-CM: I21.x (AMI, any)                                  */


/* ============================================================ */
/* Cardiac Cath Procedure Codes                                  */
/* ============================================================ */

/* ICD-9-CM Procedure Codes (pre-Oct 2015)                       */
/* Source: Shirley Cai lookup file (cardiac-cath-icd10cm.csv)    */
%LET icd9_cath_codes = "3722","3723";
%LET icd9_angio_codes = "8855","8856","8857";
%LET icd9_pci_codes = "0066","3601","3602","3605","3609";
%LET icd9_stent_codes = "3606","3607";
%LET icd9_cabg_codes = "3610","3611","3612","3613","3614",
                        "3615","3616","3617","3618","3619";

/* Combined ICD-9 invasive procedure codes (any cath/PCI/CABG)   */
%LET icd9_invasive_codes = &icd9_cath_codes,&icd9_angio_codes,
                           &icd9_pci_codes,&icd9_stent_codes,
                           &icd9_cabg_codes;

/* ICD-10-PCS Procedure Codes (post-Oct 2015)                    */
/* Source: Shirley Cai lookup file (cardiac-cath-icd10cm.csv)    */
/* Exhaustive code lists — no prefix matching.                   */
%LET icd10_cath_codes = "4A020N7","4A020N8","4A023N7","4A023N8";

%LET icd10_angio_codes = "B2000ZZ","B2001ZZ","B200YZZ","B2010ZZ",
                          "B2011ZZ","B201YZZ","B2100ZZ","B2101ZZ",
                          "B210YZZ","B2110ZZ","B2111ZZ","B211YZZ";

%LET icd10_pci_codes = "02703ZZ","02704ZZ","02713ZZ","02714ZZ",
                        "02723ZZ","02724ZZ","02733ZZ","02734ZZ",
                        "02C03ZZ","02C04ZZ","02C13ZZ","02C14ZZ",
                        "02C23ZZ","02C24ZZ","02C33ZZ","02C34ZZ";

/* No ICD-10-PCS stent codes (stenting coded under PCI in ICD-10) */

%LET icd10_cabg_codes = "0210093","0210098","0210099","021009C",
    "021009F","021009W","02100A3","02100A8","02100A9","02100AC",
    "02100AF","02100AW","02100J3","02100J8","02100J9","02100JC",
    "02100JF","02100JW","02100K3","02100K8","02100K9","02100KC",
    "02100KF","02100KW","02100Z3","02100Z8","02100Z9","02100ZC",
    "02100ZF","0210493","0210498","0210499","021049C","021049F",
    "021049W","02104A3","02104A8","02104A9","02104AC","02104AF",
    "02104AW","02104J3","02104J8","02104J9","02104JC","02104JF",
    "02104JW","02104K3","02104K8","02104K9","02104KC","02104KF",
    "02104KW","02104Z3","02104Z8","02104Z9","02104ZC","02104ZF",
    "0211098","0211099","021109C","021109W","02110A8","02110A9",
    "02110AC","02110AW","02110J8","02110J9","02110JC","02110JW",
    "02110K8","02110K9","02110KC","02110KW","02110Z8","02110Z9",
    "02110ZC","0211498","0211499","021149C","021149W","02114A8",
    "02114A9","02114AC","02114AW","02114J8","02114J9","02114JC",
    "02114JW","02114K8","02114K9","02114KC","02114KW","02114Z8",
    "02114Z9","02114ZC","021209C","021209W","02120AC","02120AW",
    "02120JC","02120JW","02120KC","02120KW","02120ZC","021249C",
    "021249W","02124AC","02124AW","02124JC","02124JW","02124KC",
    "02124KW","02124ZC","021309C","021309W","02130AC","02130AW",
    "02130JC","02130JW","02130KC","02130KW","02130ZC","021349C",
    "021349W","02134AC","02134AW","02134JC","02134JW","02134KC",
    "02134KW","02134ZC";

/* Combined ICD-10 invasive procedure codes                      */
%LET icd10_invasive_codes = &icd10_cath_codes,&icd10_angio_codes,
                            &icd10_pci_codes,&icd10_cabg_codes;


/* ============================================================ */
/* Carrier HCPCS Codes for Cardiologist Billing                  */
/* ============================================================ */

/* Inpatient E&M (care): initial hospital care                   */
%LET hcpcs_care = "99221","99222","99223";

/* Inpatient consultation                                        */
/* NOTE: CMS eliminated consult codes effective 1/1/2010.        */
/* For 2008-2009, consult codes are valid. For 2010+, consult    */
/* billing shifted to initial hospital care (99221-99223).       */
/* Molitor hierarchy still applies: prefer consult if present.   */
%LET hcpcs_consult = "99251","99252","99253","99254","99255";


/* ============================================================ */
/* Minimum Cell Size for Export                                  */
/* ============================================================ */
%LET min_patients = 11;  /* CMS minimum; also a statistical floor */


/* ============================================================ */
/* MDPPAS Library Reference                                      */
/* ============================================================ */
LIBNAME MD_PPAS "/workspace/md_ppas";


/* ============================================================ */
/* Utility: Stack Inpatient Claims for a Year                    */
/* ============================================================ */

%MACRO stack_inpatient(year);
    DATA WORK.InpatientStays_&year;
        SET RIF&year..INPATIENT_CLAIMS_01
            RIF&year..INPATIENT_CLAIMS_02
            RIF&year..INPATIENT_CLAIMS_03
            RIF&year..INPATIENT_CLAIMS_04
            RIF&year..INPATIENT_CLAIMS_05
            RIF&year..INPATIENT_CLAIMS_06
            RIF&year..INPATIENT_CLAIMS_07
            RIF&year..INPATIENT_CLAIMS_08
            RIF&year..INPATIENT_CLAIMS_09
            RIF&year..INPATIENT_CLAIMS_10
            RIF&year..INPATIENT_CLAIMS_11
            RIF&year..INPATIENT_CLAIMS_12;
    RUN;
%MEND stack_inpatient;


/* ============================================================ */
/* Elixhauser Comorbidity Format                                 */
/* ============================================================ */

/* Maps ICD-9-CM and ICD-10-CM diagnosis codes to Elixhauser     */
/* comorbidity categories. Source: Shirley Cai lookup file        */
/* (elixhauser-icd10cm.csv), originally from                     */
/* doi:10.3988/jcn.2017.13.4.351 with NBER ICD-9→10 crosswalk.  */
/* Used via PUT(code, $ICD9_ELIX.) or PUT(code, $ICD10_ELIX.)   */
/* in DATA steps. Returns category name or blank if no match.    */

/* The lookup is a SAS dataset built from DATALINES in a separate */
/* include file. A code can map to multiple categories (e.g.,    */
/* 40201 = chf + htn_cx). Upload elix_lookup.sas alongside       */
/* these scripts to the VRDC.                                    */
%INCLUDE "/workspace/pl027710/elix_lookup.sas";
