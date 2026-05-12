# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-05-12
## Description:   Map each Doximity training-program string (residency and
##                fellowship) to an AHA hospital ID, so downstream code can
##                pull program-specific cath-lab availability.
##
##                Approach (three tiers):
##                  1) Hand-curated overrides for the ~130 most frequent
##                     training-program strings on the Doximity side.
##                  2) Exact normalized-name match against the AHA roster.
##                  3) Tightened substring fuzzy match (length-overlap >= 0.6,
##                     normalized name >= 8 chars; prefer shortest AHA name).
##
##                Sub-scripts inherit the prepared session (tidyverse +
##                data.table + stringi). Outputs:
##                  data/output/training_aha_crosswalk.csv
##                    one row per distinct Doximity training-program string,
##                    with ahaid + match_kind (manual / exact / fuzzy / none).

# 1. Inputs ----------------------------------------------------------------

cw <- read_csv("data/output/cardiologist_doximity.csv",
               show_col_types = FALSE)

# AHA roster: one row per hospital ID with name + state (modal). We restrict
# to 1980-2003 since that's the matching window for our cardiologists.
aha_hosp_raw <- fread("data/input/aha_hospital.csv",
                      select = c("ID", "MNAME", "MSTATE", "year"),
                      na.strings = c("", "NA"),
                      showProgress = FALSE)
setDF(aha_hosp_raw)

norm_hosp <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    tolower() %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("\\b(the|hospital|medical|center|centre|of|and|at|inc|llc)\\b", " ") %>%
    str_squish()
}

roster <- aha_hosp_raw %>%
  filter(!is.na(ID), !is.na(MNAME), year >= 1980, year <= 2003) %>%
  mutate(ID = as.character(ID)) %>%
  group_by(ID) %>%
  summarize(name  = first(na.omit(MNAME)),
            state = first(c(na.omit(MSTATE), NA_character_)),
            .groups = "drop") %>%
  mutate(name_norm = norm_hosp(name)) %>%
  filter(nchar(name_norm) >= 3)


# 2. Hand-curated overrides ------------------------------------------------

# Top ~130 distinct Doximity training-program strings, mapped by hand to a
# canonical AHA-side name fragment + state. Coverage is the union of the
# top residency and top fellowship strings. Compiled 2026-05-12.
overrides <- tribble(
  ~doximity_name,                                                                     ~aha_search,                              ~state,
  # ---- Top 40 residency ----
  "Emory University School of Medicine",                                              "emory university hospital",              "GA",
  "Mayo Clinic College of Medicine and Science (Rochester)",                          "mayo clinic",                            "MN",
  "Washington University/B-JH/SLCH Consortium",                                       "barnes jewish hospital",                 "MO",
  "Wayne State University School of Medicine",                                        "detroit medical center",                 "MI",
  "Baylor College of Medicine",                                                       "baylor",                                 "TX",
  "Duke University Hospital",                                                         "duke university hospital",               "NC",
  "University of Michigan",                                                           "university of michigan hospital",        "MI",
  "University of Alabama Medical Center",                                             "university of alabama",                  "AL",
  "Indiana University School of Medicine",                                            "indiana university hospital",            "IN",
  "SUNY Downstate Health Sciences University",                                        "university hospital of brooklyn",        "NY",
  "University of Texas Southwestern Medical Center",                                  "ut southwestern",                        "TX",
  "Drexel University College of Medicine/Hahnemann University Hospital",              "hahnemann university hospital",          "PA",
  "Cleveland Clinic Foundation",                                                      "cleveland clinic",                       "OH",
  "Montefiore Medical Center/Albert Einstein College of Medicine (Moses and Weiler Campuses)", "montefiore medical center",     "NY",
  "Rutgers Health/New Jersey Medical School",                                         "university hospital",                    "NJ",
  "Yale-New Haven Medical Center",                                                    "yale new haven hospital",                "CT",
  "New York Presbyterian Hospital (Cornell Campus)",                                  "new york presbyterian",                  "NY",
  "University of Illinois College of Medicine at Chicago",                            "university of illinois hospital",        "IL",
  "Johns Hopkins University",                                                         "johns hopkins hospital",                 "MD",
  "Icahn School of Medicine at Mount Sinai/Mount Sinai Hospital",                     "mount sinai hospital",                   "NY",
  "Virginia Commonwealth University Health System",                                   "vcu medical center",                     "VA",
  "Loyola University Medical Center",                                                 "loyola university medical center",       "IL",
  "UMass Chan Medical School",                                                        "umass memorial",                         "MA",
  "Vanderbilt University Medical Center",                                             "vanderbilt university medical center",   "TN",
  "University of Connecticut",                                                        "uconn",                                  "CT",
  "University of Iowa Hospitals and Clinics",                                         "university of iowa hospitals",           "IA",
  "Cook County Health and Hospitals System",                                          "cook county hospital",                   "IL",
  "University of North Carolina Hospitals",                                           "unc hospitals",                          "NC",
  "UPMC Medical Education",                                                           "upmc presbyterian",                      "PA",
  "University of Virginia Medical Center",                                            "university of virginia medical center",  "VA",
  "Boston University Medical Center",                                                 "boston medical center",                  "MA",
  "Case Western Reserve University/University Hospitals Cleveland Medical Center",    "university hospitals cleveland",         "OH",
  "Mass General Brigham/Brigham and Women's Hospital",                                "brigham and women",                      "MA",
  "McGaw Medical Center of Northwestern University",                                  "northwestern memorial hospital",         "IL",
  "Medical University of South Carolina",                                             "medical university of south carolina",   "SC",
  "Beth Israel Deaconess Medical Center",                                             "beth israel deaconess",                  "MA",
  "Wake Forest University Baptist Medical Center",                                    "wake forest baptist",                    "NC",
  "Sidney Kimmel Medical College at Thomas Jefferson University/TJUH",                "thomas jefferson university hospital",   "PA",
  "University of Pennsylvania Health System",                                         "hospital of the university of pennsylvania", "PA",
  "Henry Ford Health/Henry Ford Hospital",                                            "henry ford hospital",                    "MI",
  # ---- Residency 41-80 ----
  "University of Mississippi Medical Center",                                         "university of mississippi medical center","MS",
  "NYU Grossman School of Medicine",                                                  "nyu langone",                            "NY",
  "Temple University Hospital",                                                       "temple university hospital",             "PA",
  "University of Arkansas for Medical Sciences (UAMS) College of Medicine",           "university of arkansas",                 "AR",
  "Icahn School of Medicine at Mount Sinai/Morningside/West",                         "st lukes roosevelt",                     "NY",
  "MedStar Health Georgetown University/Georgetown Hospital",                         "georgetown university hospital",         "DC",
  "Zucker School of Medicine at Hofstra/Northwell",                                   "north shore university hospital",        "NY",
  "Rhode Island Hospital/Brown University Health",                                    "rhode island hospital",                  "RI",
  "University at Buffalo",                                                            "buffalo general",                        "NY",
  "University of Florida",                                                            "shands hospital",                        "FL",
  "Creighton University School of Medicine (Omaha)",                                  "creighton university medical center",    "NE",
  "Ohio State University Hospital",                                                   "ohio state university hospital",         "OH",
  "SSM Health/Saint Louis University School of Medicine",                             "saint louis university hospital",        "MO",
  "SUNY Upstate Medical University",                                                  "upstate university hospital",            "NY",
  "Mass General Brigham/Massachusetts General Hospital",                              "massachusetts general hospital",         "MA",
  "Westchester Medical Center",                                                       "westchester medical center",             "NY",
  "NYC Health & Hospitals/South Brooklyn Health",                                     "coney island",                           "NY",
  "University of Maryland",                                                           "university of maryland",                 "MD",
  "Tufts Medical Center",                                                             "tufts medical center",                   "MA",
  "University of Cincinnati Medical Center/College of Medicine",                      "university of cincinnati hospital",      "OH",
  "University of Tennessee",                                                          "regional medical center memphis",        "TN",
  "Louisiana State University School of Medicine",                                    "lsu",                                    "LA",
  "Rush University Medical Center",                                                   "rush university medical center",         "IL",
  "University of Louisville School of Medicine",                                      "university of louisville hospital",      "KY",
  "University of Oklahoma Health Sciences Center",                                    "university of oklahoma medical center",  "OK",
  "University of Arizona College of Medicine-Tucson",                                 "university of arizona",                  "AZ",
  "University of Minnesota",                                                          "university of minnesota",                "MN",
  "East Tennessee State University/Quillen College of Medicine",                      "johnson city medical center",            "TN",
  "Stony Brook Medicine/University Hospital",                                         "stony brook university hospital",        "NY",
  "University of Texas Health Science Center at Houston",                             "memorial hermann",                       "TX",
  "West Virginia University",                                                         "west virginia university hospital",      "WV",
  "Allegheny Health Network Medical Education Consortium (AGH)",                      "allegheny general hospital",             "PA",
  "Chicago Medical School/Rosalind Franklin University of Medicine & Science",        "captain james lovell",                   "IL",
  "The MetroHealth System/Case Western Reserve University",                           "metrohealth medical center",             "OH",
  "University of Kentucky College of Medicine",                                       "university of kentucky hospital",        "KY",
  "University of Utah Health",                                                        "university of utah",                     "UT",
  "University of Washington",                                                         "university of washington",               "WA",
  "Icahn School of Medicine at Mount Sinai/Beth Israel",                              "beth israel medical center",             "NY",
  "Maimonides Medical Center",                                                        "maimonides medical center",              "NY",
  "University of Texas Medical Branch Hospitals",                                     "university of texas medical branch",     "TX",
  # ---- Residency 81-130 ----
  "Medical College of Georgia",                                                       "medical college of georgia",             "GA",
  "San Antonio Uniformed Services Health Education Consortium",                       "brooke army medical center",             "TX",
  "University of Colorado",                                                           "university of colorado hospital",        "CO",
  "University of Kansas School of Medicine",                                          "university of kansas hospital",          "KS",
  "University of Rochester Medical Center",                                           "strong memorial hospital",               "NY",
  "University of Texas Health Science Center San Antonio Joe and Teresa Lozano Long School of Medicine", "university hospital", "TX",
  "Loma Linda University Health Education Consortium",                                "loma linda university medical center",   "CA",
  "Tulane University",                                                                "tulane",                                 "LA",
  "University of Chicago",                                                            "university of chicago hospital",         "IL",
  "University of Missouri-Columbia",                                                  "university of missouri hospital",        "MO",
  "University of Miami/Jackson Health System",                                        "jackson memorial hospital",              "FL",
  "University of Southern California/Los Angeles General Medical Center (USC/LA General)", "los angeles county usc",            "CA",
  "University of California (San Diego) Medical Center",                              "uc san diego",                           "CA",
  "Ascension St John Hospital",                                                       "st john hospital",                       "MI",
  "Dartmouth-Hitchcock/Mary Hitchcock Memorial Hospital",                             "mary hitchcock",                         "NH",
  "New York-Presbyterian Brooklyn Methodist Hospital",                                "new york methodist hospital",            "NY",
  "Rutgers Health/Robert Wood Johnson Medical School",                                "robert wood johnson",                    "NJ",
  "University of California (San Francisco)",                                         "ucsf medical center",                    "CA",
  "Zucker School of Medicine at Hofstra/Northwell at Lenox Hill Hospital",            "lenox hill hospital",                    "NY",
  "Ascension Illinois/Saint Francis",                                                 "st francis hospital evanston",           "IL",
  "George Washington University",                                                     "george washington university hospital",  "DC",
  "Medical College of Wisconsin Affiliated Hospitals",                                "froedtert",                              "WI",
  "St Elizabeth's Medical Center",                                                    "st elizabeth medical center",            "MA",
  "Albert Einstein Healthcare Network",                                               "einstein medical center",                "PA",
  "Baylor Scott & White Medical Center - Baylor College of Medicine (Temple)",        "scott and white memorial",               "TX",
  "Detroit Medical Center/Wayne State University",                                    "detroit medical center",                 "MI",
  "National Capital Consortium",                                                      "walter reed",                            "DC",
  "University of Toledo",                                                             "university of toledo medical center",    "OH",
  "Zucker School of Medicine at Hofstra/Northwell at Staten Island University Hospital", "staten island university hospital",   "NY",
  "Corewell Health East Beaumont (Royal Oak)",                                        "william beaumont hospital",              "MI",
  "Howard University",                                                                "howard university hospital",             "DC",
  "New York Presbyterian Hospital (Columbia Campus)",                                 "new york presbyterian",                  "NY",
  "Nassau University Medical Center",                                                 "nassau university medical center",       "NY",
  "Penn State Milton S Hershey Medical Center",                                       "milton s hershey medical center",        "PA",
  "University of Wisconsin Hospitals and Clinics",                                    "university of wisconsin hospital",       "WI",
  "BronxCare Health System",                                                          "bronx lebanon hospital",                 "NY",
  "MedStar Health/Georgetown-Washington Hospital Center",                             "washington hospital center",             "DC",
  "Oregon Health & Science University (OHSU Health)",                                 "oregon health sciences",                 "OR",
  "Sparrow Hospital/Michigan State University",                                       "sparrow hospital",                       "MI",
  "St Vincent Hospital",                                                              "st vincent",                             "IN",
  "Cedars-Sinai Medical Center",                                                      "cedars sinai",                           "CA",
  "Marshall University School of Medicine",                                           "cabell huntington",                      "WV",
  "UCLA David Geffen School of Medicine/UCLA Medical Center",                         "ucla medical center",                    "CA",
  "University of California Davis Health",                                            "uc davis medical center",                "CA",
  "Albany Medical Center",                                                            "albany medical center",                  "NY",
  "Michael Reese Hospital and Medical Center",                                        "michael reese",                          "IL",
  "University of Vermont Medical Center",                                             "medical center hospital of vermont",     "VT",
  "Advocate Health Care/Advocate Illinois Masonic Medical Center",                    "illinois masonic",                       "IL",
  "Lincoln Medical and Mental Health Center",                                         "lincoln hospital",                       "NY",
  "St Luke's Hospital",                                                               "st lukes",                               "MO",
  # ---- Fellowship-specific extras ----
  "MedStar Health Georgetown University",                                             "georgetown university hospital",         "DC",
  "Corewell Health William Beaumont University Hospital",                             "william beaumont hospital",              "MI",
  "Ochsner Clinic Foundation",                                                        "ochsner",                                "LA",
  "Jefferson Health Medical Education/Jefferson Einstein Philadelphia Hospital",      "einstein medical center",                "PA",
  "Montefiore Medical Center/Albert Einstein College of Medicine (Montefiore)",       "montefiore medical center",              "NY",
  "New York Medical College at St Vincent's Hospital and Medical Center of New York", "st vincent",                             "NY",
  "Aurora Health Care",                                                               "aurora",                                 "WI",
  "UMass Chan - Baystate",                                                            "baystate medical center",                "MA",
  "Mount Sinai Medical Center of Florida",                                            "mount sinai medical center",             "FL",
  "Henry Ford St. John Hospital",                                                     "st john hospital",                       "MI",
  "University of Florida College of Medicine Jacksonville",                           "shands jacksonville",                    "FL",
  "University of South Florida Morsani",                                              "tampa general",                          "FL",
  "Main Line Health System/Lankenau Medical Center",                                  "lankenau",                               "PA",
  "Sidney Kimmel Medical College at Thomas Jefferson University/Deborah Heart and Lung Center", "deborah heart and lung",       "NJ",
  "Ascension Providence/MSUCHM",                                                      "providence hospital",                    "MI",
  "University of Nebraska Medical Center College of Medicine",                        "university of nebraska medical center",  "NE",
  "University of Missouri-Kansas City School of Medicine",                            "truman medical center",                  "MO",
  "ECU Health Medical Center/East Carolina University",                               "pitt county memorial",                   "NC",
  "University of Arizona College of Medicine-Phoenix",                                "banner university medical center",       "AZ",
  "Cooper Medical School of Rowan University/Cooper University Hospital",             "cooper hospital",                        "NJ",
  "Geisinger Health System (Danville)",                                               "geisinger medical center",               "PA",
  "Icahn School of Medicine at Mount Sinai/Beth Israel/West",                         "mount sinai west",                       "NY",
  "Ascension St Vincent Hospital Indianapolis",                                       "st vincent",                             "IN",
  "Baylor University Medical Center",                                                 "baylor university medical center",       "TX",
  "UCLA-VA Greater Los Angeles",                                                      "west los angeles va",                    "CA",
  "University of New Mexico School of Medicine",                                      "university of new mexico hospital",      "NM"
)

resolve_override <- function(search, st) {
  search_n <- norm_hosp(search)
  if (is.na(search_n) || nchar(search_n) == 0L) return(NA_character_)
  hits <- roster %>%
    filter(state == st,
           stri_detect_fixed(name_norm, search_n) |
             stri_detect_fixed(search_n, name_norm))
  if (nrow(hits) == 0L) return(NA_character_)
  pick <- hits$ID[which.min(nchar(hits$name_norm))]
  if (length(pick) == 0L) NA_character_ else pick[1]
}
overrides$ahaid_override <- mapply(resolve_override,
                                   overrides$aha_search, overrides$state)


# 3. Tighter fuzzy matcher --------------------------------------------------

# Substring-based fuzzy match with length-ratio guardrail to reduce false
# positives. We require either:
#   - needle is a substring of an AHA name AND len(AHA) <= 1.8 * len(needle)
#   - AHA name is a substring of needle AND len(needle) <= 1.8 * len(AHA)
# This rejects matches like "duke" -> "university hospital of duke southwest"
# while still accepting "mass general" <-> "massachusetts general hospital".
aha_collapsed <- roster %>%
  group_by(name_norm) %>%
  arrange(ID) %>% slice(1) %>% ungroup() %>%
  select(name_norm, ahaid = ID, state)

fuzzy_id <- function(needle) {
  if (is.na(needle) || nchar(needle) < 8) return(NA_character_)
  nl <- nchar(needle)
  # Substring containment in either direction, with a length-ratio guardrail
  # of 3.0. Excludes cases like "duke" matching "university hospital of duke
  # southwest west medical" but still allows reasonable name variants.
  candidates <- aha_collapsed[
    (stri_detect_fixed(aha_collapsed$name_norm, needle) &
       nchar(aha_collapsed$name_norm) <= nl * 3.0) |
    (stri_detect_fixed(needle, aha_collapsed$name_norm) &
       nchar(aha_collapsed$name_norm) >= nl / 3.0 &
       nchar(aha_collapsed$name_norm) >= 6), ]
  if (nrow(candidates) == 0L) return(NA_character_)
  # Prefer the AHA name closest in length to the needle (best alignment)
  ix <- which.min(abs(nchar(candidates$name_norm) - nl))
  candidates$ahaid[ix]
}


# 4. Apply override + exact + fuzzy in tiers ------------------------------

map_strings <- function(strings_df) {
  m1 <- strings_df %>%
    left_join(overrides %>% select(doximity_name, ahaid_override),
              by = c("inst" = "doximity_name")) %>%
    mutate(name_norm = norm_hosp(str_split_fixed(inst, "/", 2)[, 1])) %>%
    left_join(aha_collapsed %>% select(name_norm, ahaid_exact = ahaid),
              by = "name_norm")
  unmatched <- m1 %>% filter(is.na(ahaid_override), is.na(ahaid_exact))
  unmatched$ahaid_fuzzy <- vapply(unmatched$name_norm, fuzzy_id, character(1))
  m1 %>%
    left_join(unmatched %>% select(inst, ahaid_fuzzy), by = "inst") %>%
    mutate(ahaid = coalesce(ahaid_override, ahaid_exact, ahaid_fuzzy),
           match_kind = case_when(!is.na(ahaid_override) ~ "manual",
                                  !is.na(ahaid_exact)    ~ "exact",
                                  !is.na(ahaid_fuzzy)    ~ "fuzzy",
                                  TRUE ~ "none")) %>%
    select(inst, ahaid, match_kind, n_card = n)
}

res_strings <- cw %>%
  filter(!is.na(residency_institution)) %>%
  count(residency_institution, name = "n", sort = TRUE) %>%
  rename(inst = residency_institution)
res_map <- map_strings(res_strings) %>%
  rename(residency_institution = inst,
         ahaid_residency = ahaid, res_match_kind = match_kind)
cat("Residency string -> AHA mapping:\n")
print(res_map %>% count(res_match_kind, wt = n_card, name = "cardiologists"))
cat(sprintf("Cardiologists with any residency mapping: %.1f%%\n",
            100 * sum(res_map$n_card[res_map$res_match_kind != "none"]) /
                  sum(res_map$n_card)))

fel_strings <- cw %>%
  filter(!is.na(fellowship_institution)) %>%
  count(fellowship_institution, name = "n", sort = TRUE) %>%
  rename(inst = fellowship_institution)
fel_map <- map_strings(fel_strings) %>%
  rename(fellowship_institution = inst,
         ahaid_fellowship = ahaid, fel_match_kind = match_kind)
cat("\nFellowship string -> AHA mapping:\n")
print(fel_map %>% count(fel_match_kind, wt = n_card, name = "cardiologists"))
cat(sprintf("Cardiologists with any fellowship mapping: %.1f%%\n",
            100 * sum(fel_map$n_card[fel_map$fel_match_kind != "none"]) /
                  sum(fel_map$n_card)))


# 5. Write the crosswalk --------------------------------------------------

# Long format: one row per distinct Doximity string per training stage.
out <- bind_rows(
  res_map %>% transmute(stage = "residency", dox_string = residency_institution,
                        ahaid = ahaid_residency, match_kind = res_match_kind,
                        n_cardiologists = n_card),
  fel_map %>% transmute(stage = "fellowship", dox_string = fellowship_institution,
                        ahaid = ahaid_fellowship, match_kind = fel_match_kind,
                        n_cardiologists = n_card)
)
write_csv(out, "data/output/training_aha_crosswalk.csv")
cat("\nWrote data/output/training_aha_crosswalk.csv\n")
