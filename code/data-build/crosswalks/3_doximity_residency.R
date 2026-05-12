# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-05-11
## Description:   Link our cardiologist NPIs to Doximity profile data so we
##                can pull residency program + fellowship program for each
##                cardiologist. Doximity has no NPI, so we block on
##                normalized (first_name, last_name) and tie-break in this
##                order:
##                  1) exact grad_year
##                  2) state agreement
##                  3) med_school similarity (string)
##                Then write one row per matched cardiologist with the
##                Doximity training fields.
##
##                Outputs:
##                  data/output/cardiologist_doximity.csv
##                    (npi -> doximity_uuid, residency_*, fellowship_*, internship_*)

# 1. Cardiologist NPIs + augmented PC pull (NPI, first/last name, state) ----

cardio_pc <- read_csv("data/output/cardiologist_pc.csv",
                      col_types = cols(npi = col_character(),
                                       grad_year = col_integer(),
                                       .default = col_character()))
cardio_npis <- unique(cardio_pc$npi)
cat("Cardiologist NPIs in PC crosswalk:", length(cardio_npis), "\n")

# PC files ship in two schema vintages (transition at 2017 Q4). The existing
# script 1_medschool_list.R already collapses school/grad_year, but it does
# not retain first/last/state, which we need here.
old_names <- c(last = "Last Name", first = "First Name", state = "State")
new_names <- c(last = "lst_nm",     first = "frst_nm",    state = "st")

read_pc_names <- function(f) {
  header <- str_trim(str_split(readLines(f, n = 1), ",")[[1]])
  cols_map <- if (all(old_names %in% header)) old_names
              else if (all(new_names %in% header)) new_names
              else stop("Unrecognized PC schema in ", f)
  raw <- read_csv(f,
                  col_types = cols(.default = col_character()),
                  name_repair = ~ str_trim(.x),
                  show_col_types = FALSE)
  tibble(npi   = raw[["NPI"]],
         last  = raw[[cols_map[["last"]]]],
         first = raw[[cols_map[["first"]]]],
         state = raw[[cols_map[["state"]]]])
}

pc_files <- list.files("data/input/physician-compare",
                       pattern = "^(2013|2015|2016|2017|2018)_Q[1-4]\\.csv$|^2014_Q[34]\\.csv$",
                       recursive = TRUE, full.names = TRUE)
cat("Scanning", length(pc_files), "PC quarterly files for name + state\n")

pc_names <- map_dfr(pc_files, read_pc_names) %>%
  mutate(across(everything(), str_trim),
         across(everything(), ~ if_else(.x == "", NA_character_, .x))) %>%
  filter(npi %in% cardio_npis, !is.na(last), !is.na(first)) %>%
  distinct()

# Collapse to modal (npi, last, first) tuple and most recent state.
# Last/first are stable; state is current-practice and may shift across
# quarters for a few NPIs, so prefer the value from the latest vintage.
pc_modal_name <- pc_names %>%
  count(npi, last, first) %>%
  group_by(npi) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(npi, last, first)

# Latest practice state per NPI: pick the modal state, which is robust to
# the cross-quarter noise typical of multi-state cardiologists.
pc_modal_state <- pc_names %>%
  count(npi, state) %>%
  filter(!is.na(state)) %>%
  group_by(npi) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(npi, state_pc = state)

pc_for_match <- pc_modal_name %>%
  left_join(pc_modal_state, by = "npi") %>%
  left_join(cardio_pc %>% select(npi, med_school_pc = med_school,
                                 grad_year_pc  = grad_year),
            by = "npi")

cat("PC NPIs with name + grad_year + state:",
    sum(!is.na(pc_for_match$first) & !is.na(pc_for_match$grad_year_pc) &
        !is.na(pc_for_match$state_pc)), "of", nrow(pc_for_match), "\n")


# 2. Doximity cardiology subset --------------------------------------------

dox_path <- "D:/research-data/doximity/doximity_profiles.csv"
cat("\nReading Doximity profiles (this is a ~1GB file)\n")

dox_all <- fread(dox_path,
                 select = c("url", "doximity_uuid", "first_name", "last_name",
                            "gender", "specialty", "state", "med_school",
                            "med_school_grad_year",
                            "residency_institution", "residency_specialty",
                            "residency_years",
                            "residency2_institution", "residency2_specialty",
                            "residency2_years",
                            "fellowship_institution", "fellowship_specialty",
                            "fellowship_years",
                            "fellowship2_institution", "fellowship2_specialty",
                            "fellowship2_years",
                            "internship_institution", "internship_specialty",
                            "internship_years",
                            "training_raw"),
                 na.strings = c("", "NA"),
                 showProgress = FALSE)
setDF(dox_all)

dox <- dox_all %>%
  filter(grepl("cardio", specialty, ignore.case = TRUE)) %>%
  as_tibble()

cat("Doximity cardio rows:", nrow(dox), "\n")
cat("  med_school filled :", sum(!is.na(dox$med_school)),
    sprintf("(%.1f%%)", 100 * mean(!is.na(dox$med_school))), "\n")
cat("  grad_year filled  :", sum(!is.na(dox$med_school_grad_year)),
    sprintf("(%.1f%%)", 100 * mean(!is.na(dox$med_school_grad_year))), "\n")
cat("  residency_inst    :", sum(!is.na(dox$residency_institution)),
    sprintf("(%.1f%%)", 100 * mean(!is.na(dox$residency_institution))), "\n")


# 3. Normalize names for blocking ------------------------------------------

# Lowercase, transliterate non-ASCII, strip credentials and punctuation.
norm_name <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    tolower() %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("\\b(md|do|mph|phd|jr|sr|ii|iii|iv)\\b", " ") %>%
    str_squish()
}

pc_for_match <- pc_for_match %>%
  mutate(first_n = norm_name(first),
         last_n  = norm_name(last))

dox <- dox %>%
  mutate(first_n = norm_name(first_name),
         last_n  = norm_name(last_name))


# 4. Block on (last_n, first_n) and classify ------------------------------

cat("\nBlocking on (last_n, first_n)\n")

candidates <- inner_join(
  pc_for_match %>% select(npi, first_n, last_n, grad_year_pc, state_pc, med_school_pc),
  dox %>%
    mutate(dox_row = row_number()) %>%
    select(dox_row, first_n, last_n, doximity_uuid, gender,
           state_dox = state, med_school_dox = med_school,
           grad_year_dox = med_school_grad_year,
           residency_institution, residency_specialty,
           residency2_institution, fellowship_institution, fellowship_specialty),
  by = c("first_n", "last_n"),
  relationship = "many-to-many"
)

cat("Candidate (NPI x Doximity) pairs after name block:", nrow(candidates), "\n")
cat("Unique NPIs with >=1 candidate:", n_distinct(candidates$npi), "\n")
cat("Unique Doximity rows with >=1 candidate:", n_distinct(candidates$dox_row), "\n")


# 5. Per-NPI candidate counts + tie-break ---------------------------------

# A pair is "unique" if it is the only candidate for both sides (a 1:1
# match in the bipartite candidate graph).
np_n <- candidates %>% count(npi, name = "n_dox_per_npi")
dx_n <- candidates %>% count(dox_row, name = "n_npi_per_dox")

candidates <- candidates %>%
  left_join(np_n, by = "npi") %>%
  left_join(dx_n, by = "dox_row") %>%
  mutate(grad_match  = !is.na(grad_year_pc) & !is.na(grad_year_dox) &
                       grad_year_pc == grad_year_dox,
         state_match = !is.na(state_pc)     & !is.na(state_dox)     &
                       state_pc == state_dox,
         med_match   = !is.na(med_school_pc) & !is.na(med_school_dox) &
                       norm_name(med_school_pc) == norm_name(med_school_dox))

# 5a. Unique matches: exactly one Doximity profile per NPI AND that profile
# claims only that NPI.
unique_pairs <- candidates %>%
  filter(n_dox_per_npi == 1, n_npi_per_dox == 1) %>%
  mutate(match_tier = "unique_name")

cat("\nStage 1 (unique name match, both sides 1:1):", nrow(unique_pairs), "\n")

# 5b. Among the rest, tie-break per NPI:
#     priority: exact grad_year > state agreement > med_school string match.
non_unique <- candidates %>%
  anti_join(unique_pairs %>% select(npi, dox_row), by = c("npi", "dox_row"))

# Compute a tie-break score per candidate row (higher = better).
non_unique <- non_unique %>%
  mutate(score = as.integer(grad_match) * 100L +
                 as.integer(state_match) * 10L +
                 as.integer(med_match)   * 1L)

# For each NPI, keep the maximum-score candidate; require a strict max
# (no ties) AND require score > 0 (some agreement beyond name).
tie_pairs <- non_unique %>%
  group_by(npi) %>%
  mutate(max_score = max(score, na.rm = TRUE),
         n_at_max  = sum(score == max_score)) %>%
  ungroup() %>%
  filter(score == max_score, n_at_max == 1, score > 0L)

# Reject any tie_pair whose dox_row would also be claimed by a different
# NPI in the same step.
dx_claims <- tie_pairs %>% count(dox_row, name = "claims")
tie_pairs <- tie_pairs %>%
  left_join(dx_claims, by = "dox_row") %>%
  filter(claims == 1) %>%
  select(-claims) %>%
  mutate(match_tier = case_when(
    grad_match               ~ "tiebreak_grad_year",
    !grad_match & state_match ~ "tiebreak_state",
    TRUE                     ~ "tiebreak_med_school"))

cat("Stage 2 (multi-name, tie-broken by grad/state/med):", nrow(tie_pairs), "\n")
cat("  by grad_year :", sum(tie_pairs$match_tier == "tiebreak_grad_year"), "\n")
cat("  by state     :", sum(tie_pairs$match_tier == "tiebreak_state"), "\n")
cat("  by med_school:", sum(tie_pairs$match_tier == "tiebreak_med_school"), "\n")


# 5c. Stage 3: nickname / initial fuzzy expansion for unmatched NPIs ------

# Common-first-name canonical map: dictionary normalizes nicknames to a
# canonical form so that "Bob" and "Robert" match. Coverage is intentionally
# conservative; we only collapse variants that are well-established and
# would not introduce false positives.
nickname_canon <- c(
  bob = "robert", robby = "robert", rob = "robert", bobby = "robert",
  bill = "william", billy = "william", will = "william", willy = "william",
  jim = "james", jimmy = "james", jamie = "james",
  jack = "john", johnny = "john", jon = "john",
  mike = "michael", mick = "michael", micky = "michael",
  chuck = "charles", charlie = "charles", chip = "charles",
  rick = "richard", rich = "richard", ricky = "richard", dick = "richard",
  tom = "thomas", tommy = "thomas",
  joe = "joseph", joey = "joseph",
  ed = "edward", eddie = "edward", edd = "edward",
  tony = "anthony",
  chris = "christopher",
  dan = "daniel", danny = "daniel",
  dave = "david", davey = "david",
  steve = "steven", stevie = "steven", stephen = "steven",
  andy = "andrew", drew = "andrew",
  ken = "kenneth", kenny = "kenneth",
  don = "donald", donny = "donald",
  jerry = "gerald", gerry = "gerald",
  larry = "lawrence",
  pat = "patrick", patty = "patrick",
  nick = "nicholas", nicky = "nicholas",
  matt = "matthew",
  ben = "benjamin", benny = "benjamin",
  ted = "theodore", teddy = "theodore",
  fred = "frederick", freddie = "frederick",
  alex = "alexander", al = "alexander",
  hank = "henry", harry = "henry",
  frank = "francis",
  gene = "eugene",
  sam = "samuel", sammy = "samuel",
  tim = "timothy", timmy = "timothy",
  greg = "gregory",
  ray = "raymond",
  rocky = "rocco",
  zach = "zachary", zack = "zachary",
  liz = "elizabeth", beth = "elizabeth", betty = "elizabeth",
  kate = "katherine", katie = "katherine", kathy = "katherine", kathleen = "katherine",
  sue = "susan", susie = "susan",
  patty2 = "patricia", patti = "patricia", trish = "patricia",
  peggy = "margaret", maggie = "margaret",
  jenny = "jennifer", jen = "jennifer",
  jess = "jessica", jessie = "jessica",
  amy = "amy",
  becky = "rebecca", bec = "rebecca",
  cathy = "catherine", cath = "catherine"
)

canonical_first <- function(x) {
  toks <- str_split(x, "\\s+")
  vapply(toks, function(v) {
    head <- v[1]
    if (is.na(head) || head == "") return(NA_character_)
    if (head %in% names(nickname_canon)) nickname_canon[[head]] else head
  }, character(1))
}

# Pool of NPIs that did not match in Stage 1 or 2:
matched_so_far <- bind_rows(unique_pairs, tie_pairs) %>%
  select(npi, dox_row)
pc_unmatched <- pc_for_match %>% anti_join(matched_so_far, by = "npi")
dox_unclaimed <- dox %>%
  mutate(dox_row = row_number()) %>%
  anti_join(matched_so_far, by = "dox_row")

cat("\nStage 3 pool: NPIs unmatched =", nrow(pc_unmatched),
    "; Doximity rows unclaimed =", nrow(dox_unclaimed), "\n")

pc_unmatched <- pc_unmatched %>%
  mutate(first_canon = canonical_first(first_n))
dox_unclaimed <- dox_unclaimed %>%
  mutate(first_canon = canonical_first(first_n))

# Also build first-initial keys: matching when full first name is abbreviated
# on one side (common in PC for older vintages) but full on the other.
pc_unmatched  <- pc_unmatched  %>%
  mutate(first_init = str_sub(first_n, 1, 1))
dox_unclaimed <- dox_unclaimed %>%
  mutate(first_init = str_sub(first_n, 1, 1))

# Round A: canonical first name + last name
candA <- inner_join(
  pc_unmatched %>% select(npi, last_n, first_canon, grad_year_pc,
                          state_pc, med_school_pc),
  dox_unclaimed %>% select(dox_row, last_n, first_canon, doximity_uuid,
                           gender, state_dox = state, med_school_dox = med_school,
                           grad_year_dox = med_school_grad_year,
                           residency_institution, residency_specialty,
                           residency2_institution, fellowship_institution,
                           fellowship_specialty),
  by = c("last_n", "first_canon"),
  relationship = "many-to-many"
) %>%
  filter(!is.na(first_canon))

# Require BOTH grad_year exact (or NA on one side) AND no contradicting state
candA <- candA %>%
  mutate(grad_ok  = is.na(grad_year_dox) | is.na(grad_year_pc) |
                    grad_year_pc == grad_year_dox,
         state_ok = is.na(state_dox) | is.na(state_pc) |
                    state_pc == state_dox) %>%
  filter(grad_ok, state_ok)

# Take only one-to-one canonical matches (no ties on either side)
nA_npi <- candA %>% count(npi, name = "n_a_npi")
nA_dox <- candA %>% count(dox_row, name = "n_a_dox")
nickname_pairs <- candA %>%
  left_join(nA_npi, by = "npi") %>%
  left_join(nA_dox, by = "dox_row") %>%
  filter(n_a_npi == 1, n_a_dox == 1) %>%
  mutate(match_tier  = "nickname_canonical",
         grad_match  = !is.na(grad_year_pc) & !is.na(grad_year_dox) &
                       grad_year_pc == grad_year_dox,
         state_match = !is.na(state_pc)     & !is.na(state_dox)     &
                       state_pc == state_dox,
         med_match   = !is.na(med_school_pc) & !is.na(med_school_dox) &
                       norm_name(med_school_pc) == norm_name(med_school_dox))

cat("Stage 3a (nickname canonical match):", nrow(nickname_pairs), "\n")

# Round B: first-initial + last + exact grad_year + state agreement
matched_so_far <- bind_rows(matched_so_far,
                            nickname_pairs %>% select(npi, dox_row))
pc_unmatched   <- pc_unmatched %>% anti_join(matched_so_far, by = "npi")
dox_unclaimed  <- dox_unclaimed %>% anti_join(matched_so_far, by = "dox_row")

candB <- inner_join(
  pc_unmatched %>% select(npi, last_n, first_init, grad_year_pc,
                          state_pc, med_school_pc),
  dox_unclaimed %>% select(dox_row, last_n, first_init, doximity_uuid, gender,
                           state_dox = state, med_school_dox = med_school,
                           grad_year_dox = med_school_grad_year,
                           residency_institution, residency_specialty,
                           residency2_institution, fellowship_institution,
                           fellowship_specialty),
  by = c("last_n", "first_init"),
  relationship = "many-to-many"
)

# Initial-only is weak, so require BOTH exact grad_year and state agreement
candB <- candB %>%
  filter(!is.na(grad_year_pc), !is.na(grad_year_dox),
         grad_year_pc == grad_year_dox,
         !is.na(state_pc), !is.na(state_dox), state_pc == state_dox)

nB_npi <- candB %>% count(npi, name = "n_b_npi")
nB_dox <- candB %>% count(dox_row, name = "n_b_dox")
initial_pairs <- candB %>%
  left_join(nB_npi, by = "npi") %>%
  left_join(nB_dox, by = "dox_row") %>%
  filter(n_b_npi == 1, n_b_dox == 1) %>%
  mutate(match_tier  = "first_initial",
         grad_match  = TRUE,
         state_match = TRUE,
         med_match   = !is.na(med_school_pc) & !is.na(med_school_dox) &
                       norm_name(med_school_pc) == norm_name(med_school_dox))

cat("Stage 3b (first-initial + grad_year + state):", nrow(initial_pairs), "\n")


# 6. Assemble final crosswalk ----------------------------------------------

matched <- bind_rows(unique_pairs, tie_pairs, nickname_pairs, initial_pairs) %>%
  select(npi, dox_row, match_tier, grad_match, state_match, med_match)

# Pull the per-row Doximity training fields from the original cardio frame.
dox_fields <- dox %>%
  mutate(dox_row = row_number()) %>%
  select(dox_row, doximity_uuid, dox_url = url,
         dox_first = first_name, dox_last = last_name,
         dox_state = state, dox_med_school = med_school,
         dox_grad_year = med_school_grad_year,
         residency_institution, residency_specialty, residency_years,
         residency2_institution, residency2_specialty, residency2_years,
         fellowship_institution, fellowship_specialty, fellowship_years,
         fellowship2_institution, fellowship2_specialty, fellowship2_years,
         internship_institution, internship_specialty, internship_years,
         training_raw)

cw <- matched %>%
  left_join(dox_fields, by = "dox_row") %>%
  select(-dox_row)

cat("\nFinal crosswalk rows:", nrow(cw),
    sprintf("(%.1f%% of %d cardiologist NPIs)",
            100 * nrow(cw) / length(cardio_npis), length(cardio_npis)), "\n")
cat("  with residency_institution:", sum(!is.na(cw$residency_institution)),
    sprintf("(%.1f%% of matched)",
            100 * mean(!is.na(cw$residency_institution))), "\n")
cat("  with fellowship_institution:", sum(!is.na(cw$fellowship_institution)),
    sprintf("(%.1f%% of matched)",
            100 * mean(!is.na(cw$fellowship_institution))), "\n")


# 7. Verification: agreement among Stage-1 (name-only) matches -------------

# Stage 1 matched purely on (last_n, first_n) and didn't use grad_year,
# state, gender, or med_school. If the name block is producing REAL matches,
# we should see high agreement on these orthogonal fields. Low agreement
# would indicate the name block is picking up coincidental name collisions.
verif <- unique_pairs %>%
  left_join(dox %>% mutate(dox_row = row_number()) %>%
              select(dox_row, gender_dox = gender), by = "dox_row") %>%
  left_join(cardio_pc %>% select(npi, gender_pc = gender), by = "npi") %>%
  mutate(gender_pc  = toupper(str_sub(gender_pc, 1, 1)),
         gender_dox = toupper(str_sub(gender_dox, 1, 1)),
         grad_ok   = !is.na(grad_year_pc) & !is.na(grad_year_dox) &
                     grad_year_pc == grad_year_dox,
         grad_have = !is.na(grad_year_pc) & !is.na(grad_year_dox),
         state_ok  = !is.na(state_pc) & !is.na(state_dox) & state_pc == state_dox,
         state_have = !is.na(state_pc) & !is.na(state_dox),
         gender_ok = !is.na(gender_pc) & !is.na(gender_dox) & gender_pc == gender_dox,
         gender_have = !is.na(gender_pc) & !is.na(gender_dox),
         med_ok    = !is.na(med_school_pc) & !is.na(med_school_dox) &
                     norm_name(med_school_pc) == norm_name(med_school_dox),
         med_have  = !is.na(med_school_pc) & !is.na(med_school_dox))

agree_rate <- function(ok, have) sum(ok) / sum(have)

cat("\nStage-1 match validation (orthogonal-field agreement):\n")
cat(sprintf("  grad_year  agree: %4.1f%% (n have both = %d)\n",
            100 * agree_rate(verif$grad_ok,   verif$grad_have),   sum(verif$grad_have)))
cat(sprintf("  state      agree: %4.1f%% (n have both = %d)\n",
            100 * agree_rate(verif$state_ok,  verif$state_have),  sum(verif$state_have)))
cat(sprintf("  gender     agree: %4.1f%% (n have both = %d)\n",
            100 * agree_rate(verif$gender_ok, verif$gender_have), sum(verif$gender_have)))
cat(sprintf("  med_school agree: %4.1f%% (n have both = %d, exact string)\n",
            100 * agree_rate(verif$med_ok,    verif$med_have),    sum(verif$med_have)))

# By tier overall (excluding Stage 1 which is the validation set above)
cat("\nMatch-tier breakdown:\n")
print(cw %>% count(match_tier, name = "n"))

write_csv(cw, "data/output/cardiologist_doximity.csv")
cat("\nWrote data/output/cardiologist_doximity.csv\n")


# 7. Coverage on the analytic panel ----------------------------------------

# How many of the cardiologists in our analytic panel did we match?
# (analysis_panel.csv may not exist yet if data-build hasn't been run; skip
# gracefully if so.)
if (file.exists("data/output/analysis_panel.csv")) {
  analytic_npis <- read_csv("data/output/analysis_panel.csv",
                            col_types = cols(npi = col_character(),
                                             .default = col_guess())) %>%
    pull(npi) %>% unique()
  in_panel <- sum(cw$npi %in% analytic_npis)
  cat("\nAnalytic-panel cardiologists matched to Doximity:",
      in_panel, "of", length(analytic_npis),
      sprintf("(%.1f%%)", 100 * in_panel / length(analytic_npis)), "\n")
}
