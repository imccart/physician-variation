# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2026-04-22
## Description:   Reshape Dartmouth Atlas ZipHsaHrr15.xls into a clean
##                zip -> HRR crosswalk for the main pipeline.
##
##                ZipHsaHrr15 is used as the single reference for 2008-2018
##                (HRR boundaries are largely static; midpoint year chosen).
##
##                Input:  data/input/zip-hrr/ZipHsaHrr15.xls
##                Output: data/crosswalks/zip-hrr-crosswalk.csv

# 1. Read and clean -------------------------------------------------------

zip_hrr <- read_excel("data/input/zip-hrr/ZipHsaHrr15.xls") %>%
  transmute(
    zip5    = str_pad(as.character(zipcode15), width = 5, pad = "0"),
    hrrnum,
    hrrcity,
    hrrstate
  )

cat("Rows:", nrow(zip_hrr), "\n")
cat("Unique ZIPs:", n_distinct(zip_hrr$zip5), "\n")
cat("Unique HRRs:", n_distinct(zip_hrr$hrrnum), "\n")
cat("States:     ", n_distinct(zip_hrr$hrrstate), "\n")


# 2. Sanity check: any duplicate ZIPs? -----------------------------------

dup_zips <- zip_hrr %>%
  count(zip5) %>%
  filter(n > 1)

if (nrow(dup_zips) > 0) {
  cat("WARN: ", nrow(dup_zips), "ZIPs appear multiple times\n")
}


# 3. Save -----------------------------------------------------------------

if (!dir.exists("data/crosswalks")) dir.create("data/crosswalks")
write_csv(zip_hrr, "data/crosswalks/zip-hrr-crosswalk.csv")
cat("Wrote data/crosswalks/zip-hrr-crosswalk.csv\n")
