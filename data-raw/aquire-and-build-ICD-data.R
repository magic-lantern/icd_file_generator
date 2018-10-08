#'---
#'title: "Acquire and Build ICD Reference Tables"
#'date:  `r format(Sys.Date(), "%d %b %Y")`
#'---
#'
#' This vignette source code is in the github repository data-raw directory and
#' has been spun into a vignette so the user can see the steps taken to generate
#' the ICD 9 and ICD 10 reference tables.
#'
#' # Acquire the Raw Data
#' The raw data is provided by the Centers for Medicare & Medicaid Services
#' (CMS).
#'
#' ## ICD-9-CM Diagnostic and Procedure Codes: Abbreviated and Full Code Titles
#' We will acquire [Version 32 Full and Abbreviated Code Titles - Effective Date
#' October 1, 2014 [ZIP,
#' 1MB]](https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/ICD-9-CM-v32-master-descriptions.zip)
#' form CMS.
download.file("https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/ICD-9-CM-v32-master-descriptions.zip",
              destfile = "ICD-9-CM-v32-master-descriptions.zip")

tools::md5sum("ICD-9-CM-v32-master-descriptions.zip")
unzip("ICD-9-CM-v32-master-descriptions.zip", list = TRUE)

tmpdir <- tempdir()
unzip("ICD-9-CM-v32-master-descriptions.zip", exdir = tmpdir)

for(f in list.files(tmpdir, pattern = "*.txt$", full.names = TRUE)) {
  system(paste("file", f))
}

# The ISO-8859 file needs to be translated to ascii
system(paste0("iconv -f ISO-8859-1 -t ASCII//TRANSLIT ",
              tmpdir, "/CMS32_DESC_LONG_DX.txt > ",
              tmpdir, "/CMS32_DESC_LONG_DX_ascii.txt"))

# Scan the files
icd9_long_dx <- scan(paste0(tmpdir, "/CMS32_DESC_LONG_DX_ascii.txt"),
                     what = character(),
                     sep = "\n")
icd9_long_sg <- scan(paste0(tmpdir, "/CMS32_DESC_LONG_SG.txt"),
                     what = character(),
                     sep = "\n")
icd9_short_dx <- scan(paste0(tmpdir, "/CMS32_DESC_SHORT_DX.txt"),
                     what = character(),
                     sep = "\n")
icd9_short_sg <- scan(paste0(tmpdir, "/CMS32_DESC_SHORT_SG.txt"),
                     what = character(),
                     sep = "\n")

# check head and tails
head(icd9_long_dx); tail(icd9_long_dx)
head(icd9_short_dx); tail(icd9_short_dx)
head(icd9_long_sg); tail(icd9_long_sg)
head(icd9_short_sg); tail(icd9_short_sg)

#'
#' Building a useful data set.  A data frame with three columns:
#' 1. code
#' 2. long explanation
#' 3. short explanation
#+label='icd9build', eval = FALSE
icd9_long_dx <-
  tibble::tibble(code = substr(icd9_long_dx, start = 1L, stop = 5L),
                 long = substr(icd9_long_dx, start = 7L, stop = 1e5))
icd9_short_dx <-
  tibble::tibble(code = substr(icd9_short_dx, start = 1L, stop = 5L),
                 short = substr(icd9_short_dx, start = 7L, stop = 1e5))
icd9_long_sg <-
  tibble::tibble(code = substr(icd9_long_sg, start = 1L, stop = 4L),
                 long = substr(icd9_long_sg, start = 7L, stop = 1e5))
icd9_short_sg <-
  tibble::tibble(code = substr(icd9_short_sg, start = 1L, stop = 4L),
                 short = substr(icd9_short_sg, start = 7L, stop = 1e5))

icd9_dx <- dplyr::left_join(icd9_long_dx, icd9_short_dx)
icd9_dx$code %<>% gsub("^(.+)\\s+$", "\\1", .)
icd9_pc <- dplyr::left_join(icd9_long_sg, icd9_short_sg)

icd9_dx
icd9_pc
