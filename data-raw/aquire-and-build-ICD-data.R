#'---
#'title: "Acquire and Build ICD Reference Tables"
#'date:  `r format(Sys.Date(), "%d %b %Y")`
#'---
#'
#' This vignette source code is in the github repository data-raw directory and
#' has been spun into a vignette so the user can see the steps taken to generate
#' the ICD 9 and ICD 10 reference tables.
#'
#' This work was done on a linux system and some system utilities are used.
#' Specifically `file` used to determine file type and `iconv` to convert test
#' from one character encoding to another.  Through out this script/vignette
#' namespaces are explicitly used.
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
#+label='icd9build'
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

icd9dx <- dplyr::left_join(icd9_long_dx, icd9_short_dx)
icd9dx$code <- gsub("\\s+$", "", icd9dx$code)
icd9pc <- dplyr::left_join(icd9_long_sg, icd9_short_sg)

icd9dx
icd9pc

#'
#' # ICD 10
#' The ICD 10 codes for 2018 are acquired from two files provided by CMS.
#' [2018 Code Descriptions in Tabualr Oder [ZIP, 2 MB]](https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2018-ICD-10-Code-Descriptions.zip)
#' and
#' [2018 ICD-10-PCS Codes File [ZIP, 880KB]](https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2018-ICD-10-PCS-Codes-File.zip)
download.file("https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2018-ICD-10-Code-Descriptions.zip",
              destfile = "icd10dx.zip")
download.file("https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2018-ICD-10-PCS-Codes-File.zip",
              destfile = "icd10pc.zip")

tools::md5sum("icd10dx.zip")
tools::md5sum("icd10pc.zip")

unzip("icd10dx.zip", list = TRUE)
unzip("icd10dx.zip", exdir = tmpdir)

unzip("icd10pc.zip", list = TRUE)
unzip("icd10pc.zip", exdir = tmpdir)

for(f in list.files(tmpdir, pattern = "icd10.+_codes_2018.txt$", full.names = TRUE)) {
  system(paste("file", f))
}

icd10dx <- scan(paste0(tmpdir, "/icd10cm_codes_2018.txt"),
                what = character(), sep = "\n")
icd10pc <- scan(paste0(tmpdir, "/icd10pcs_codes_2018.txt"),
                what = character(), sep = "\n")

head(icd10dx); tail(icd10dx)
head(icd10pc); tail(icd10pc)

icd10dx <-
  tibble::tibble(code = substr(icd10dx, start = 1L, stop = 8L),
                 long = substr(icd10dx, start = 9L, stop = 1e5))
icd10dx$code <- gsub("\\s+$", "", icd10dx$code)

icd10pc <-
  tibble::tibble(code = substr(icd10pc, start = 1L, stop = 8L),
                 long = substr(icd10pc, start = 9L, stop = 1e5))
icd10pc$code <- gsub("\\s+$", "", icd10pc$code)

#'
#' Generate a file to document the data sets:

"
# Auto generated by data-raw/acquire-and-build-ICD-data.R
# DO NOT EDIT BY HAND.
#' ICD-9-CM Diagnostic Codes and Labels
#'
#' @format a data frame with", qwraps2::frmt(nrow(icd9dx)), "rows and 3 columns.
#' \\describe{
#' \\item{code}{A character string of alpha numerics representing the ICD-9
#' diagnostic code.}
#' \\itme{long}{Long form label for the ICD-9 diagnostic code}
#' \\itme{short}{short (abbreviated) form label for the ICD-9 diagnostic code}
#' }
#'
",
qwraps2::frmt(nrow(icd9dx)))

doc

#'
#' # Save the Data Sets
devtools::use_data(icd9dx, overwrite = TRUE)
devtools::use_data(icd9pc, overwrite = TRUE)
devtools::use_data(icd10dx, overwrite = TRUE)
devtools::use_data(icd10pc, overwrite = TRUE)
