#'---
#'title: "ICD Sample Generator"
#'date:  "`r Sys.Date()`"
#'output: rmarkdown::html_vignette
#'vignette: >
#'  %\VignetteIndexEntry{icdgenerator}
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteEncoding{UTF-8}
#'---

#+ label=setup, include = FALSE
library(knitr)
knitr::opts_chunk$set(collapse = TRUE)

#'
# /*
# =============================================================================
# */
#'
#' # Introduction
#'
#' When the icdgenerator package is loaded the data.table namespace is also
#' loaded.  This is done partially so that the `print.data.table` method from
#' the `data.table` package is available.  If you need to use the features of
#' `data.table` you should explicitly load and attach the namespace.
library(icdgenerator)
library(data.table)

#'
#' # The Data Sets
#' There are four data sets provided in this package.  Each data set is a
#' data.frame given the ICD 9 codes and labels.  Additional details on these
#' data set can be found in their specific documentation files, see `?icd10dx`
data(list = c("icd9dx", "icd9pc", "icd10dx", "icd10pc"), package = "icdgenerator")
ls()

#'
#' A quick view of each of the data sets.  All four data sets have a column
#' `code` and `long` for the ICD-9 or ICD-10 code and a label.  The ICD-9-CM
#' data sets also have a short labels provides.  The `code` columns are
#' character columns, even if all the values are numeric characters.
#'
#' `icd9dx` are the ICD-9-CM diagnostic codes with long and short labels.
str(icd9dx)
icd9dx

#'
#' `icd9pc` has the same format as `icd9dx` but reports the ICD-9-CM procedure
#' codes.
str(icd9pc)
icd9pc

#'
#' The ICD-10 diagnostic codes are in the `icd10dx` data.table and the procedure
#' codes are in `icd10pc`.  These codes and labels are based on the 2018 version
#' of ICD-10.
str(icd10dx)
icd10dx

#'
#'
str(icd10pc)
icd10pc


#'
# /*
# =============================================================================
# */
#'
#' # Searching for ICD Codes
#' We recommend using the data.table paradigms to search.  If you are unfamiliar
#' with the syntax for data.table you may find the documentation on the
#' [data.table homepage](https://github.com/Rdatatable/data.table/wiki).
#'
#' A simple example, find all ICD-9 diagnostic codes containing the string 516
icd9dx[grepl("516", code), .(code, short)]

#'
#' Find the specific code 516.0 for Pulmonary alveolar proteinosis
icd9dx[code == 5160]

#'
# /*
# =============================================================================
# */
#'
#' # Generating Random Data Sets
#' `generate_sample` creates a sample data set of N rows with X columns of
#' data. User can specify D columns of ICD diagnosis codes, P columns of ICD
#' procedure codes, and G columns of extra data.  Either ICD9 or ICD10 codes can
#' be generated.
#'
str(generate_sample)

eg_data <- generate_sample(
  version = 9,
  nrows = 3407146,
  dcols = 29,
  pcols = 15,
  gcols = 114,
  pct_empty = 0.2,
  quiet = FALSE
)

str(eg_data)


# /*
# =============================================================================
# */
#'
#' # Session Info
#'
print(sessionInfo(), local = FALSE)

# /*
# =============================================================================
# */


