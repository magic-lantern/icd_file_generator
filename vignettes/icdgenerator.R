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
#' This vignette shows how to create a sample data set of N rows with X columns of
#' data. User can specify D columns of ICD diagnosis codes, P columns of ICD
#' procedure codes, and G columns of extra data.  Either ICD9 or ICD10 codes can
#' be generated.
#'
#' # The Data Sets
#' There are four data sets provided in this package.  Each data set is a
#' data.frame given the ICD 9 codes and labels.  Additional details on these
#' data set can be found in their specific documentation files, see `?icd10dx`
data(list = c("icd9dx", "icd9pc", "icd10dx", "icd10pc"), package = "icdgenerator")
loadNamespace("data.table")
icd9dx
icd9pc
icd10dx
icd10pc


#'
# /*
# =============================================================================
# */
#'
#' # Section 2
#'

#'
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


