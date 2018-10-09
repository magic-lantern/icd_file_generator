#'---
#'title: "icdgenerator"
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
#' # Section 1
#' 
#' Text goes here
#+ label = "r-chunk-1"
2 + 2

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
