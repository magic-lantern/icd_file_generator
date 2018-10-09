#' Generate Random ICD Data
#'
#' Create a data frame using randomly selected ICD diagnosis and procedure codes
#'
#' @param version ICD version 9 or 10.  Defaults to 10.
#' @param nrows number of rows to generate
#' @param dcols number of diagnostic codes to generate for each row
#' @param pcols number of procedure codes to generate for each row
#' @param gcols  number of "other" columns to generate
#' @param pct_empty the percentage of diagnostic and procedure codes to be
#' "missing" in the resulting data frame.
#'
#' @examples
#'
#' eg <- generate_sample(
#'   version = 9,
#'   nrows = 340, 
#'   dcols = 29,
#'   pcols = 15,
#'   gcols = 114
#' )
#'
#' head(eg)
#' @export
generate_sample <- function(version = 10, nrows = 1, dcols = 1, pcols = 1, gcols = 0, pct_empty = 0.20) {

  if (!(version %in% c(9, 10))) stop("version must be 9 or 10")
  nrows <- as.integer(nrows)
  dcols <- as.integer(dcols)
  pcols <- as.integer(pcols)
  gcols <- as.integer(gcols)
  if (nrows < 1L) stop("nrows must be greater or equal to 1")
  if ( (dcols < 0L | pcols < 0L) | (dcols + pcols < 1L) ) stop("dcols and pcols need to be non-negative integers and sum to at least 1.")
  if (gcols < 0L) stop("gcols needs to be a non-negative integer.")
  if (pct_empty < 0 | pct_empty >= 1) stop("pct_empty needs to be within [0, 1).")

  
  e <- new.env()
  utils::data(list = c("icd9dx", "icd9pc", "icd10dx", "icd10pc"), package = "icdgenerator", envir = e)
  dxri <- seq(1, nrow(e[[paste0("icd", version, "dx")]]))
  pcri <- seq(1, nrow(e[[paste0("icd", version, "pc")]]))

  dx <- pc <- g <- matrix(nrow = nrows, ncol = 0)

  if (dcols > 0L) {
    dx <- sample(dxri, size = nrows * dcols, replace = TRUE)
    dx[sample(c(TRUE, FALSE), size = length(dx), prob = c(pct_empty, 1 - pct_empty), replace = TRUE)] <- NA 
    dx <- matrix(e[[paste0("icd", version, "dx")]]$code[dx], nrow = nrows)
    colnames(dx) <- paste("d", seq(1, dcols, by = 1), sep = "_") 
  }

  if (pcols > 0L) {
    pc <- sample(pcri, size = nrows * pcols, replace = TRUE) 
    pc[sample(c(TRUE, FALSE), size = length(pc), prob = c(pct_empty, 1 - pct_empty), replace = TRUE)] <- NA 
    pc <- matrix(e[[paste0("icd", version, "pc")]]$code[pc], nrow = nrows)
    colnames(pc) <- paste("p", seq(1, pcols, by = 1), sep = "_") 
  }

  if (gcols > 0L) {
    g  <- stats::rnorm(n = nrows * gcols)
    g[ sample(c(TRUE, FALSE), size = length(g),  prob = c(pct_empty, 1 - pct_empty), replace = TRUE)] <- NA
    g  <- matrix(g, nrow = nrows)
    colnames(g)  <- paste("g", seq(1, gcols, by = 1), sep = "_") 
  }

  tibble::add_column(dplyr::as_data_frame(cbind(dx, pc, g)), id = seq(1, nrows, by = 1), .before = 1L)
}

