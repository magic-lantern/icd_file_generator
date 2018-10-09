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
#' @param quiet If \code{FALSE}, then report incremental timing.
#' To suppress incremental timing set \code{quiet = TRUE}.
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
generate_sample <- function(version = 10, nrows = 1, dcols = 1, pcols = 1, gcols = 0, pct_empty = 0.20, quiet = TRUE) {

  if (!(version %in% c(9, 10))) stop("version must be 9 or 10")
  nrows <- as.integer(nrows)
  dcols <- as.integer(dcols)
  pcols <- as.integer(pcols)
  gcols <- as.integer(gcols)
  if (nrows < 1L) stop("nrows must be greater or equal to 1")
  if ( (dcols < 0L | pcols < 0L) | (dcols + pcols < 1L) ) stop("dcols and pcols need to be non-negative integers and sum to at least 1.")
  if (gcols < 0L) stop("gcols needs to be a non-negative integer.")
  if (pct_empty < 0 | pct_empty >= 1) stop("pct_empty needs to be within [0, 1).")


  tictoc::tic("Total Time to Generate Data Set")
  e <- new.env()
  utils::data(list = c("icd9dx", "icd9pc", "icd10dx", "icd10pc"), package = "icdgenerator", envir = e)
  dxri <- seq(1, nrow(e[[paste0("icd", version, "dx")]]))
  pcri <- seq(1, nrow(e[[paste0("icd", version, "pc")]]))

  dx <- pc <- dn <- pn <- gn <- character()
  g <- numeric()
  d_i <- d_j <- p_i <- p_j <- g_i <- g_j <- integer()

  tictoc::tic("Generating Diagnostic Codes")
  if (dcols > 0L) {
    dx <- sample(dxri, size = nrows * dcols * (1 - pct_empty), replace = TRUE)
    dx <- e[[paste0("icd", version, "dx")]]$code[dx]
    d_i <- sample(seq(1, nrows, by = 1), length(dx), replace = TRUE)
    d_j <- sample(seq(1, dcols, by = 1), length(dx), replace = TRUE)
    dn  <- paste("d", 1:dcols, sep = "_")
  }
  tictoc::toc(quiet = quiet)

  tictoc::tic("Generating Procedure Codes")
  if (pcols > 0L) {
    pc <- sample(pcri, size = nrows * pcols * (1 - pct_empty), replace = TRUE)
    pc <- e[[paste0("icd", version, "pc")]]$code[pc]
    p_i <- sample(seq(1, nrows, by = 1), length(pc), replace = TRUE)
    p_j <- sample(seq(1, pcols, by = 1), length(pc), replace = TRUE)
    pn  <- paste("p", 1:pcols, sep = "_")
  }
  tictoc::toc(quiet = quiet)

  tictoc::tic("Generating gcols of random data")
  if (gcols > 0L) {
    g  <- sample(c(letters, LETTERS), size = nrows * gcols * (1 - pct_empty), replace = TRUE)
    g_i <- sample(seq(1, nrows, by = 1), length(g), replace = TRUE)
    g_j <- sample(seq(1, gcols, by = 1), length(g), replace = TRUE)
    gn  <- paste("g", 1:gcols, sep = "_")
  }
  tictoc::toc(quiet = quiet)

  tictoc::tic("Building the data set as a sparse matrix")
  out <- matrix(character(), nrow = nrows, ncol = dcols + pcols + gcols)
  rws <- c(d_i, p_i, g_i)
  cls <- c(d_j, dcols + p_j, dcols + pcols + g_j)
  dat <- c(dx, pc, g)
  for(i in seq(1, length(dx) + length(pc) + length(g))) {
    out[rws[i], cls[i]] <- dat[i]
  }
  tictoc::toc(quiet = quiet)

  tictoc::tic("coercing to standard matrix")
  out <- as.matrix(out)
  tictoc::toc(quiet = quiet)

  tictoc::tic("coercing to a data_frame")
  out <- dplyr::as_data_frame(out)
  tictoc::toc(quiet = quiet)

  tictoc::tic("adding id column and naming generated data columns")
  names(out) <- c(dn, pn, gn) 
  out <- tibble::add_column(out, id = seq(1, nrows, by = 1), .before = 1L)
  tictoc::toc(quiet = quiet)

  tictoc::toc(quiet = quiet)

  out
}

