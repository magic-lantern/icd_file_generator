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

  tictoc::tic("Generating Diagnostic Codes")
  d <- data.table::as.data.table(replicate(dcols, sample(e[[paste0("icd", version, "dx")]]$code, nrows, replace=TRUE)))
  colnames(d) <- paste('dx', 1:dcols, sep = "_")
  tictoc::toc(quiet = quiet)

  tictoc::tic("Generating Procedure Codes")
  p <- data.table::as.data.table(replicate(pcols, sample(e[[paste0("icd", version, "pc")]]$code, nrows, replace=TRUE)))
  colnames(p) <- paste('pc', 1:pcols, sep = "_")
  tictoc::toc(quiet = quiet)
  
  tictoc::tic("Generating gcols of random data")
  g <- data.table::as.data.table(replicate(gcols, sample(100000:999999, nrows, replace=TRUE)))
  colnames(g) <- paste('g', 1:gcols, sep = "_")
  tictoc::toc(quiet = quiet)

  tictoc::tic("Binding one large data.table")
  all_data <- data.table::copy(d)
  data.table::set(all_data, j = colnames(p), value = p)
  data.table::set(all_data, j = colnames(g), value = g)
  tictoc::toc(quiet = quiet)

  tictoc::tic("removing data at random")
  # for (j in 1:nrow(all_data)) {
  #   s <- sample(1:length(all_data), length(all_data) * pct_empty, replace = FALSE)
  #   data.table::set(all_data, i = j , s, NA) # integers using 'L' passed for efficiency
  # }
  mkmissing <- lapply(stats::rbinom(nrow(all_data), ncol(all_data), pct_empty),
                      function(size) {
                        sample(colnames(all_data), size)
                      })
  for (i in 1:length(mkmissing)) {
    if (length(mkmissing[[i]])) {
      data.table::set(all_data, i = i, j = mkmissing[[i]], value = NA)
    }
  }
  tictoc::toc(quiet = quiet)
  
  tictoc::tic("adding id column")
  # all_data[, id := 1:nrows, ]
  data.table::set(all_data, j = "id", value = 1:nrows)
  data.table::setcolorder(all_data, c("id", names(all_data)[1:(length(all_data)-1)]))
  tictoc::toc(quiet = quiet)
  
  tictoc::toc() # total time
  all_data 
}

