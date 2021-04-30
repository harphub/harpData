#' Get harpData directories
#'
#' @return A named list
#' @export
#'
#' @examples
#' harpData_dirs()
harpData_dirs <- function() {
  list(
    FCTABLE_det = file.path("FCTABLE/deterministic"),
    FCTABLE_ens = file.path("FCTABLE/ensemble")
  )
}
