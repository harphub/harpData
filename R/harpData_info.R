#' Get info about data in the harpData package
#'
#' @param file_format The file format in harpData that you want information for.
#' @param fctable_fcst_type In the case of \code{file_format = "fctable"}, the
#'   data are separated into deterministic and ensemble groups. Set to "det" to
#'   get the info for deterministic data and "ens" for ensemble data.
#'
#' @return A named list giving information about directories, start and end
#'   dates, available forecast models for the file format and in the case of
#'   ensemble data, the members and where appropriate the lags of the members.
#' @export
#'
#' @examples
#' harpData_dirs("vfld")
#' harpData_dirs("grib")
#' harpData_dirs("netcdf")
#' harpData_dirs("fctable", "det")
#' harpData_dirs("fctable", "ens")
#' harpData_dirs("vobs")
#' harpData_dirs("obstable")
#'
harpData_info <- function(
  file_format       = c(
    "vfld", "grib", "netcdf", "fctable", "vobs", "obstable"
  ),
  fctable_fcst_type = c("det", "ens")
) {

  file_format       <- match.arg(file_format)
  fctable_fcst_type <- match.arg(fctable_fcst_type)

  switch(
    file_format,
    "vfld" = list(
      dir                = system.file("vfld", package = "harpData"),
      start_date         = 2019021700,
      end_date           = 2019021718,
      meps_prod_members  = seq(0, 10),
      cmeps_prod_members = c(0, 1, 3, 4, 5, 6),
      cmeps_prod_lags    = c(0, 0, 2, 2, 1, 1),
      fcst_models        = c(
        "AROME_Arctic_eps",
        "AROME_Arctic_prod",
        "CMEPS_prod",
        "MEPS_prod"
      )
    ),

    "grib" = list(
      dir         = system.file("grib", "AROME_Arctic", package = "harpData"),
      start_date  = 2018071000,
      end_date    = 2018071000,
      fcst_models = "AROME_Arctic"
    ),

    "netcdf" = list(
      dir                = system.file("netcdf", "AAEPS", package = "harpData"),
      start_date         = 2018071000,
      end_date           = 2018071000,
      fcst_models        = "AAEPS",
      format_options_set = "met_norway_eps"
    ),

    "fctable" = switch(
      fctable_fcst_type,
      "det" = list(
        dir        = system.file(
          "fctable", "deterministic", package = "harpData"
        ),
        start_date  = 2019021700,
        end_date    = 2019021718,
        fcst_models = c("AROME_Arctic_prod", "MEPS_prod")
      ),
      "ens" = list(
        dir        = system.file(
          "fctable", "ensemble", package = "harpData"
        ),
        start_date  = 2019021700,
        end_date    = 2019021718,
        fcst_models = c("CMEPS_prod", "MEPS_prod", "multimodel_eps")
      )
    ),

    "vobs" = list(
      dir        = system.file("vobs", package = "harpData"),
      start_date = 2019021700,
      end_date   = 2019022023,
      by         = "1h"
    ),

    "obstable" = list(
      dir        = system.file("OBSTABLE", package = "harpData"),
      start_date = 2019021700,
      end_date   = 2019022023,
      by         = "1h"
    )

  )

}
