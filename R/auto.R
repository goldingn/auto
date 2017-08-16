#' @importFrom goodpractice gp
#' @importFrom utils unzip
get_source <- function (pkg, dir = NULL) {

  temp_dir <- tempfile()
  dir.create(temp_dir)

  pkg_remote <- remotes:::github_remote(pkg)
  pkg_zip_path <- remotes:::remote_download(pkg_remote)
  utils::unzip(pkg_zip_path, exdir = temp_dir)
  src_path <- list.files(temp_dir, full.names = TRUE)[1]

  if (!is.null(dir)) {

    pkg_name <- strsplit(pkg, "/")[[1]]
    pkg_name <- pkg_name[length(pkg_name)]

    src_files <- list.files(src_path,
                            all.files = TRUE,
                            full.names = TRUE)
    src_files <- src_files[-(1:2)]

    src_path <- file.path(dir, pkg_name)
    if (!dir.exists(src_path))
      dir.create(src_path)

    file.copy(src_files,
              src_path,
              recursive = TRUE)

  }

  invisible(src_path)

}

has_tests <- function (path)
  dir.exists(file.path(path, "tests"))

#' @importFrom devtools check spell_check
#' @importFrom goodpractice all_checks gp results
run_checks <- function (path) {

  message("running R CMD check (as CRAN)")
  devtools::check(path)

  message("running spell check")
  devtools::spell_check(path)

  gp_checks <- goodpractice::all_checks()
  gp_checks <- gp_checks[grep("^rcmdcheck", gp_checks, invert = TRUE)]
  gp_checks <- gp_checks[gp_checks != "no_description_date"]
  gp_checks <- gp_checks[gp_checks != "description_url"]
  gp_checks <- gp_checks[gp_checks != "description_bugreports"]

  if (!has_tests(path))
    gp_checks <- gp_checks[gp_checks != "covr"]

  message("running goodpractice checks")
  gp_object <- goodpractice::gp(path, checks = gp_checks)
  gp_results <- goodpractice::results(gp_object)

  # suppress rstudio markers when printing
  old_val <- options()$goodpractice.rstudio_source_markers
  options(goodpractice.rstudio_source_markers = FALSE)
  on.exit({options(goodpractice.rstudio_source_markers = old_val)})
  print(gp_object)

  invisible(gp_results)

}

#' Run Automatic Package Checks
#'
#' @description Run some quick automatic checks on a package hosted on GitHub,
#'   using \code{\link[devtools:check]{devtools::check}()} and
#'   \code{\link[goodpractice:gp]{goodpractice::gp}()}.
#'
#' @param pkg a string giving the directory of the github repo in the format
#'   "user/repo"
#' @param dir an optional file path giving a directory into which the package
#'   source code will be saved. If \code{NULL}, a temporary directory will be
#'   used.
#'
#' @details The results of \code{devtools::check()} and
#'   \code{goodpractice::gp()} will be printed to the console. RStudio markers
#'   are suppressed, and some checks are skipped.
#'
#'   To check a package hosted on CRAN, you can use the cran github mirror,
#'   e.g.: "cran/devtools".
#'
#' @export
#'
#' @examples
#' \dontrun{
#' auto::check("goldingn/auto")
#' }
check <- function (pkg, dir = NULL) {

  path <- get_source(pkg, dir)
  run_checks(path)

  if (!has_tests(path))
    message("\nthe package has no tests\n")

}

#' Run Automatic Package Checks
#' @name auto
#'
#' @description A personal package to automate the checks I think are
#'   important when reviewing R package Application papers for Methods in
#'   Ecology and Evolution. It isn't not designed to be configurable, nor
#'   intended as a statement of how all R packages should be implemented.
#'
#'   The only exported function is \code{\link{check}}
#'
#' @docType package
NULL
