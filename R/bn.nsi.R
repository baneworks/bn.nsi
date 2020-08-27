# Manifest --------------------------------------------------------------------------------
#
# bn.nsi.R, Copyrights by Renat Levchuk (bane.tsk@gmail.com), 2020
#
# Part of package bn.nsi
#

# objects ---------------------------------------------------------------------------------

.nsi <- new.env(parent = emptyenv())
.nsi$fn <- NULL
.nsi$global <- NULL

.nsi$template_base <- list(tag = "",
                           dirs = list(data = "./data",
                                       results = "./res",
                                       graphs = "./res/graphs"))

.nsi$template_full <- list(tag = "",
                           dirs = list(data = "./data",
                                       results = "./res",
                                       rdata = "./res/data",
                                       graphs = "./res/graphs",
                                       reports = "./res",
                                       dicts = "./dicts"))

#' NSI struct template
#'
#' @format NSI is an list() and may contains at least this elements:
#' \describe{
#'   \item{tag}{Unique, amoung yours projects, tag with version}
#'   \item{data}{A path to input datasets}
#'   \item{dirs}{A list() with subpaths}
#'   \item{...}{Any records of any types you want}
#' }
#' \code{dirs} fields are follow:
#' \describe{
#'   \item{data}{A path to input datasets, mandatory}
#'   \item{results}{A path for all outputs produced, mandatory}
#'   \item{rdata}{A path for resulting datasets produced, optional}
#'   \item{graphs}{A path for graphics rendered, mandatory}
#'   \item{reports}{A path for store generated reports, , optional. Usual is same as \code{results}}
#'   \item{dicts}{A path for loading dictionaries, optional (if any)}
#' }
#' @name nsi-template
NULL

# internal funcs --------------------------------------------------------------------------

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("bn.nsi: setting TZ = \"UTC\"")
  Sys.setenv(TZ = "UTC")
}

# external funcs --------------------------------------------------------------------------

#' Check what object is a NSI structure
#'
#' \code{is.nsi} returns TRUE if object is a NSI structure
#'
#' \code{is.nsi(nsi)} ensures that \code{nsi} is a list and then check for this fields in \code{nsi}:
#' \describe{
#'  \item{tag}{a unique tag needed to distinct one nsi from another}
#'  \item{dir}{a list with data, results, graphs, reports, dict fields}
#' }
#'
#' @param nsi an NSI structure
#' @return TRUE if \code{nsi} is an NSI structure or FALSE if not
#' @seealso nsi-template
#' @export
is.nsi <- function(nsi) {
  if (!is.list(nsi)) return(FALSE)
  if (!is.character(nsi$tag)) return(FALSE)
  if (!is.list(nsi$dirs)) return(FALSE)
  # checking mandatory dirs
  if (!is.character(nsi$dirs$data)) return(FALSE)
  if (!is.character(nsi$dirs$results)) return(FALSE)
  if (!is.character(nsi$dirs$graphs)) return(FALSE)
  # all's ok
  return(TRUE)
}

#' Return TRUE if .nsi.global is set
#'
#' Ensures for .nsi.global is set
#'
#' @return TRUE if .nsi.global is not NULL
#' @export
is.nsi_loaded <- function() {
  if (!is.null(.nsi$global) && !is.null(.nsi$fn)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Return a copy of .nsi.global
#'
#' Wrapper for getting copy of .nsi.global
#'
#' @return copy of .nsi.global or fail
#' @export
nsi.global <- function() {
  if (is.nsi_loaded()) {
    return(.nsi$global)
  }
  abort("Global NSI missing!")
}

#' Wrapper function to check & load nsi from param or from nsi.global
#'
#' \code{nsi.params} intended for use inside functions
#' if \code{nsi} is correct NSI - check (if needed) and simple returns it
#' if \code{nsi == NULL} - return nsi.global or fail if not exists
#' checks may be suppressed
#'
#' @param nsi an NSI structure or NULL to load nsi.global
#' @param global If TRUE - try return nsi.global
#' @param check if FALSE - suppress checking (do this in stable code)
#' @return NSI structure
#' @seealso nsi-template
#' @export
nsi.params <- function(nsi = NULL, global = TRUE, check = FALSE) {
  if (!is.null(nsi) & !global) abort("Wrong args!")
  if (is.null(nsi)) {
    if (global) {
      return(nsi.global())
    } else {
      abort("NSI not specified and global is disabled!")
    }
  } else {
    if (check) {
      if (is.nsi(nsi)) {
        return(nsi)
      } else {
        abort("Param is not a NSI")
      }
    }
    return(nsi)
  }
}

#' Load NSI from file
#'
#' Load NSI from \code{file} (default) to nsi.global or reload
#'
#' @param file An name to JSON file, in case success load - will be saved to .nsi.fn
#' @param reload if FALSE (default) - do not overwrite nsi.globals
#' @return TRUE if success
#' @export
nsi.load <- function(file, reload = FALSE) {
  if (!file.exists(file)) abort(str_c("NSI file ", file, " do not exists!"))
  if (is.nsi_loaded()) {
    if (!reload) abort("To reload NSI, please specify \"reload\" agr")
  }
  nsi <- read_json(file)
  if (!is.nsi(nsi)) abort("Wrong NSI!")
  .nsi$fn <- file
  .nsi$global <- nsi
  return(TRUE)
}

#' Read NSI from file and return
#'
#' Used in case modification of NSI
#' Just read NSI from JSON and returns it
#'
#' @param file An name to JSON file
#' @return NSI if success
#' @export
nsi.read <- function(file) {
  if (!file.exists(file)) abort(str_c("NSI file ", file, "do not exists!"))
  nsi <- read_json(file)
  if (!is.nsi(nsi)) abort("Wrong NSI!")
  return(nsi)
}

#' Save NSI to JSON file
#'
#' Used in case modification of NSI
#' Just read NSI from JSON and returns it
#'
#' @inheritParams nsi.read
#' @param nsi if NULL - saving .nsi.global to file, else - save given nsi
#' @param force Overwrite JSON file
#' @return TRUE if seccess
#' @export
nsi.save <- function(file = NULL, nsi = NULL, force = FALSE) {
  if (is.null(nsi)) nsi <- .nsi$global
  if (is.null(file)) file <- .nsi$fn
  if (is.null(file)) abort("JSON filename unknown!")
  if (force & file.exists(file)) abort(str_c("NSI file ", file, "exists, please specify \"force\" to overwrite!"))
  if (!is.nsi(nsi)) abort("NSI corrupted!")
  write_json(nsi, file)
  return(TRUE)
}

#' Create NSI structure
#'
#' Create NSI structure from inner template
#' Template can be with optional fields
#'
#' @inheritParams nsi.read
#' @param tag A tag for project
#' @param optional if TRUE - use NSI with optional fields
#' @return TRUE if success
#' @export
nsi.init <- function(tag, file, optional = FALSE) {
   if (file.exists(file)) abort(str_c("NSI file ", file, "exists!"))
   if (optional) {
     .nsi$global <- .nsi$template_full
   } else {
     .nsi$global <- .nsi$template_base
   }
   .nsi$global$tag <- tag
   .nsi$fn <- file
   return(TRUE)
}

#' Utility functions for operate files
#'
#' @family functions for operate files
#'
#' @param nsi.dir A desired directory from NSI
#' @param fname A filename to glue with
#' @examples
#' ## generic
#' fpath("data", "dataset.xlsx")
#' ## rendered graphics
#' fgraph("plot.png")
#' @name fpath.funcs
NULL

#' @rdname fpath.funcs
#' @export
fpath <- function(nsi.dir, fname) {
  return(str_c(.nsi$global$dirs[[nsi.dir]], fname, sep = "/"))
}

#' @rdname fpath.funcs
#' @export
fdata <- function(fname) {
  return(str_c(.nsi$global$dirs["data"], fname, sep = "/"))
}

#' @rdname fpath.funcs
#' @export
frdata <- function(fname) {
  # FIXME: this isn't working beyong package
  # if (!exists(.nsi$global$dirs$rdata)) abort("Missing 'rdata' in NSI!")
  return(str_c(.nsi$global$dirs["rdata"], fname, sep = "/"))
}

#' @rdname fpath.funcs
#' @export
fgraph <- function(fname) {
  return(str_c(.nsi$global$dirs["graphs"], fname, sep = "/"))
}

#' @rdname fpath.funcs
#' @export
freport <- function(fname) {
  # if (!exists(.nsi$global$dirs$reports)) abort("Missing 'reports' in NSI!")
  return(str_c(.nsi$global$dirs["reports"], fname, sep = "/"))
}

#' @rdname fpath.funcs
#' @export
fdict <- function(fname) {
  # if (!exists(.nsi$global$dirs$dicts)) abort("Missing 'dicts' in NSI!")
  return(str_c(.nsi$global$dirs["dicts"], fname, sep = "/"))
}

#' Make vector from data.frame row
#'
#' @param row A row from data.frame / tibble
#' @return raw vector
#' @export
rflat <- function(row) {
  return(unlist(row, use.names = FALSE))
}
