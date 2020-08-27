# Manifest --------------------------------------------------------------------------------
#
# bn-nsi.R, Copyrigth by Renat Levchuk (bane.tsk@gmail.com), 2020
#
# Package: bn.nsi
#
# NSI load and save
#

#' bn.nsi: A package for loading, saving and passing to function project settings
#'
#' Interface for loading, saving and passing to function project settings
#'
#' @section Main functions for operate NSI:
#' \link{is.nsi}, \link{nsi.params}, \link{nsi.global}, \link{is.nsi_loaded}, \link{nsi.load}
#' \link{nsi.read}, \link{nsi.save}, \link{nsi.init}
#'
#' @section Utility functions for operate files:
#' \link{fdata}, \link{frdata}, \link{fgraph}, \link{freport}, \link{fdict}.
#'
#' @section Misc functions:
#' \link{rflat}
#'
#' @importFrom rlang abort
#' @importFrom stringr str_c
#' @importFrom jsonlite read_json write_json
#' @author Rinat Levchuk (bane.tsk@gmail.com)
#' @docType package
#' @name bn.nsi
NULL
