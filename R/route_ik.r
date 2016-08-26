#------------------------------------------------------------------------------#
#----------------------------- route::route_ik.r ------------------------------#
#------------------------- author: gyang274@gmail.com -------------------------#
#------------------------------------------------------------------------------#

#--------+---------+---------+---------+---------+---------+---------+---------#
#234567890123456789012345678901234567890123456789012345678901234567890123456789#

#------------------------------------------------------------------------------#
#------------------------------------ data ------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------------ icon ------------------------------------#
#------------------------------------------------------------------------------#

#' ik_create_rgmp_icon_db
#' @description
#' create a rgmp_icon db for icon using in employee delivery application
#' icon: <icons code>-<color code>
#'  icons code (nm): offc - building, home - house, pkgs - package
#'  icons file (fl): offc - building, home - house, pkgs - package
#'  color code (cc): info, success, warning, danger, primary, purple and grey.
ik_create_rgmp_icon_db <- function() {

  rgmp_icon_nm <- c("offc", "home", "pkgs")
  rgmp_icon_fl <- c("building", "house", "package")
  rgmp_icon_cc <- c("info", "success", "warning", "danger", "primary", "purple", "grey")

  rgmp_icon <- ik_create_icon_list(rgmp_icon_nm, rgmp_icon_fl, rgmp_icon_cc)

  return( rgmp_icon )

}

ik_create_icon_list <- function(ik_nm, ik_fl, ik_cc) {

  ik_ll <- data.table::CJ(nm = ik_nm, cc = ik_cc) %>%
    dplyr::mutate(
      fl = plyr::mapvalues(nm, from = ik_nm, to = ik_fl, warn_missing = FALSE)
    ) %>%
    dplyr::mutate(
      ik = purrr::pmap(list(nm, fl, cc), ik_create_icon_list_single)
    ) %>%
    `class<-`(c("data.table", "data.frame"))

  eval(parse(text = paste0(
    'ik <- list(', paste0(paste0(ik_ll[["nm"]], '_', ik_ll[["cc"]], ' = ', 'ik_ll[nm == "', ik_ll[["nm"]], '" & ', 'cc == "', ik_ll[["cc"]], '", ik][[1]]'), collapse = ", "), ')'
  )))

  attr(ik, "class") <- "leaflet_icon_set"

  return(ik)

}

ik_create_icon_list_single <- function(nm, fl, cc) {

  eval(parse(text = paste0(

    'ik <- leaflet::makeIcon(',

    'iconUrl = system.file("icon/', cc, '/', fl, '-128.png", package = "route"), ',

    'iconRetinaUrl = system.file("/icon/', cc, '/', fl, '-512.png", package = "route"), ',

    'iconWidth = 24, iconHeight = 24',

    ')'

  )))

  return(ik)

}

#------------------------------------------------------------------------------#
