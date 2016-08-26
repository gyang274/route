#------------------------------------------------------------------------------#
#------------------------------- route::route.r -------------------------------#
#------------------------- author: gyang274@gmail.com -------------------------#
#------------------------------------------------------------------------------#

#--------+---------+---------+---------+---------+---------+---------+---------#
#234567890123456789012345678901234567890123456789012345678901234567890123456789#

#------------------------------------------------------------------------------#
#------------------------------------ main ------------------------------------#
#------------------------------------------------------------------------------#

#' create_shortest_route_ft_visit
#' @description
#' create shortest route from a start to an end visit several stops using TSP
#' @param from start address (text)
#' @param to ende address (text)
#' @param visits stops and visits address in between start and ende
#' @return a named list of list(rftv, rftv_tbl)
#'  rftv: c(from, visits, to) address ordered by visit order in shortest route
#'  rftv_tbl: route data.frame in legs structure by following above rftv route
#' @rdname create_shortest_route_ft_visit
#' @export
create_shortest_route_ft_visit <- function(
  from, to, visits = NULL,
  mode = c("driving", "walking", "bicycling", "transit"),
  method = "concorde", control = NULL, ...
) {

  mode <- match.arg(mode)

  ## route from to visits
  rftv <- c(from, visits, to)

  nftv <- length(rftv)

  ## solve TSP when visit more than 1 address
  if ( nftv > 3L ) {

    mftv <- data.table::CJ(from = rftv, to = rftv, sorted = FALSE) %>%
      .[ , `:=`(
        fid = rep(1L:nftv, each = nftv), tid = rep(1L:nftv, times = nftv),
        miles = ggmap::mapdist(from = from, to = to, mode = mode)[["miles"]]
      )] %>%
      data.table::dcast(fid ~ tid, value.var = "miles") %>%
      .[ , fid := NULL] %>%
      as.matrix() %>% `rownames<-`(colnames(.))

    oftv <- create_tsp_path(m = mftv , mode = "tsp", method = method, control = control, ...)

    rftv <- rftv[oftv]

  }

  rftv_tbl <- NULL

  eval(parse(text = paste0(
    'rftv_tbl_', 1L:(nftv - 1L), ' <- ggmap::route(', 'from = rftv[', 1L:(nftv - 1L), '], to = rftv[', 2L:nftv , '], mode = mode, structure = "legs"', ')'
  )))

  eval(parse(text = paste0(
    'rftv_tbl <- rbind(', paste0('rftv_tbl_', 1L:(nftv - 1L), collapse = ", "), ')'
  )))

  return( list(rftv = rftv, rftv_tbl = rftv_tbl) )

}

#' create_tsp_path
#' @description
#' create a tsp tour wrapper over TSP package
#' @param m dist matrix
#' @param mode tsp or atsp
#' @param method pass to TSP::solve_TSP()
#' @param control pass to TSP::solve_TSP()
#' @param ... pass to TSP::solve_TSP()
#' @return
#'  tsp path start from 1st node and ended to last node in dist matrix
#' @rdname create_tsp_path
#' @export
create_tsp_path <- function(
  m, mode = c("tsp", "atsp"), method = "concorde", control = NULL, ...
) {

  mode <- match.arg(mode)

  # browser()

  m <- insert_dummy_node_tsp(m)

  if ( mode == "tsp" ) {

    m <- (m + t(m)) / 2

    a_tsp_instance <- TSP::TSP(m)

  } else if ( mode == "atsp" ) {

    a_tsp_instance <- TSP::ATSP(m)

  } else {

    stop("create_tsp_path: mode must be either tsp or atsp.\n")

  }

  if ( method == "concorde" ) concorde_path(check_os_concorde_path())

  a_tsp_solution <- TSP::solve_TSP(
    x = a_tsp_instance, method = method, control = control, ...
  )

  # a_tsp_dist <- tour_length(a_tsp_solution)

  a_tsp_tour = cut_tour(a_tsp_solution, "dmNode")

  if ( a_tsp_tour[1] == 2L ) {

    a_tsp_path = a_tsp_tour

  } else if (a_tsp_tour[length(a_tsp_tour)] == 2L ) {

    a_tsp_path = rev(a_tsp_tour)

  } else {

    stop("create_tsp_path: sth wrong, check route? ...\n")

    a_tsp_path = NULL

  }


  ## compensate for index taken
  ## by a dummy node previously
  a_tsp_path <- a_tsp_path - 1L

  return( a_tsp_path )

}

#' insert_dummy_node_tsp
#' @description
#' insert dummy node into a tsp/atsp dist matrix, subroutine of create_tsp_path.
#' @rdname create_tsp_path
#' @export
insert_dummy_node_tsp <- function(m) {

  rbind(
    dmNode = c(0L, 0L, rep(Inf, ncol(m) - 2L), 0L),
    cbind(
      dmNode = c(0L, rep(Inf, nrow(m) - 2L), 0L), m
    )
  )

}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------------ util ------------------------------------#
#------------------------------------------------------------------------------#
#' check_os_concorde_path
#' @description
#' check os and determine os specific concorde path
#' @export
check_os_concorde_path <- function() {

  a_os <- .Platform$OS.type

  if ( a_os == "unix" ) {

    os_concorde_path <- system.file("bin/unix", package = "route")

  } else if ( a_os == "windows" ) {

    os_concorde_path <- system.file("bin/windows", package = "route")

  } else {

    stop(
      "check_os_concorde_path: no unix no windows, in solaris? \n",
      "copy inst/bin/solaris/[x32|x64]/concorde and specify path."
    )

  }

  if ( os_concorde_path == "" ) {

    stop("check_os_concorde_path: cannot find concorde executable.\n")

  }

  return( os_concorde_path )

}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------------ draw ------------------------------------#
#------------------------------------------------------------------------------#

#' decode_line
#' @description
#' decode line from ggmap::route(from, to, structure = "route", output = "all")
#' wrapper over decode_line_cpp() and decode_line_r() depends on Rcpp available
#' @export
decode_line <- function(encoded) {

  if ( require(Rcpp) ) {

    return( decode_line_cpp(encoded) )

  } else {

    warning("decode_line: Rcpp is unavailable, decode_line_cpp() fallback to decode_line_r().\n")

    return( decode_line_r(encoded) )

  }

  # return( decode_line_r(encoded) )
}

#' create_and_draw_route_on_map
#' @description
#' create and draw route on map: an illustration of using decode_line_cpp()
#' @export
create_and_draw_route_on_map <- function(from, to) {

  local_rlt <- ggmap::route(
    from = from, to = to, structure = "route", output = "all"
  )

  local_rlt_latlng <- decode_line(
    local_rlt$routes[[1]]$overview_polyline$points
  )

  local_map <- leaflet() %>%
    addProviderTiles(
      "CartoDB.Positron",
      options = providerTileOptions(noWrap = TRUE)
    ) %>%
    addPolylines(
      local_rlt_latlng$lng, local_rlt_latlng$lat, fill = FALSE
    ) %>%
    addPopups(
      local_rlt_latlng$lng[1], local_rlt_latlng$lat[1], 'Origin'
    ) %>%
    addPopups(
      local_rlt_latlng$lng[length(local_rlt_latlng$lng)],
      local_rlt_latlng$lat[length(local_rlt_latlng$lng)],
      'Destination'
    )

  return( local_map )

}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------------ .cpp ------------------------------------#
#------------------------------------------------------------------------------#
# .onUnload: clean up when package is unloaded as C++ code use in package.
.onUnload <- function (libpath) {
  library.dynam.unload("route", libpath)
}
#------------------------------------------------------------------------------#
