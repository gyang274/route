#------------------------------------------------------------------------------#
#----------------------------- route::route_rp.r ------------------------------#
#------------------------- author: gyang274@gmail.com -------------------------#
#------------------------------------------------------------------------------#

#--------+---------+---------+---------+---------+---------+---------+---------#
#234567890123456789012345678901234567890123456789012345678901234567890123456789#

#------------------------------------------------------------------------------#
#------------------------------------ init ------------------------------------#
#------------------------------------------------------------------------------#

#' route::route_rp.r
#' @description
#'  a collection of operation on route in employee delivery application
#' @rdname rp
rp <- function() {}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#---------------------------------- init dat ----------------------------------#
#------------------------------------------------------------------------------#

#' offc
#' @description
#' offc tbl hold office info
#' @return
#' offc tbl in data.table
#'  id: int scalar: office id
#'  nm: chr string: office name
#'  vd: chr string: office address
#'  lat: num scalar: latitude
#'  lng: num scalar: longitude
rp_init_offc_tbl <- function(loadInitTbl = TRUE) {

  offc <- NULL

  if ( loadInitTbl ) {

    offc <- data(offc)

  }

  if ( is.null(offc) ) {

    offc <- data.table(
      id = integer(0L),
      nm = character(0L),
      vd = character(0L),
      lat = numeric(0L),
      lng = numeric(0L)
    )

  }

  return( offc )

}

#' home
#' @description
#' home tbl hold employee info
#' @return
#' home tbl in data.table
#'  id: int scalar: home id
#'  fn: chr string: employee first name
#'  ln: chr string: employee last name
#'  vd: chr string: home address
#'  md: chr string: commute mode
#'    driving, walking, bicycling, transit
#'  img: chr string: path to image file
#'  lat: num scalar: latitude
#'  lng: num scalar: longitude
#'  fid: int scalar: employee's office id
#'    office id must exist when insert an employee in such office
#'  pid: int single list: pending request(s) of package id(s)
#'  aid: int single list: accepted request(s) of package id(s)
#'  did: int single list: declined request(s) of package id(s)
#'  lid: int single list: delivered request(s) of package id(s)
rp_init_home_tbl <- function(loadInitTbl = TRUE) {

  home <- NULL

  if ( loadInitTbl ) {

    home <- data(home)

  }

  if ( is.null(home) ) {

    home <- data.table(
      id = integer(0L),
      fn = character(0L),
      ln = character(0L),
      vd = character(0L),
      md = character(0L),
      img = character(0L),
      lat = numeric(0L),
      lng = numeric(0L),
      fid = integer(0L),
      pid = list(),
      aid = list(),
      did = list(),
      lid = list()
    )

  }

  return( home )

}

#' pkgs
#' @description
#' pkgs tbl hold package info
#' @return
#' pkgs tbl in data.table
#'  id: int scalar: package id
#'  vd: chr string: package address
#'  lat: num scalar: latitude
#'  lng: num scalar: longitude
#'  fid: int scalar: package's office id
#'    office id must exist when insert a package in such office
#'  kid: int single list: ranked home id for sending request [x]
#'    kid is removed from pkgs tbl as rank should be determined
#'    at runtime depends on other packages accepted by employee
#'  hid: int single list: home id delivery request has been sent
#'  aid: int scalar: employee home id accepted delivery request first
#'    aid will be cleaned after delivery, move into lid
#'  did: int single list: employee home id declined delivery request
#'  lid: int scalar: employee home id delivered package should be aid
#'    lid should be package id previouly delivered and will include aid
#'    lid should be cleaned after a period of time for performance issue
#'    and in that case we should create another column for tracking n_lid
#'  sid: int scalar: package status code:
#'    0L open, 1L accepted, and 2L delivered
rp_init_pkgs_tbl <- function(loadInitTbl = TRUE) {

  pkgs <- NULL

  if ( loadInitTbl ) {

    pkgs <- data(pkgs)

  }

  if ( is.null(pkgs) ) {

    pkgs <- data.table(
      id = integer(0L),
      vd = character(0L),
      lat = numeric(0L),
      lng = numeric(0L),
      fid = integer(0L),
      hid = list(),
      aid = integer(0L),
      did = list(),
      lid = integer(0L),
      sid = integer(0L)
    )

  }

  return( pkgs )

}

#' rlth
#' @description
#' rlth tbl hold route to home info:
#'  indexed by id - pid: shortest route of offc - (pkgs.ak + (pkgs.rq)) - home
#'  pkgs.ak: packages accepted by employee to deliver on way to home
#'  pkgs.rq: packages with delivery request send to employee pending for reply
#'    pending means not accepted, not declined, and not accepted by other employee
#'  (): optional
#' @return
#' rlth tbl in data.table
#'  id: int scalar: home id
#'  pid: int scalar: package id in request (0L as current route to home w.o. request)
#'  lid: int scalar: package id going to visit and deliver within each id - pid route
#'  miles: numeric scalar: miles from (startLat, startLng) to (endLat, endLng)
#'  minutes: numeric scalar: minutes from (startLat, startLng) to (endLat, endLng)
#'  leg: int scalar: leg from ggmap::route(from, to, structure = "legs")
#'  startLat: numeric scalar: start latitude in leg
#'  startLng: numeric scalar: start longitude in leg
#'  endLat: numeric scalar: end latitude in leg
#'  endLng: numeric scalar: end longitude in leg
#' @note
#'  rlth tbl is indexed by home id and package pid
#'  rlth tbl on server side (global environment) should store current route to home only,
#'    e.g., package pid is 0L, whereas rlth tbl in client side (local environment) should
#'    contain both current route to home and potential route one each for all pid which a
#'    deliver request received.
#'  rlth tbl on server side should update when
#'    1. new home_id joined in home tbl
#'    2. a package deliver request is accepted
#'  rlth tbl on client side should generated at runtime when
#'    1. a client call to rp_create_offc_rt_home()
#'  also, lid are package's id within each route indexed by id - pid,
#'    it is the package we are heading toward, and as before 0L is home.
rp_init_rlth_tbl <- function(loadInitTbl = TRUE) {

  rlth <- NULL

  if ( loadInitTbl ) {

    rlth <- data(rlth)

  }

  if ( is.null(rlth) ) {

    rlth <- data.table(
      id = integer(0L),
      pid = integer(0L),
      lid = integer(0L),
      miles = numeric(0L),
      minutes = numeric(0L),
      leg = integer(0L),
      startLat = numeric(0L),
      startLng = numeric(0L),
      endLat = numeric(0L),
      endLng = numeric(0L)
    )

  }

  return( rlth )

}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#----------------------- init dat: create db id records -----------------------#
#------------------------------------------------------------------------------#

#' rp_create_offc_id
#' @description
#' create an offc id record into offc tbl:
#'  1. in situ update global offc tbl
#' @return
#' status code:
#'  0L ok!
#'  1L id exists,
#'  2L NA latlon.
rp_create_offc_id <- function(id, nm, vd) {

  if ( id %in% offc[["id"]] ) return(1L)

  latlon <- ggmap::geocode(
    location = vd, output = "latlon"
  )

  if ( any(is.na(latlon)) ) return(2L)

  offc <<- offc %>%
    rbind(
      data.table(
        id = id,
        nm = nm,
        vd = vd,
        lat = latlon[["lat"]],
        lng = latlon[["lon"]]
      )
    )

  return(0L)

}

#' rp_create_home_id
#' @description
#' create an home id record into home tbl:
#'  1. in situ update global home tbl
#'  2. in situ update global rlth tbl - init current route as offc - home
#' @return
#' status code:
#'  0L ok!
#'  1L id exists,
#'  2L NA latlon,
#'  3L fid not exists.
rp_create_home_id <- function(id, fn, ln, vd, md, img, fid) {

  if ( id %in% home[["id"]] ) return(1L)

  latlon <- ggmap::geocode(
    location = vd, output = "latlon"
  )

  if ( any(is.na(latlon)) ) return(2L)

  home <<- home %>%
    rbind(
      data.table(
        id = id,
        fn = fn,
        ln = ln,
        vd = vd,
        md = md,
        img = img,
        lat = latlon[["lat"]],
        lng = latlon[["lon"]],
        fid = fid,
        pid = list(),
        aid = list(),
        did = list(),
        lid = list()
      )
    )

  if ( !(fid %in% offc[["id"]]) ) return(3L)

  #- update global rlth tbl
  rlth <<- rlth %>%
    rbind(
      rp_create_offc_rt_home_single(
        offc_id = fid, home_id = id, pkgs_id = NULL, pkgs_pq_id = 0L
      )
    )

  return(0L)

}

#' rp_create_pkgs_id
#' @description
#' create an pkgs id record into pkgs tbl:
#'  1. in situ update global pkgs tbl
#' @return
#' status code:
#'  0L ok!
#'  1L id exists,
#'  2L NA latlon,
#'  3L fid not exists.
rp_create_pkgs_id <- function(id, vd, fid) {

  if ( id %in% pkgs[["id"]] ) return(1L)

  latlon <- ggmap::geocode(
    location = vd, output = "latlon"
  )

  if ( any(is.na(latlon)) ) return(2L)

  pkgs <<- pkgs %>%
    rbind(
      data.table(
        id = id,
        vd = vd,
        lat = latlon[["lat"]],
        lng = latlon[["lon"]],
        fid = fid,
        hid = list(),
        aid = NA_integer_,
        did = list(),
        lid = NA_integer_,
        sid = 0L
      )
    )

  #- update global offc tbl

  if ( !(fid %in% offc[["id"]]) ) return(3L)

  return(0L)

}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------- main: create route to home -------------------------#
#------------------------------------------------------------------------------#

#' rp_create_offc_rt_home
#' @description
#' create route from offc to home with stops at package accepted
#'  (+ one requested) for delivery, calculate shortest route by TSP
#' @param
#' home_id: home id employee
#' @return
#' home_id rlth tbl (current + potential)
#' @note
#' one of the most often used function in application
rp_create_offc_rt_home <- function(home_id) {

  #- home
  lk_home <- home[id == home_id]

  pkgs_pq_id <- c(0L, lk_home[["pid"]][[1]])

  #- note must use L in last to keep pkgs_pq_id as integer in call
  eval(parse(text = paste0(
    'rlth_', pkgs_pq_id, ' <- rp_create_offc_rt_home_single(',
    'offc_id = lk_home[["fid"]], home_id = home_id, ',
    'pkgs_id = lk_home[["aid"]][[1]], pkgs_pq_id = ', pkgs_pq_id, 'L',
    ')'
  )))

  eval(parse(text = paste0(
    'lk_rlth <- rbind(',
    paste0('rlth_', pkgs_pq_id, collapse = ", "),
    ')'
  )))

  return( lk_rlth )

}

#' rp_create_offc_rt_home_krnt
#' @description
#' create route from offc to home with stops at package accepted
#'  for delivery in an earlier time, calculate shortest route by TSP
#' @param
#' home_id: home id employee
#' @return
#' status code: 0L ok!
#' @note
#' a wrap over rp_create_offc_rt_home_single
#' a special case of rp_create_offc_rt_home, with current route only,
#'  update on rlth tbl directly and return status code instead of rlth tbl
rp_create_offc_rt_home_krnt <- function(home_id) {

  a_home <- home[id == home_id]

  a_rlth <- rp_create_offc_rt_home_single(
    offc_id = a_home[["fid"]],
    home_id = home_id,
    pkgs_id = a_home[["aid"]][[1L]],
    pkgs_pq_id = 0L
  )

  rlth <<- rlth[id != home_id] %>% rbind(a_rlth)

  return(0L)

}

#' rp_create_offc_rt_home_single
#' @description
#' subroutine of rp_create_offc_rt_home
#' create a signle route from offc to home with stops at packages pkgs_id
#'  and optional pkgs_pq_id
#' if pkgs_pq_id = 0L:
#'  create current route without additional delivery request
#' if pkgs_pq_id > 0L:
#'  create potential route with one additional at pkgs_pq_id
#' @return
#' rlth tb
rp_create_offc_rt_home_single <- function(
  offc_id, home_id, pkgs_id, pkgs_pq_id = 0L
) {

  #- create a local dt with offc, pkgs, home id and vd
  #- lk_dt has optimized visit sequence using TSP
  #- overwrite offc and home (local) id as 0L focuse on pkgs id order
  lk_dt <- rp_create_offc_rt_home_single_vs(
    offc_id, home_id, pkgs_id, pkgs_pq_id
  )

  #- ns: number of stops
  ns <- nrow(lk_dt)

  #- create current/potential route to home
  lk_md <- home[id == home_id, md]

  ## note: Error (list) object cannot be coerced to type 'integer'
  ## error when ns > 12, e.g., when make 11+ request simultaneously - google maps rate limits
  ## http://stackoverflow.com/questions/25083512/error-list-object-cannot-be-coerced-to-type-integer
  ## https://developers.google.com/maps/documentation/directions/usage-limits
  eval(parse(text = paste0(
    'rlt_', 1L:(ns - 1L), ' <- ggmap::route(',
    'from = lk_dt[', 1L:(ns - 1L), ', vd, drop = TRUE], to = lk_dt[', 2L:ns , ', vd, drop = TRUE], mode = lk_md, structure = "legs"',
    ') %>% data.table() %>% ',
    '.[ , `:=`(lid = lk_dt[', 2L:ns , ', id, drop = TRUE])] %>% ',
    'setnames("startLon", "startLng") %>% ',
    'setnames("endLon", "endLng") %>% ',
    '.[ , .(lid, miles, minutes, leg, startLat, startLng, endLat, endLng)]; ',
    'Sys.sleep(0.5)'
  )))

  eval(parse(text = paste0(
    'lk_rlth <- rbind(',
    paste0('rlt_', 1L:(ns - 1L), collapse = ", "),
    ') %>% ',
    '.[ , `:=`(id = home_id, pid = pkgs_pq_id)] %>% ',
    'setcolorder(c("id", "pid", setdiff(colnames(.), c("id", "pid"))))'
  )))

  return(lk_rlth)

}

#' rp_create_offc_rt_home_single_vs
#' @description
#' subroutine of rp_create_offc_rt_home_single
#' create a signle route from offc to home with stops at packages pkgs_id
#'  and optional pkgs_pq_id, address visit sequence only
#' if pkgs_pq_id = 0L:
#'  create current route without additional delivery request
#' if pkgs_pq_id > 0L:
#'  create potential route with one additional at pkgs_pq_id
#' @return
#' rltX tb
#'  id: pkgs_id (0L - offc/home)
#'  vd: pkgs_vd, offc_vd, home_vd
rp_create_offc_rt_home_single_vs <- function(
  offc_id, home_id, pkgs_id, pkgs_pq_id = 0L
) {

  #- pkgs_id
  if ( pkgs_pq_id > 0L ) pkgs_id <- c(pkgs_id, pkgs_pq_id)

  #- create a local dt with offc, pkgs, home id and vd
  #- overwrite offc and home (local) id as 0L focuse on pkgs id order
  lk_dt <- rbind(
    offc[id == offc_id, .(id = 0L, vd)],
    pkgs[id %in% pkgs_id, .(id, vd)],
    home[id == home_id, .(id = 0L, vd)]
  )

  #- ns: number of stops
  ns <- nrow(lk_dt)

  if ( ns > 3L ) {

    #- dist matrix
    mm <- data.table::CJ(
      from = lk_dt[["vd"]], to = lk_dt[["vd"]], sorted = FALSE
    ) %>%
      .[ , `:=`(
        fid = rep(1L:ns, each  = ns),
        tid = rep(1L:ns, times = ns),
        miles = ggmap::mapdist(
          from = from, to = to, mode = home[id == home_id, md]
        )[["miles"]]
      )] %>%
      data.table::dcast(
        fid ~ tid, value.var = "miles"
      ) %>%
      .[ , fid := NULL] %>%
      as.matrix() %>% `rownames<-`(colnames(.))

    #- sorted stop
    ss <- create_tsp_path(
      m = mm , mode = "tsp", method = "concorde", control = NULL
    )

    lk_dt <- lk_dt[ss, ]

  }

  return(lk_dt)

}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#----------------------------- main: event handle -----------------------------#
#------------------------------------------------------------------------------#
#' rp_create_pkgs_rk_home_sdrqst
#' @description
#' create pkgs a ranked home_idlist and send deliver request to top ns_rqst
#'  unsent home_id, using rp_create_pkgs_rk_home_idlist()
#' @return
#' status code: 0L ok!, 1L request has been sent to all home_id in this offc_id.
rp_create_pkgs_rk_home_sdrqst <- function(pkgs_id, ns_rqst = 2L) {

  a_pkgs <- pkgs[id == pkgs_id]

  ## all employee registered in the same office
  lk_home <- home[fid == a_pkgs[["fid"]]]

  ## all employee registered in the same office - request has been sent
  lk_home_sent <- a_pkgs[["hid"]][[1L]]

  if ( length(lk_home_sent) >= nrow(lk_home) ) return(1L)

  ## all employee registered in the same office - updated ranked dist
  lk_home_rank <- rp_create_pkgs_rk_home_idlist(pkgs_id = pkgs_id)

  ## all employee registered in the same office - who should request be sent
  lk_home_idlist_sdrqst <- lk_home_rank %>%
    setdiff(lk_home_sent) %>% .[1L:min(ns_rqst, length(.))]

  ## update on pkgs tbl - hid
  pkgs[
    id == pkgs_id,
    `:=`(
      hid = lapply(hid, function(x) { c(x, lk_home_idlist_sdrqst) } )
    )
  ]

  ## update on home tbl - pid
  home[
    id %in% lk_home_idlist_sdrqst,
    `:=`(
      pid = lapply(pid, function(x) { c(x, pkgs_id) })
    )
  ]

  return(0L)

}

#' rp_create_pkgs_rk_home_idlist
#' @description
#' create pkgs a ranked home_idlist for sending deliver request
#' @note
#' This approach is ranking home_id based on dist(pkgs_id, current_route_segment)
#'  using yg::calculate_dist_point_segment().
#' This ranking algorithm should be run on runtime to catch recent accepted pkgs
#'  and which causes changes on current route to home.
#' @note
#' TODO: use ggmap::route(from, to, structure = "route", output = 'all') to
#'  capture detailed route path and calculate dist(pkgs, route) [?]
#' This is a better approximation on extramile and extraminutes, but way more
#'  computation, and hard to save into df format.
#' This approach has its advantage siginificant only when there is long route
#'  such as highway travel.
#' However, in such case, employee might not want to detour and exit highway.
rp_create_pkgs_rk_home_idlist <- function(pkgs_id) {

  a_pkgs <- pkgs[id == pkgs_id]

  ## all employee registered in the same office
  lk_home <- home[fid == a_pkgs[["fid"]]]

  ## all employee registered in the same office - current route to home
  lk_rlth <- rlth[id %in% lk_home[["id"]]] # & pid == 0L]

  ## check dist: package_latlon -> route_leg_latlon
  ranked_home_idlist <- lk_rlth[ , .(
    id, startLat, startLng, endLat, endLng,
    pkgsLat = a_pkgs[["lat"]],
    pkgsLng = a_pkgs[["lng"]]
  )][ , dist := yg::calculate_dist_point_segment(
    pkgsLat, pkgsLng, startLat, startLng, endLat, endLng
  )][ , .(dist = min(dist)), by = id][order(dist)][["id"]]

  return(ranked_home_idlist)

}

#' rp_create_home_ak_pkgs
#' @description
#' operation on server side as response to event from client side:
#'  an employee (home_id) accept (ak) a package (pkgs_id) delivery request first
#' @event
#' home:
#'  id == home_id: move pkgs_id from pid into aid
#'  id != home_id: remove pkgs_id from pid
#' pkgs:
#'  id %in% pkgs_id: assign home_id into aid, update status sid 0L -> 1L
#' rlth:
#'  id == home_id: update current route to home by including pkgs_id in route
#' @param
#' home_id: int scalar
#' @param
#' pkgs_id: int vector
#' @return
#' status code: 0L ok!, 1L home_id not receive deliver request for pkgs_id
#' @note
#' empoloyee is the first employee who accepts delivery request of this package
rp_create_home_ak_pkgs <- function(home_id, pkgs_id) {

  #- check pkgs_id in home_id's pid list
  if (!(all(pkgs_id %in% home[id == home_id][["pid"]][[1L]]))) return(1L)

  #- update on home tbl
  ## id == home_id: push pkgs_id into aid
  home[
    id == home_id,
    `:=`(
      aid = lapply(aid, function(x) { c(x, pkgs_id) } )
    )
  ]
  ## all id remove pkgs_id from pid list
  home[
    ,
    `:=`(
      pid = lapply(pid, function(x) { setdiff(x, pkgs_id) })
    )
  ]

  #- update on pkgs tbl
  pkgs[
    id %in% pkgs_id,
    `:=`(
      aid = home_id, sid = 1L
    )
  ]

  #- update on rlth tbl
  rp_create_offc_rt_home_krnt(home_id)

  return(0L)

}

#' rp_create_home_dk_pkgs
#' @description
#' operation on server side as response to event from client side:
#'  an employee (home_id) decline (dk) a package (pkgs_id) delivery request
#'  decline before anyone accept - might trigger server sending request [?]
#'  if so, server should check has all request sent been declined and decide
#' @event
#' home:
#'  id == home_id: move pkgs_id from pid into did
#' pkgs:
#'  id %in% pkgs_id: push home_id into did, check all hid in did [?]
#' @param
#' home_id: int scalar
#' @param
#' pkgs_id: int vector
#' @return
#' status code:
#'  0L ok!
#'  1L home_id not receive deliver request for pkgs_id
#'  2L pkgs_id all request sent earlier has been declined
rp_create_home_dk_pkgs <- function(home_id, pkgs_id) {

  #- check pkgs_id in home_id's pid list
  if (!(all(pkgs_id %in% home[id == home_id][["pid"]][[1L]]))) return(1L)

  #- update on home tbl
  ## id == home_id: move pkgs_id from pid into did
  home[
    id == home_id,
    `:=`(
      pid = lapply(pid, function(x) { setdiff(x, pkgs_id) } ),
      did = lapply(did, function(x) { c(x, pkgs_id) } )
    )
  ]

  #- update on pkgs tbl
  pkgs[
    id %in% pkgs_id,
    `:=`(
      did = lapply(did, function(x) { c(x, home_id) } )
    )
  ]

  #- check if pkgs_id being declined has any other active request alive
  a_pkgs_active_request <- pkgs[
    id %in% pkgs_id, .(all_declined = (length(hid[[1L]]) == length(did[[1L]]))), by = id
  ]

  if ( any(a_pkgs_active_request[["all_declined"]]) ) {

    message(
      "rp_create_home_dk_pkgs: pkgs_id: ",
      paste0(a_pkgs_active_request[force(all_declined), id], collapse = ", "), " ",
      "all request sent has been declined: should send a new deliver request?"
    )

    return(2L)

  }

  return(0L)

}

#' rp_create_home_dl_pkgs
#' @description
#' operation on server side as response to event from client side:
#'  an employee (home_id) delivered (dl) a package (pkgs_id) accepted earlier
#' allow deliver all accepted packages at once in current version,
#' as a simulation of confirmation of delivery when arrived back home,
#' should allow confirm on delivery one package at a time in later version
#' @event
#' home:
#'  id == home_id: move pkgs_id from aid into lid
#' pkgs:
#'  id %in% pkgs_id: assign home_id into lid, update status sid 1L -> 2L
#' rlth:
#'  id == home_id: update current route to home by removing lid in route [?]
#'  should postpone update on rlth in later version and show partial route till home [?]
#' @param
#' home_id: int scalar
#' @param
#' pkgs_id: int vector
#' @return
#' status code:
#'  0L ok!
#'  1L home_id not accept deliver request for some pkgs_id, or
#'     pkgs_id not all accepted being confirmed as delivered.
#' @note
#' param home_id is enough in current version, as pkgs_id should always be aid,
#' still keep pkgs_id as a param for a later extension on confirm pkgs_id being
#' delivered one at a time
rp_create_home_dl_pkgs <- function(home_id, pkgs_id) {

  #- check pkgs_id is exactly home_id's aid list
  ## allow deliver all accepted packages at once
  if (!(setequal(pkgs_id, home[id == home_id][["aid"]][[1L]]))) return(1L)

  #- update on home tbl
  ## id == home_id: move pkgs_id from aid into lid
  home[
    id == home_id,
    `:=`(
      aid = lapply(aid, function(x) { setdiff(x, pkgs_id) } ),
      lid = lapply(lid, function(x) { c(x, pkgs_id) } )
    )
  ]

  #- update on pkgs tbl
  ## id %in% pkgs_id: assign home_id into lid, update status sid 1L -> 2L
  pkgs[
    id %in% pkgs_id,
    `:=`(
      lid = home_id, sid = 2L
    )
  ]

  #- update on rlth tbl
  rp_create_offc_rt_home_krnt(home_id)

  return(0L)

}

#------------------------------------------------------------------------------#
