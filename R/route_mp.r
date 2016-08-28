#------------------------------------------------------------------------------#
#----------------------------- route::route_mp.r ------------------------------#
#------------------------- author: gyang274@gmail.com -------------------------#
#------------------------------------------------------------------------------#

#--------+---------+---------+---------+---------+---------+---------+---------#
#234567890123456789012345678901234567890123456789012345678901234567890123456789#

#------------------------------------------------------------------------------#
#------------------------------------ init ------------------------------------#
#------------------------------------------------------------------------------#

#' route::route_mp.r
#' @description
#'  a collection of operation on route maps in employee delivery application
#' @rdname mp
mp <- function() {}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#---------------------------------- init map ----------------------------------#
#------------------------------------------------------------------------------#

#' rgmp
#' @description
#' rgmp object hold global server leaflet map info
#' @return
#' rgmp object as a leaflet map object with icon and clickable popup
mp_init_rgmp_obj <- function() {

  leaflet(
    width = "100%", height = "400px"
  ) %>%
    addProviderTiles(
      "CartoDB.Positron",
      options = providerTileOptions(noWrap = TRUE)
    )

}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#----------- init map: create offc home pkgs db marker popup on map -----------#
#------------------------------------------------------------------------------#

#' mp_create_offc_marker
#' @description
#' create offc marker df:
#'  mk: layerId,
#'  ik: icon file name (depends on route r-pkgs inst/icon)
#'  lat: latitude
#'  lng: longitude
#'  popup: popup info
mp_create_offc_marker <- function(offc_id = NULL) {

  if (is.null(offc_id)) offc_id <- offc[["id"]]

  if (is.null(offc_id)) return(NULL)

  iJet <- grepl("Jet", offc[id %in% offc_id, nm], ignore.case = TRUE)

  dfmk <- offc %>%
    subset(
      id %in% offc_id
    ) %>%
    dplyr::mutate(
      mk = paste0("offc-", id)
    ) %>%
    dplyr::mutate(
      ik = paste0("offc_", ifelse(iJet, "purple", "primary"))
    ) %>%
    dplyr::mutate(
      popup = paste0(
        '<style> div.leaflet-popup-content {width: 325px; height: 120px;} </style>',
        '<style> img.leaflet-popup-offc-logo {width: 100px; height: auto; ',
        'border-radius: 10%; padding: 5px; object-fit: contain;} </style> ',
        '<div style="float:left; width:100px;">',
        '<img class="leaflet-popup-offc-logo" src="img/', ifelse(iJet, "jet.png", "walmart.png"), '" alt="office logo"/>',
        '</div>',
        '<div style="width:200px; float:right;" vertical-align="middle">',
        '<br/>',
        "<b>", nm, "</b> (ID: <b>", formatC(id, width = 4L, flag = "0"), ")</b><br/><br/>",
        "Office Address:<br/><b>", vd, "</b>",
        "</div>"
      )
    ) %>%
    dplyr::select(
      mk, ik, lat, lng, popup
    ) %>%
    `class<-`(c("data.table", "data.frame"))

  return(dfmk)

}

#' mp_create_home_marker
#' @description
#' create home marker df:
#'  mk: layerId,
#'  ik: icon file name, depends on route r-pkgs inst/icon
#'    and outside app folder www/img/<user-image.jpg/png>
#'  lat: latitude
#'  lng: longitude
#'  popup: popup info
#' @note
#' choose to put user image in outside app folder www/img/ for easy extension
mp_create_home_marker <- function(home_id) {

  if (is.null(home_id)) home_id <- home[["id"]]

  if (is.null(home_id)) return(NULL)

  dfmk <- home %>%
    subset(
      id %in% home_id
    ) %>%
    dplyr::mutate(
      mk = paste0("home-", id)
    ) %>%
    dplyr::mutate(
      ik = paste0("home_", "success")
    ) %>%
    dplyr::mutate(
      popup = paste0(
        '<style> div.leaflet-popup-content {width: 325px; height: 140px;} </style>',
        '<style> img.leaflet-popup-user-image {width: 100px; height: auto; ',
        'border-radius: 20%; padding: 5px; object-fit: contain; vertical-align: middle;} </style> ',
        '<div style="float:left; width:100px;">',
        '<br/>',
        '<img class="leaflet-popup-user-image" src="img/', img, '" alt="user image"/>',
        '</div>',
        '<div style="width:200px; float:right;">',
        '<br/><b>', fn, ' ', ln, '</b>', ' ',
        '(ID: <b>', formatC(id, width = 4L, flag = "0"), '</b>)<br/><br/>',
        'Home Address:<br/><b>', vd, '</b><br/><br/>',
        '# Package To  Devliver : <b>', lapply(aid, length), '</b><br/>',
        '# Package Has Delivered: <b>', lapply(did, length), '</b>',
        '</div>'
      )
    ) %>%
    dplyr::select(
      mk, ik, lat, lng, popup
    ) %>%
    `class<-`(c("data.table", "data.frame"))

  return(dfmk)

}

#' mp_create_pkgs_marker
#' @description
#' create pkgs marker df:
#'  mk: layerId,
#'  ik: icon file name (depends on route r-pkgs inst/icon)
#'  lat: latitude
#'  lng: longitude
#'  popup: popup info
mp_create_pkgs_marker <- function(pkgs_id) {

  if (is.null(pkgs_id)) pkgs_id <- pkgs[["id"]]

  if (is.null(pkgs_id)) return(NULL)

  iJet <- grepl("Jet", offc[match(pkgs[id %in% pkgs_id, fid], id), nm], ignore.case = TRUE)

  dfmk <- pkgs %>%
    subset(
      id %in% pkgs_id
    ) %>%
    dplyr::mutate(
      mk = paste0("pkgs-", id)
    ) %>%
    dplyr::mutate(
      ik = paste0("pkgs_", lapply(sid + 1L, switch, "primary", "purple", "success"))
    ) %>%
    dplyr::mutate(
      popup = paste0(
        '<style> div.leaflet-popup-content {width: 325px; height: 140px;} </style>',
        '<style> img.leaflet-popup-pkgs-logo {width: 100px; height: auto; ',
        'border-radius: 10%; padding: 5px; object-fit: contain;} </style> ',
        '<div style="float:left; width:100px;">',
        '<br/>',
        '<img class="leaflet-popup-pkgs-logo" src="img/pkg_', ifelse(iJet, "jet.jpg", "walmart.jpg"), '" alt="pkgs logo"/>',
        '</div>',
        '<div style="width:200px; float:right;">',
        '<br/>',
        "Package ID: <b>", formatC(id, width = 4L, flag = "0"), "</b><br/><br/>",
        "Package Address:<br/><b>", vd, "</b><br/><br/>",
        "Package Status: <b>", lapply(sid + 1L, switch, "Open", "Delivery Request Accepted", "Delivered"), "</b>.",
        "</div>"
      )
    ) %>%
    dplyr::select(
      mk, ik, lat, lng, popup
    ) %>%
    `class<-`(c("data.table", "data.frame"))

  return(dfmk)

}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#--------- main: add offc home pkgs db marker popup and route on map ----------#
#------------------------------------------------------------------------------#

#' mp_add_marker_on_maps
#' @description
#' map operation add marker in dfmk onto map rgmp
#' use mp_create_offc_marker, mp_create_home_marker, mp_create_pkgs_marker
mp_add_marker_on_maps <- function(mm, dfmk) {

  if (is.null(dfmk)) return(mm)

  mm %>%
    addMarkers(
      lat = dfmk[["lat"]],
      lng = dfmk[["lng"]],
      layerId = dfmk[["mk"]],
      icon = rgmp_icon[dfmk[["ik"]]],
      popup = dfmk[["popup"]]
    )

}

#' mp_add_popup_on_maps
#' @description
#' map operation add popup in dfmk onto map rgmp
#' use mp_create_offc_marker, mp_create_home_marker, mp_create_pkgs_marker
mp_add_popup_on_maps <- function(mm, dfmk) {

  if (is.null(dfmk)) return(mm)

  mm %>%
    addPopups(
      lat = dfmk[["lat"]],
      lng = dfmk[["lng"]],
      layerId = paste0("popup_id_", dfmk[["mk"]]),
      popup = dfmk[["popup"]]
    )

}

#' mp_add_route_on_maps
#' @description
#' map operation add route at run-time onto map (server/client)
#' @param
#' rltX list of ordered address vector
#' @return
#' a map object with one continuous route for each list component
mp_add_route_on_maps <- function(mm, rltX, color = "#605ca8") {

  if ( is.null(rltX) ) return( mm )

  for ( i in 1L:length(rltX) ) {

    if ( length(rltX[[i]]) >= 2L ) {

      for ( j in 1L:(length(rltX[[i]]) - 1L) ) {

        local_rlt <- ggmap::route(
          from = rltX[[i]][j], to = rltX[[i]][j + 1L],
          structure = "route", output = "all"
        )

        if ( length(local_rlt$routes) > 0L ) {

          local_rlt_latlng <- decode_line(
            local_rlt$routes[[1L]]$overview_polyline$points
          )

          mm <- mm %>%
            addPolylines(
              lat = local_rlt_latlng$lat,
              lng = local_rlt_latlng$lng,
              color = color,
              fill = FALSE
            )

        }

        ## google maps rate limits
        ## see rp_create_offc_rt_home_single() in route::route_rp.r
        Sys.sleep(0.5)

      }

    }

  }

  return( mm )

}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#--------- main: create rgmp server map and make server map operation ---------#
#------------------------------------------------------------------------------#

#' mp_create_rgmp_from_db
#' @description
#' map operation: create server side map rgmp from offc/home/pkgs db
mp_create_rgmp_from_db <- function(
  offc_id = NULL, home_id = NULL, pkgs_id = NULL, showPopup = FALSE
) {

  # ik: <icons code>-<color code>
  #   icons code: office, home, package
  #   color code: info, success, warning, danger, primary, purple and grey.

  mm <- mp_init_rgmp_obj()

  #- create marker dfmk for each db
  db <- c("offc", "home", "pkgs")

  eval(parse(text = paste0(
    "dfmk_", db, " <- mp_create_", db, "_marker(" , db, "_id)"
  )))

  eval(parse(text = paste0(
    "mm <- mm %>% mp_add_marker_on_maps(dfmk_", db, ")"
  )))

  if ( showPopup ) {

    eval(parse(text = paste0(
      "mm <- mm %>% mp_add_popup_on_maps(dfmk_", db, ")"
    )))

  }

  return(mm)

}


#' mp_update_rgmp_offc_id (animation)
#' @description
#' map operation: update server side map rgmp with new offc_id
#' @return
#'  1. rgmp (side effect): updated global rgmp with new offc_id
#'  2. rgmp_ms (return object): a transient rgmp with popup on new offc_id
mp_update_rgmp_offc_id <- function(offc_id, zoom = 12L) {

  lk_dfmk <- mp_create_offc_marker(offc_id = offc_id)

  rgmp <<- rgmp %>%
    mp_add_marker_on_maps(lk_dfmk)

  rgmp_ms <- rgmp %>%
    mp_add_popup_on_maps(lk_dfmk) %>%
    leaflet::setView(
      lat = mean(lk_dfmk[["lat"]], na.rm = TRUE),
      lng = mean(lk_dfmk[["lng"]], na.rm = TRUE),
      zoom = zoom
    )

  return(rgmp_ms)

}

#' mp_update_rgmp_home_id (animation)
#' @description
#' map operation: update server side map rgmp with new home_id
#' @return
#'  1. rgmp (side effect): updated global rgmp with new home_id
#'  2. rgmp_ms (return object): a transient rgmp with popup on new home_id
mp_update_rgmp_home_id <- function(home_id, zoom = 12L) {

  lk_dfmk <- mp_create_home_marker(home_id = home_id)

  rgmp <<- rgmp %>%
    mp_add_marker_on_maps(lk_dfmk)

  rgmp_ms <- rgmp %>%
    mp_add_popup_on_maps(lk_dfmk) %>%
    leaflet::setView(
      lat = mean(lk_dfmk[["lat"]], na.rm = TRUE),
      lng = mean(lk_dfmk[["lng"]], na.rm = TRUE),
      zoom = zoom
    )

  return(rgmp_ms)

}

#' mp_update_rgmp_pkgs_id (animation)
#' @description
#' map operation: update server side map rgmp with new pkgs_id
#' @return
#' 1. rgmp (side effect): updated global rgmp with new pkgs_id
#' 2. rgmp_ms (return object): a transient rgmp with popup on new pkgs_id
mp_update_rgmp_pkgs_id <- function(pkgs_id, zoom = 12L) {

  lk_dfmk <- mp_create_pkgs_marker(pkgs_id = pkgs_id)

  rgmp <<- rgmp %>%
    mp_add_marker_on_maps(lk_dfmk)

  rgmp_ms <- rgmp %>%
    mp_add_popup_on_maps(lk_dfmk) %>%
    leaflet::setView(
      lat = mean(lk_dfmk[["lat"]], na.rm = TRUE),
      lng = mean(lk_dfmk[["lng"]], na.rm = TRUE),
      zoom = zoom
    )

  return(rgmp_ms)

}

#' mp_update_rgmp_rlth_id (animation)
#' @description
#' map operation: update server side map rgmp with (current/potential) rlth_id
#' @return
#' 1. rgmp (side effect): no side effect on rgmp
#' 2. rgmp_ms (return object): a transient rgmp with current route to home
#' @note
#' we won't use mp_update_rgmp_rlth_id to update global rgmp like we did in offc/home/pkgs
#'  so we take an additional input mm as leaflet map object as we expect to add route info
#'  on local/transient map where some marker/popup is shown
mp_update_rgmp_rlth_id <- function(home_id, pkgs_pq_id = 0L, zoom = 12L, color = "#605ca8", mm = NULL) {

  offc_id <- home[id == home_id, fid]

  if (pkgs_pq_id == 0L) {

    lk_rltX <- list(c(
      offc[id == offc_id, vd],
      pkgs[["vd"]][match(unique(rlth[id == home_id & lid != 0L, lid]), pkgs[["id"]])],
      home[id == home_id, vd]
    ))

  } else {

    lk_rltX <- rp_create_offc_rt_home_single_vs(
      offc_id = offc_id, home_id = home_id, pkgs_id = home[id == home_id, aid][[1L]], pkgs_pq_id = pkgs_pq_id
    )

  }

  if (is.null(mm)) mm <- rgmp

  rgmp_ms <- mm %>%
    mp_add_route_on_maps(
      rltX = lk_rltX, color = color
    ) %>%
    setView(
      lat = mean(c(offc[id == offc_id, lat], home[id == home_id, lat]), na.rm = TRUE),
      lng = mean(c(offc[id == offc_id, lng], home[id == home_id, lng]), na.rm = TRUE),
      zoom = zoom
    )

  return(rgmp_ms)

}


#' mp_rgmp_pkgs_rk_home_sdrqst (animation)
#' @description
#' map operation: event response - pkgs_id send request to home_idlist
#'  a wrap over rp_create_pkgs_rk_home_sdrqst()
#' @return
#' 1. pkgs tbl and home tbl (side effect): update pkgs and home as request sent/sending
#' 2. rgmp_ms (return_object): a transient rgmp with ployline and popup on request sent/sending
mp_rgmp_pkgs_rk_home_sdrqst <- function(pkgs_id, ns_rqst = 2L, zoom = 12L) {

  a_pkgs <- pkgs[id == pkgs_id]

  a_pkgs_lat <- a_pkgs[["lat"]]
  a_pkgs_lng <- a_pkgs[["lng"]]

  #- home id who received request previously
  a_pkgs_hid_0 <- a_pkgs[["hid"]][[1L]]

  # message("mp_rgmp_pkgs_rk_home_sdrqst: call rp_create_pkgs_rk_home_sdrqst as make request sent.\n")

  rp_create_pkgs_rk_home_sdrqst(
    pkgs_id = pkgs_id, ns_rqst = ns_rqst
  )

  #- home id who received and is receiving request
  a_pkgs_hid_1 <- pkgs[id == pkgs_id][["hid"]][[1L]]

  a_pkgs_msgs <- NULL

  if ( length(a_pkgs_hid_1) > length(a_pkgs_hid_0) ) {

    #- home id who is receiving request
    a_pkgs_hid_d <- setdiff(a_pkgs_hid_1, a_pkgs_hid_0)

    a_pkgs_msgs <- paste0("Request has been sent to <b>", paste0(home[id %in% a_pkgs_hid_d, paste0(fn, " ", ln)], collapse = ", "), "</b>")

  } else {

    a_pkgs_hid_d <- NULL

    a_pkgs_msgs <- paste0("Request has been sent to all employee in this office who joined the program.\n")

  }

  #- modify map center and zoom level w.r.t event
  ## https://rstudio.github.io/leaflet/shiny.html
  ## http://wiki.openstreetmap.org/wiki/Zoom_levels
  rgmp_ms <- rgmp %>%
    setView(
      lat = a_pkgs_lat,
      lng = a_pkgs_lng,
      zoom = zoom
    ) %>%
    addPopups(
      layerId = paste0("active_pkgs_", pkgs_id),
      lat = a_pkgs_lat,
      lng = a_pkgs_lng,
      popup = a_pkgs_msgs
    )

  #- add ploylines for home received request previously
  if ( length(a_pkgs_hid_0) > 0L ) {

    dfS0 <- rbind(
      data.table(
        id = a_pkgs_hid_0,
        lat = rep(a_pkgs_lat, length(a_pkgs_hid_0)),
        lng = rep(a_pkgs_lng, length(a_pkgs_hid_0))
      ),
      data.table(
        id = a_pkgs_hid_0,
        lat = home[id %in% a_pkgs_hid_0, lat],
        lng = home[id %in% a_pkgs_hid_0, lng]
      )
    ) %>%
      .[order(id)]

    rgmp_ms <- rgmp_ms %>%
      addPolylines(
        data = dfS0,
        lat = ~lat,
        lng = ~lng,
        group = ~id,
        color = "#337ab7" # primary # "#00c0ef" # info
      )

  }

  #- add popup and ploylines for home receive request now
  if ( length(a_pkgs_hid_d) > 0L ) {

    dfS1 <- rbind(
      data.table(
        id = a_pkgs_hid_d,
        lat = rep(a_pkgs_lat, length(a_pkgs_hid_d)),
        lng = rep(a_pkgs_lng, length(a_pkgs_hid_d))
      ),
      data.table(
        id = a_pkgs_hid_d,
        lat = home[id %in% a_pkgs_hid_d, lat],
        lng = home[id %in% a_pkgs_hid_d, lng]
      )
    ) %>%
      .[order(id)]

    rgmp_ms <- rgmp_ms %>%
      addPopups(
        layerId = paste0("active_home_", home[id %in% a_pkgs_hid_d, id]),
        lat = home[id %in% a_pkgs_hid_d, lat],
        lng = home[id %in% a_pkgs_hid_d, lng],
        popup = paste0("<b>", home[id %in% a_pkgs_hid_d, paste0(fn, " ", ln)], ": Message Received!</b>")
      ) %>%
      addPolylines(
        data = dfS1,
        lat = ~lat,
        lng = ~lng,
        group = ~id,
        color = "#605ca8"
      )

  }

  return( rgmp_ms )

}

#' mp_rgmp_home_ak_pkgs (animation)
#' @description
#' map operation: event response - home_id accept pkgs_id delivery request
#'  a wrap over rp_create_home_ak_pkgs()
#' @return
#' 1. pkgs tbl and home tbl (side effect): update pkgs and home as request accepted
#' 2. rgmp_ms (return_object): a transient rgmp with ployline and popup on request accepted
mp_rgmp_home_ak_pkgs <- function(home_id, pkgs_id, zoom = 12L) {

  # message("mp_rgmp_home_ak_pkgs: call rp_create_home_ak_pkgs as request accepted.\n")

  rp_create_home_ak_pkgs(
    home_id = home_id, pkgs_id = pkgs_id
  )

  #- create a transient map view on event

  a_home <- home[id == home_id]

  a_home_lat <- a_home[["lat"]]

  a_home_lng <- a_home[["lng"]]

  pkgs_id <- sort(pkgs_id)

  a_pkgs <- pkgs[id %in% pkgs_id]

  a_pkgs_lat <- a_pkgs[["lat"]]

  a_pkgs_lng <- a_pkgs[["lng"]]

  a_home_msgs <- paste0(
    a_home[ , .(nm = paste0(fn, " ", ln))][["nm"]], ": ",
    "Package (", paste0(pkgs_id, collapse = ", "), ") Package Devliery Request Accepted!"
  )

  ## update package status
  rgmp <<- rgmp %>%
    leaflet::removeMarker(
      layerId = paste0("pkgs-", pkgs_id)
    )

  lk_dfmk <- mp_create_pkgs_marker(pkgs_id = pkgs_id)

  rgmp <<- rgmp %>%
    mp_add_marker_on_maps(lk_dfmk)

  ## update route to home
  rgmp_ms <- mp_update_rgmp_rlth_id(
    home_id = home_id, pkgs_pq_id = 0L, zoom = zoom
  )

  rgmp_ms <- rgmp_ms %>%
    # setView(
    #   lat = a_home_lat,
    #   lng = a_home_lng,
    #   zoom = zoom
    # ) %>%
    addPopups(
      layerId = paste0("active_home_", home_id),
      lat = a_home_lat,
      lng = a_home_lng,
      popup = a_home_msgs
    )

  #- add popup and ploylines for home receive request now
  if ( length(pkgs_id) > 0L ) {

    dfS0 <- rbind(
      data.table(
        id = pkgs_id,
        lat = rep(a_home_lat, length(pkgs_id)),
        lng = rep(a_home_lng, length(pkgs_id))
      ),
      data.table(
        id = pkgs_id,
        lat = a_pkgs_lat,
        lng = a_pkgs_lng
      )
    ) %>%
      .[order(id)]

    rgmp_ms <- rgmp_ms %>%
      addPopups(
        layerId = paste0("active_pkgs_", pkgs_id),
        lat = a_pkgs_lat,
        lng = a_pkgs_lng,
        popup = "Ok!"
      ) %>%
      addPolylines(
        data = dfS0,
        lat = ~lat,
        lng = ~lng,
        group = ~id,
        color = "#605ca8"
      )

  }

  return( rgmp_ms )

}

#' mp_rgmp_home_dk_pkgs (animation)
#' @description
#' map operation: event response - home_id decline pkgs_id delivery request
#'  a wrap over rp_create_home_dk_pkgs()
#' @return
#' 1. pkgs tbl and home tbl (side effect): update pkgs and home as request declined
#' 2. rgmp_ms (return_object): a transient rgmp with ployline and popup on request declined
mp_rgmp_home_dk_pkgs <- function(home_id, pkgs_id, zoom = 12L) {

  # message("mp_rgmp_home_ak_pkgs: call rp_create_home_ak_pkgs as request accepted.\n")

  rp_create_home_dk_pkgs(
    home_id = home_id, pkgs_id = pkgs_id
  )

  #- create a transient map view on event

  a_home <- home[id == home_id]

  a_home_lat <- a_home[["lat"]]

  a_home_lng <- a_home[["lng"]]

  pkgs_id <- sort(pkgs_id)

  a_pkgs <- pkgs[id %in% pkgs_id]

  a_pkgs_lat <- a_pkgs[["lat"]]

  a_pkgs_lng <- a_pkgs[["lng"]]

  a_home_msgs <- paste0(
    a_home[ , .(nm = paste0(fn, " ", ln))][["nm"]], ": ",
    "Package (", paste0(pkgs_id, collapse = ", "), ") Package Devliery Request Declined!"
  )

  ## update route to home
  # rgmp_ms <- mp_update_rgmp_rlth_id(
  #   home_id = home_id, pkgs_pq_id = 0L, zoom = zoom
  # )

  rgmp_ms <- rgmp %>%
    setView(
      lat = a_home_lat,
      lng = a_home_lng,
      zoom = zoom
    ) %>%
    addPopups(
      layerId = paste0("active_home_", home_id),
      lat = a_home_lat,
      lng = a_home_lng,
      popup = a_home_msgs
    )

  #- add popup and ploylines for home receive request now
  if ( length(pkgs_id) > 0L ) {

    dfS0 <- rbind(
      data.table(
        id = pkgs_id,
        lat = rep(a_home_lat, length(pkgs_id)),
        lng = rep(a_home_lng, length(pkgs_id))
      ),
      data.table(
        id = pkgs_id,
        lat = a_pkgs_lat,
        lng = a_pkgs_lng
      )
    ) %>%
      .[order(id)]

    rgmp_ms <- rgmp_ms %>%
      addPopups(
        layerId = paste0("active_pkgs_", pkgs_id),
        lat = a_pkgs_lat,
        lng = a_pkgs_lng,
        popup = "Ok!"
      ) %>%
      addPolylines(
        data = dfS0,
        lat = ~lat,
        lng = ~lng,
        group = ~id,
        color = "#605ca8"
      )

  }

  return( rgmp_ms )

}

#' mp_rgmp_home_dl_pkgs (animation)
#' @description
#' map operation: event response - home_id deliver pkgs_id delivery request
#'  a wrap over rp_create_home_dk_pkgs()
#' @return
#' 1. pkgs tbl and home tbl (side effect): update pkgs and home as request delivered
#' 2. rgmp (side effect): update global rgmp with pkgs status change
#' 3. rgmp_ms (return_object): a transient rgmp with ployline and popup on request delivered
mp_rgmp_home_dl_pkgs <- function(home_id, pkgs_id, zoom = 12L) {

  # message("mp_rgmp_home_ak_pkgs: call rp_create_home_ak_pkgs as request accepted.\n")

  rp_create_home_dl_pkgs(
    home_id = home_id, pkgs_id = pkgs_id
  )

  #- create a transient map view on event

  a_home <- home[id == home_id]

  a_home_lat <- a_home[["lat"]]

  a_home_lng <- a_home[["lng"]]

  pkgs_id <- sort(pkgs_id)

  a_pkgs <- pkgs[id %in% pkgs_id]

  a_pkgs_lat <- a_pkgs[["lat"]]

  a_pkgs_lng <- a_pkgs[["lng"]]

  a_home_msgs <- paste0(
    a_home[ , .(nm = paste0(fn, " ", ln))][["nm"]], ": ",
    "Package (", paste0(pkgs_id, collapse = ", "), ") Package Devliered!"
  )

  ## update package status
  rgmp <<- rgmp %>%
    leaflet::removeMarker(
      layerId = paste0("pkgs-", pkgs_id)
    )

  lk_dfmk <- mp_create_pkgs_marker(pkgs_id = pkgs_id)

  rgmp <<- rgmp %>%
    mp_add_marker_on_maps(lk_dfmk)

  ## update route to home
  rgmp_ms <- mp_update_rgmp_rlth_id(
    home_id = home_id, pkgs_pq_id = 0L, zoom = zoom
  )

  rgmp_ms <- rgmp_ms %>%
    # setView(
    #   lat = a_home_lat,
    #   lng = a_home_lng,
    #   zoom = zoom
    # ) %>%
    addPopups(
      layerId = paste0("active_home_", home_id),
      lat = a_home_lat,
      lng = a_home_lng,
      popup = a_home_msgs
    )

  #- add popup and ploylines for home receive request now
  if ( length(pkgs_id) > 0L ) {

    dfS0 <- rbind(
      data.table(
        id = pkgs_id,
        lat = rep(a_home_lat, length(pkgs_id)),
        lng = rep(a_home_lng, length(pkgs_id))
      ),
      data.table(
        id = pkgs_id,
        lat = a_pkgs_lat,
        lng = a_pkgs_lng
      )
    ) %>%
      .[order(id)]

    rgmp_ms <- rgmp_ms %>%
      addPopups(
        layerId = paste0("active_pkgs_", pkgs_id),
        lat = a_pkgs_lat,
        lng = a_pkgs_lng,
        popup = "Ok!"
      ) %>%
      addPolylines(
        data = dfS0,
        lat = ~lat,
        lng = ~lng,
        group = ~id,
        color = "#605ca8"
      )

  }

  return( rgmp_ms )

}

#' mp_remove_rgmp_dl_pkgs (animation)
#' @description
#' map operation: update server side map rgmp by removing package delivered
mp_remove_rgmp_dl_pkgs <- function(pkgs_id = NULL) {

  if (is.null(pkgs_id)) pkgs_id <- pkgs[sid == 2L][["id"]]

  rgmp <<- rgmp %>%
    leaflet::removeMarker(
      layerId = paste0("pkgs-", pkgs_id)
    )

  return(0L)
}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#--------- main: create cgmp client map and make client map operation ---------#
#------------------------------------------------------------------------------#

#' mp_create_cswb_home_id
#' @description
#' create social widget box for employee with home id impersonating
#' @return
#' a function wrapper over ygdashboard::socialWigetBox that take any shinyUI
#' component as ... input - simply a specific socialWigetBox for an employee
#' @note
#' This is a nicer box that will be used to wrap over client single tbl and map
mp_create_cswb_home_id <- function(home_id, ...) {

  a_home <- home[id == home_id]

  function(...) {

    ygdashboard::socialWigetBox(
      name = a_home[, paste0(fn, " ", ln)],
      image = paste0("img/", a_home[["img"]]), # "img/quicksilver.jpg",
      description = "",
      stat1 = length(a_home[["pid"]][[1L]]), subStat1 = "Pending Request(s)",
      stat2 = length(a_home[["aid"]][[1L]]), subStat2 = "Accepted Request(s)",
      stat3 = length(a_home[["did"]][[1L]]), subStat3 = "Delivered in Total",
      background = "purple",
      backgroundImage = "img/preikestolen.jpg",
      ...
    )

  }

}

#' mp_create_ctbl_home_id
#' @description
#' map operation: create client side tbl tb0 and tb1 for home_id impersonated
#' @return
#' tb0: current route to home include stops at accepted packages
#' tb1: potential route to home include one additional stops at each received request
mp_create_ctbl_home_id <- function(home_id) {

  #- create tb0
  tb0 <- rlth[id == home_id, .(
    miles = sum(miles),
    minutes = sum(minutes),
    turns = max(leg) - min(leg) + 1L
  ), by = lid]

  lid_list <- setdiff(tb0[["lid"]], 0L)

  tb0 <- tb0[ , .(
    fid = c(0L, lid_list),
    tid = c(lid_list, 0L),
    miles, minutes, turns
  )]

  tb0DT <- tb0 %>%
    dplyr::mutate(
      from = ifelse(fid == 0L, "Office", paste0("Package-", fid)),
      to = ifelse(tid == 0L, "Home", paste0("Package-", tid))
    ) %>%
    dplyr::select(
      -fid, -tid
    ) %>%
    data.table::setcolorder(
      c("from", "to", "miles", "minutes", "turns")
    ) %>%
    # dplyr::mutate(
    #   colname = enc2utf8(colname)
    # ) %>%
    DT::datatable(
      extensions = c("FixedHeader", "FixedColumns"),
      # filter = 'bottom',
      options = list(
        dom = "lfrtip",
        fixedHeader = TRUE,
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 2L, rightColumns = 0L),
        pageLength = 10L,
        lengthMenu = c(10L, 20L, 30L, 50L),
        initComplete = JS(
          "function(settings, json) {
            $(this.api().table().header()).css({'background-color': '#ffe0b3', 'color': '#000000'});
            $(this.api().table().body()  ).css({'background-color': '#cceeff', 'color': '#000000'});
          }")
      ),
      rownames = TRUE,
      colnames = c(
        "From" = "from",
        "To" = "to",
        "Miles" = "miles",
        "Minutes" = "minutes",
        "Turns" = "turns"
      ),
      escape = FALSE
        ) %>%
    DT::formatRound(
      columns = c(
        "Miles", "Minutes"
      ),
      digits = 4L
    )

  #- create tb1
  rltp <- rp_create_offc_rt_home(home_id = home_id)

  tb1 <- rltp[ , .(
    miles = sum(miles),
    minutes = sum(minutes),
    turns = max(leg)
  ), by = pid]

  tb1_0 <- tb1[pid == 0L]

  tb1_1 <- tb1[pid != 0L]

  tb1 <- rbind(
    tb1_0[ , .(
      pid, miles, minutes, turns,
      exmiles = 0.00,
      exminutes = 0.00,
      exturns = 0L
    )],
    tb1_1[ , .(
      pid, miles, minutes, turns,
      exmiles = miles - tb1_0[["miles"]],
      exminutes = minutes - tb1_0[["minutes"]],
      exturns = turns - tb1_0[["turns"]]
    )]
  )

  tb1DT <- tb1 %>%
    dplyr::mutate(
      pid = ifelse(pid == 0L, "Current Route", paste0("+ Package ", pid))
    ) %>%
    # dplyr::mutate(
    #   colname = enc2utf8(colname)
    # ) %>%
    DT::datatable(
      extensions = c("FixedHeader", "FixedColumns"),
      # filter = 'bottom',
      options = list(
        dom = "lfrtip",
        fixedHeader = TRUE,
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 1L, rightColumns = 0L),
        pageLength = 10L,
        lengthMenu = c(10L, 20L, 30L, 50L),
        initComplete = JS(
          "function(settings, json) {
            $(this.api().table().header()).css({'background-color': '#ffe0b3', 'color': '#000000'});
            $(this.api().table().body()  ).css({'background-color': '#cceeff', 'color': '#000000'});
          }")
      ),
      rownames = TRUE,
      colnames = c(
        "Route ID" = "pid",
        "Miles" = "miles",
        "Minutes" = "minutes",
        "Turns" = "turns",
        "Extra Miles" = "exmiles",
        "Extra Minutes" = "exminutes",
        "Extra Turns" = "exturns"
      ),
      escape = FALSE
        ) %>%
    DT::formatRound(
      columns = c(
        "Miles", "Minutes", "Extra Miles", "Extra Minutes"
      ),
      digits = 4L
    )

  return( list(tb0 = tb0, tb0DT = tb0DT, tb1 = tb1,  tb1DT = tb1DT) )

}

#' mp_create_cgmp_home_id
#' @description
#' map operation: create client side map cgmp for home_id impersonated
#' @return
#' cgmp:
#'  1. offc/home/ak_pkgs/rq_pkgs,
#'  2. current route to home include stops at accepted packages.
#' @note
#' we choose not to keep a global cgmp as we want to create carousel view in later version
mp_create_cgmp_home_id <- function(home_id, zoom = 12L) {

  a_home <- home[id == home_id]

  a_home_pkgs_id <- c(a_home[["aid"]][[1L]], a_home[["pid"]][[1L]])

  if (is.null(a_home_pkgs_id)) a_home_pkgs_id <- 0L # NULL would create a cgmp with all pkgs_id

  cgmp <- mp_create_rgmp_from_db(
    offc_id = a_home[["fid"]],
    home_id = home_id,
    pkgs_id = a_home_pkgs_id
  ) %>%
    leaflet::setView(
      lat = a_home[["lat"]],
      lng = a_home[["lng"]],
      zoom = zoom
    )

  rltX_0 <- rp_create_offc_rt_home_single_vs(
    offc_id = a_home[["fid"]],
    home_id = home_id,
    pkgs_id = a_home[["aid"]][[1L]],
    pkgs_pq_id = 0L
  ) %$% list(vd)

  cgmp <- cgmp %>%
    mp_add_route_on_maps(
      rltX = rltX_0, color = "#605ca8" # purple
    )

  return(cgmp)
}

#' mp_update_cgmp_home_pkgs
#' @description
#' map operation: update client side map cgmp with potential route stopped at pkgs_id
#' @return
#' cgmp:
#'  1. offc/home/ak_pkgs/rq_pkgs,
#'  2. current route to home include stops at accepted packages,
#'  3. potential route to home with one additonal stop at pkgs_id.
mp_update_cgmp_home_pkgs <- function(cgmp, home_id, pkgs_id) {

  a_home <- home[id == home_id]

  if ( !(pkgs_id %in% a_home[["pid"]][[1L]]) ) {

    warning('mp_update_cgmp_home_pkgs: pkgs_id should in a_home[["pid"]][[1L]]?\n')

  }

  rltX_1 <- rp_create_offc_rt_home_single_vs(
    offc_id = a_home[["fid"]],
    home_id = home_id,
    pkgs_id = a_home[["aid"]][[1L]],
    pkgs_pq_id = pkgs_id
  ) %$% list(vd)

  cgmp <- cgmp %>%
    mp_add_route_on_maps(
      rltX = rltX_1, color = "#337ab7" # primary
    )

  return(cgmp)
}

#' mp_create_cswb_ctbl_home_id
#' @description
#' map operation: create client view (socialWidget(tb0DT + tb1DT))
#' @return
#' a shinyUI widget
mp_create_cswb_ctbl_home_id <- function(
  input, output, session, home_id,
  outputId_tb0DT = "ctb0",
  inputId_tb0_dl = "ctb0_dl",
  outputId_tb1DT = "ctb1",
  inputId_tb1_ak = "ctb1_ak",
  inputId_tb1_dk = "ctb1_dk"
) {

  cswb <- mp_create_cswb_home_id(home_id = home_id)

  cswb(

    width = 4L,

    ygdashboard::chatMessage(
      name = "Scarlet Witch",
      image = "img/scarlet_witch.jpg",
      text = paste0("Hi, ", home[id == home_id, paste0(fn, " ", ln)], "! Welcome to the Employee Delivery Program. I am Scarlet Witch, and I will work with you on making some delivery today!"),
      position = "left",
      timestamp = Sys.time()
    ),

    br(),

    tags$div(

      align = "center",

      h4("Current Route to Home")

    ),

    DT::dataTableOutput(outputId = outputId_tb0DT),

    tags$div(

      align = "center",

      actionButton(

        inputId = inputId_tb0_dl, label = HTML("&nbsp;&nbsp;All Delivered!"), width = '85%',

        icon = icon("truck fa-lg"), class = "btn-primary", style = "margin-left: 18px; margin-right: 18px;"

      )

    ),

    br(),

    hr(),

    br(),

    tags$div(

      align = "center",

      h4("Route to Home with Package Requested for Devliery")

    ),

    DT::dataTableOutput(outputId = outputId_tb1DT),

    fluidRow(

      column(

        width = 6L,

        tags$div(

          align = "center",

          actionButton(

            inputId = inputId_tb1_dk, label = HTML("&nbsp;&nbsp;Decline"), width = '85%',

            icon = icon("times fa-lg"), class = "btn-primary", style = "margin-left: 18px; margin-right: 18px;"

          )

        )

      ),

      column(

        width = 6L,

        tags$div(

          align = "center",

          actionButton(

            inputId = inputId_tb1_ak, label = HTML("&nbsp;&nbsp;I Would Love to Deliver It!"), width = '85%',

            icon = icon("check fa-lg"), class = "btn-primary", style = "margin-left: 18px; margin-right: 18px;"

          )

        )

      )

    )

  )

}

#' mp_create_cswb_cgmp_home_id
#' @description
#' map operation: create client view (socialWidget(cgmp))
#' @return
#' a shinyUI widget
mp_create_cswb_cgmp_home_id <- function(
  input, output, session, home_id, outputId_cgmp = "cgmp"
) {

  cswb <- mp_create_cswb_home_id(home_id = home_id)

  cswb(

    width = 8L, leaflet::leafletOutput(outputId = outputId_cgmp, width = "100%", height = "800px")

  )

}

#' mp_create_cswb_view_home_id
#' @description
#' map operation:
#'  1. create client view into output (outputId_sw_ctbl, outputId_sw_cgmp):
#'    1.1 ctbl view mp_create_cswb_ctbl_home_id (socialWidget(tb0DT + tb1DT)) into outputId_sw_ctbl
#'    1.2 cgmp view mp_create_cswb_cgmp_home_id (socialWidget(cgmp)) into outputId_sw_cgmp
#'  2. fulfill tb0DT, tb1DT, cgmp in client view created with
#'    1.1 ctbl (tb0DT, tb1DT) from mp_create_ctbl_home_id
#'    1.2 cgmp (cgmp) from mp_create_cgmp_home_id
#' @return
#' ctbl list (tb0, tb0DT, tb1, tb1DT) and cgmp object (cgmp)
mp_create_cswb_view_home_id <- function(
  input, output, session, home_id,
  outputId_sw_ctbl = "sw_ctbl",
  outputId_sw_cgmp = "sw_cgmp",
  outputId_tb0DT = "ctb0",
  inputId_tb0_dl = "ctb0_dl",
  outputId_tb1DT = "ctb1",
  inputId_tb1_ak = "ctb1_ak",
  inputId_tb1_dk = "ctb1_dk",
  outputId_cgmp = "cgmp"
) {

  #- client ctbl (keep track in global environment)
  lk_ctbl <- mp_create_ctbl_home_id(home_id = home_id)

  output[[outputId_sw_ctbl]] <- shiny::renderUI({

    mp_create_cswb_ctbl_home_id(
      input, output, session, home_id = home_id,
      outputId_tb0DT = outputId_tb0DT,
      inputId_tb0_dl = inputId_tb0_dl,
      outputId_tb1DT = outputId_tb1DT,
      inputId_tb1_ak = inputId_tb1_ak,
      inputId_tb1_dk = inputId_tb1_dk
    )

  })

  output[[outputId_tb0DT]] <- DT::renderDataTable({ lk_ctbl[["tb0DT"]] })

  output[[outputId_tb1DT]] <- DT::renderDataTable({ lk_ctbl[["tb1DT"]] })

  #- client cgmp (keep track in global environment)
  lk_cgmp <- mp_create_cgmp_home_id(home_id = home_id)

  output[[outputId_sw_cgmp]] <- shiny::renderUI({

    mp_create_cswb_cgmp_home_id(
      input, output, session, home_id = home_id, outputId_cgmp = outputId_cgmp
    )

  })

  output[[outputId_cgmp]] <- leaflet::renderLeaflet({ lk_cgmp })

  return( c(lk_ctbl, cgmp = list(lk_cgmp)) )

}

#' mp_create_cswb_view_home_id_static
#' @description
#' map operation:
#'  create static client view into output - plain html tagList
#'    1 ctbl view socialWidget(tb0DT + tb1DT) - fulfilled with tb0DT, tb1DT
#'    2 cgmp view socialWidget(cgmp) fufilled with cgmp
#' @return
#'  a static view
#' @note
#'  this is designed to be used to create carouselItem in carousel overview
mp_create_cswb_view_home_id_static <- function(home_id) {

  #- client socialWidget
  cswb <- mp_create_cswb_home_id(home_id = home_id)

  #- client ctbl
  lk_ctbl <- mp_create_ctbl_home_id(home_id = home_id)

  lk_ctbl[["tb0DT"]][["width"]] = "100%"

  lk_ctbl[["tb0DT"]][["height"]] =  "180px"

  lk_ctbl[["tb1DT"]][["width"]] = "100%"

  lk_ctbl[["tb1DT"]][["height"]] =  "360px"

  #- client cgmp
  lk_cgmp <- mp_create_cgmp_home_id(home_id = home_id)

  lk_cgmp[["width"]] = "100%"

  lk_cgmp[["height"]] = "800px"

  #- client view (static)
  shiny::fluidRow(

    cswb(

      width = 4L,

      ygdashboard::chatMessage(
        name = "Scarlet Witch",
        image = "img/scarlet_witch.jpg",
        text = paste0("Hi, ", home[id == home_id, paste0(fn, " ", ln)], "! Welcome to the Employee Delivery Program. I am Scarlet Witch, and I will work with you on making some delivery today!"),
        position = "left",
        timestamp = Sys.time()
      ),

      br(),

      tags$div(

        align = "center",

        style="width:100%; height:200px;",

        h4("Current Route to Home"),

        lk_ctbl[["tb0DT"]],

        actionButton(

          inputId = paste0("ctb0_dl_hm", home_id), label = HTML("&nbsp;&nbsp;All Delivered!"), width = '85%',

          icon = icon("truck fa-lg"), class = "btn-primary", style = "margin-left: 18px; margin-right: 18px;"

        )

      ),

      br(),

      hr(),

      br(),

      tags$div(

        align = "center",

        style="width:100%; height:400px;",

        h4("Route to Home with Package Requested for Devliery"),

        lk_ctbl[["tb1DT"]],

        fluidRow(

          column(

            width = 6L,

            tags$div(

              align = "center",

              actionButton(

                inputId = paste0("ctb1_dk_hm", home_id), label = HTML("&nbsp;&nbsp;Decline"), width = '85%',

                icon = icon("times fa-lg"), class = "btn-primary", style = "margin-left: 18px; margin-right: 18px;"

              )

            )

          ),

          column(

            width = 6L,

            tags$div(

              align = "center",

              actionButton(

                inputId = paste0("ctb1_ak_hm", home_id), label = HTML("&nbsp;&nbsp;I Would Love to Deliver It!"), width = '85%',

                icon = icon("check fa-lg"), class = "btn-primary", style = "margin-left: 18px; margin-right: 18px;"

              )

            )

          )

        )

      )

    ),

    cswb(

      width = 8L, lk_cgmp

    )

  )

}

#------------------------------------------------------------------------------#
