% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/route_rp.r
\name{rp_create_home_id}
\alias{rp_create_home_id}
\title{rp_create_home_id}
\usage{
rp_create_home_id(id, fn, ln, vd, md, img, fid)
}
\value{
status code:
 0L ok!
 1L id exists,
 2L NA latlon,
 3L fid not exists.
}
\description{
create an home id record into home tbl:
 1. in situ update global home tbl
 2. in situ update global rlth tbl - init current route as offc - home
}

