% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/route_rp.r
\name{rp_create_offc_rt_home_single_vs}
\alias{rp_create_offc_rt_home_single_vs}
\title{rp_create_offc_rt_home_single_vs}
\usage{
rp_create_offc_rt_home_single_vs(offc_id, home_id, pkgs_id, pkgs_pq_id = 0L)
}
\value{
rltX tb
 id: pkgs_id (0L - offc/home)
 vd: pkgs_vd, offc_vd, home_vd
}
\description{
subroutine of rp_create_offc_rt_home_single
create a signle route from offc to home with stops at packages pkgs_id
 and optional pkgs_pq_id, address visit sequence only
if pkgs_pq_id = 0L:
 create current route without additional delivery request
if pkgs_pq_id > 0L:
 create potential route with one additional at pkgs_pq_id
}

