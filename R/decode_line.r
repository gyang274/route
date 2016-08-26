#' decode_line_r
#' @description
#' google polyline decoder borrowed from:
#' http://facstaff.unca.edu/mcmcclur/GoogleMaps/EncodePolyline/decode.js
#' @export
decode_line_r <- function(encoded) {

  if ( !require(stringr) ) {
    stop("decode_line_r: require stringr.\n")
  }

  if ( !require(bitops) ) {
    stop("decode_line_r: require bitops.\n")
  }

  len = stringr::str_length(encoded)
  encoded <- strsplit(encoded, NULL)[[1]]
  index = 1
  N <- 100000
  df.index <- 1
  array = matrix(nrow = N, ncol = 2)
  lat = 0
  dlat = 0
  lng = 0
  dlng = 0
  b = 0
  shift = 0
  result = 0

  while(index <= len) {
    shift = 0
    result = 0

    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if(b < 0x20) break
    }
    dlat = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lat = lat + dlat;

    shift = 0
    result = 0
    b = 0
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if(b < 0x20) break
    }
    dlng = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lng = lng + dlng

    array[df.index,] <- c(lat = lat * 1e-05, lng = lng * 1e-5)
    df.index <- df.index + 1
  }

  ret <- data.frame(array[1:df.index - 1,])
  names(ret) <- c("lat", "lng")
  return(ret)
}
