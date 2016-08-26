#include <Rcpp.h>
using namespace Rcpp;

//' decode_line_cpp
//' @description
//' decode line return from ggmap::route(from, to, output = "all")
//' useful when draw route on map so route will follow real route.
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame decode_line_cpp(std::string encoded) {

  int index = 0;
  int len = encoded.size();
  int df_index = 0;
  long double lat = 0;
  long double lng = 0;
  std::vector<long double> longitude(0);
  std::vector<long double> latitude(0);
  if(encoded.size() == 0)
    return R_NilValue;
  longitude.reserve(30000);
  latitude.reserve(30000);
  while(index < len) {
    int b;
    int shift = 0;
    int result = 0;

    do {
      b = encoded[index++] - 63;
      result |= (b & 0x1f) << shift;
      shift += 5;
    } while(b >= 0x20);
    long double dlat = ((result & 1) ? ~(result >> 1) : (result >> 1));
    lat += dlat;
    latitude.push_back(lat * 1e-5);
    shift = 0;
    result = 0;
    do {
      b = encoded[index++] - 63;
      result |= (b & 0x1f) << shift;
      shift += 5;
    } while(b >= 0x20);
    long double dlng = ((result & 1) ? ~(result >> 1) : (result >> 1));
    lng += dlng;
    longitude.push_back(lng * 1e-5);
    df_index++;
  }
  return Rcpp::DataFrame::create(_["lat"] = latitude, _["lng"] = longitude);
}
