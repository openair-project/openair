#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

//' Calculate Euclidean distance for trajectories
//' @export
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix distEuclid(NumericMatrix Am, NumericMatrix Bm) {
    int ncolumns = Am.ncol();
    int nrows = Am.nrow();
    
    // Create the results matrix directly in C++
    NumericMatrix Zm(ncolumns, ncolumns);
    
    for (int i = 0; i < ncolumns; i++) {
        for (int j = i; j < ncolumns; j++) { // j >= i: filling lower triangle logic
            double total_sum = 0.0;
            
            for (int k = 0; k < nrows; k++) {
                double d_lat = Am(k, i) - Am(k, j);
                double d_lon = Bm(k, i) - Bm(k, j);
                double lat_rad = Am(k, i) * M_PI / 180.0;
                
                total_sum += 110.0 * std::sqrt(std::pow(d_lat, 2) + 
                             std::pow(d_lon * std::cos(lat_rad), 2));
            }
            // Original logic assigns to Zm(j, i)
            Zm(j, i) = total_sum; 
        }
    }
    return Zm;
}

//' Distance matrix based on similarity of trajectory angles
//' @export
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix distAngle(NumericMatrix Lonm, NumericMatrix Latm) {
    int ncolumns = Lonm.ncol();
    int nrows = Lonm.nrow();
    
    NumericMatrix Zm(ncolumns, ncolumns);
    
    // Origin points (from the last row of the first column)
    double X0 = Lonm(nrows - 1, 0);
    double Y0 = Latm(nrows - 1, 0);
    
    for (int i = 0; i < ncolumns; i++) {
        for (int j = i + 1; j < ncolumns; j++) { // j > i: lower triangle
            double total_sum = 0.0;
            
            for (int k = 0; k < nrows - 1; k++) {
                double A = std::pow(Lonm(k, i) - X0, 2) + std::pow(Latm(k, i) - Y0, 2);
                double B = std::pow(Lonm(k, j) - X0, 2) + std::pow(Latm(k, j) - Y0, 2);
                double C = std::pow(Lonm(k, j) - Lonm(k, i), 2) + std::pow(Latm(k, j) - Latm(k, i), 2);
                
                // Dot product / Law of Cosines to find angle
                double val = 0.5 * (A + B - C) / std::sqrt(A * B);
                
                // Clamp value to handle potential floating point precision issues with acos
                if (val > 1.0) val = 1.0;
                if (val < -1.0) val = -1.0;
                
                total_sum += std::acos(val);
            }
            Zm(j, i) = total_sum / (nrows - 1);
        }
    }
    return Zm;
}