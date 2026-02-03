#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

//' Optimized Euclidean distance for trajectories
//' @export
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix distEuclid(NumericMatrix Am, NumericMatrix Bm) {
    int ncolumns = Am.ncol();
    int nrows = Am.nrow();
    
    NumericMatrix Zm(ncolumns, ncolumns);
    
    // Constant for degree-to-radian conversion
    const double deg2rad = M_PI / 180.0;
    
    // Buffer to store pre-calculated cosine values for the current column 'i'
    std::vector<double> cos_lats(nrows);
    
    for (int i = 0; i < ncolumns; i++) {
        
        // 1. OPTIMIZATION: Pre-calculate Cosine terms for column i
        // This moves the heavy trigonometry out of the inner 'j' loop.
        // We also create pointers to column i for faster access.
        NumericMatrix::Column col_Am_i = Am.column(i);
        NumericMatrix::Column col_Bm_i = Bm.column(i);
        
        for (int k = 0; k < nrows; k++) {
            cos_lats[k] = std::cos(col_Am_i[k] * deg2rad);
        }
        
        // 2. Inner Loop
        for (int j = i; j < ncolumns; j++) { 
            double total_sum = 0.0;
            
            // Pointers to column j
            NumericMatrix::Column col_Am_j = Am.column(j);
            NumericMatrix::Column col_Bm_j = Bm.column(j);
            
            for (int k = 0; k < nrows; k++) {
                double d_lat = col_Am_i[k] - col_Am_j[k];
                double d_lon = col_Bm_i[k] - col_Bm_j[k];
                
                // 3. OPTIMIZATION: Use pre-calculated cosine and simple multiplication
                // d_lon needs to be scaled by the cosine of the latitude (from column i)
                double d_lon_scaled = d_lon * cos_lats[k];
                
                total_sum += std::sqrt((d_lat * d_lat) + (d_lon_scaled * d_lon_scaled));
            }
            
            // 4. OPTIMIZATION: Apply the 110.0 scalar once at the end
            Zm(j, i) = total_sum * 110.0;
        }
    }
    return Zm;
}

// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

//' Optimized Distance matrix based on similarity of trajectory angles
//' @export
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix distAngle(NumericMatrix Lonm, NumericMatrix Latm) {
    int ncolumns = Lonm.ncol();
    int nrows = Lonm.nrow();
    
    // We only iterate up to k < nrows - 1, just like the original code
    int n_points = nrows - 1; 
    
    NumericMatrix Zm(ncolumns, ncolumns);
    
    // Origin points (Fixed based on the last row of the first column)
    double X0 = Lonm(nrows - 1, 0);
    double Y0 = Latm(nrows - 1, 0);
    
    // 1. PRE-CALCULATION STEP
    // Instead of calculating lengths and differences repeatedly in the nested loop,
    // we calculate the normalized (unit) vector for every point relative to the origin once.
    
    // Store as flat vectors for speed. 
    // Layout: Point 0 of Col 0, Point 1 of Col 0 ... Point N of Col M
    std::vector<double> Ux(n_points * ncolumns);
    std::vector<double> Uy(n_points * ncolumns);
    
    for (int i = 0; i < ncolumns; i++) {
        for (int k = 0; k < n_points; k++) {
            double dx = Lonm(k, i) - X0;
            double dy = Latm(k, i) - Y0;
            
            // Calculate magnitude squared
            double mag_sq = dx * dx + dy * dy;
            
            int idx = k + i * n_points; // Linear index
            
            if (mag_sq > 0) {
                double mag = std::sqrt(mag_sq);
                Ux[idx] = dx / mag;
                Uy[idx] = dy / mag;
            } else {
                Ux[idx] = 0.0;
                Uy[idx] = 0.0;
            }
        }
    }
    
    // 2. MAIN LOOP
    // Now we just compute the dot product of the pre-calculated unit vectors.
    // Cost reduced from ~20 math ops per point to ~4 ops per point.
    
    for (int i = 0; i < ncolumns; i++) {
        // Pointers to the start of the data for column i
        const double* uxi_ptr = &Ux[i * n_points];
        const double* uyi_ptr = &Uy[i * n_points];
        
        for (int j = i + 1; j < ncolumns; j++) { 
            double total_sum = 0.0;
            
            // Pointers to the start of the data for column j
            const double* uxj_ptr = &Ux[j * n_points];
            const double* uyj_ptr = &Uy[j * n_points];
            
            for (int k = 0; k < n_points; k++) {
                // The Cosine of the angle is simply the dot product of unit vectors
                // No Sqrt, No Div, No Pow needed here.
                double val = uxi_ptr[k] * uxj_ptr[k] + uyi_ptr[k] * uyj_ptr[k];
                
                // Clamp to domain of acos [-1, 1] to handle float imprecision
                if (val > 1.0) val = 1.0;
                else if (val < -1.0) val = -1.0;
                
                total_sum += std::acos(val);
            }
            
            Zm(j, i) = total_sum / n_points;
        }
    }
    
    return Zm;
}