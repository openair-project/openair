#include <Rcpp.h>
#include <vector>
#include <cmath>

using namespace Rcpp;

// --- Helper: LDLT Solver (Same as before) ---
std::vector<double> solve_ldlt(std::vector<double>& d0, 
                               std::vector<double>& d1, 
                               std::vector<double>& d2, 
                               const std::vector<double>& b, 
                               int n, int d) {
    for(int i = 0; i < n; i++) {
        if(d0[i] < 1e-12) d0[i] = 1e-12; 
        if(i < n-1) {
            double div1 = d1[i] / d0[i]; d1[i] = div1; d0[i+1] -= d0[i] * div1 * div1;
            if(d == 2 && i < n-2) d1[i+1] -= d0[i] * div1 * (d2[i]/d0[i]);
        }
        if(d == 2 && i < n-2) {
            double div2 = d2[i] / d0[i]; d2[i] = div2; d0[i+2] -= d0[i] * div2 * div2;
        }
    }
    std::vector<double> x = b;
    for(int i = 0; i < n; i++) {
        if(i > 0) x[i] -= d1[i-1] * x[i-1];
        if(d == 2 && i > 1) x[i] -= d2[i-2] * x[i-2];
    }
    for(int i = n-1; i >= 0; i--) {
        x[i] /= d0[i];
        if(i < n-1) x[i] -= d1[i] * x[i+1];
        if(d == 2 && i < n-2) x[i] -= d2[i] * x[i+2];
    }
    return x;
}

// --- Helper: Matrix Assembler (Same as before) ---
void assemble_matrix(const std::vector<double>& w, const std::vector<double>& x,
                     double lambda, int n, int d,
                     std::vector<double>& d0, std::vector<double>& d1, std::vector<double>& d2) {
    for(int i=0; i<n; i++) { d0[i] = w[i]; if(i<n-1) d1[i]=0; if(i<n-2) d2[i]=0; }
    
    if (d == 1) {
        for(int i = 0; i < n - 1; i++) {
            double h = x[i+1] - x[i]; if(h<=0) h=1e-6;
            double c0 = -1.0/h; double c1 = 1.0/h;
            d0[i]+=lambda*c0*c0; d0[i+1]+=lambda*c1*c1; d1[i]+=lambda*c0*c1;
        }
    } else {
        for(int i = 0; i < n - 2; i++) {
            double h0 = x[i+1]-x[i]; double h1 = x[i+2]-x[i+1]; double H = x[i+2]-x[i];
            if(h0<=0) h0=1e-6; if(h1<=0) h1=1e-6;
            double c0 = (2.0/H)*(1.0/h0); double c1 = (2.0/H)*(-1.0/h0 - 1.0/h1); double c2 = (2.0/H)*(1.0/h1);
            d0[i]+=lambda*c0*c0; d0[i+1]+=lambda*c1*c1; d0[i+2]+=lambda*c2*c2;
            d1[i]+=lambda*c0*c1; d1[i+1]+=lambda*c1*c2; d2[i]+=lambda*c0*c2;
        }
    }
}

// --- Main Baseline Function ---
// [[Rcpp::export]]
List whittaker_baseline(NumericVector y_in, 
                        Nullable<NumericVector> x_in = R_NilValue,
                        double lambda = 1000.0, 
                        double p = 0.001, 
                        int d = 2,
                        int n_iter = 10) {
    int n = y_in.size();
    
    // 1. Safe Data Copy
    std::vector<double> y(n), x(n), w(n), y_clean(n);
    std::vector<bool> is_missing(n, false);
    
    if (x_in.isNull()) for(int i=0; i<n; i++) x[i] = (double)i;
    else {
        NumericVector xr(x_in);
        for(int i=0; i<n; i++) x[i] = xr[i];
    }
    
    // 2. Initialize Weights
    // Start with uniform weights (1.0) for valid data, 0.0 for NAs
    for(int i=0; i<n; i++) {
        if (R_IsNA(y_in[i])) { 
            is_missing[i] = true;
            w[i] = 0.0; y_clean[i] = 0.0;
        } else {
            y[i] = y_in[i]; y_clean[i] = y[i];
            w[i] = 1.0; 
        }
    }
    
    // 3. Iterative Asymmetric Smoothing
    std::vector<double> z(n); // The Baseline
    std::vector<double> d0(n), d1(n), d2(n), b(n);
    
    for(int iter = 0; iter < n_iter; iter++) {
        // A. Solve System
        for(int i=0; i<n; i++) b[i] = w[i] * y_clean[i];
        
        // Reset diagonals for assembly
        std::fill(d0.begin(), d0.end(), 0.0);
        assemble_matrix(w, x, lambda, n, d, d0, d1, d2);
        
        // Solve
        // (Copy diagonals because solver modifies them)
        std::vector<double> d0_c=d0, d1_c=d1, d2_c=d2;
        z = solve_ldlt(d0_c, d1_c, d2_c, b, n, d);
        
        // B. Update Weights (The "Asymmetric" part)
        for(int i = 0; i < n; i++) {
            if (is_missing[i]) {
                w[i] = 0.0; // Always ignore missing data
            } else {
                // If Data > Baseline -> It's a Peak -> Low Weight (p)
                // If Data < Baseline -> It's Baseline -> High Weight (1-p)
                if (y[i] > z[i]) {
                    w[i] = p;
                } else {
                    w[i] = 1.0 - p;
                }
            }
        }
    }
    
    // 4. Correct the signal
    NumericVector corrected(n);
    for(int i=0; i<n; i++) {
        if(is_missing[i]) corrected[i] = NA_REAL;
        else corrected[i] = y[i] - z[i];
    }
    
    return List::create(
        _["baseline"] = z,
        _["corrected"] = corrected,
        _["lambda"] = lambda
    );
}