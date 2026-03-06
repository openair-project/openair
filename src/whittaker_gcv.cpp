#include <Rcpp.h>
#include <vector>
#include <cmath>

using namespace Rcpp;

// --- Helper 1: Tridiagonal Solver (d=1) ---
// Solves (W + lambda * D1'D1) * x = b
// Bandwidth = 1. Used for "Linear" trends.
std::vector<double> solve_tridiagonal(const std::vector<double>& w, 
                                      const std::vector<double>& b, 
                                      double lambda, int n) {
  std::vector<double> lower(n-1);
  std::vector<double> diag(n);
  std::vector<double> upper(n-1);
  
  // 1. Construct Matrix A = W + lambda * D1'D1
  // D1'D1 has structure: [1, -1] on corners, [-1, 2, -1] in middle
  for(int i=0; i<n; i++) {
    diag[i] = w[i];
    
    // Add D1'D1 contributions
    if (i==0 || i==n-1) diag[i] += lambda * 1.0;
    else diag[i] += lambda * 2.0;
    
    if (i < n-1) {
      upper[i] = -lambda;
      lower[i] = -lambda;
    }
  }
  
  // 2. Thomas Algorithm (TDMA) for Tridiagonal Systems
  // Forward elimination
  std::vector<double> x = b; // Make a copy of RHS to work on
  
  // Modify coefficients
  // Note: We modify 'diag' and 'x' in place. 'lower' is eliminated.
  for (int i = 0; i < n - 1; i++) {
    if (diag[i] < 1e-12) diag[i] = 1e-12; // Stability check
    double m = lower[i] / diag[i];
    diag[i+1] -= m * upper[i];
    x[i+1] -= m * x[i];
  }
  
  // Backward substitution
  if (diag[n-1] < 1e-12) diag[n-1] = 1e-12;
  x[n-1] = x[n-1] / diag[n-1];
  
  for (int i = n - 2; i >= 0; i--) {
    x[i] = (x[i] - upper[i] * x[i+1]) / diag[i];
  }
  
  return x;
}

// --- Helper 2: Pentadiagonal Solver (d=2) ---
// Solves (W + lambda * D2'D2) * x = b
// Bandwidth = 2. Used for "Curved" trends.
std::vector<double> solve_pentadiagonal(const std::vector<double>& w, 
                                        const std::vector<double>& b, 
                                        double lambda, int n) {
  std::vector<double> d0(n);   // Main diagonal
  std::vector<double> d1(n-1); // 1st super-diagonal
  std::vector<double> d2(n-2); // 2nd super-diagonal
  
  // 1. Construct Matrix A = W + lambda * D2'D2
  for(int i = 0; i < n; i++) {
    d0[i] = w[i]; 
    // Diagonal coeffs: 1, 5, 6, ..., 6, 5, 1
    if(i < 2 || i >= n-2) d0[i] += lambda * ((i==0||i==n-1) ? 1.0 : 5.0);
    else d0[i] += lambda * 6.0;
    
    if(i < n-1) {
      // 1st off-diag coeffs: -2, -4, ..., -4, -2
      if(i == 0 || i == n-2) d1[i] = lambda * -2.0;
      else d1[i] = lambda * -4.0;
    }
    if(i < n-2) {
      // 2nd off-diag coeffs: 1
      d2[i] = lambda * 1.0;
    }
  }
  
  // 2. In-place LDLT Factorization (Pentadiagonal specialized)
  for(int i = 0; i < n - 2; i++) {
    if (d0[i] < 1e-12) d0[i] = 1e-12; 
    double div1 = d1[i] / d0[i];
    double div2 = d2[i] / d0[i];
    
    d1[i] = div1; // Store L
    d2[i] = div2; // Store L
    
    d0[i+1] -= d0[i] * div1 * div1;
    d1[i+1] -= d0[i] * div1 * div2;
    d0[i+2] -= d0[i] * div2 * div2;
  }
  if (n > 1) {
    if (d0[n-2] < 1e-12) d0[n-2] = 1e-12;
    double div = d1[n-2] / d0[n-2];
    d1[n-2] = div;
    d0[n-1] -= d0[n-2] * div * div;
  }
  if (d0[n-1] < 1e-12) d0[n-1] = 1e-12;
  
  // 3. Solves
  std::vector<double> x = b;
  // Forward (Ly = b)
  for(int i = 0; i < n; i++) {
    if(i > 0) x[i] -= d1[i-1] * x[i-1];
    if(i > 1) x[i] -= d2[i-2] * x[i-2];
  }
  // Backward (DL'x = y)
  for(int i = n-1; i >= 0; i--) {
    x[i] /= d0[i];
    if(i < n-1) x[i] -= d1[i] * x[i+1];
    if(i < n-2) x[i] -= d2[i] * x[i+2];
  }
  
  return x;
}

// --- Main Exported Function ---
// [[Rcpp::export]]
List whittaker_smooth(NumericVector y_in, double lambda = -1.0, int d = 2) {
  int n = y_in.size();
  
  // --- Step 1: Safe Data Conversion (R -> C++) ---
  std::vector<double> y(n);
  std::vector<double> w(n);
  std::vector<double> y_clean(n);
  int n_obs = 0;
  
  for(int i=0; i<n; i++) {
    if (R_IsNA(y_in[i])) { 
      w[i] = 0.0;
      y_clean[i] = 0.0;
    } else {
      y[i] = y_in[i];
      w[i] = 1.0;
      y_clean[i] = y[i];
      n_obs++;
    }
  }
  
  if (n_obs < 3) return List::create(_["error"] = "Not enough data points");
  
  // Prepare RHS vector b = W * y
  std::vector<double> b(n);
  for(int i=0; i<n; i++) b[i] = w[i] * y_clean[i];
  
  // --- Mode 1: Fixed Lambda ---
  if (lambda >= 0) {
    std::vector<double> res;
    if (d == 1) res = solve_tridiagonal(w, b, lambda, n);
    else        res = solve_pentadiagonal(w, b, lambda, n);
    return List::create(_["smoothed"] = res, _["lambda"] = lambda, _["d"] = d);
  }
  
  // --- Mode 2: Automated GCV Selection ---
  
  // Initialize Random Probes for Trace Estimation
  int n_probes = 10;
  std::vector<std::vector<double>> probes(n_probes, std::vector<double>(n));
  RNGScope scope; // Lock RNG state
  for(int k=0; k<n_probes; k++) {
    for(int i=0; i<n; i++) probes[k][i] = (R::runif(0,1) > 0.5) ? 1.0 : -1.0;
  }
  
  double best_gcv = 1e20;
  double best_lam = 1.0;
  std::vector<double> best_fit;
  
  // Grid Search for Lambda
  // We scan from 10^-1 to 10^8
  for(double log_lam = -1.0; log_lam <= 8.0; log_lam += 0.25) {
    double current_lam = pow(10.0, log_lam);
    
    std::vector<double> z;
    if (d == 1) z = solve_tridiagonal(w, b, current_lam, n);
    else        z = solve_pentadiagonal(w, b, current_lam, n);
    
    // 1. Calculate RSS (Weighted)
    double rss = 0;
    for(int i=0; i<n; i++) {
      if(w[i] > 0) rss += w[i] * (y_clean[i] - z[i]) * (y_clean[i] - z[i]);
    }
    
    // 2. Estimate Trace (Effective Parameters)
    double trace_sum = 0;
    for(int k=0; k<n_probes; k++) {
      std::vector<double> probe_rhs(n);
      for(int i=0; i<n; i++) probe_rhs[i] = w[i] * probes[k][i];
      
      std::vector<double> u;
      if (d == 1) u = solve_tridiagonal(w, probe_rhs, current_lam, n);
      else        u = solve_pentadiagonal(w, probe_rhs, current_lam, n);
      
      for(int i=0; i<n; i++) trace_sum += probes[k][i] * u[i];
    }
    double eff_params = trace_sum / n_probes;
    
    // 3. GCV Calculation
    double denom = n_obs - eff_params;
    // Check for overfitting (eff_params approaching n_obs)
    double gcv = (denom <= 0.1) ? 1e20 : (rss / n_obs) / pow(denom/n_obs, 2);
    
    if (gcv < best_gcv) {
      best_gcv = gcv;
      best_lam = current_lam;
      best_fit = z;
    }
  }
  
  return List::create(
    _["smoothed"] = best_fit, 
    _["lambda"] = best_lam,
    _["d"] = d,
    _["gcv"] = best_gcv
  );
}