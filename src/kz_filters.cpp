#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector kz_cpp(NumericVector x, int m, int k, double data_thresh = 0.75) {
  int n = x.length();
  NumericVector current_x = clone(x);
  NumericVector temp_x(n);

  // Calculate half-window. If m is 5, half is 2. Window is i-2 to i+2.
  int half_m = m / 2;

  for (int iter = 0; iter < k; iter++) {
    for (int i = 0; i < n; i++) {
      double sum = 0;
      int count = 0;

      // Define local window boundaries (handles edges automatically)
      int start = std::max(0, i - half_m);
      int end = std::min(n - 1, i + half_m);
      int window_size = end - start + 1;

      for (int j = start; j <= end; j++) {
        if (!NumericVector::is_na(current_x[j])) {
          sum += current_x[j];
          count++;
        }
      }

      if (count >= data_thresh * window_size) {
        temp_x[i] = sum / count;
      } else {
        temp_x[i] = NA_REAL;
      }
    }
    // Update for next iteration
    current_x = clone(temp_x);
  }

  return current_x;
}

// [[Rcpp::export]]
NumericVector kza_cpp(NumericVector x, int m, int k, double sensitivity = 1.0,
                      double data_thresh = 0.75) {
  int n = x.length();

  // 1. Get the baseline KZ filter to detect structural breaks
  NumericVector kz_base = kz_cpp(x, m, k, data_thresh);

  // 2. Calculate local rate of change (absolute derivative)
  NumericVector diff(n, 0.0);
  double max_diff = 0.0001; // Avoid division by zero

  for (int i = 1; i < n - 1; i++) {
    if (!NumericVector::is_na(kz_base[i+1]) && !NumericVector::is_na(kz_base[i-1])) {
      diff[i] = std::abs(kz_base[i+1] - kz_base[i-1]);
      if (diff[i] > max_diff) max_diff = diff[i];
    }
  }

  // 3. Apply Adaptive Filter
  NumericVector result(n);

  for (int i = 0; i < n; i++) {
    // Normalize difference [0, 1]
    double norm_diff = diff[i] / max_diff;

    // Dynamically shrink window size where rate of change is high
    // sensitivity controls how aggressively the window shrinks
    double dynamic_m = m * (1.0 - std::min(1.0, norm_diff * sensitivity));
    int half_dyn = std::max(0, (int)(dynamic_m / 2));

    double sum = 0;
    int count = 0;

    int start = std::max(0, i - half_dyn);
    int end = std::min(n - 1, i + half_dyn);
    int window_size = end - start + 1;

    for (int j = start; j <= end; j++) {
      if (!NumericVector::is_na(x[j])) {
        sum += x[j];
        count++;
      }
    }

    result[i] = (count >= data_thresh * window_size) ? (sum / count) : NA_REAL;
  }

  return result;
}

