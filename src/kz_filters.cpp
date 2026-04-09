#include <Rcpp.h>
using namespace Rcpp;

// Kolmogorov-Zurbenko filter: applies k passes of a uniform moving average of
// window width m. NAs are excluded from the average; if the fraction of
// non-NA values in the window falls below data_thresh the output is NA.
//
// Uses a sliding window so each pass is O(n) regardless of m.
// [[Rcpp::export]]
NumericVector kz_cpp(NumericVector x, int m, int k, double data_thresh = 0.75) {
  int n = x.length();

  // Ping-pong between two buffers to avoid allocating inside the loop.
  // buf_a is the input for the current pass; results are written to buf_b,
  // then the two are swapped so buf_a always holds the latest output.
  NumericVector buf_a = clone(x);
  NumericVector buf_b(n);

  // half_m defines how far the window extends either side of element i,
  // giving an effective window of [i - half_m, i + half_m].
  int half_m = m / 2;

  for (int iter = 0; iter < k; iter++) {

    // --- Initialise the sliding window at i = 0 ---
    // The window for the first element is [0, min(n-1, half_m)].
    double sum = 0.0;
    int count = 0;          // number of non-NA values currently in the window
    int cur_left  = 0;
    int cur_right = std::min(n - 1, half_m);

    for (int j = cur_left; j <= cur_right; j++) {
      if (!NumericVector::is_na(buf_a[j])) {
        sum += buf_a[j];
        count++;
      }
    }

    // --- Slide the window across the array ---
    for (int i = 0; i < n; i++) {
      int window_size = cur_right - cur_left + 1;

      // Emit average if enough non-NA data, otherwise NA.
      buf_b[i] = (count >= data_thresh * window_size) ? (sum / count) : NA_REAL;

      if (i < n - 1) {
        // Remove the element that drops off the left as the window advances.
        // The left edge only moves once i exceeds half_m (interior region).
        int new_left = std::max(0, i + 1 - half_m);
        if (new_left > cur_left) {
          if (!NumericVector::is_na(buf_a[cur_left])) {
            sum -= buf_a[cur_left];
            count--;
          }
          cur_left = new_left;
        }

        // Add the element that enters on the right as the window advances.
        // The right edge stops growing once it reaches the end of the array.
        int new_right = std::min(n - 1, i + 1 + half_m);
        if (new_right > cur_right) {
          if (!NumericVector::is_na(buf_a[new_right])) {
            sum += buf_a[new_right];
            count++;
          }
          cur_right = new_right;
        }
      }
    }

    // Make buf_a the new input for the next pass (O(1) pointer swap).
    std::swap(buf_a, buf_b);
  }

  return buf_a;
}

// Kolmogorov-Zurbenko Adaptive filter: like kz_cpp but the window width
// shrinks in regions of high local rate of change, preserving sharp features.
// [[Rcpp::export]]
NumericVector kza_cpp(NumericVector x, int m, int k, double sensitivity = 1.0,
                      double data_thresh = 0.75) {
  int n = x.length();

  // --- Step 1: Baseline KZ filter ---
  // Run the standard KZ filter to obtain a smooth reference signal used to
  // detect structural breaks (rapid changes) in the data.
  NumericVector kz_base = kz_cpp(x, m, k, data_thresh);

  // --- Step 2: Local rate of change ---
  // Approximate the absolute first derivative at each point using a central
  // difference on the smoothed signal. The result is normalised by the
  // global maximum so it lies in [0, 1].
  NumericVector diff(n, 0.0);
  double max_diff = 0.0001; // small floor to avoid division by zero later

  for (int i = 1; i < n - 1; i++) {
    if (!NumericVector::is_na(kz_base[i+1]) && !NumericVector::is_na(kz_base[i-1])) {
      diff[i] = std::abs(kz_base[i+1] - kz_base[i-1]);
      if (diff[i] > max_diff) max_diff = diff[i];
    }
  }

  // --- Step 3: Prefix arrays for O(1) window queries ---
  // Because the adaptive window width varies per element a sliding window is
  // not applicable. Instead, precompute cumulative sum and count arrays so
  // that the sum and non-NA count over any sub-range [start, end] can be
  // retrieved in O(1) as a single subtraction.
  std::vector<double> prefix_sum(n + 1, 0.0);
  std::vector<int>    prefix_count(n + 1, 0);
  for (int j = 0; j < n; j++) {
    prefix_sum[j + 1]   = prefix_sum[j]   + (NumericVector::is_na(x[j]) ? 0.0 : x[j]);
    prefix_count[j + 1] = prefix_count[j] + (NumericVector::is_na(x[j]) ? 0   : 1);
  }

  // --- Step 4: Adaptive filter ---
  NumericVector result(n);

  for (int i = 0; i < n; i++) {
    // Normalise the local rate of change to [0, 1].
    double norm_diff = diff[i] / max_diff;

    // Scale the half-window: where norm_diff is high (sharp feature) the
    // window shrinks toward zero; where norm_diff is low (smooth region) it
    // stays at the full half-width. sensitivity controls the aggressiveness
    // of the shrinkage.
    double dynamic_m = m * (1.0 - std::min(1.0, norm_diff * sensitivity));
    int half_dyn = std::max(0, (int)(dynamic_m / 2));

    // Clamp window to array bounds.
    int start = std::max(0, i - half_dyn);
    int end   = std::min(n - 1, i + half_dyn);
    int window_size = end - start + 1;

    // O(1) range query using the prefix arrays.
    int    count = prefix_count[end + 1] - prefix_count[start];
    double sum   = prefix_sum[end + 1]   - prefix_sum[start];

    result[i] = (count >= data_thresh * window_size) ? (sum / count) : NA_REAL;
  }

  return result;
}
