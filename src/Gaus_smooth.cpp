#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

//' Fast Rolling Gaussian Smoother
//' @param x Numeric vector to smooth
//' @param sigma Standard deviation of the Gaussian kernel
//' @param threshold Min fraction of valid data in window (0-1)
//' @keywords internal
// [[Rcpp::export]]
List rolling_gaussian_cpp(NumericVector x, double sigma, double threshold = 0.5) {
    int n = x.size();
    NumericVector smoothed(n, NA_REAL);
    IntegerVector counts(n);

    if (sigma <= 0) {
        stop("Sigma must be positive.");
    }

    // Determine window radius: 3 sigma captures 99.7% of the distribution
    int radius = std::ceil(3.0 * sigma);
    int width = 2 * radius + 1;

    // Pre-calculate Gaussian weights for the window
    std::vector<double> weights(width);
    for (int i = 0; i < width; ++i) {
        double dist = i - radius;
        weights[i] = std::exp(-0.5 * (dist * dist) / (sigma * sigma));
    }

    // Pointers for raw speed access
    // Note: Rcpp vectors are essentially wrappers around contiguous memory
    double* x_ptr = x.begin();
    double* smooth_ptr = smoothed.begin();
    int* counts_ptr = counts.begin();
    const double* w_ptr = weights.data();

    // 1. LEFT EDGE (Indices where window goes out of bounds to the left)
    for (int i = 0; i < radius && i < n; ++i) {
        double weighted_sum = 0.0;
        double weight_norm = 0.0;
        int valid_count = 0;

        for (int j = -radius; j <= radius; ++j) {
            int data_idx = i + j;
            if (data_idx >= 0 && data_idx < n) {
                double val = x_ptr[data_idx];
                if (!std::isnan(val)) {
                    double w = w_ptr[j + radius];
                    weighted_sum += val * w;
                    weight_norm += w;
                    valid_count++;
                }
            }
        }

        if (static_cast<double>(valid_count) / width >= threshold && weight_norm > 0) {
            smooth_ptr[i] = weighted_sum / weight_norm;
        }
        counts_ptr[i] = valid_count;
    }

    // 2. MIDDLE (Safe zone: no bounds checking needed)
    // This is the "Hot Loop" where 99% of time is spent
    int end_safe = n - radius;
    for (int i = radius; i < end_safe; ++i) {
        double weighted_sum = 0.0;
        double weight_norm = 0.0;
        int valid_count = 0;

        // Pointer to the start of the data window for this i
        // x_ptr[i + j] is equivalent to x_ptr[i - radius] incrementing
        const double* d_window = x_ptr + (i - radius); 
        const double* w_window = w_ptr;

        for (int k = 0; k < width; ++k) {
            double val = d_window[k];
            // std::isnan is generally faster than Rcpp::NumericVector::is_na
            if (!std::isnan(val)) { 
                double w = w_window[k];
                weighted_sum += val * w;
                weight_norm += w;
                valid_count++;
            }
        }

        if (static_cast<double>(valid_count) / width >= threshold && weight_norm > 0) {
            smooth_ptr[i] = weighted_sum / weight_norm;
        }
        counts_ptr[i] = valid_count;
    }

    // 3. RIGHT EDGE (Indices where window goes out of bounds to the right)
    for (int i = std::max(radius, end_safe); i < n; ++i) {
        double weighted_sum = 0.0;
        double weight_norm = 0.0;
        int valid_count = 0;

        for (int j = -radius; j <= radius; ++j) {
            int data_idx = i + j;
            if (data_idx >= 0 && data_idx < n) {
                double val = x_ptr[data_idx];
                if (!std::isnan(val)) {
                    double w = w_ptr[j + radius];
                    weighted_sum += val * w;
                    weight_norm += w;
                    valid_count++;
                }
            }
        }

        if (static_cast<double>(valid_count) / width >= threshold && weight_norm > 0) {
            smooth_ptr[i] = weighted_sum / weight_norm;
        }
        counts_ptr[i] = valid_count;
    }

    return List::create(
        Named("values") = smoothed,
        Named("counts") = counts
    );
}