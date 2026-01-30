#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

//' Rolling Gaussian Smoother
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
        weights[i] = std::exp(-0.5 * std::pow(dist / sigma, 2.0));
    }

    for (int i = 0; i < n; ++i) {
        double weighted_sum = 0.0;
        double weight_norm = 0.0;
        int valid_count = 0;

        for (int j = -radius; j <= radius; ++j) {
            int data_idx = i + j;
            int weight_idx = j + radius;

            if (data_idx >= 0 && data_idx < n) {
                double val = x[data_idx];
                
                if (!NumericVector::is_na(val)) {
                    double w = weights[weight_idx];
                    weighted_sum += val * w;
                    weight_norm += w;
                    valid_count++;
                }
            }
        }

        // Apply thresholding based on the theoretical window width
        double fraction = static_cast<double>(valid_count) / width;

        if (fraction >= threshold && weight_norm > 0) {
            smoothed[i] = weighted_sum / weight_norm;
        }
        counts[i] = valid_count;
    }

    return List::create(
        Named("values") = smoothed,
        Named("counts") = counts
    );
}