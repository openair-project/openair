#include <Rcpp.h>
using namespace Rcpp;

//' Rolling Average with Threshold
//' @param x A numeric vector
//' @param width Integer window width
//' @param alignment String: "left", "centre", or "right"
//' @param threshold Numeric fraction (0-1) of non-NA data required
//' @export
//' @keywords internal
// [[Rcpp::export]]
List rolling_average_cpp(NumericVector x, int width, std::string alignment, double threshold) {
    int n = x.size();
    NumericVector averages(n, NA_REAL);
    IntegerVector counts(n);

    if (width <= 0) {
        stop("Width must be a positive integer.");
    }

    for (int i = 0; i < n; ++i) {
        int start, end;

        // Alignment Logic
        if (alignment == "right") {
            start = i - width + 1;
            end = i;
        } else if (alignment == "left") {
            start = i;
            end = i + width - 1;
        } else if (alignment == "centre") {
            int offset = (width - 1) / 2;
            start = i - offset;
            end = start + width - 1;
        } else {
            stop("Alignment must be 'left', 'centre', or 'right'");
        }

        double sum = 0.0;
        int valid_count = 0;

        // Window Calculation
        for (int j = start; j <= end; ++j) {
            if (j >= 0 && j < n) {
                if (!NumericVector::is_na(x[j])) {
                    sum += x[j];
                    valid_count++;
                }
            }
        }

        // Calculate fraction based on full window width
        double fraction = static_cast<double>(valid_count) / width;

        if (fraction >= threshold) {
            averages[i] = sum / valid_count;
        }
        counts[i] = valid_count;
    }

    return List::create(
        Named("values") = averages,
        Named("counts") = counts
    );
}
