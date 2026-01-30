#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <string>
#include <sstream>

using namespace Rcpp;

// Helper to format probs as "XX%"
CharacterVector format_probs(NumericVector p) {
    CharacterVector names(p.size());
    for (int i = 0; i < p.size(); ++i) {
        std::ostringstream oss;
        oss << p[i] * 100 << "%";
        names[i] = oss.str();
    }
    return names;
}

//' @keywords internal
// [[Rcpp::export]]
List rolling_average_cpp(NumericVector x, 
                       int width, 
                       std::string alignment, 
                       double threshold,
                       std::string statistic = "mean", 
                       Nullable<NumericVector> probs = R_NilValue) {
    
    int n = x.size();
    IntegerVector counts(n);
    
    // Initialize return objects
    NumericVector averages(n, NA_REAL);
    NumericMatrix quant_mat;
    
    bool do_mean = (statistic == "mean" || statistic == "both");
    bool do_quant = (statistic == "quantile" || statistic == "both");
    
    NumericVector p;
    if (do_quant) {
        if (probs.isNull()) stop("probs must be provided when statistic is 'quantile' or 'both'");
        p = NumericVector(probs);
        quant_mat = NumericMatrix(n, p.size());
        std::fill(quant_mat.begin(), quant_mat.end(), NA_REAL);
        colnames(quant_mat) = format_probs(p);
    }

    if (width <= 0) stop("Width must be a positive integer.");

    std::vector<double> window_data;
    window_data.reserve(width);

    for (int i = 0; i < n; ++i) {
        int start, end;

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

        window_data.clear();
        double sum = 0.0;

        for (int j = start; j <= end; ++j) {
            if (j >= 0 && j < n) {
                if (!NumericVector::is_na(x[j])) {
                    window_data.push_back(x[j]);
                    if (do_mean) sum += x[j];
                }
            }
        }

        int valid_count = window_data.size();
        double fraction = static_cast<double>(valid_count) / width;

        if (fraction >= threshold && valid_count > 0) {
            if (do_mean) {
                averages[i] = sum / valid_count;
            }
            
            if (do_quant) {
                std::sort(window_data.begin(), window_data.end());
                for (int k = 0; k < p.size(); ++k) {
                    double index = p[k] * (valid_count - 1);
                    int lhs = std::floor(index);
                    int rhs = std::ceil(index);
                    if (lhs == rhs) {
                        quant_mat(i, k) = window_data[lhs];
                    } else {
                        double weight = index - lhs;
                        quant_mat(i, k) = (1.0 - weight) * window_data[lhs] + weight * window_data[rhs];
                    }
                }
            }
        }
        counts[i] = valid_count;
    }

    // Build the return list based on user choice
    List res;
    if (do_mean) res["values"] = averages;
    if (do_quant) res["quantiles"] = quant_mat;
    res["counts"] = counts;

    return res;
}