// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <unordered_map>
#include <algorithm>
#include <cmath> 

using namespace Rcpp;

// Helper function to calculate the statistic
static double calc_stat(std::vector<double>& x,
                        const std::string& statistic,
                        const double probs,
                        const double threshold,
                        const int group_size) {
  
  // 1. Remove NAs to get valid data
  x.erase(std::remove_if(x.begin(), x.end(),
                         [](double v){ return Rcpp::NumericVector::is_na(v); }),
          x.end());

  int n = static_cast<int>(x.size());

  // 2. Check group validity (Total rows vs Valid rows)
  if (group_size == 0) return NA_REAL;

  double prop = static_cast<double>(n) / static_cast<double>(group_size);

  // If valid data proportion is less than threshold, return NA for ALL stats
  if (prop < threshold) return NA_REAL; 

  // 3. Calculate requested statistic
  
  // Metadata stats (do not require numeric data values)
  if (statistic == "frequency") {
    return static_cast<double>(n);
  }
  
  if (statistic == "data.cap") {
    return prop;
  }

  // 4. Numeric Stats (require valid data)
  
  // If no valid data remains, return NA
  if (n == 0) return NA_REAL;

  if (statistic == "mean") {
    double s = 0.0;
    for (double v : x) s += v;
    return s / n;
  }
  
  if (statistic == "sum") {
    double s = 0.0;
    for (double v : x) s += v;
    return s;
  }
  
  if (statistic == "sd") {
    if (n < 2) return NA_REAL; // Standard deviation requires at least 2 points
    
    // Calculate Mean
    double s = 0.0;
    for (double v : x) s += v;
    double mean = s / n;
    
    // Calculate Sum of Squared Differences
    double sq_diff = 0.0;
    for (double v : x) {
      double diff = v - mean;
      sq_diff += diff * diff;
    }
    
    // Sample SD formula: sqrt( sum((x - mean)^2) / (n - 1) )
    return std::sqrt(sq_diff / (n - 1.0));
  }

  // Sorting required for order-based stats
  std::sort(x.begin(), x.end());

  if (statistic == "median") {
    if (n % 2 == 1) return x[n/2];
    return (x[n/2 - 1] + x[n/2]) / 2.0;
  }

  if (statistic == "min") return x.front();
  if (statistic == "max") return x.back();

  if (statistic == "percentile") {
    if (probs <= 0.0) return x.front();
    if (probs >= 1.0) return x.back();
    double h = (n - 1) * probs + 1.0;
    int hf = static_cast<int>(std::floor(h));
    double frac = h - hf;
    double v1 = x[hf - 1];
    double v2 = x[hf];
    return v1 + frac * (v2 - v1);
  }

  // Redundant with frequency, but kept for compatibility
  if (statistic == "count") {
    return static_cast<double>(n);
  }

  stop("Unknown statistic: %s", statistic);
  return NA_REAL;
}

// [[Rcpp::export]]
DataFrame dateAggregate(DataFrame df,
                            std::string date_col = "date",
                            std::string statistic = "mean",
                            double threshold = 0.0,
                            double probs = 0.5) {

  if (threshold < 0.0 || threshold > 1.0) {
    stop("threshold must be between 0 and 1.");
  }

  CharacterVector dates = df[date_col];
  int n = dates.size();

  // Identify numeric columns
  std::vector<int> num_cols;
  std::vector<std::string> num_names;

  for (int j = 0; j < df.size(); ++j) {
    CharacterVector all_names = df.names();
    std::string name = Rcpp::as<std::string>(all_names[j]);
    if (name == date_col) continue;
    SEXP col = df[j];
    if (TYPEOF(col) == REALSXP || TYPEOF(col) == INTSXP) {
      num_cols.push_back(j);
      num_names.push_back(name);
    }
  }

  int p = static_cast<int>(num_cols.size());
  if (p == 0) stop("No numeric columns found.");

  // Grouping structures
  std::unordered_map<std::string, int> idx;
  std::vector<std::string> groups;
  std::vector<std::vector<std::vector<double>>> buckets; 
  std::vector<int> group_sizes;

  for (int i = 0; i < n; ++i) {
    if (dates[i] == NA_STRING) continue;
    std::string key = Rcpp::as<std::string>(dates[i]);
    
    auto it = idx.find(key);
    int g;
    if (it == idx.end()) {
      g = groups.size();
      idx[key] = g;
      groups.push_back(key);
      buckets.push_back(std::vector<std::vector<double>>(p));
      group_sizes.push_back(0);
    } else {
      g = it->second;
    }

    group_sizes[g] += 1;

    for (int c = 0; c < p; ++c) {
      SEXP col = df[num_cols[c]];
      
      if (TYPEOF(col) == REALSXP) {
        double v = REAL(col)[i];
        buckets[g][c].push_back(v);
      } else {
        int v = INTEGER(col)[i];
        // Handle Integer NA explicitly
        if (v == NA_INTEGER) {
          buckets[g][c].push_back(NA_REAL);
        } else {
          buckets[g][c].push_back(static_cast<double>(v));
        }
      }
    }
  }

  int G = groups.size();
  List out(p + 1);
  out[0] = wrap(groups);
  CharacterVector out_names(p + 1);
  out_names[0] = date_col;

  for (int c = 0; c < p; ++c) {
    NumericVector res(G);
    for (int g = 0; g < G; ++g) {
      std::vector<double> values = buckets[g][c];
      res[g] = calc_stat(values, statistic, probs, threshold, group_sizes[g]);
    }
    out[c + 1] = res;
    out_names[c + 1] = num_names[c];
  }

  out.attr("names") = out_names;
  out.attr("class") = "data.frame";
  out.attr("row.names") = seq_len(G);
  
  return DataFrame(out);
}