#include <Rcpp.h>
#include <algorithm>
#include <map>
#include <vector>
using namespace Rcpp;

// Helper function to calculate median
double calculateMedian(std::vector<double>& values) {
  int n = values.size();
  if (n == 0) return NA_REAL;
  
  std::sort(values.begin(), values.end());
  
  if (n % 2 == 0) {
    return (values[n/2 - 1] + values[n/2]) / 2.0;
  } else {
    return values[n/2];
  }
}

// Helper function to calculate quantile
double calculateQuantile(std::vector<double>& values, double prob) {
  int n = values.size();
  if (n == 0) return NA_REAL;
  
  std::sort(values.begin(), values.end());
  
  // Using R's default quantile type 7 (linear interpolation)
  double index = (n - 1) * prob;
  int lower_index = floor(index);
  int upper_index = ceil(index);
  
  if (lower_index == upper_index) {
    return values[lower_index];
  } else {
    double weight = index - lower_index;
    return values[lower_index] * (1 - weight) + values[upper_index] * weight;
  }
}

// Main aggregation function
// [[Rcpp::export]]
DataFrame dateAggregate(DataFrame data, 
                        std::string date_col = "date",
                        std::string statistic = "mean",
                        double threshold = 0.0,
                        NumericVector probs = NumericVector::create(0.5)) {
  
  // Validate inputs
  if (!data.containsElementNamed(date_col.c_str())) {
    stop("Date column '" + date_col + "' not found in data frame");
  }
  
  if (threshold < 0.0 || threshold > 1.0) {
    stop("Threshold must be between 0 and 1");
  }
  
  // Validate statistic
  bool is_mean = (statistic == "mean");
  bool is_median = (statistic == "median");
  bool is_min = (statistic == "min" || statistic == "minimum");
  bool is_max = (statistic == "max" || statistic == "maximum");
  bool is_count = (statistic == "count" || statistic == "n");
  bool is_quantile = (statistic == "percentile" || statistic == "quantile");
  bool is_sum = (statistic == "sum");
  bool is_sd = (statistic == "sd" || statistic == "stdev");
  bool is_valid_count = (statistic == "frequency" || statistic == "valid_count");
  bool is_dat_cap = (statistic == "data.cap" || statistic == "data.capture" || statistic == "dat_cap");
  
  if (!is_mean && !is_median && !is_min && !is_max && !is_count && 
      !is_quantile && !is_sum && !is_sd && !is_valid_count && !is_dat_cap) {
    stop("Unknown statistic: " + statistic + ". Use 'mean', 'median', 'min', 'max', 'sum', 'sd', 'count', 'valid.count', 'dat.cap', or 'quantile'");
  }
  
  // Validate probs for quantile
  if (is_quantile) {
    for (int i = 0; i < probs.size(); i++) {
      if (probs[i] < 0.0 || probs[i] > 1.0) {
        stop("Probabilities must be between 0 and 1");
      }
    }
  }
  
  // Get date column
  CharacterVector dates = data[date_col];
  int n = dates.size();
  
  if (n == 0) {
    stop("Data frame is empty");
  }
  
  // Get all column names
  CharacterVector col_names = data.names();
  
  // Build a map of dates to row indices
  std::map<std::string, std::vector<int>> date_indices;
  
  for (int i = 0; i < n; i++) {
    std::string date_str = as<std::string>(dates[i]);
    date_indices[date_str].push_back(i);
  }
  
  // Get unique dates in sorted order
  std::vector<std::string> unique_dates;
  for (auto const& pair : date_indices) {
    unique_dates.push_back(pair.first);
  }
  std::sort(unique_dates.begin(), unique_dates.end());
  
  int n_dates = unique_dates.size();
  
  // Prepare output data frame
  CharacterVector output_dates(n_dates);
  for (int i = 0; i < n_dates; i++) {
    output_dates[i] = unique_dates[i];
  }
  
  List output;
  output.push_back(output_dates, date_col);
  
  // Process each numeric column
  for (int col_idx = 0; col_idx < col_names.size(); col_idx++) {
    std::string col_name = as<std::string>(col_names[col_idx]);
    
    // Skip the date column
    if (col_name == date_col) continue;
    
    // Check if column is numeric
    SEXP col_data = data[col_name];
    
    if (TYPEOF(col_data) == REALSXP || TYPEOF(col_data) == INTSXP) {
      NumericVector values = as<NumericVector>(col_data);
      
      if (is_quantile && probs.size() > 1) {
        // Multiple quantiles - create separate columns
        for (int q = 0; q < probs.size(); q++) {
          NumericVector aggregated(n_dates, NA_REAL);
          
          for (int i = 0; i < n_dates; i++) {
            std::vector<int>& indices = date_indices[unique_dates[i]];
            std::vector<double> group_values;
            
            // Collect non-missing values for this date
            for (int idx : indices) {
              if (!NumericVector::is_na(values[idx])) {
                group_values.push_back(values[idx]);
              }
            }
            
            // Check threshold
            double data_fraction = (double)group_values.size() / indices.size();
            
            if (data_fraction >= threshold && group_values.size() > 0) {
              aggregated[i] = calculateQuantile(group_values, probs[q]);
            }
          }
          
          // Create column name with quantile probability
          std::ostringstream col_name_stream;
          col_name_stream << col_name << "_q" << probs[q];
          output.push_back(aggregated, col_name_stream.str());
        }
      } else {
        // Single statistic per column
        NumericVector aggregated(n_dates, NA_REAL);
        
        for (int i = 0; i < n_dates; i++) {
          std::vector<int>& indices = date_indices[unique_dates[i]];
          std::vector<double> group_values;
          double sum = 0.0;
          double sum_sq = 0.0;
          double min_val = R_PosInf;
          double max_val = R_NegInf;
          
          // Collect non-missing values for this date
          for (int idx : indices) {
            if (!NumericVector::is_na(values[idx])) {
              double val = values[idx];
              group_values.push_back(val);
              sum += val;
              sum_sq += val * val;
              if (val < min_val) min_val = val;
              if (val > max_val) max_val = val;
            }
          }
          
          int count = group_values.size();
          int total_count = indices.size();
          double data_fraction = (double)count / total_count;
          
          // For valid.count and dat.cap, always return a value (no threshold check)
          if (is_valid_count) {
            aggregated[i] = count;
          } else if (is_dat_cap) {
            aggregated[i] = data_fraction;
          } else {
            // Calculate other statistics only if threshold is met
            if (data_fraction >= threshold && count > 0) {
              if (is_mean) {
                aggregated[i] = sum / count;
              } else if (is_median) {
                aggregated[i] = calculateMedian(group_values);
              } else if (is_min) {
                aggregated[i] = min_val;
              } else if (is_max) {
                aggregated[i] = max_val;
              } else if (is_count) {
                aggregated[i] = count;
              } else if (is_sum) {
                aggregated[i] = sum;
              } else if (is_sd) {
                if (count > 1) {
                  double mean = sum / count;
                  double variance = (sum_sq - count * mean * mean) / (count - 1);
                  aggregated[i] = sqrt(variance);
                }
              } else if (is_quantile) {
                aggregated[i] = calculateQuantile(group_values, probs[0]);
              }
            }
          }
        }
        
        output.push_back(aggregated, col_name);
      }
    }
  }
  
  // Set data frame attributes
  output.attr("class") = "data.frame";
  output.attr("row.names") = IntegerVector::create(NA_INTEGER, -n_dates);
  
  return output;
}