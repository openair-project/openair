#include <Rcpp.h>
#include <algorithm>
#include <cmath>

using namespace Rcpp;

//' C++ kernel for SQTBA computation
//'
//' Accumulates Gaussian-weighted Q and Q_c sums over (trajectory, grid) pairs.
//' The grid vectors **must** be sorted by latitude ascending so that binary
//' search can skip irrelevant latitude bands and `break` past them.
//'
//' @param traj_lat  Trajectory point latitudes (degrees).
//' @param traj_lon  Trajectory point longitudes (degrees).
//' @param traj_sigma  Per-point sigma (km) = sigma_base * |hour.inc|.
//' @param traj_pollutant  Pollutant concentrations at trajectory points.
//' @param traj_weight  Per-point weight (1 / n_steps for each date group).
//' @param grid_lat  Grid cell latitudes (degrees), sorted ascending.
//' @param grid_lon  Grid cell longitudes (degrees), same order as grid_lat.
//' @return Named list with `Q` and `Q_c`, each a numeric vector of length n_grid.
//' @keywords internal
// [[Rcpp::export]]
List calc_sqtba_cpp(
    NumericVector traj_lat,
    NumericVector traj_lon,
    NumericVector traj_sigma,
    NumericVector traj_pollutant,
    NumericVector traj_weight,
    NumericVector grid_lat,
    NumericVector grid_lon
) {
    const int n_traj = traj_lat.size();
    const int n_grid = grid_lat.size();

    const double DEG_TO_RAD     = M_PI / 180.0;
    const double EARTH_RADIUS   = 6378.137;   // km
    const double KM_PER_DEG     = 111.32;
    const double SIGMA_CUTOFF   = 5.0;        // 5-sigma: exp(-12.5) ≈ 4e-6

    // Pre-compute grid radians once
    std::vector<double> g_lat_rad(n_grid), g_lon_rad(n_grid);
    for (int g = 0; g < n_grid; g++) {
        g_lat_rad[g] = grid_lat[g] * DEG_TO_RAD;
        g_lon_rad[g] = grid_lon[g] * DEG_TO_RAD;
    }

    NumericVector Q(n_grid, 0.0);
    NumericVector Q_c(n_grid, 0.0);

    for (int t = 0; t < n_traj; t++) {
        const double sigma_t    = traj_sigma[t];
        const double inv_sig2   = 1.0 / (sigma_t * sigma_t);
        // Degrees equivalent of 5*sigma; most grid cells fail this cheap check
        const double cutoff_deg = SIGMA_CUTOFF * sigma_t / KM_PER_DEG;

        const double t_lat    = traj_lat[t];
        const double t_lon    = traj_lon[t];
        const double sin_tlat = std::sin(t_lat * DEG_TO_RAD);
        const double cos_tlat = std::cos(t_lat * DEG_TO_RAD);
        const double t_lon_r  = t_lon * DEG_TO_RAD;
        const double c_poll   = traj_pollutant[t];
        const double c_wt     = traj_weight[t];

        const double lat_lo = t_lat - cutoff_deg;
        const double lat_hi = t_lat + cutoff_deg;
        const double lon_lo = t_lon - cutoff_deg;
        const double lon_hi = t_lon + cutoff_deg;

        // Binary search: jump to first grid cell with lat >= lat_lo
        int g_start = (int)(std::lower_bound(
            grid_lat.begin(), grid_lat.end(), lat_lo
        ) - grid_lat.begin());

        for (int g = g_start; g < n_grid; g++) {
            const double glat = grid_lat[g];
            if (glat > lat_hi) break;               // Past the lat band — stop

            const double glon = grid_lon[g];
            if (glon < lon_lo || glon > lon_hi) continue;  // Cheap lon bbox

            // Great-circle distance (acos formula)
            double cos_angle =
                sin_tlat * std::sin(g_lat_rad[g]) +
                cos_tlat * std::cos(g_lat_rad[g]) *
                std::cos(g_lon_rad[g] - t_lon_r);

            // Clamp to [-1, 1] to guard against floating-point noise
            if (cos_angle >  1.0) cos_angle =  1.0;
            if (cos_angle < -1.0) cos_angle = -1.0;

            const double d_km = std::acos(cos_angle) * EARTH_RADIUS;
            const double q    = inv_sig2 * std::exp(-0.5 * d_km * d_km * inv_sig2);

            Q[g]   += q * c_wt;
            Q_c[g] += q * c_poll * c_wt;
        }
    }

    return List::create(Named("Q") = Q, Named("Q_c") = Q_c);
}
