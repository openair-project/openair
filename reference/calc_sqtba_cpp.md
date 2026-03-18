# C++ kernel for SQTBA computation

Accumulates Gaussian-weighted Q and Q_c sums over (trajectory, grid)
pairs. The grid vectors **must** be sorted by latitude ascending so that
binary search can skip irrelevant latitude bands and
[`break`](https://rdrr.io/r/base/Control.html) past them.

## Usage

``` r
calc_sqtba_cpp(
  traj_lat,
  traj_lon,
  traj_sigma,
  traj_pollutant,
  traj_weight,
  grid_lat,
  grid_lon
)
```

## Arguments

- traj_lat:

  Trajectory point latitudes (degrees).

- traj_lon:

  Trajectory point longitudes (degrees).

- traj_sigma:

  Per-point sigma (km) = sigma_base \* \|hour.inc\|.

- traj_pollutant:

  Pollutant concentrations at trajectory points.

- traj_weight:

  Per-point weight (1 / n_steps for each date group).

- grid_lat:

  Grid cell latitudes (degrees), sorted ascending.

- grid_lon:

  Grid cell longitudes (degrees), same order as grid_lat.

## Value

Named list with `Q` and `Q_c`, each a numeric vector of length n_grid.
