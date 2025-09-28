# openair (development version)

## New Features

- `timePlot()` has gained the `x.relation` argument, allowing for different x ranges on different panels.

## Bug Fixes

- `timePlot()` now allows duplicate dates when `time.avg` is used. The user will still recieve a warning from `timeAverage()`, which is used internally, but the plot will still be created.

- `importUKAQ()` now closes its `url()` connections and generally fails more gracefully when `data_type %in% c("annual", "monthly", "daqi")`. This was already the case for other data types.

# openair 2.19.0

## Deprecations

`importEurope()` relies on the same back-end database as the `saqgetr` package (<https://github.com/skgrange/saqgetr>), which was retired in February 2024. `importEurope()` will now warn users of this, and outright error if `year >= 2025`. Users are instead encouraged to use the EEA Air Quality Download Service <https://eeadmz1-downloads-webapp.azurewebsites.net> to obtain European data for the time being. An R package, <https://github.com/openair-project/euroaq>, has been developed to facilitate its use.

## New Features

### Data Access

- The `source` argument of `importUKAQ()` now defaults to `NULL`. This option allows the function to assign the `source` of each `site` itself, with some caveats:

    - Ambiguous codes (e.g., `"AD1"`, which corresponds to a SAQN and locally managed site) will preferentially import from the national networks (AURN, then AQE/SAQN/WAQN/NIAQN) over locally-managed networks. To override this users should manually define `source`.

    - Incorrect codes not found in `importMeta()` will error if `importUKAQ()` is left to assign the `source`.

    - When `data_type` is one of the aggregate types (e.g., `"annual"`) and a `site` isn't defined, a `source` must be provided.

    - It is likely *slightly* slower for the function to assign `source` itself than for users to specify it themselves.
    
- The specific metadata columns appended when `importUKAQ(meta = TRUE)` can now be controlled using the `meta_columns` argument. For example, setting `meta_columns` to `c("zone", "agglomeration")` will append the zone/agglomeration information instead of the default site type/latitude/longitude.

- DAQI information imported using `importUKAQ(data_type = "daqi")` will be returned with the relevant DAQI band appended as an additional factor column; either "Low" (1-3), "Moderate" (4-6), "High" (7-9), or "Very High" (10). See <https://uk-air.defra.gov.uk/air-pollution/daqi> for more information.

- `importImperial()` has been added, superseding `importKCL()`. They are functionally identical, but reflect that londonair is now managed by Imperial College London. Function arguments have been renamed in `importImperial()` to better match `importUKAQ()`.

### Utility Functions

- `cutData()` gained numerous new features:

    - Added the `names` argument to specify the name of the appended columns. For example, `cutData(mydata, "wd", names = c("windDir"))` will append a column named "windDir".
    
    - Added the `suffix` argument as an alternative to `names`. If a new column would otherwise overwrite an existing column, `suffix` will be appended. For example, `cutData(mydata, c("nox", "o3"), suffix = "_cuts")` would append `nox_cuts` and `o3_cuts` columns.
    
    - `cutData()` is now less destructive and better cleans up after itself. For example, when `type = "yearseason"`, it will no longer leave 'year' and 'season' columns behind, or overwrite existing 'year' and 'season' columns.
    
    - `cutData()` will now give an informative error message if the user provides a `type` which is in neither an in-built option nor a column in their dataframe.

- `calcPercentile()` gained the following arguments:

    - Added the `type` argument, in line with `timeAverage()`.
    
    - Added the `prefix` argument to control the naming of the returned columns.

- `binData()` gained the following arguments:

    - Added the `type` argument, passed to `cutData()`.
    
    - Added the `B` and `conf.int` arguments, passed to `bootMeanDF()`.
    
- `selectRunning()` gained the following arguments:

    - Added the `type` argument, passed to `cutData()`.
    
    - Added the `name` argument, which changes the name of the new column appended by the function.
    
    - Added the `mode` argument, which allows `selectRunning()` to filter the dataset rather than append a column.

- `rollingMean()` has gained the `type` argument. This will likely be of most use for distinguishing between - and calculating separate statistics for - different monitoring stations within the same data frame.

- `splitByDate()` can now more consistently take `Date` / `POSIXct` inputs as well as characters, and provides more flexibility over inputs with a new `format` argument.

- `aqStats()` gained the `progress` argument, in line with `timeAverage()`.

- Many 'data utility' functions will now either warn or error if duplicate dates are detected, which is suggestive of a mix of either sites or averaging times within the same dataframe. The following functions have new behaviour:

    - `selectRunning()` and `rollingMean()` will error (duplicate dates break the logic of 'rolling window' functions). 
    
    - `aqStats()` will also error, as it relies on `rollingMean()`.
    
    - `timeAverage()` will warn the user but proceed with calculations, as averaging across different sites may be a legitimate action.
    
    - Functions which rely on `timeAverage()` will also warn but not error (notably `calcPercentile()` but also many plotting functions with `avg.time` arguments).

### Plotting Functions

- Added new features for `openColours()`:

    - Added new qualitative colour palettes: the "tol" family are colour-blind friendly palettes based on the work of Paul Tol, and "tableau" and "observable" provide access to the "Tableau10" and "Observable10" palettes to aid in consistency with plots made in those platforms.
  
    - When `n` isn't defined for a qualitative palette (e.g., "Dark2"), the full qualitative palette will be returned. Previously this errored with the default of `100`.
    
    - `openColours()` will now check whether the provided `scheme` is either a known scheme name *or* a vector of valid R colours, and provide an informative error if this is not the case.

- `polarDiff()` has gained the `type` argument, and correctly responds to `main`, `key.footer` and `key.header` via the `...` options.

- `trendLevel()` has gained new `statistic` types to match `timeAverage()`, including `"mean"`, `"median"`, `"min"`, `"max"`, `"sd"`, `"sum"`, `"frequency"` and `"percentile"`.

- `trendLevel()` will now automatically generate appropriate `labels` if `breaks` are provided. The `labels` argument can still be used to provide custom labels per break.

- The `formula.label` argument of `polarPlot()` will now control whether concentration information is printed when `statistic = "cpf"`.

- Added `calm.thresh` as an option to `windRose()`. This change allows users to set a non-zero wind speed threshold that is considered as calm.

- Added the `map.lwd`, `map.lty` and `map.border` arguments to `trajPlot()`, `trajLevel()` and `trajCluster()` for greater control over the 'basemap' of each plot.

## Bug fixes

- Fixed repeated day number in `calendarPlot()` when `statistic = max`.

- Fixed `annotate = FALSE` in `windRose()` where axes and labels were not shown

- Fixed an issue wherein `importUKAQ()` would drop sites if importing from `local` sites *and* another network.

- `polarCluster()` will no longer error with multiple `pollutant`s and a single `n.clusters`.

- `importUKAQ()` will correctly append site meta data when `meta = TRUE`, `source` is a length greater than 1, and a single site is repeated in more than one source (e.g., `importUKAQ(source = c("waqn", "aurn"), data_type = "daqi", year = 2024L))`)

- `calcPercentile()` will now correctly pass its arguments (e.g., `date.start`) to `timeAverage()`.

- `timeAverage()` will now more consistently return `NA` values rather than `NaN` or `Inf` when all values are `NA`. This specifically affects the `"mean"` and `"min"` statistics.

- `importUKAQ()` will now correctly label a measurement as ratified when it is on the day of `ratified_to`. i.e., if a site is ratified to `2020/01/01`, the measurement at `2020/01/01 23:00` will now be labelled as ratified.

- Fixed `importImperial()` URLs.

# openair 2.18-2

## New Features

- add option to `corPlot()` to carry through "use" option in `cor`.

## Bug fixes

- fix date formatting issue in `aqStats()`.
- fix wrong formula for Euclidian distances in `trajCluster()` that did not transform coordinates before distance matrix was calculated. Thanks to Dan Jaffe.
- "15_min" can once again be used as a `data_type` in the `importUKAQ()` family.
- `importUKAQ()` can now be used to import annual, monthly, and DAQI statistics for multiple combinations of `source` and `year`.

# openair 2.18-0

## New Features

- added a new `importUKAQ()` function, which supersedes `importAURN()`, `importAQE()`, `importWAQN()`, `importSAQN()`, `importNI()` and `importLocal()`. `importUKAQ()` brings a lot of new functionality to accessing UK air quality data through `{openair}`.

  - `importUKAQ()` has all of the same arguments as the functions it supersedes, as well as "source" to define the specific network of interest. The "source" argument can either be of length 1 or equal to the length of the "site" argument. This means that `importUKAQ()` can import statistics from multiple different networks at once.
  
  - `importUKAQ()` allows `ratified = TRUE` and `to_narrow = TRUE` simultaneously. This will return a tibble with two columns per observation - "value" containing the concentration and "qc" containing `TRUE`/`FALSE` which indicates whether the concentration is validated.
  
  - The AQE, WAQN, SAQN and NI networks now allow for `data_type = "daqi"`. At time of writing, only data from 2022 is present.
  
  - the `site` and `pollutant` arguments of `importUKAQ()` are respected when `data_type` is "daqi", "annual" or "monthly". The default behaviour to return all available statistics has not changed. (#346)
  
  - the various arguments which augment import function outputs now behave more consistently with one another. For example, `ratified` now respects `pollutant` and only returns the "_qc" columns for the pollutants of interest.
  
  - `importAURN()`, `importAQE()`, `importWAQN()`, `importSAQN()`, `importNI()` and `importLocal()` are still exported by `{openair}`. These are all simply wrappers around `importUKAQ()` with forced "source" arguments, and remain for back-compatibility and convenience.
  
  - While `importKCL()` also imports UK air quality data, it is not currently made available through `importUKAQ()`. Users should continue to import KCL data via `importKCL()` for the time being.

- `importMeta()` has gained two new "source" options to assist with the new `importUKAQ()` function:

  - `source = "ukaq"` will return metadata for all of the "UKAQ" networks.
  
  - `source = "all"` will return all available metadata (including KCL and Europe).

- new function `runRegression()` for extracting 'dilution lines' from air quality and other data. Online manual will be updated with principles and examples. 

- `calendarPlot()` now automatically creates its own `labels` if `breaks` are specified. For example, `c(0, 10, 20)` will create the labels `c("0 - 10", "10 - 20")`. `labels` can still be used to override the default values. (#341)

- Added the `w.abbr.len` argument to `calendarPlot()` which controls the length of the weekday abbreviation. This was requested to help people using written Chinese, but will be more broadly useful (e.g., to use "Mon", "Tue", "Wed" in place of "M", "T", "W"). (#101)

- return tibble from `timeAverage()`.

- add option `panel.gap` to leave space between panels in `timeAverage` day-hour plots. Set to 0 for previous behaviour.

- Move regression formula off main plot for `polarPlot()` for clarity and label slope as 'm'.

- Tweak seasonal trend decomposition using STL to allow the seasonal amplitude to vary more. Affects `smoothTrend()` and `TheilSen()`.

- Added colours recommended by the UK Government Analysis Function (https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-colours-in-charts/) to `openColours()`.

- `polarCluster` now prints the cluster contributions and returns a data frame of them in the output.

## Bug Fixes

- The order of columns in `importUKAQ()` will remain consistent (metadata, date, pollutants, meteo) regardless of whether `hc` is `TRUE` or `FALSE`.

- `quickText()` will now automatically capitalise "no" to "NO". (#343)

- The `year` argument of `importMeta()` is now respected when `source = "kcl"` and `"europe"`.

- Several of the directional analysis plot family (e.g., `polarFreq()`) have been refactored to use `is.null()` or `is.na()` over `missing()`. While predominantly an internal change, this should be make these functions easier to use inside of other functions (e.g., `function(data, breaks = NA) polarFreq(data, breaks = breaks))` will now run successfully).

- For `calendarPlot` when annotated with ws or wd arrows, use max ws/wd that corresponds to hour of maximum pollutant concentration and not simple the max ws/wd for a day.

- `summaryPlot()` no longer forces time zones to be GMT. (#356)

# openair 2.17-0

## New Features

- add option `meteo` to `importAURN()` family of functions. By default modelled wind speed, direction and ambient temperature are returned if available, but not if `meteo = FALSE`.

- added a new column to `modStats()`, "P", which represents the P-value of the correlation as reported by `cor.test()`.

- update processing of ADMS meteorological (.MOP) files to return stability and tidier data.

- refined the output of `pollutionRose()` where the `ws2` and `wd2` options are provided; instead of the misleading N/E/S/W, the markers become 0, +90, +/-180, -90.

- the `timeAverage()` progress bar is now powered by `{cli}`/`{purrr}` and can be silenced using the new `progress` argument.

- the `polarCluster()` progress bar is now also powered by `{cli}`/`{purrr}`.

- colours corresponding to the UK daily air quality index (<https://uk-air.defra.gov.uk/air-pollution/daqi>) have been added as options to `openColours()`.

- the `{openair}` object's `data` property returned by the `trajLevel()` function is now consistently formatted regardless of `statistic` choice, and contains relevant statistic-specific information (e.g., `sigma` for `"SQTBA"`).

## Bug Fixes

- fixed issue with `modStats()` such that "method" can now be changed (e.g., to "spearman").

- fixed issue with `aqStats()` where output wasn't being properly split by pollutant/type.

- fixed issue with `conditionalQuantile()` where the plot would fail to be produced when "type" was not specified.

- fixed issue with `cutData()` where "season" wouldn't respect system locale (e.g., would still show "(DJF)" on Italian systems, instead of the correct "(gla)"). Note that the season name itself (e.g., "Winter") cannot be automatically converted.

- fixed the `theilSen()` `silent` argument. The message "taking bootstrap samples. please wait" is also now sent via `message()` rather than `print()` and only appears once per function call.

- fixed issue where `trajCluster()` proportions would overlap when clustering forward trajectories. Proportions should now appear at the end of cluster paths, regardless of whether the trajectory is back or forward.

- fixed issue where not all `{openair}` plotting functions would properly return an openair S3 object, and that not all `data` objects were tibbles.

- Fixed issue with `timeAverage()` where date formatting caused problems, possibly due to latest version of R (4.3.0).

# openair 2.15

- do not use native pipe yet - does not work with old versions of R

- fix issue with `polarDiff()` where the resulting `openair` object did not contain the `plot` element.

# openair 2.14

- add `year` as an option to `importMeta`. This allows the user to select sites that were only open at some point in the chosen year or duration of years.

- make sure full daily gravimetric data are returned for PM10 and PM2.5 if available when using `importAURN` family of functions. These data will be returned as `gr_pm2.5` and `gr_pm10` if `data_type = "daily"`

- add `alpha` argument to all polar directional analysis functions. This is mainly for use in `openairmaps` but may be of general interest for specific use cases.

- fix small bug in `smoothTrend` in returned fit data when data is missing.

# openair 2.13

- add `importLocal` to access locally-managed automatic monitoring data 
- fix bug in `pollutionRose` when single number of breaks given (was ignored)
- remove legacy functions `import`, `import.2`, `importAURNcsv` and `kernelExceed`

# openair 2.11

- add `sigma` to options for SQTBA trajectory analysis for control over plume spread assumptions; set to 1.5 km (in one hour)
- fix bug introduced in `timeAverage` for multi-time period averages
- fix bug in `timeProp` and simplify code

# openair 2.10

- add Daily Air Quality Index (DAQI) to `importAURN` as `data_type`.
- add rolling 8 and 24 hour statistics to `importAURN` family as `data_type`.
- use https on importing meta data in `importMeta`
- fix `calendarPlot` issue when there are gaps between selected months e.g.`month = c(1, 2, 12)`
- fix warning message in `calendarPlot`
- don't pad missing dates in `timeAverage` if DST
- deprecate `resolution` in `polarPlot` --- now uses predictions at a coarser resolution that are interpolated. Will also speed-up plotting
- add new source method to trajectory analysis (`trajlevel`), SQTBA --- Simplified Quantitative Transport Bias Analysis
- fix bivariate Gaussian function in `polarPlot` when two pollutants supplied for different statistics 
- add initial version of 'York regression' approach as an option to `polarPlot`. This regression approach determines a best fit line when there is error in the 'x' and the 'y'. The openair book will be updated to cover this in more detail.

# openair 2.9-1

- Do not return hydrocarbon measurements by default for annual and monthly summaries in `importAURN`.
- fix issue with class of date-time object for importing some air quality data e.g. `importAURN`.
- refine default grid lines in `windRose` and `pollutionRose`.

# openair 2.9-0

- add option to `TaylorDiagram` for annotation of observed data.
- fix issue with `timevariation` when `difference = TRUE`
- fix `trajCluster` issue to do with `dplyr`
- access other time-averaged air quality data using `importAURN`, `importSAQN`, `importWAQN`, `importAQE` and `importNI`. New option `data_type`, which can be "hourly" (default), "annual", "monthly", "daily" and "15min" (for SO~2~). These new data sources should make it much easier to work with long-term time series with many sites. See [the openair manual](https://openair-project.github.io/book/sections/data-access/UK-air-quality-data.html) for more details.
- fix terrible spelling in `selectRunning`.

# openair 2.8-6

- fix annotation bug when comparing two data sets in `windRose`
- enhance `selectRunning`. Now returns full data frame with a new condition column.
- make sure "hemisphere" argument goes to type "monthyear" and "yearmonth"
- argument `month` not passed in `calendarPlot`

# openair 2.8-4

- fix bug in `windRose` where whole period is calm
- add optimisation to `polarCluster` to speed up clustering through option `pamonce = 3`. This should not appreciably affect results.
- fix strange bug in `aqStats` due to `lubridate` time zone issue.
- fix bug in `TaylorDiagram` when group was present.
- Do not convert times with Daylight Saving Time when checking data --- just report presence.
- add option `plot.type` to `summaryPlot` to change line style; most useful for vertical lines in time series with `plot.type = "h"`

## openair 2.8-1

- Fix bug that crept in for `polarCluster`

## openair 2.8-0

- New function `polarDiff` to consider the difference surface between two polar plots.
- Modify `polarCluster` to consider clustering of differences in polar plot surfaces.
- Fix user annotation in `windRose` and pass on option for number of significant figures used to annotate plots (`dig.lab`)

## openair 2.7-6

- fix bug with `percentileRose` when `statistic = "cpf"` and multiple pollutants
- fix bug in `timeAverage` when `type = "season"` and `avg.time = "season"`
- add `week` to openair default types
- fix bug in `timeVariation` when considering difference plots with missing data
- fix CRAN check when using `return`

## openair 2.7-4

- add ambient temperature (`air_temp`) to meteorological variables returned from `importAURN`, `importSAQN` and `importWAQN` (using WRF model).
- re-format date returned in `importAQE` due to strange `dplyr` join issues
- add `statistic = "Spearman"` to `polarPlot` as an option when considering two pollutants.
- add `method` option to `corPlot` to allow different correlation methods ("pearson", "spearman" or "kendall")
- re-factor all UK air quality data import functions i.e. `importAURN`, `importSAQN`, `importWAQN`, `importAQE`.
- Add `importNI` to import data from Northern Ireland.
- Add option to UK air quality import functions to return information on whether individual pollutants have been quality-assured using option `ratified`. These functions include `importAURN`, `importSAQN`, `importWAQN`, `importAQE` and `importNI`
- Clean up what is returned from `importMeta`
- Return air quality data site name and code as character rather than factor

## openair 2.7-2

- fix tibble recycling issue
- fix shading issue in lowest percentile range in `percentileRose`.
- fix strange bug in `importWAQN` that would intermittently fail; sometimes crashing R.
- add option `date.format` to `TheilSen`.

## openair 2.7-0

- fix `calendarPlot` slowness on MacOS
- Refine use of Gaussian kernels when two pollutant statistics are
  considered in `polarPlot`
- Add option `statistic = "nwr"` in `polarPlot` that implements the
  Non-parametric Wind Regression based on Henry et al. (2009). The
  `openair` implementation is not identical but should yield similar
  results.
- NEW `importAQE` function to import data from [Air Quality
  England](https://www.airqualityengland.co.uk/) sites.
- NEW `importEurope` to provide access to some the data from the
  `saqgetr` package.


## openair 2.6-6

- fix bug in `TheilSen` when no missing data and `deseason = TRUE`
- fix bug in `timeAverage` when interval padding dates and date is "Date" class and not "POSIXct"
- fix example for `trajCluster`, should be `n.cluster` not `n.clusters`
- fix issue with DST in `cutData`
- allow 'hemisphere' to be supplied as an argument to `timeAverage`, used for avg.time = "season". 
- add option `to_narrow` to `importAURN`, `importSAQN`, `importKCL` and `importWAQN` to stack data into a tidy format. The data are now returned as a 'tibble'
- Allow meta data to be returned in `importSAQN` and `importWAQN`.

## openair 2.6-4

- fix issue with `TheilSen` when conditioning and < 6 annual measurements
- remove arrow heads in `polarPlot` axes.
- Use a Kalman filter and Kalman smooth to impute missing monthly means when `deseason = TRUE` in `smoothTrend` and `TheilSen`. This replaces simple linear interpolation.
- fix bug in `smoothTrend` when `ci = FALSE` (no smooth was fitted).
- Add `importWAQN` to access data from the [Welsh air quality network](https://airquality.gov.wales/).
- Add "waqn" as a data source to `importMeta`.

## openair 2.6-1

- Update handling of meta data in `importMeta`.

## openair 2.6-0

- In `aqStats` use default `data.thresh = 0` rather than 75% to ensure summaries are calculated
- Fix confidence intervals in `timeVariation` when statistic - "median". Revision will result in narrower range.
- Change default in `corPlot` to plot lower and upper triangles; add `lower` as an option.
- Update meta data for Scottish Air Quality Network ("saqn"); was badly out of date
- fix bug in `timeAverage` when interval more than one time unit e.g. "10 day"

## openair 2.5-0

- add simple versions of **viridis** colour palettes: "viridis", "plasma", "magma", "inferno" and "cividis" e.g. `polarPlot(mydata, cols = "plasma")`
- allow option `align` to be used in `aqStats` to determine how rolling means are calculated. Can take the values "centre" (default), "left" and "right".
- make sure full year present in `importAURN`
- fix issue with multiple pollutants in `polarAnnulus`
- fix issues in trajectory functions due to `dplyr`.

## openair 2.4-0

- use `lubridate` in `timeAverage` to improve speed / simplicity
- make sure all strip colours are white for `openair` objects
- fix bug in `polarPlot` when `statistic = "cpf"` when using tibbles
- fix bug in `polarCluster` with exported data (date was not correctly merged to produce  single date column)
- convert internal `mydata` to a 'tibble' for easier printing.
- allow `npoints = NA` in `trajPlot` to suppress plotting of interval points.
- fix bias correction bug in `windRose` when ws/wd have different names
- fix bug in `timeAverage` for wind direction (`wd`) when `statistic = "data.cap"`
- New built-in colour palette for the colour blind "cbPalette". Note maximum number of colours is 8 e.g. `windRose(mydata, col = "cbPalette", breaks = 6)`. Thanks to Jerry Martin.

## openair 2.3-0

- add option `plot` to `TheilSen`. `FALSE` can be useful when analysing data to extract the trend components and plot in other ways and when the `TheilSen` plot is not required.
- add option `silent` to `TheilSen` to avoid printing updates to trend fitting. By default it is `FALSE`.
- fix wrong ordering of names in `timeVariation` when more than one pollutant
- fix date parsing issues in `selectByDate`.
- fix wrong ordering in `timePlot`
- allow `calendarPlot` to span any time period. The function can now straddle parts of two years or several years.
- add option `col.arrow` to `calendarPlot` to control colour of the wind speed / direction annotation arrows.

# openair 2.2-3

## Main changes

- refine `selectByDate` due to changes in `lubridate`
- fix issue with `importSAQN` when no data
- remove site information from help of `importAURN` and point users to `importMeta`
- fixed bug in `scatterPlot` when `method = "level"` due to NSE
- use `tidyr` in place of `reshape2`
- remove dependency of `plyr` and `reshape2`
- fix bug in `percentileRose` for `method = "cpf"` with multiple pollutants

# openair 2.1-5

## Main changes

- change `polarCluster` resolution to "normal" rather than "fine" to speed up.
- fix regression when fitting smooth using `trajLevel`
- remove all Airbase functions, will be available via new package that also incorporates newer e-reporting data
- fix bug in `timeAverage` when `statistic = "sum"` and all data in period was missing (would return 0 rather than `NA`)
- make sure that same data used in all `conditionalEval` plots by using data where there are no missing data for all variables used. This is more important of `var.obs ` and `var.mod` are supplied and hence additional variables are considered relative to only `obs` and `mod`.
- fix date issues in `calendarPlot` to do with time zones
- remove missing data when type is a numeric value split up into quantiles
- fix bug in `windRose`/`pollutionRose` when two conditioning variables were given (problem in bias correction)

# openair 2.1-0

## Main changes

- add `angle` option to `percentileRose` to allow wind direction averages for sectors >10 degrees.
- fix bug in `aqStats` if only a few lines of data
- allow `statistic` to equal "mean", "median" or "frequency" in `scatterPlot`, when `method = "level"`
- change smoothing in `scatterPlot` when `method = "level"` to use tensor interaction to allow for better smoothing when x and y are on different scales.
- warn when >10% negative data detected in `polarPlot` and suggest setting `force.postive = FALSE`
- make `maps` Suggests rather than Depends
- refine check on whether bias correction is needed in `windRose`
- still plot data when <6 points in `TheilSen` and when no trend information is given
- New function `binData` to easily summarise mean and 95% confidence intervals for intervals of a variable
- Export `bootMeanDF`, used to calculate the bootstrap uncertainty in the mean of a vector.

# openair 2.0-0

## Main changes

- silence download progress by default in `importAURN`
- update `polarPlot` to work with pairwise statistics to compare two pollutants. The function can consider Pearson correlation and slopes from ordinary linear regression, robust regression (using **MASS** function `rlm`) and quantile regression (requires the **quantreg** package to be installed).
- change default `polarPlot` plot resolution to "fine".
- fix `windRose` problem with some data due to missing data
- move `mapdata` package to Suggests
- add option to return meta data (site type, lat, lon) from `importAURN` and `importKCL`.
- fix statistic = "weighted.mean" issue in `polarPlot`.
- make sure `y.relation` is used when there is no grouping in `timePlot`. To retain the behaviour of earlier versions use `relation = "free"`
- fix issue with `type` when used with `timeProp`
- fix bias correction when not default type in `windRose`
- fix pch colour bug in `timePlot`
- add option `alpha` to `polarPlot` to control transparency of plotted surface. Mostly useful for overlaying polar plots on leaflet maps (see [openairmaps](https://github.com/openair-project/openairmaps) package)
- enhance `grid.line` option in `windRose` so that users can control grid spacing, line type and line colour

# openair 1.9-9

## Main changes

- correct citation information
- Now depends on R version of >= 3.2.0 so that `download.file` can use `libcurl` for access to https (used in `importAURN`)
- add `avg.time` option to `summaryPlot` to control the averaging times of the time series lines and `print.datacap` to control whether the data capture % is shown for each interval.
- fix bug in `selectByDate` where a day number would not work
- remove arrows in `windRose`, `polarPlot` to avoid ambiguous interpretation of wind direction
- add `key.position` option to `timePlot` to control the location of the key.
- fix labels in `timeVariation` when data for some types is missing.
- fix `trendLevel` issue due to `dplyr`
- use `lubridate` package in `timeAverage`
- fix bug in `summaryPlot` related to `dplyr` use (would not plot missing data correctly)
- better handling of precision in `windRose` mean and statistics returned in data (thanks to Dr Ulrich Quass)
- fix bug in `importKCL` when incomplete time series (would drop site code and site name)
- fix bug in `pollutionRose` due to issue with calms
- fix bug in `smoothTrend` where model uncertainties were not returned
- fix bug in `summaryPlot` where missing data would not be shown correctly when date was not ordered in sequence
- fix annotation in `windRose` when comparing two data sets
- `GoogleMapsPlot` is deprecated and will be replaced with a better function.

# openair 1.8-6

- Only plot lower triangle for `corPlot`
- make sure date class is POSIXct, POSIXt in `importAURN`
- fix problem in `importMeta` for AURN - change in database source format
- fix `pollutionRose` plot warning
- fix NMB in `modStats`
- fix bug in `timeAverage` where estimating the time interval in input data could be unreliable due to low data availability
- fix time expansion bug in `timeAverage`
- add new `type` "yearseason" (or "seasonyear"). This will split data by every year / season combination, making sure the seasons are contiguous. For example, in winter in the northern hemisphere December 2010 will be considered part of winter 2011, rather than winter 2010. Thanks to Ralf Weisse for the suggestion.
- fix download issues with `importAURN` and `importMeta` when users are within an organisational network. Problems likely due to move from http to https and SSL Certificates.
- fix `TheilSen` bug when two types.

# openair 1.8-2

- fixes for new version of `dplyr`
- adjust legends in `conditionalEval` to avoid plot error.
- fix problem on linux and simplify `importAURN`
- update `importAURN` meta data in help function.

# openair 1.8-0

- Allow `scatterPlot` to have control over plot symbol fill and colour (for symbols 21 to 25). Use `cols` and `fill` to control.
- `scatterPlot` can now fit more than one linear equation when there is a grouping variable
- give message when using a users' own `type` such as month, year etc.
- fix ordering of bars on `timeProp`, remove `box.width` option
- fix bug introduced in 1.7-4 that affected `windRose` bias correction (thanks to Eric Christensen)
- Add option for `statistic = "r"` in `polarPlot` for comparing polar plot correlation surfaces between two pollutants using Gaussian kernel weighting.
- fix bug in `type = "daylight"` when time zone not UTC.
- remove `cutDaylight` as a separate function (`cutData` works for everything)
- add `w.shift` option to `calendarPlot` to control the first day of the week and subsequent order (thanks to Giovanni Bonafè)

# openair 1.7.3

- Fix regression in `timeAverage` when expanding time series 
- Remove `dplyr` warnings in `TheilSen`
- Keep season order correct when averaging time is season and `type = "season"` in `TheilSen` and `smoothTrend`
- add origin marker to `trajCluster`
- better date padding when >1 type
- fix some `dplyr` bugs where some functions would fail with two types
- return data frame of cluster information in `trajCluster`

# openair 1.7 

- Allow `linearRelation` to use any arbitrary time averaging period.
- don't add line to monthly plot in `timeVariation` when group = "season"
- fix `NA` factors in `trajPlot`
- make border black when using maps for improved clarity
- fix bug in `trajPlot` that sometimes failed to print map when grouping
- add percentage total trajectories for `trajCluster` and option `by.type`
- don't touch existing date-based types in `cutData`
- refine date checks
- speed up `timeAverage`
- new dependent package `lubridate` for easier / faster date-time manipulations
- fix slow `timeProp` (lattice `panel.barchart` is very slow)
- only remove missing wind speed in `windRose` (wd can be `NA` and ws zero i.e. calm)
- correct order of labels in `timeVariation` when `difference = TRUE` for some factor levels (were in alphabetical) 

# openair 1.6.6 

- Not all labels shown if >25 in `trendLevel`; make sure strip is white
- correct bug in `timeVariation` (`vector.ws` did not work)
- fix some cases where background strip was not white

# openair 1.6.5

- changes to fix examples not run during the R CMD check
- make `TaylorDiagram` more flexible when using two groups and the
  second is date-based

# openair 1.6.4 

- fix bug in `GoogleMapsPlot` when pollutant not given
- don't clutter up working directory with `GoogleMapsPlot`; write to
temporary file instead
- fix bug when trying to access multiple sites with no data in
- fix problem with multiple sites in `aqStats`
- `TheilSen` should always give trend in units/year (would use xlab if
supplied)
- fix `cutData` bug where quantile cuts are made
- don't remove missing data in `scatterPlot` so that factors with no data
  still shown
- `TheilSen` should always give trend in units/year (would use xlab if supplied)
- New option `slope.text` in `TheilSen` to allow users to add their
  own text i.e. not the default "units/year"
- refine check on DST
- Always report data capture % in `aqStats`


# openair 1.6 

- Don't force integer results for `importAURN`
- fix bug with `period = "months"` in `summaryPlot`
- allow linear fit with `method = "hexbin"` in `scatterPlot`
- allow users to define own map limits in trajectory functions and
better scaled map grids
- fix renaming bug in `airbaseStats`
- bug in `windRose` when all calm
- date bug fixes in `scatterPlot`
- add option `windflow` to `scatterPlot` and `timePlot` to allow wind
flow plots
- suppress harmless warnings in `smoothTrend`
- fix bug in some functions when type more than one
- add support in `pollutionRose` for option `normalise` to show
probability by wind sector (0 to 1).
- `timeAverage` now has an option `type` similar to other functions. A
  common use would be to apply `timeAverage` to a data frame with
  multiple sites where there is a column representing site name
  e.g. `type = "site"`.
- Add receptor location for trajectory plots `trajLevel` and
`trajPlot`.
- Add an option `trend` to `TheilSen` to control how the trend lines
are drawn.
- fix bug in `calendarPlot` when partial month available
- fix bug in `calendarPlot`, don't need to cut data first
- add `npoints` option to `trajPlot` to control time spacing of dots shown on back trajectories
- don't include missing data when `statistic = "frequency` in `timeAverage`
- fix bug in `timeProp` due to point above
- fix bug in `timeVariation` with `type = "season"` when space in pollutant name

# openair 1.5

- Add 'days' as a time unit to `summaryPlot` - useful for shorter time
series
- Initial changes to use `dplyr` to speed up some of the code
  e.g. `timeAverage`
- Automate `x.inc` and `y.inc` if not supplied by user in `scatterPlot`
- Fix regression in `trajLevel` frequency calculation
- Fix a few problems with trajectory plotting - some methods would
fail given recent updates
- Make trajectory gridded analysis faster
- Fix bug in `importMeta` introduced since using `dplyr`
- Add `angle.scale` to `windRose` to control placement of radial scale
(helps to avoid clash with wind rose paddles)
- Fix bug in `timePlot` when `avg.time` given (regression)
- Allow `TaylorDiagram` to have `group` of length two. This will show
  all group combinations but will only differentiate them by
  colour/symbol according to the first grouping variable.
- `timeVariation` can now take a `ylim` list to control the y-limits
on each individual plot
- For trajectory plotting allow `map.res` to be "state" to show the US
States.
- Fix bug in `importKCL` when date was not at beginning of the year

# in openair 1.1-2 

- Fix regression for openair methods e.g. affected plot method for
`timeVariation` subsets
- Check data are numeric before applying running mean (would crash R if
not)
- Begin transition to Github, more details to follow
- Add option dist to `scatterPlot` for surface modelling
- Sort out package dependencies etc. to make maps easier to load
- Add fontsize option to all openair plot functions
- Change contact details, fix citation problem
- Make sure `importKCL` is file of full year
- Fix warning messages in `aqStats` when multiple pollutants selected

# openair 1.1 

*	Add ref.y option to `timeVariation` for y references line(s)
*	Fix type = "wd" labelling to `corPlot`
*	Refine airbaseStats to include site type and city by default
*	Make trendLevel colour scaling consistent with other functions
	and allow missing data to be shown in different colour
*	Allow categorical scales in trendLevel
* 	Fix type = 'season' in `trajLevel` (winter period not properly
	calculated)
*	Allow multiple reference lines to be added to `timePlot`,
	`scatterPlot`, `timeVariation` and add to `smoothTrend` together
	with full control of their properties. Note - ref.x and
	ref.y must now be lists; see help file for details.
*	Don't open graphics window in aqStats
*	Add more flexibility to `timeAverage` for irregular time
	intervals
*	Add 12-hour interval points on back trajectory lines
*	Fix bug in `percentileRose` with stat = "cpf" and non-default
	type (now uses single percentile based on all data, not
	each panel)
*	Check if date is in POSIXt format and throw error if TRUE
*	Improve date checks in selectByDate
*	Add dendrogram option to `corPlot` (thanks to James Durant for
	the suggestion)
*	Remove strip in `corPlot` when type = "default"
*	Remove statistic description in `pollutionRose` when annotate =
	FALSE.
*	Fix colour scaling bug in `scatterPlot`/`trajPlot` when user
	limits supplied
*	Check period = "years" or "months" in `summaryPlot`; some users
	supplied "year" resulting in incorrect statistics
*	Give mean and percent calm in `pollutionRose` when statistic =
	"prop.mean" (was erroneously percentage)
*	getMet function for downloading Hysplit met files in manual
	did not download as binary files; now corrected
*	Add name.pol argument to `smoothTrend` for more control over
	names used for plotting
*	Show first few dates when import fails to apply correct date format
	(helps to provide a clue as to actual date format)
*	Updates to trajectory plots to allow for different map
	projections using the mapproj package (new dependency)
*	Fix trajectory frequency calculation - underestimated frequencies.

# openair 1.0-0 

*	Pass all arguments in `corPlot`
*	Enhance `timeVariation` to consider median + quantiles through
	option 'statistic'
*	Do not remove NA results from modStats
*	Better scaling in polarCluster; consistent with `polarPlot`
*	Fix `importKCL` where dates were filled if two non contiguous
	years were chosen
*	Refine scaling of ws in `polarPlot` when upper is set
*	Fix bug when type = "weekday" but not all days of the week
	are present
*	Add annotate option to TaylorDiagram
*	Allow modStat statistics to be chosen by user and add Index of Agreement
*	Needs to be >24 months to deseason in `smoothTrend`/`TheilSen`
	(was >=)
* 	Refine names returned by importADMS when used with .pst file
*	Fix trajectory code in appendix D in the manual (some function
	arguments were not passed)
*	Better user defined limits scaling in `polarPlot`, `scatterPlot`,
	polarAnnulus when limits within data range
*	Clarify time zone for importing data to openair and checking
	(see manual for details)
*	Initial versions of functions to import EEA airbase data - see
	newsletter/manual for details
*	Better treatment of daylight saving time in cutData,
	`timeVariation` and polarAnnulus (allow any local time zone to be used;
	was just GMT/BST before)
*	Remove existing date-based columns in cutData to ensure date
	is used instead
* 	Allow users to shade/not shade alternate years in `smoothTrend`
	and `TheilSen`
*	Do not smooth `percentileRose` by wind direction by default.
* 	Add option 'wd' to `percentileRose`
* 	Fix `trajPlot` bug when trajectory data was not at 3-hour intervals
* 	Return data and smooth fit information in `smoothTrend`

# openair 0.9-0 

*	Fix bug introduced in 0.8-0 for `scatterPlot` surfaces
*	Add statistic and percentile option to polarAnnulus; allow
	Conditional Probability Functions
*	Allow percentile intervals to be considered in `polarPlot` when
	statistic = "cpf"
*	Correct calculation of AOT40 in aqStats to take account of
	daylight hours and growing season (Apr. to Sep.). Can use
	latitude/longitude, if supplied
*	Fix bug in aqStats when non predefined pollutant used
*	Allow log10 factor of 2 lines in `scatterPlot` when x and/or
	y are on a log scale
*	Allow users to supply own transform/inverse functions to hexbin
*	Don't try and pad-out missing data in rollingMean when
	returning results
*	Make latticeExtra a 'depends'
*	Don't copy-down data by default in `timeAverage` when requested
	averaging time is < original. Better handling when 'site' is
	present
*	Update AURN help file site information; add site type
*	Show where data removed when min.bin > 1 for `polarPlot`
*	Add 'trim' value to percentiles in `polarPlot` to extract more
	source information.
*	Add 'weights' argument to `polarPlot` to down-weight bins with
	few data points - alternative to min.bin
*	Use more wd sectors for surface modelling when wd is not
	already rounded to 10 degree intervals in `polarPlot`
*	Fix bug in `timePlot` when pch supplied and group = TRUE.
*	Add Air Quality Standard for O3 in aqStats (days >120 ug/m3)
	not to be exceeded more than 10 days a year.
*	Add max.freq option to `windRose`/`pollutionRose` to control
	extent of radial limits
*	Add map.res option to `trajPlot` and `trajLevel` and make default
	lower resolution. Make default pollutant 'height' (always
	present)
*	Add Oslo and Rotterdam to trajectory database for 2010-2012
*	Better limits control on `calendarPlot`, use first year if not
	supplied, add option month to allow only selected month(s)
*	Fix bug when type = "wd" and some are missing (remove them)
*	Don't open up graphic window in `timeAverage`
*	Fix indexing problem in polarCluster which sometimes caused
	the function to trip up
*	Only plot full length trajectories in `trajPlot`
*	Allow smoothing parameter k to be used when smooth = TRUE for
	more control in `scatterPlot`
*	Add a bias correction to `windRose` (thanks to Philippe Barneoud
	from Environment Canada for pointing out the need and
	solution)
*	Better treatment of `pollutionRose` when comparing two met
	datasets when ws bias is zero
*	Fix conditionalEval to use COE, not IOA and other minor
	changes
*	Allow method = "density" in `trajLevel`


# openair 0.8-0 

*	make sure missing dates are plotted properly in `scatterPlot`
*	fix regression in `corPlot` - main did not work
*	remove time zone options in import - users must supply data in
	GMT (UTC). Too many problems introduced due to daylight saving
	time
*	Use Legates and McCabe Coefficient of Efficiency in modStats
	in place of the Index of Agreement - easier to interpret.
*	Allow type = "month" etc to be used in `timeVariation` (was
	variable clash). Allow more flexibility when group and type
	are used
*	Better handling of user-defined limits in `polarPlot` and
	`scatterPlot` (method = "level")
*	Add optional mean line option to `percentileRose`.
*	Correct `pollutionRose` documentation about comparing 2 data
	sets (first subtracted from second)
*	Fix bug in `timeVariation` that showed extra NA level for
	certain groups/types
*	Fix scaling bug in polarCluster when there is negative data
	e.g. x = "temp"
*	Allow statistic = "median" in `trajLevel`
*	NEW FUNCTION timeProp to plot time series by category as a
	bar chart
*	Fix `windRose` bug when wind direction name was not wd
*	Fix bug in `importAURN` when pollutant = "all" was specified
*	Allow minimum value of breaks in ws to be above minimum ws
	value, but warn.
*	Allow day to be numeric in selectByDate to select days of the
	month
*	Better base maps with trajectory plotting (filled and alpha
	transparency)
*	Fix `windRose` bug where all data are missing
*	Use higher resolution mapdata not maps package for trajectory
	plots
*	Allow method = "hexbin" in `trajLevel` for hexagonal binning
	of trajectory frequencies
*	Refine rollingMean to allow moving window to be aligned
	centre/center, left or right + better treatment of ends when
	data capture threshold used. Use option 'width' rather than
	'hours' because non-hourly data can be considered.
*	Fix `pollutionRose` scaling issue, which sometimes missed the
	lower interval
*	Fix `timeAverage` data capture issue - was not always setting
	data to NA
*	Output clusters as C1, C2 etc. not 1, 2.
*	Use same labelling in `calendarPlot` as other functions (wrong
	month order in non-English locales)
*	Export dendogram in `corPlot` - see example in help file
*	Fix scaling bug in `polarPlot` when radial variable was negative
	and small
*	Update 'mydata' to set negative data to NA
*	Add method = CPF to percentile rose and `polarPlot`
*	Update trajectory analysis to allow Potential Source
	Contribution Function (PSCF) and Concentration Weighted
	Trajectory (CWT) to be considered

# openair 0.7-0 

*	Allow more flexible layout when two or more pollutants
	are used with `polarPlot` and type is default
*	Fix colour scale problem for `windRose` when interval does not
	exist
*	Added 'nativeRaster' class handler for GoogleMapsPlots
*	(test) Fix for GoogleMapsPlot xlim, ylim
*	Do not remove missing data from `timeVariation`
*	Fix bug in FAC2 in modStats when observations = 0 (exclude
	from calculation because indeterminate)
*	Fix bug in `windRose` for empty panels and panel labelling when
	data missing
*	New option start.day in cutData that affects most plots;
	allows users to set start day when type = "weekday"
*	Allow categorical scales in `calendarPlot`
*	Allow two met data sets to be compared in `pollutionRose`
*	Allow statistic = "percentile" to be used with `polarPlot`
*	Allow 'method' to be passed to cor in `corPlot`
*	Added more back trajectory locations.
*	Try harder to plot `polarPlot`s when there is insufficient data
	to calculate a smooth surface.
* 	Rename some variables in `timeAverage` to avoid variable clash.
*	Add gridded frequency capability to `trajLevel`
*	Fix bug in colour scaling in polarFreq when statistic =
	"stdev" (0 would not be plotted)
*	Fix bug in `summaryPlot`: type = "density" broken in recent versions

# openair 0.6-0 

*	Add y.relation option to `timePlot`
*	Fix interpolation bug in calcFno2 and names in documentation.
*	Refine conditionalQuantile scales
*	Provide volatile and non-volatile components for FDMS PM10 and
	PM2.5 in `importKCL` - now consistent with `importAURN`
*	NEW FUNCTION conditionalEval for model evaluation - allows
	other variable performance to be assessed.
*	Make lattice strips white by default for cleaner look on
	complicated plots
*	Complete re-write of import to simplify - changes are NOT
	backward compatible but will allow more developments
*	Allow line breaks in titles using \n to work with quickText
	- thanks to Karl
*	Allow better annotation of `calendarPlot` - highlight values
	above/below a certain threshold.


# openair 0.5-25 

*	Fix bug when type = "wd" - would add missing data to north
	sector
*	Add observed histogram to conditionalQuantile
*	Fix bug in `timeAverage` when ws was not available but wd was
*	Temporary fix to GoogleMapsPlot documentation due to new
	package version

# openair 0.5-23 

*	Fix bug in `windRose` when paddle = FALSE

# openair 0.5-22 

*	Fix scaling bug that could sometime affect `polarPlot` grid
	lines
*	Fix regression in `importKCL` that was introduced in 0.5-21
*	Make sure wd data are rounded to 10 degrees in polarFreq
*	Fix date padding issue in `smoothTrend` when type = "site"
*	`windRose` now gives mean ws in each panel rather than count
*	add option date.format to `timePlot` for more control over date
	format on axis

# openair 0.5-21 
*	Use C++ code for rolling mean calcs. Much faster, more to
	follow
*	NEW function trajCluster to carry out cluster analysis on back
        trajectories
*	Simple model ranking available in modStats
# Changes in openair 0.5-18 [2012-01-16]
*	Update trajectory files to 2011 and add Berlin, Paris
*	Add option seg to `pollutionRose` to control the width of the
	segments
*	Add option start.day to `timeVariation` to control the order of
	weekdays.
*	Fixed bug in polarAnnulus where 360 degree winds were absent.
*	Remove dependency on zoo and proto.
* 	Allow `importAURN` to import new ws/wd from pre-calculated WRF
	data at AURN sites

# openair 0.5-17 


*	Update to [IN DEVELOPMENT] GoogleMapsPlot.
        (1 lat,lon default handling)

# openair 0.5-15 

*	Update imports etc.

# openair 0.5-15

*   Update to [IN DEVELOPMENT] GoogleMapsPlot.
*	Tidy up calcFno2 plotting.
*	Improve speed of `timeAverage` by removing date.pad.
*	Add vector averaging option to `timeAverage` for wind speed.

# openair 0.5-14 


- NEW FUNCTION polarCluster for undertaking k-means clustering of
bivariate polar plots

* Remove any considerations of time zones in selectByDate. If start
        and end are supplied, whole days based on Date format are used
        to select. Now accepts start/end in the form "YYYY-mm-dd" as
        well as UK format e.g. dd/mm/YYYY.

# openair 0.5-13 

* Major update to `polarPlot` allowing variables other than
	"ws" to be plotted with wind direction.
* Improve documentation for `polarPlot`/TaylorDiagram
* Update openair citation information to Journal article

# openair 0.5-12 

* do not clip `polarPlot` concentrations unless upper is supplied
* new option 'intervals' for `percentileRose`
* add min.bin option to polarAnnulus
* add Index of Agreement to modStats
* new averaging time "season" for `timeAverage`
*	better treatment of avg.time = "season" and type = "season" in
	`TheilSen` and `smoothTrend`
*	use bootstrap methods to calculate 95% confidence intervals in
        the mean for `timeVariation`
*	New option 'difference' in `timeVariation` to show difference
        between two variables with bootstrap 95% CI in the mean
*	Byte-compile package
*	Place key on right be default in `scatterPlot` to avoid clash
        with x or y axis labels.
*	Include all colour schemes defined in the RColorBrewer package

# openair 0.5-11 

* update to `corPlot`, `scatterPlot`, `smoothTrend`, linearRelation,
        `percentileRose`, `trajPlot`, `trajLevel`, `timeVariation`, TaylorDiagram,
        `timePlot`, `summaryPlot`
        improved ... handling
*	remove warnings when importing air pollution data
*	additional update to `corPlot`
        added pollutant option, and openair class output
*	tweak scaling on `percentileRose` deal with negative data
*	change MannKendall to use `TheilSen` for all estimates for
        consistency. May slightly affect some p estimates.

# openair 0.5-10 

* update to `calendarPlot`, kernelExceed,
        MannKendall and conditionalQuantile
        improved ... handling
*       minor update to GoogleMapsPlot

# openair 0.5-9 

* allow model performance change to be considered in TaylorDiagram
* update to `windRose` and `pollutionRose`
        stat related annotation
* update to `polarPlot`, ploarFreq and polarAnnulus
        improved ... handling

# openair 0.5-8

* NEW FUNCTION `importMeta` to import site meta data from air
         pollution networks

# openair 0.5-7 

* update to trendLevel: improved ... handling
* update to `windRose`/`pollutionRose`: added statistic option
	"abs.count"; improved scaling of segment widths; improved
	... handling

# openair 0.5-6 

*	NEW FUNCTIONS importTraj, `trajPlot`, `trajLevel` for importing
	and plotting pre-defined HYSPLIT back trajectories. These
	functions are under active development and are for testing
	purposes only!
*	Fix error in % upper/lower uncertainty intervals for
	MannKendall
*	More control over trend information placement for MannKendall
	- arguments text.col, lab.frac (for vertical position) and
	lab.cex (for font size)

# openair 0.5-4 

*	Use roxygen2 for package documentation and future
	maintainability
*	Allow splitByDate to handle multiple sites and output a new
	column controlled by argument 'name'
*	More options for MannKendall: control of colour and variable
	x-axis scales

# openair 0.5-0 

*	NEW FUNCTION `corPlot` for correlation matrices
*	add sep argument to import

# openair 0.4-23 

*	enhance 'normalise' option in `timePlot`
*	add annotate option to `windRose`/`pollutionRose`
*	NEW FUNCTION TaylorDiagram for model evaluation.

# openair 0.4-22 

*	NOTE - use reshape2 in place of reshape for speed and reliability
* 	allow more than one pollutant with `percentileRose`
* 	fixed title bug with `scatterPlot` - not shown for some methods
* 	modified key handling on plots using drawOpenKey, so
  	key = NULL or FALSE now removes colour key.
* 	added method = "level" to `scatterPlot` for binning data with
	optional smoothing, plus other code clean-ups
* 	fix bug in conditionalQuantile that labelled plots wrongly
	when site(s) has missing data
*	added ref.x and ref.y to `timePlot` to allow reference lines to
	be added
* 	do not remove calm wind speed conditions in any functions
	where this is not necessary

# openair 0.4-21 

* 	fixed bug in polarAnnulus that resulted in a failure to
	annotate the plot properly with period = "trend" and less than 1 year
	of data; improved smoothing default options used
* 	new 'statistic' option to `pollutionRose` to show contribution
	to counts and contribution to the mean. The latter is useful for
	displaying those wind directions that make most contribution to the
	overall mean. Panel mean is also now shown.
* 	small change to final example of re-shaping data in `importKCL`
* 	`timeAverage` can now expand data to shorter time periods
	e.g. hourly to 15-minute. This makes it more flexible to combine data
	sets with differing averaging times. For example, daily mean particle
	data can be expanded to 1-hour means and combined with an hourly
	meteorological data set.
* 	Fix treatment of calms in checkPrep, which affected cases with
	zero wind speeds in `timeAverage`
*	Fix to vector averaging in `timeAverage` (did not include wind
	speed in calculations). For most data this will make very little
	difference, but will be more important for low wind speeds and/or
	variable wind directions.
* 	allow type = 'variable' in `smoothTrend`
* 	Add avg.time to `smoothTrend` for "month" or "year" averages

# openair 0.4-20 

* 	Do not remove calms in `timeAverage`
* 	NEW FUNCTION importSAQN to import data from the Scottish Air
	Quality Network

# openair 0.4-19 

* 	improved import date.name/time.name handling of spaces
* 	Improved scaling for polarFreq
* 	`scatterPlot` type = "wd" labels aligned
* 	Added option "statistic" to `polarPlot`, which can now consider
	"mean", "median", "max" (maximum), "frequency". "stdev"
	(standard deviation) or "weighted.mean" in a similar way to polarFreq
* 	Fixed bad.24 and international tz bugs in import
* 	updated import/import.2

# openair 0.4-18 

*	Better handling of missing data in `smoothTrend`/MannKendall;
	particularly when there are multiple sites
*	Do not run checkPrep on selectByDate because it removes calms
*	Fixed scaling bug in polarFreq when ws.int != 1
*	use avg.time in calcPercentile instead of 'period' to ensure
	consistency with other functions (`timePlot` would fail with
	percentiles)
*	return NA in aqStats when no data present; previously it tried
	to calculate quantities, returning -Inf etc

# openair 0.4-17 

*	use readRDS and not .readRDS (caused failure on R Dev)
*	openair now depends on >=R2.13.0
*	NEW FUNCTION sqStats to calculate common and pollutant-specific
	air quality statistics by year and site.
*	Changed option data.capture in rollingMean to data.thresh to
	be consistent with other functions.

# openair 0.4-16 

*	Fixed bugs in cutData when a partial year was used with type
  	= "month". Also affected type = "season" and "weekday". The
  	plots were labelled in the wrong order.
*	Ensure that missing wind sectors are skipped in type = "wd"
	for `smoothTrend`, MannKendall, `timePlot` and `scatterPlot`

# openair 0.4-15 

*	Preparation for CRAN release
*	removed some restrictions on type/period combinations in polarAnnulus

# openair 0.4-14 

*	Added preliminary greyscale method to openColours and linked in
      strip background and other text and line colour control for
      standard openair plots.

# openair 0.4-13 

*	Removed ad-hoc sites from `importKCL`, added site classification
	to the help file and ensured GMT is exported.

# openair 0.4-12 


*	Added y.relation option to `smoothTrend` and MannKendall to
         allow flexible y-scales.
*	NEW FUNCTION '`percentileRose`' for flexible plotting of
         percentiles by wind direction
*	 NEW FUNCTION `selectRunning` for selecting run lengths of a numeric
         variable above a certain threshold.
*	Modified plot layout in `polarPlot` to maximise plotting area
*	Initial fixes to functions affected by time zone settings
*	polarFreq now has consistent annotation cf. `windRose` and
         `percentileRose`; new option grid.line to control radial axes.
*	 Fixed couple of minor auto.text bugs in `summaryPlot` and
         `timePlot`
*	 Removed dependency of date for polar functions where
         time-based types are not required.


# openair 0.4-11 


*	More consistent use of strips in default plot (remove them)
*	Add percentage units to `windRose` and `pollutionRose` on radial scale

# openair 0.4-10 


*	Fixed `calendarPlot` main/quickText handling.
*	Fixed `calendarPlot` first day of month error.
*	Fixed `summaryPlot` site ordering when more than one site.

# openair 0.4-9 

*	Added new options to `scatterPlot` (plot.type, lwd, and lty)
         making it possible to add points and/or lines

# openair 0.4-8

*	Added new cutData type option 'daylight', enabled using new function
	cutDaylight.


# openair 0.4-7 

*     Added new function conditionalQuantile for model
      evaluation purposes.
*     Fixed `importAURN` to account for a change in web domain
      address at AEA.


