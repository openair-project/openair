# CERC Atmospheric Dispersion Modelling System (ADMS) data import function(s) for openair

Function(s) to import various ADMS file types into openair. Currently
handles ".met", ".bgd", ".mop" and ".pst" file structures. Uses
[`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html) to read
in data, format for R and openair and apply some file structure testing.

## Usage

``` r
importADMS(
  file = file.choose(),
  file.type = "unknown",
  drop.case = TRUE,
  drop.input.dates = TRUE,
  keep.units = TRUE,
  simplify.names = TRUE,
  test.file.structure = TRUE,
  drop.delim = TRUE,
  add.prefixes = TRUE,
  names = NULL,
  all = FALSE,
  ...
)
```

## Arguments

- file:

  The ADMS file to be imported. Default,
  [`file.choose()`](https://rdrr.io/r/base/file.choose.html) opens
  browser. Use of
  [`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html) also
  allows this to be a readable text-mode connection or url (although
  these options are currently not fully tested).

- file.type:

  Type of ADMS file to be imported. With default, "unknown", the import
  uses the file extension to identify the file type and, where
  recognised, uses this to identify the file structure and import method
  to be applied. Where file extension is not recognised the choice may
  be forced by setting `file.type` to one of the known `file.type`
  options: "bgd", "met", "mop" or "pst".

- drop.case:

  Option to convert all data names to lower case. Default, `TRUE`.
  Alternative, `FALSE`, returns data with name cases as defined in file.

- drop.input.dates:

  Option to remove ADMS "hour", "day", and "year" data columns after
  generating openair "date" timeseries. Default, `TRUE`. Alternative,
  `FALSE`, returns both "date" and the associated ADMS data columns as
  part of openair data frame.

- keep.units:

  Option to retain ADMS data units. Default, `TRUE`, retains units (if
  recoverable) as character vector in data frame comment if defined in
  `file`. Alternative, `FALSE`, discards units. (NOTE: currently, only
  `.bgd` and `.pst` files assign units. So, this option is ignored when
  importing `.met` or `.mop` files.)

- simplify.names:

  Option to simplify data names in accordance with common `openair`
  practices. Default, `TRUE`. Alternative, `FALSE`, returns data with
  names as interpreted by standard R. (NOTE: Some ADMS file data names
  include symbols and structures that R does not allow as part of a
  name, so some renaming is automatic regardless of `simplify.names`
  setting. For example, brackets or symbols are removed from names or
  replaced with ".", and names in the form "1/x" may be returned as
  "X1.x" or "recip.x".)

- test.file.structure:

  Option to test file structure before trying to import. Default,
  `TRUE`, tests for expected file structure and halts import operation
  if this is not found. Alternative, `FALSE`, attempts import regardless
  of structure.

- drop.delim:

  Option to remove delim columns from the data frame. ADMS .mop files
  include two columns, "INPUT_DATA:" and "PROCESSED_DATA:", to separate
  model input and output types. Default, `TRUE`, removes these.
  Alternative, `FALSE`, retains them as part of import. (Note: Option
  ignored when importing `.bgd`, `.met` or `.pst` files.)

- add.prefixes:

  Option to add prefixes to data names. ADMS .mop files include a number
  of input and process data types with shared names. Prefixes can be
  automatically added to these so individual data can be readily
  identified in the R/openair environment. Default, `TRUE`, adds
  "process." as a prefix to processed data. Other options include:
  `FALSE` which uses no prefixes and leave all name rationalisation to
  R, and character vectors which are treated as the required prefixes.
  If one vector is sent, this is treated as processed data prefix. If
  two (or more) vectors are sent, the first and second are treated as
  the input and processed data prefixes, respectively. For example, the
  argument (`add.prefixes="out"`) would add the "out" prefix to
  processed data names, while the argument
  (`add.prefixes=c("in","out")`) would add "in" and "out" prefixes to
  input and output data names, respectively. (Note: Option ignored when
  importing `.bgd`, `.met` or `.pst` files.)

- names:

  Option applied by `simplifyNamesADMS` when `simplify.names` is
  enabled. All names are simplified for the default setting, `NULL`.

- all:

  For .MOP files, return all variables or not. If `all = TRUE` a large
  number of processed variables are returned.

- ...:

  Arguments passed on to
  [`utils::read.csv`](https://rdrr.io/r/utils/read.table.html)

  `header`

  :   a logical value indicating whether the file contains the names of
      the variables as its first line. If missing, the value is
      determined from the file format: `header` is set to `TRUE` if and
      only if the first row contains one fewer field than the number of
      columns.

  `sep`

  :   the field separator character. Values on each line of the file are
      separated by this character. If `sep = ""` (the default for
      `read.table`) the separator is ‘white space’, that is one or more
      spaces, tabs, newlines or carriage returns.

  `quote`

  :   the set of quoting characters. To disable quoting altogether, use
      `quote = ""`. See [`scan`](https://rdrr.io/r/base/scan.html) for
      the behaviour on quotes embedded in quotes. Quoting is only
      considered for columns read as character, which is all of them
      unless `colClasses` is specified.

  `dec`

  :   the character used in the file for decimal points.

  `fill`

  :   logical. If `TRUE` then in case the rows have unequal length,
      blank fields are implicitly added. See ‘Details’.

  `comment.char`

  :   character: a character vector of length one containing a single
      character or an empty string. Use `""` to turn off the
      interpretation of comments altogether.

## Value

In standard use `importADMS()` returns a data frame for use in openair.
By comparison to the original file, the resulting data frame is modified
as follows:

Time and date information will combined in a single column "date",
formatted as a conventional timeseries (`as.POSIX*`). If
`drop.input.dates` is enabled data series combined to generated the new
"date" data series will also be removed.

If `simplify.names` is enabled common chemical names may be simplified,
and some other parameters may be reset to openair standards (e.g. "ws",
"wd" and "temp") according to operations defined in `simplifyNamesADMS`.
A summary of simplification operations can be obtained using, e.g., the
call `importADMS(simplify.names)`.

If `drop.case` is enabled all upper case characters in names will be
converted to lower case.

If `keep.units` is enabled data units information may also be retained
as part of the data frame comment if available.

With `.mop` files, input and processed data series names may also been
modified on the basis of `drop.delim` and `add.prefixes` settings

## Details

The `importADMS` function were developed to help import various ADMS
file types into openair. In most cases the parent import function should
work in default configuration, e.g. `mydata <- importADMS()`. The
function currently recognises four file formats: `.bgd`, `.met`, `.mop`
and `.pst`. Where other file extensions have been set but the file
structure is known, the import call can be forced by, e.g,
`mydata <- importADMS(file.type="bgd")`. Other options can be adjusted
to provide fine control of the data structuring and renaming.

## Note

Times are assumed to be in GMT. Zero wind directions reset to 360 as
part of `.mop` file import.

## See also

Other import functions:
[`importAURN()`](https://openair-project.github.io/openair/reference/importUKAQ-wrapper.md),
[`importEurope()`](https://openair-project.github.io/openair/reference/importEurope.md),
[`importImperial()`](https://openair-project.github.io/openair/reference/importImperial.md),
[`importMeta()`](https://openair-project.github.io/openair/reference/importMeta.md),
[`importTraj()`](https://openair-project.github.io/openair/reference/importTraj.md),
[`importUKAQ()`](https://openair-project.github.io/openair/reference/importUKAQ.md)

## Author

Karl Ropkins, David Carslaw and Matthew Williams (CERC).

## Examples

``` r
##########
# example 1
##########
# To be confirmed

# all current simplify.names operations
importADMS(simplify.names)
#> Simplification operation summary
#> [ADMS => R => OPENAIR]:
#>  1/LMO => X1.LMO => RECIP.LMO
#>  1/MONIN-OBUKHOV LENGTH => X1.MONIN.OBUKHOV.LENGTH => RECIP.LMO
#>  ALBEDO(D) => ALBEDO.D. => ALBEDO.DISP
#>  ALBEDO (D) => ALBEDO..D. => ALBEDO.DISP
#>  ALBEDO(DISP) => ALBEDO.DISP. => ALBEDO.DISP
#>  ALBEDO (DISP) => ALBEDO..DISP. => ALBEDO.DISP
#>  ALBEDO (DISPERSION AREA) => ALBEDO..DISPERSION.AREA. => ALBEDO.DISP
#>  ALBEDO(M) => ALBEDO.M. => ALBEDO.MET
#>  ALBEDO (M) => ALBEDO..M. => ALBEDO.MET
#>  ALBEDO(MET) => ALBEDO.MET. => ALBEDO.MET
#>  ALBEDO (MET) => ALBEDO..MET. => ALBEDO.MET
#>  ALBEDO (MET SITE) => ALBEDO..MET.SITE. => ALBEDO.MET
#>  ALPHA(D) => ALPHA.D. => ALPHA.DISP
#>  ALPHA (D) => ALPHA..D. => ALPHA.DISP
#>  ALPHA(DISP) => ALPHA.DISP. => ALPHA.DISP
#>  ALPHA (DISP) => ALPHA..DISP. => ALPHA.DISP
#>  ALPHA(M) => ALPHA.M. => ALPHA.MET
#>  ALPHA (M) => ALPHA..M. => ALPHA.MET
#>  ALPHA(MET) => ALPHA.MET. => ALPHA.MET
#>  ALPHA (MET) => ALPHA..MET. => ALPHA.MET
#>  BL DEPTH => BL.DEPTH => H
#>  BOUNDARY LAYER DEPTH => BOUNDARY.LAYER.DEPTH => H
#>  BUOYANCY FREQUENCY ABOVE BOUNDARY LAYER => BUOYANCY.FREQUENCY.ABOVE.BOUNDARY.LAYER => NU
#>  CLOUD => CLOUD => CL
#>  CLOUD AMOUNT (OKTAS) => CLOUD.AMOUNT..OKTAS. => CL
#>  Conc|ppb|NAME|SOURCES|-| RESOLUTION => Conc.ppb.NAME.SOURCES....RESOLUTION => NAME.SOURCES.RESOLUTION
#>  Conc|ppm|NAME|SOURCES|-| RESOLUTION => Conc.ppm.NAME.SOURCES....RESOLUTION => NAME.SOURCES.RESOLUTION
#>  Conc|ug/m3|NAME|SOURCES|-| RESOLUTION => Conc.ug.m3.NAME.SOURCES....RESOLUTION => NAME.SOURCES.RESOLUTION
#>  NAME.All.sources.1hr => NAME.All.sources.1hr => NAME
#>  NAME.All.sources.RESOLUTION => NAME.All.sources.RESOLUTION => NAME.RESOLUTION
#>  NAME.SOURCE.1hr => NAME.SOURCE.1hr => NAME.SOURCE
#>  D(RELATIVE HUMIDITY)/DZ ABOVE BOUNDARY LAYER (PERCENT/M) => D.RELATIVE.HUMIDITY..DZ.ABOVE.BOUNDARY.LAYER..PERCENT.M. => DRHDZU
#>  DELTAPHI => DELTAPHI => DELTA.WD
#>  DELTAT => DELTAT => DELTA.T
#>  DELTA T => DELTA.T => DELTA.T
#>  DELTATHETA => DELTATHETA => DELTA.THETA
#>  DELTA THETA => DELTA.THETA => DELTA.THETA
#>  DIRN CHANGE => DIRN.CHANGE => DELTA.WD
#>  DRH/DZ => DRH.DZ => DRHDZU
#>  GEOSTROPHIC MINUS SURFACE WIND DIRECTION (DEGREES) => GEOSTROPHIC.MINUS.SURFACE.WIND.DIRECTION..DEGREES. => DELTA.WD
#>  HEAT FLUX => HEAT.FLUX => FTHETA0
#>  INCOMING SOLAR RADIATION => INCOMING.SOLAR.RADIATION => K
#>  LATENT HEAT FLUX => LATENT.HEAT.FLUX => LAMBDAE
#>  LAT HT FLUX => LAT.HT.FLUX => LAMBDAE
#>  MODIFIED PRIESTLEY-TAYLOR PARAMETER (DISPERSION AREA) => MODIFIED.PRIESTLEY.TAYLOR.PARAMETER..DISPERSION.AREA. => ALPHA.DISP
#>  MODIFIED PRIESTLEY-TAYLOR PARAMETER (MET SITE) => MODIFIED.PRIESTLEY.TAYLOR.PARAMETER..MET.SITE. => ALPHA.MET
#>  N ABOVE BL => N.ABOVE.BL => NU
#>  PHI => PHI => WD
#>  PHI0 => PHI0 => WD.0
#>  PHIG => PHIG => WD.G
#>  PHISEC => PHISEC => WD.SEC
#>  PRECIP => PRECIP => P
#>  PRECIPITATION RATE (MM/HOUR) => PRECIPITATION.RATE..MM.HOUR. => P
#>  R => R => ALBEDO.MET
#>  RECIPLMO => RECIPLMO => RECIP.LMO
#>  RELATIVE HUMIDITY ABOVE BOUNDARY LAYER (PERCENT) => RELATIVE.HUMIDITY.ABOVE.BOUNDARY.LAYER..PERCENT. => RHU
#>  RH ABOVE BL => RH.ABOVE.BL => RHU
#>  RHUM => RHUM => RHU
#>  ROUGHNESS LENGTH (DISPERSION AREA) => ROUGHNESS.LENGTH..DISPERSION.AREA. => Z0.DISP
#>  ROUGHNESS LENGTH (MET SITE) => ROUGHNESS.LENGTH..MET.SITE. => Z0.MET
#>  S HUMIDITY => S.HUMIDITY => SHU
#>  SEA SURFACE TEMPERATURE (C) => SEA.SURFACE.TEMPERATURE..C. => TSEA
#>  SEA TEMP => SEA.TEMP => TSEA
#>  SENSIBLE HEAT FLUX => SENSIBLE.HEAT.FLUX => FTHETA0
#>  SIGMATHETA => SIGMATHETA => SIGMA.THETA
#>  SIGMA THETA => SIGMA.THETA => SIGMA.THETA
#>  SIGMA THETA (DEGREES) => SIGMA.THETA..DEGREES. => SIGMA.THETA
#>  SOLAR RAD => SOLAR.RAD => K
#>  SPECIFIC HUMIDITY => SPECIFIC.HUMIDITY => SHU
#>  T0C => T0C => TEMP
#>  TEMPERATURE => TEMPERATURE => TEMP
#>  TEMPERATURE (C) => TEMPERATURE..C. => TEMP
#>  TEMPERATURE JUMP ACROSS BOUNDARY LAYER TOP => TEMPERATURE.JUMP.ACROSS.BOUNDARY.LAYER.TOP => DELTA.THETA
#>  TEMPERATURE OVER LAND MINUS SEA SURFACE TEMPERATURE => TEMPERATURE.OVER.LAND.MINUS.SEA.SURFACE.TEMPERATURE => DELTA.T
#>  Time(s) => Time.s. => Time
#>  U => U => WS
#>  UG => UG => WS.G
#>  UGSTAR => UGSTAR => WS.GSTAR
#>  USTAR => USTAR => WS.STAR
#>  WIND DIRN => WIND.DIRN => WD
#>  WIND DIRECTION (DEGREES) => WIND.DIRECTION..DEGREES. => WD
#>  WIND HEIGHT => WIND.HEIGHT => WIND.HEIGHT
#>  WIND MEASUREMENT HEIGHT => WIND.MEASUREMENT.HEIGHT => WIND.HEIGHT
#>  WIND SPEED => WIND.SPEED => WS
#>  X(m) => X.m. => X
#>  Y(m) => Y.m. => Y
#>  Z(m) => Z.m. => Z
#>  Z0(D) => Z0.D. => Z0.DISP
#>  Z0 (D) => Z0..D. => Z0.DISP
#>  Z0(DISP) => Z0.DISP. => Z0.DISP
#>  Z0 (DISP) => Z0..DISP. => Z0.DISP
#>  Z0(M) => Z0.M. => Z0.MET
#>  Z0 (M) => Z0..M. => Z0.MET
#>  Z0(MET) => Z0.MET. => Z0.MET
#>  Z0 (MET) => Z0..MET. => Z0.MET

# to see what simplify.names does to adms data series name PHI
new.name <- importADMS(simplify.names, names = "PHI")
new.name
#> [1] "WD"
```
