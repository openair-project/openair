#' CERC Atmospheric Dispersion Modelling System (ADMS) data import function(s)
#' for openair
#'
#' Function(s) to import various ADMS file types into openair. Currently handles
#' ".met", ".bgd", ".mop" and ".pst" file structures. Uses [utils::read.csv()]
#' to read in data, format for R and openair and apply some file structure
#' testing.
#'
#' The `importADMS` function were developed to help import various ADMS
#' file types into openair. In most cases the parent import function should work
#' in default configuration, e.g. `mydata <- importADMS()`. The function
#' currently recognises four file formats: `.bgd`, `.met`, `.mop`
#' and `.pst`.  Where other file extensions have been set but the file
#' structure is known, the import call can be forced by, e.g, `mydata <-
#' importADMS(file.type="bgd")`. Other options can be adjusted to provide fine
#' control of the data structuring and renaming.
#'
#' @aliases importADMS importADMSBgd importADMSMet importADMSMop importADMSPst
#' @param file The ADMS file to be imported. Default, [file.choose()] opens
#'   browser. Use of [utils::read.csv()] also allows this to be a readable
#'   text-mode connection or url (although these options are currently not fully
#'   tested).
#' @param file.type Type of ADMS file to be imported. With default, "unknown",
#'   the import uses the file extension to identify the file type and, where
#'   recognised, uses this to identify the file structure and import method to
#'   be applied. Where file extension is not recognised the choice may be forced
#'   by setting `file.type` to one of the known `file.type` options:
#'   "bgd", "met", "mop" or "pst".
#' @param drop.case Option to convert all data names to lower case. Default,
#'   `TRUE`.  Alternative, `FALSE`, returns data with name cases as
#'   defined in file.
#' @param drop.input.dates Option to remove ADMS "hour", "day", and "year" data
#'   columns after generating openair "date" timeseries. Default, `TRUE`.
#'   Alternative, `FALSE`, returns both "date" and the associated ADMS data
#'   columns as part of openair data frame.
#' @param keep.units Option to retain ADMS data units. Default, `TRUE`,
#'   retains units (if recoverable) as character vector in data frame comment if
#'   defined in `file`.  Alternative, `FALSE`, discards units. (NOTE:
#'   currently, only `.bgd` and `.pst` files assign units. So, this
#'   option is ignored when importing `.met` or `.mop` files.)
#' @param simplify.names Option to simplify data names in accordance with common
#'   `openair` practices. Default, `TRUE`. Alternative, `FALSE`,
#'   returns data with names as interpreted by standard R. (NOTE: Some ADMS file
#'   data names include symbols and structures that R does not allow as part of
#'   a name, so some renaming is automatic regardless of `simplify.names`
#'   setting.  For example, brackets or symbols are removed from names or
#'   replaced with ".", and names in the form "1/x" may be returned as "X1.x" or
#'   "recip.x".)
#' @param test.file.structure Option to test file structure before trying to
#'   import. Default, `TRUE`, tests for expected file structure and halts
#'   import operation if this is not found.  Alternative, `FALSE`, attempts
#'   import regardless of structure.
#' @param drop.delim Option to remove delim columns from the data frame. ADMS
#'   .mop files include two columns, "INPUT_DATA:" and "PROCESSED_DATA:", to
#'   separate model input and output types.  Default, `TRUE`, removes
#'   these. Alternative, `FALSE`, retains them as part of import.  (Note:
#'   Option ignored when importing `.bgd`, `.met` or `.pst`
#'   files.)
#' @param add.prefixes Option to add prefixes to data names. ADMS .mop files
#'   include a number of input and process data types with shared names.
#'   Prefixes can be automatically added to these so individual data can be
#'   readily identified in the R/openair environment. Default, `TRUE`, adds
#'   "process." as a prefix to processed data. Other options include:
#'   `FALSE` which uses no prefixes and leave all name rationalisation to
#'   R, and character vectors which are treated as the required prefixes. If one
#'   vector is sent, this is treated as processed data prefix. If two (or more)
#'   vectors are sent, the first and second are treated as the input and
#'   processed data prefixes, respectively. For example, the argument
#'   (`add.prefixes="out"`) would add the "out" prefix to processed data
#'   names, while the argument (`add.prefixes=c("in","out")`) would add
#'   "in" and "out" prefixes to input and output data names, respectively.
#'   (Note: Option ignored when importing `.bgd`, `.met` or
#'   `.pst` files.)
#' @param names Option applied by `simplifyNamesADMS` when
#'   `simplify.names` is enabled.  All names are simplified for the default
#'   setting, `NULL`.
#' @param all For .MOP files, return all variables or not. If `all = TRUE`
#'   a large number of processed variables are returned.
#' @inheritDotParams utils::read.csv
#' @export
#' @return In standard use [importADMS()] returns a data frame for use in
#'   openair. By comparison to the original file, the resulting data frame is
#'   modified as follows:
#'
#'   Time and date information will combined in a single column "date",
#'   formatted as a conventional timeseries (`as.POSIX*`). If
#'   `drop.input.dates` is enabled data series combined to generated the
#'   new "date" data series will also be removed.
#'
#'   If `simplify.names` is enabled common chemical names may be
#'   simplified, and some other parameters may be reset to openair standards
#'   (e.g. "ws", "wd" and "temp") according to operations defined in
#'   `simplifyNamesADMS`. A summary of simplification operations can be
#'   obtained using, e.g., the call `importADMS(simplify.names)`.
#'
#'   If `drop.case` is enabled all upper case characters in names will be
#'   converted to lower case.
#'
#'   If `keep.units` is enabled data units information may also be retained
#'   as part of the data frame comment if available.
#'
#'   With `.mop` files, input and processed data series names may also been
#'   modified on the basis of `drop.delim` and `add.prefixes` settings
#' @note Times are assumed to be in GMT. Zero wind directions reset to 360 as
#'   part of `.mop` file import.
#' @author Karl Ropkins, David Carslaw and Matthew Williams (CERC).
#' @family import functions
#' @examples
#' ##########
#' # example 1
#' ##########
#' # To be confirmed
#'
#' # all current simplify.names operations
#' importADMS(simplify.names)
#'
#' # to see what simplify.names does to adms data series name PHI
#' new.name <- importADMS(simplify.names, names = "PHI")
#' new.name
importADMS <- function(
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
) {
  # importADMS
  # v0.2 kr
  # parent with four daughters (below)
  # Bgd, Mop and Met import methods and simplifyNamesADMS

  if (substitute(file) == "simplify.names") {
    return(simplifyNamesADMS(names))
  }

  if (file.type == "unknown") {
    file.type <- tolower(substr(file, nchar(file) - 3, nchar(file)))
    if (substr(file.type, 1, 1) == ".") {
      file.type <- substr(file.type, 2, 4)
    } else {
      stop(
        "File extension not recognised\n       [If valid ADMS file, try setting file.type to one of: bgd, mop or met]",
        call. = FALSE
      )
    }
  }

  if (file.type == "bgd") {
    return(importADMSBgd(
      file = file,
      drop.case = drop.case,
      drop.input.dates = drop.input.dates,
      keep.units = keep.units,
      simplify.names = simplify.names,
      test.file.structure = test.file.structure,
      drop.delim = drop.delim,
      add.prefixes = add.prefixes,
      ...
    ))
  }
  if (file.type == "mop") {
    return(importADMSMop(
      file = file,
      drop.case = drop.case,
      drop.input.dates = drop.input.dates,
      keep.units = keep.units,
      simplify.names = simplify.names,
      test.file.structure = test.file.structure,
      drop.delim = drop.delim,
      add.prefixes = add.prefixes,
      all = all,
      ...
    ))
  }
  if (file.type == "met") {
    return(importADMSMet(
      file = file,
      drop.case = drop.case,
      drop.input.dates = drop.input.dates,
      keep.units = keep.units,
      simplify.names = simplify.names,
      test.file.structure = test.file.structure,
      drop.delim = drop.delim,
      add.prefixes = add.prefixes,
      ...
    ))
  }
  if (file.type == "pst") {
    return(importADMSPst(
      file = file,
      drop.case = drop.case,
      drop.input.dates = drop.input.dates,
      keep.units = keep.units,
      simplify.names = simplify.names,
      test.file.structure = test.file.structure,
      drop.delim = drop.delim,
      add.prefixes = add.prefixes,
      ...
    ))
  }

  stop(
    "File extension not recognised\n       [If valid ADMS file, try setting file.type to one of: bgd, mop, met or pst]",
    call. = FALSE
  )
}

###############
## daughter
## importADMSBgd

importADMSBgd <- function(
  file = file.choose(),
  drop.case = TRUE,
  drop.input.dates = TRUE,
  keep.units = TRUE,
  simplify.names = TRUE,
  test.file.structure = TRUE,
  drop.delim = TRUE,
  add.prefixes = TRUE,
  ...
) {
  bgd <- readLines(file, n = -1)
  bgd <- sub("[[:space:]]+$", "", bgd) # strip out tail spaces

  loc.start <- which(tolower(bgd) == "backgroundversion2")
  if (test.file.structure) {
    if (length(loc.start) == 0) {
      stop(
        "File not recognised ADMS.bgd structure\n       [please contact openair if valid]",
        call. = FALSE
      )
    }
  }
  if (length(loc.start) > 1) {
    warning(
      "Multiple possible variable starts, taking last\n       [please contact openair problems encountered]",
      call. = FALSE
    )
    loc.start <- loc.start[length(loc.start)]
  }
  no.var <- suppressWarnings(as.numeric(bgd[loc.start + 1]))[1]
  if (test.file.structure & is.na(no.var)) {
    stop(
      "File not recognised ADMS.bgd structure\n       [please contact openair if valid]",
      call. = FALSE
    )
  }
  variables <- bgd[(loc.start + 2):(loc.start + 1 + no.var)]

  if (simplify.names) {
    variables <- simplifyNamesADMS(variables)
  }

  # drop messy name handling
  variables <- gsub("[.][.]", ".", variables)
  variables <- gsub("^[.]", "", variables)

  if (drop.case) {
    variables <- tolower(variables)
  }

  units.start <- which(substr(bgd, 1, 6) == "UNITS:")
  if (length(units.start) == 0) {
    warning(
      "Data units not extracted from ADMS.bgd\n       [please contact file structure if problems encountered]",
      call. = FALSE
    )
    units <- "units: undefined"
  }
  if (length(units.start) > 1) {
    warning(
      "Multiple possible unit starts, taking last\n       [please contact openair problems encountered]",
      call. = FALSE
    )
    units.start <- units.start[length(units.start)]
  }
  units <- bgd[(units.start + 1):(units.start + no.var)]
  if (length(units) == 0) {
    units <- "units: undefined"
  } else {
    units <- paste("units: ", paste(units, sep = "", collapse = ", "), sep = "")
  }
  data.start <- which(substr(bgd, 1, 5) == "DATA:")
  if (length(data.start) == 0) {
    stop(
      "Data start not not located ADMS.bgd\n       [please contact file structure if problems encountered]",
      call. = FALSE
    )
  }
  if (length(data.start) > 1) {
    warning(
      "Multiple possible data starts, taking last\n       [please contact openair problems encountered]",
      call. = FALSE
    )
    data.start <- data.start[length(data.start)]
  }

  ans <- read.csv(
    file,
    header = FALSE,
    skip = data.start,
    na.strings = c("", "NA", "-999", "-999.0"),
    ...
  )
  ans[] <- lapply(ans, function(x) {
    replace(x, x == -999, NA)
  })
  ########################
  # screening for missing data
  # confirm formats, if they get any with bgd files, etc.
  # might not be necessary

  date <- paste(ans[, 1], ans[, 2], ans[, 3], sep = "-")
  date <- as.POSIXct(strptime(date, format = "%Y-%j-%H"), "GMT")
  ans <- cbind(date = date, ans)
  if (length(variables) != ncol(ans) - 4) {
    warning(
      "Variable data mismatch, taking shortest\n       [please contact if openair problems encountered]",
      call. = FALSE
    )
    variables <- variables[
      1:min(c(length(variables), ncol(ans) - 4), na.rm = TRUE)
    ]
    ans <- ans[, 1:(length(variables) + 4)]
  }

  names(ans) <- c("date", "bgd.year", "bgd.day", "bgd.hour", variables)
  if (drop.input.dates == TRUE) {
    ans <- ans[, c(1, 5:ncol(ans))]
  }
  if (keep.units) {
    comment(ans) <- c(comment(ans), units)
  }

  # error handling for bad days
  ids <- which(is.na(ans$date))
  if (length(ids) > 0) {
    if (length(ids) == nrow(ans)) {
      stop(
        "Invalid date (and time) format requested\n       [compare openair import settings and data structure]",
        call. = FALSE
      )
    }
    ans <- ans[-ids, ]
    reply <- paste(
      "Missing dates detected, removing",
      length(ids),
      "line",
      sep = " "
    )
    if (length(ids) > 1) {
      reply <- paste(reply, "s", sep = "")
    }
    warning(reply, call. = FALSE)
  }
  print(unlist(sapply(ans, class)))
  ans
}

###############
## daughter
## importADMSMet

importADMSMet <- function(
  file = file.choose(),
  drop.case = TRUE,
  drop.input.dates = TRUE,
  keep.units = TRUE,
  simplify.names = TRUE,
  test.file.structure = TRUE,
  drop.delim = TRUE,
  add.prefixes = TRUE,
  ...
) {
  met <- readLines(file, n = -1)
  met <- sub("[[:space:]]+$", "", met) # strip out tail spaces
  loc.start <- which(met == "VARIABLES:")
  if (test.file.structure) {
    if (length(loc.start) == 0) {
      stop(
        "File not recognised ADMS.met structure\n       [please contact openair if valid]",
        call. = FALSE
      )
    }
  }
  if (length(loc.start) > 1) {
    warning(
      "Multiple possible variable starts, taking last\n       [please contact openair problems encountered]",
      call. = FALSE
    )
    loc.start <- loc.start[length(loc.start)]
  }
  variables <- suppressWarnings(as.numeric(met[loc.start + 1]))[1]
  if (test.file.structure & is.na(variables)) {
    stop(
      "File not recognised ADMS.met structure\n       [please contact openair if valid]",
      call. = FALSE
    )
  }
  variables <- met[(loc.start + 2):(loc.start + 1 + variables)]

  data.start <- which(met == "DATA:")
  if (test.file.structure) {
    if (length(data.start) == 0) {
      stop(
        "File not recognised ADMS.met structure\n       [please contact openair if valid]",
        call. = FALSE
      )
    }
  }
  if (length(data.start) > 1) {
    warning(
      "Multiple possible data starts, taking last\n       [please contact openair if problems encountered]",
      call. = FALSE
    )
    data.start <- data.start[length(data.start)]
  }

  met <- read.csv(
    file,
    skip = data.start,
    header = FALSE,
    na.strings = c(
      "-999",
      "-999.0"
    )
  )
  met[] <- lapply(met, function(x) {
    replace(x, x == -999, NA)
  })

  ## met <- met[, sapply(met, function(x) !all(is.na(x)))]

  if (length(variables) != ncol(met)) {
    warning(
      "Variable data mismatch, taking shortest\n       [please contact if openair problems encountered]",
      call. = FALSE
    )
    variables <- variables[1:min(c(length(variables), ncol(met)), na.rm = TRUE)]
    met <- met[, 1:length(variables)]
  }
  names(met) <- make.names(variables, unique = TRUE)

  # multiple year day hour name options
  fun.temp <- function(x, y, z) {
    if (all(!y %in% names(x))) {
      stop(
        paste(
          z,
          " not extracted\n       [please contact openair if valid file]",
          sep = ""
        ),
        call. = FALSE
      )
    }
    ans <- x[, y[y %in% names(x)]]
    if (!is.null(ncol(ans))) {
      ans <- ans[, 1]
    }
    ans
  }
  year <- fun.temp(met, c("YEAR"), "year")
  day <- fun.temp(met, c("DAY", "TDAY"), "day")
  hour <- fun.temp(met, c("HOUR", "THOUR"), "hour")

  met <- cbind(date = paste(year, day, hour, sep = "-"), met)
  met$date <- as.POSIXct(
    strptime(met$date, format = "%Y-%j-%H"),
    "GMT"
  )
  if (drop.input.dates) {
    met <- met[,
      !names(met) %in%
        c("YEAR", "TDAY", "THOUR", "DAY", "HOUR", "MONTH", "DAY.OF.MONTH")
    ]
  }

  if (simplify.names) {
    names(met) <- simplifyNamesADMS(names(met))
  }

  # drop messy name handling
  names(met) <- gsub("[.][.]", ".", names(met))
  names(met) <- gsub("^[.]", "", names(met))

  if (drop.case) {
    names(met) <- tolower(names(met))
  }
  met[] <- lapply(met, function(x) {
    replace(x, x == -999, NA)
  })
  ids <- which(is.na(met$date))
  if (length(ids) > 0) {
    if (length(ids) == nrow(met)) {
      stop(
        "Invalid date (and time) format requested\n       [compare openair import settings and data structure]",
        call. = FALSE
      )
    }
    met <- met[-ids, ]
    reply <- paste(
      "Missing dates detected, removing",
      length(ids),
      "line",
      sep = " "
    )
    if (length(ids) > 1) {
      reply <- paste(reply, "s", sep = "")
    }
    warning(reply, call. = FALSE)
  }
  print(unlist(sapply(met, class)))
  met
}

###############
## daughter
## importADMSMop

importADMSMop <- function(
  file = file.choose(),
  drop.case = TRUE,
  drop.input.dates = TRUE,
  keep.units = TRUE,
  simplify.names = TRUE,
  test.file.structure = TRUE,
  drop.delim = TRUE,
  add.prefixes = TRUE,
  all = FALSE,
  ...
) {
  # problem
  # mismatch in file header line end with lr; data lines end with comma then lr
  #########
  # written a catch for this

  # problem
  # no obvious file structure for testing
  ########
  # provisional tester based on delim names

  # problem
  # r handling of x(y) names and x: names is messy
  ############
  # added tidy to correct for this
  # might need to rationalise names

  # problem
  # file contains lots of same names, input and processed
  ##############
  # added an add.prefixes option to handle this

  # problem
  # no keep.units options
  ##############
  # no option to use

  ######################
  # code

  # read top line/data headers
  check.names <- read.csv(file, header = FALSE, nrow = 1, ...)
  check.names <- make.names(as.vector(apply(check.names, 1, as.character)))
  ## tidy () handling; renaming x(y) as x.y. is messy
  check.names <- ifelse(
    substr(check.names, nchar(check.names), nchar(check.names)) == ".",
    substr(check.names, 1, nchar(check.names) - 1),
    check.names
  )
  ## tidy 1/LMN
  check.names <- gsub("X1.LMO", "RECIP.LMO", check.names)

  x.1 <- which(check.names == "INPUT_DATA")
  x.2 <- which(check.names == "PROCESSED_DATA")

  if (test.file.structure) {
    # check for delim columns
    if (length(x.1) == 0 | length(x.2) == 0) {
      stop(
        "File not recognised ADMS.mop structure\n       [please contact openair if valid]",
        call. = FALSE
      )
    }
  }

  # read in data
  ans <- read.csv(
    file,
    header = FALSE,
    skip = 1,
    na.strings = c("", "NA", "-999", "-999.0"),
    ...
  )
  ans[] <- lapply(ans, function(x) {
    replace(x, x == -999, NA)
  })

  ## check for extra empty column
  if (length(ans[, ncol(ans)][!is.na(ans[, ncol(ans)])]) == 0) {
    ans <- ans[, 1:(ncol(ans) - 1)]
  }
  if (ncol(ans) != length(check.names)) {
    warning(
      "Unexpected name/data mismatch, handled pragmatically\n       [compare openair import settings and data structure]",
      call. = FALSE
    )
  }

  if (simplify.names) {
    check.names <- simplifyNamesADMS(check.names)
  }

  ## restructure names and data according to arguments and put together
  if (is.logical(add.prefixes) == TRUE) {
    if (add.prefixes == TRUE) {
      check.names[(x.2[1] + 1):length(check.names)] <- paste(
        "PROCESS",
        check.names[(x.2[1] + 1):length(check.names)],
        sep = "."
      )
    }
  } else {
    if (length(add.prefixes) == 1) {
      check.names[(x.2[1] + 1):length(check.names)] <- paste(
        add.prefixes[1],
        check.names[(x.2[1] + 1):length(check.names)],
        sep = "."
      )
    } else {
      if (length(add.prefixes) > 1) {
        check.names[(x.1[1] + 4):(x.2[1] - 1)] <- paste(
          add.prefixes[1],
          check.names[(x.1[1] + 4):(x.2[1] - 1)],
          sep = "."
        )
        check.names[(x.2[1] + 1):length(check.names)] <- paste(
          add.prefixes[2],
          check.names[(x.2[1] + 1):length(check.names)],
          sep = "."
        )
      } else {
        warning(
          "Unexpected add.prefixes option, option treated as FALSE\n       [check openair import settings]",
          call. = FALSE
        )
      }
    }
  }

  names(ans) <- make.names(check.names, unique = TRUE)

  ## reset wd 0 to 360
  ## get current PHI terminology
  temp <- if (simplify.names) simplifyNamesADMS("PHI") else "PHI"
  temp <- if (length(add.prefixes) > 1) {
    paste(add.prefixes[1], temp, sep = ".")
  } else {
    temp
  }
  if (temp %in% names(ans)) {
    ans[, temp][ans[, temp] == 0] <- 360
  }

  # drop messy name handling
  names(ans) <- gsub("[.][.]", ".", names(ans))
  names(ans) <- gsub("^[.]", "", names(ans))

  date <- paste(ans$TYEAR, ans$TDAY, ans$THOUR, sep = "-")
  date <- as.POSIXct(strptime(date, format = "%Y-%j-%H"), "GMT")
  if (drop.input.dates == TRUE) {
    ans <- ans[, !names(ans) %in% c("TYEAR", "TDAY", "THOUR")]
  }

  if (drop.delim == TRUE) {
    ans <- ans[, !names(ans) %in% c("PROCESSED_DATA", "INPUT_DATA")]
  }
  ans <- cbind(date = date, ans)

  if (drop.case == TRUE) {
    names(ans) <- tolower(names(ans))
  }

  # error handling for bad days
  ids <- which(is.na(ans$date))
  if (length(ids) > 0) {
    if (length(ids) == nrow(ans)) {
      stop(
        "Invalid date (and time) format requested\n       [compare openair import settings and data structure]",
        call. = FALSE
      )
    }
    ans <- ans[-ids, ]
    reply <- paste(
      "Missing dates detected, removing",
      length(ids),
      "line",
      sep = " "
    )
    if (length(ids) > 1) {
      reply <- paste(reply, "s", sep = "")
    }
    warning(reply, call. = FALSE)
  }

  # add stability

  ans <- ans %>%
    mutate(
      H_LMO = process.recip.lmo * process.h,
      stability = case_when(
        H_LMO > 2 ~ "Stable",
        H_LMO < -0.6 ~ "Unstable",
        is.na(H_LMO) ~ NA,
        .default = "Neutral"
      ),
      stability = factor(stability, levels = c("Stable", "Neutral", "Unstable"))
    ) %>%
    rename(
      air_temp = temp,
      recip_lmo = process.recip.lmo,
      H = process.h
    )

  if (all) {
    return(ans)
  } else {
    # select variables only
    ans <- ans %>%
      select(any_of(c(
        "date",
        "ws",
        "wd",
        "air_temp",
        "rhu",
        "cl",
        "H",
        "recip_lmo",
        "H_LMO",
        "stability"
      )))
  }

  return(tibble(ans))
}


###############
# daughter
# importADMSPst
# kr v0.2
# 08 nov 2010

importADMSPst <- function(
  file = file.choose(),
  drop.case = TRUE,
  drop.input.dates = TRUE,
  keep.units = TRUE,
  simplify.names = TRUE,
  test.file.structure = TRUE,
  drop.delim = TRUE,
  add.prefixes = TRUE,
  ...
) {
  # notes
  #########
  # used the header/data dimension mismatcher handler from importADMSMop
  # maybe not be needed.
  #########
  # no obvious file structure for testing
  # provisional tester checks Hour, Day, Year and Receptor.name  in file names
  #########
  # units are recovered from names row
  #########
  # drops Time(s) if empty

  # problems
  #########
  # my name simplifications on Conc terms may need work
  # due to compiler can't catch mu.g/m3 units
  # talk to Matt/David re mug, ug and mg

  ######################
  # code

  # read top line/data headers
  check.names <- read.csv(file, header = FALSE, nrow = 1, ...)
  check.names <- as.vector(apply(check.names, 1, as.character))
  check.names <- sub("[[:space:]]+$", "", check.names) # strip out tail spaces
  check.names <- sub("^[[:space:]]{1,}", "", check.names) # strip leading space (safer?)
  check.names <- make.names(check.names) # after removing front spaces or X.m. conflict

  # test structure
  if (test.file.structure) {
    temp <- c("Hour", "Day", "Year", "Receptor.name")
    test <- temp[temp %in% check.names]
    if (!identical(temp, test)) {
      stop(
        "File not recognised ADMS.pst structure\n       [please contact openair if valid]",
        call. = FALSE
      )
    }
  }

  # read in data
  ans <- read.csv(
    file,
    header = FALSE,
    skip = 1,
    na.strings = c("", "NA", "-999", "-999.0"),
    ...
  )
  ans[] <- lapply(ans, function(x) {
    replace(x, x == -999, NA)
  })

  # match up data and names
  if (ncol(ans) != length(check.names)) {
    warning(
      "Unexpected name/data mismatch, handled pragmatically\n       [compare openair import settings and data structure]",
      call. = FALSE
    )
  }

  names(ans) <- make.names(check.names, unique = TRUE)

  # setup date/time
  date <- paste(ans$Year, ans$Day, ans$Hour, sep = "-")
  date <- as.POSIXct(strptime(date, format = "%Y-%j-%H"), "GMT")

  if (drop.input.dates == TRUE) {
    ans <- ans[, !names(ans) %in% c("Year", "Day", "Hour")]
  }

  # drop Time.s. if empty
  if (all(is.na(ans$Time.s.))) {
    ans <- ans[, !names(ans) %in% c("Time.s.")]
  }

  # recover units from names
  units <- rep(NA, ncol(ans))
  units[grep("[.]s[.]", names(ans))] <- "s"
  units[grep("^.[.]m", names(ans))] <- "m"
  units[grep("[.]ug.m.", names(ans))] <- "ug/m3" # both 3 and superscript 3
  units[grep("[.]ppb", names(ans))] <- "ppb"
  units[grep("[.]ppm", names(ans))] <- "ppm"
  if (length(na.omit(units)) == 0) {
    units <- "units: unknown"
  } else {
    units <- paste("units: ", paste(units, sep = "", collapse = ", "), sep = "")
  }

  if (simplify.names) {
    names(ans) <- simplifyNamesADMS(names(ans))
  }

  ans <- cbind(date = date, ans)

  if (drop.case == TRUE) {
    names(ans) <- tolower(names(ans))
  }

  comment(ans) <- c(comment(ans), units)

  # error handling for bad days
  ids <- which(is.na(ans$date))
  if (length(ids) > 0) {
    if (length(ids) == nrow(ans)) {
      stop(
        "Invalid date (and time) format requested\n       [compare openair import settings and data structure]",
        call. = FALSE
      )
    }
    ans <- ans[-ids, ]
    reply <- paste(
      "Missing dates detected, removing",
      length(ids),
      "line",
      sep = " "
    )
    if (length(ids) > 1) {
      reply <- paste(reply, "s", sep = "")
    }
    warning(reply, call. = FALSE)
  }

  print(unlist(sapply(ans, class)))
  names(ans) <- gsub("..", replacement = "", names(ans), fixed = TRUE)
  ans
}


###############
## daughter
## simplifyNamesADMS
# kr v0.5
# 08 nov 2010

# notes
################
# two handlers: fun.temp and fun.temp.2
################
# fun.temp(x,y,z)
# replaces full term y with full term z in x
################
# fun.temp.2(x,y,z, y.names)
# replaces partial term y with partial term 2 in x
# if y.names = TRUE makes y r-friend first
# [make.names(y)...]
#

simplifyNamesADMS <- function(names = NULL) {
  # simplify.names lookup table for import.adms functions
  # v0.2 kr
  # handles as inputs (don't use after drop.case option)
  # names=NULL returns simplification operation summary

  if (is.null(names)) {
    message("Simplification operation summary")
    message("[ADMS => R => OPENAIR]:")
    fun.temp <- function(x, y, z) {
      temp <- c(y, make.names(y), z)
      message(paste("\t", paste(temp, collapse = " => "), sep = ""))
      temp <- data.frame(
        cbind(
          adms.input = temp[1],
          r.handling = temp[2],
          simplify.names = temp[3]
        ),
        stringsAsFactors = FALSE
      )
      x <- rbind(x, temp)
      x
    }
    fun.temp.2 <- function(x, y, z, y.name) x
  } else {
    names <- make.names(names)
    fun.temp <- function(x, y, z) {
      x[which(x == make.names(y))] <- z
      x
    }
    fun.temp.2 <- function(x, y, z, y.name) {
      x <- if (y.name) gsub(make.names(y), z, x) else (gsub(y, z, x))
    }
  }

  ############
  # update list
  ############
  # 1/LMO
  names <- fun.temp(names, "1/LMO", "RECIP.LMO")
  # 1/MONIN-OBUKHOV LENGTH
  names <- fun.temp(names, "1/MONIN-OBUKHOV LENGTH", "RECIP.LMO")
  # ALBEDO(D)
  names <- fun.temp(names, "ALBEDO(D)", "ALBEDO.DISP")
  names <- fun.temp(names, "ALBEDO (D)", "ALBEDO.DISP")
  # ALBEDO(DISP)
  names <- fun.temp(names, "ALBEDO(DISP)", "ALBEDO.DISP")
  names <- fun.temp(names, "ALBEDO (DISP)", "ALBEDO.DISP")
  # ALBEDO (DISPERSION AREA)
  names <- fun.temp(names, "ALBEDO (DISPERSION AREA)", "ALBEDO.DISP")
  # ALBEDO(M)
  names <- fun.temp(names, "ALBEDO(M)", "ALBEDO.MET")
  names <- fun.temp(names, "ALBEDO (M)", "ALBEDO.MET")
  # ALBEDO(MET)
  names <- fun.temp(names, "ALBEDO(MET)", "ALBEDO.MET")
  names <- fun.temp(names, "ALBEDO (MET)", "ALBEDO.MET")
  # ALBEDO (MET SITE)
  names <- fun.temp(names, "ALBEDO (MET SITE)", "ALBEDO.MET")
  # ALPHA
  ##########
  ## conflict
  ##########
  ## both alpha.disp and alpha.met seem to have been abbrev. to alpha
  # ALPHA(D)
  names <- fun.temp(names, "ALPHA(D)", "ALPHA.DISP")
  names <- fun.temp(names, "ALPHA (D)", "ALPHA.DISP")
  # ALPHA(DISP)
  names <- fun.temp(names, "ALPHA(DISP)", "ALPHA.DISP")
  names <- fun.temp(names, "ALPHA (DISP)", "ALPHA.DISP")
  # ALPHA(M)
  names <- fun.temp(names, "ALPHA(M)", "ALPHA.MET")
  names <- fun.temp(names, "ALPHA (M)", "ALPHA.MET")
  # ALPHA(MET)
  names <- fun.temp(names, "ALPHA(MET)", "ALPHA.MET")
  names <- fun.temp(names, "ALPHA (MET)", "ALPHA.MET")
  # BL DEPTH
  names <- fun.temp(names, "BL DEPTH", "H")
  # BOUNDARY LAYER DEPTH
  names <- fun.temp(names, "BOUNDARY LAYER DEPTH", "H")
  # BUOYANCY FREQUENCY ABOVE BOUNDARY LAYER
  names <- fun.temp(names, "BUOYANCY FREQUENCY ABOVE BOUNDARY LAYER", "NU")
  # BUTADIENE
  # CL
  # CLOUD
  names <- fun.temp(names, "CLOUD", "CL")
  # CLOUD AMOUNT (OKTAS)
  names <- fun.temp(names, "CLOUD AMOUNT (OKTAS)", "CL")

  # Conc|ppb|NAME|All sources|-| 1hr
  names <- fun.temp(
    names,
    "Conc|ppb|NAME|SOURCES|-| RESOLUTION",
    "NAME.SOURCES.RESOLUTION"
  )
  # Conc|ppm|NAME|All sources|-| 1hr
  names <- fun.temp(
    names,
    "Conc|ppm|NAME|SOURCES|-| RESOLUTION",
    "NAME.SOURCES.RESOLUTION"
  )
  # Conc|ug/m3|NAME|All sources|-| 1hr
  names <- fun.temp(
    names,
    "Conc|ug/m3|NAME|SOURCES|-| RESOLUTION",
    "NAME.SOURCES.RESOLUTION"
  )
  # NAME.All.sources.1hr
  names <- fun.temp(names, "NAME.All.sources.1hr", "NAME")
  # NAME.All.sources.RESOLUTION
  names <- fun.temp(names, "NAME.All.sources.RESOLUTION", "NAME.RESOLUTION")
  # NAME.SOURCE.1hr
  names <- fun.temp(names, "NAME.SOURCE.1hr", "NAME.SOURCE")

  # general for above
  names <- fun.temp.2(names, "Conc|ppb|", "", TRUE)
  names <- fun.temp.2(names, "Conc|ppm|", "", TRUE)
  names <- fun.temp.2(names, "Conc|.g/m.|", "", TRUE)
  # above covers
  ## u, m and mu for 1st and
  ## 3 and superscript3 for second
  names <- fun.temp.2(names, "[.][.][.][.]", ".", FALSE)
  names <- fun.temp.2(names, "[.][.][.]", ".", FALSE)
  names <- fun.temp.2(names, "[.]All[.]sources", "", FALSE)
  names <- fun.temp.2(names, "[.]1hr", "", FALSE)

  # D(RELATIVE HUMIDITY)/DZ ABOVE BOUNDARY LAYER (PERCENT/M)
  names <- fun.temp(
    names,
    "D(RELATIVE HUMIDITY)/DZ ABOVE BOUNDARY LAYER (PERCENT/M)",
    "DRHDZU"
  )
  # DAY
  # DELTAPHI
  names <- fun.temp(names, "DELTAPHI", "DELTA.WD")
  # DELTAT
  names <- fun.temp(names, "DELTAT", "DELTA.T")
  names <- fun.temp(names, "DELTA T", "DELTA.T")
  # DELTATHETA
  names <- fun.temp(names, "DELTATHETA", "DELTA.THETA")
  names <- fun.temp(names, "DELTA THETA", "DELTA.THETA")
  # DIRN CHANGE
  names <- fun.temp(names, "DIRN CHANGE", "DELTA.WD")
  # DRH/DZ
  names <- fun.temp(names, "DRH/DZ", "DRHDZU")
  # DRHDZU
  # FR
  # FREQUENCY
  # FTHETA0
  # GEOSTROPHIC MINUS SURFACE WIND DIRECTION (DEGREES)
  names <- fun.temp(
    names,
    "GEOSTROPHIC MINUS SURFACE WIND DIRECTION (DEGREES)",
    "DELTA.WD"
  )
  # H
  # HEAT FLUX
  names <- fun.temp(names, "HEAT FLUX", "FTHETA0")
  # HOUR
  # HOURS
  # INCOMING SOLAR RADIATION
  names <- fun.temp(names, "INCOMING SOLAR RADIATION", "K")
  # INPUT_DATA:
  # K
  # LAMBDAE
  # LATENT HEAT FLUX
  names <- fun.temp(names, "LATENT HEAT FLUX", "LAMBDAE")
  # LAT HT FLUX
  names <- fun.temp(names, "LAT HT FLUX", "LAMBDAE")
  # MODIFIED PRIESTLEY-TAYLOR PARAMETER (DISPERSION AREA)
  names <- fun.temp(
    names,
    "MODIFIED PRIESTLEY-TAYLOR PARAMETER (DISPERSION AREA)",
    "ALPHA.DISP"
  )
  # MODIFIED PRIESTLEY-TAYLOR PARAMETER (MET SITE)
  names <- fun.temp(
    names,
    "MODIFIED PRIESTLEY-TAYLOR PARAMETER (MET SITE)",
    "ALPHA.MET"
  )
  # MONTHS
  # N ABOVE BL
  names <- fun.temp(names, "N ABOVE BL", "NU")
  # NO2
  # NOx
  # NU
  # P
  # PM10
  # PM2.5
  # O3
  # Q0
  # PHI
  names <- fun.temp(names, "PHI", "WD")
  # PHI0
  names <- fun.temp(names, "PHI0", "WD.0")
  # PHIG
  names <- fun.temp(names, "PHIG", "WD.G")
  # PHISEC
  names <- fun.temp(names, "PHISEC", "WD.SEC")
  # PRECIP
  names <- fun.temp(names, "PRECIP", "P")
  # PRECIPITATION RATE (MM/HOUR)
  names <- fun.temp(names, "PRECIPITATION RATE (MM/HOUR)", "P")
  # PROCESSED_DATA:
  # R
  names <- fun.temp(names, "R", "ALBEDO.MET")
  # RECIPLMO
  names <- fun.temp(names, "RECIPLMO", "RECIP.LMO")
  # RELATIVE HUMIDITY ABOVE BOUNDARY LAYER (PERCENT)
  names <- fun.temp(
    names,
    "RELATIVE HUMIDITY ABOVE BOUNDARY LAYER (PERCENT)",
    "RHU"
  )
  # RH ABOVE BL
  names <- fun.temp(names, "RH ABOVE BL", "RHU")
  # RHU
  # RHUM
  names <- fun.temp(names, "RHUM", "RHU")
  # ROUGHNESS LENGTH (DISPERSION AREA)
  names <- fun.temp(names, "ROUGHNESS LENGTH (DISPERSION AREA)", "Z0.DISP")
  # ROUGHNESS LENGTH (MET SITE)
  names <- fun.temp(names, "ROUGHNESS LENGTH (MET SITE)", "Z0.MET")
  # RUN
  # S HUMIDITY
  names <- fun.temp(names, "S HUMIDITY", "SHU")
  # SEA SURFACE TEMPERATURE (C)
  names <- fun.temp(names, "SEA SURFACE TEMPERATURE (C)", "TSEA")
  # SEA TEMP
  names <- fun.temp(names, "SEA TEMP", "TSEA")
  # SENSIBLE HEAT FLUX
  names <- fun.temp(names, "SENSIBLE HEAT FLUX", "FTHETA0")
  # SIGMATHETA
  names <- fun.temp(names, "SIGMATHETA", "SIGMA.THETA")
  names <- fun.temp(names, "SIGMA THETA", "SIGMA.THETA")
  # SIGMA THETA (DEGREES)
  names <- fun.temp(names, "SIGMA THETA (DEGREES)", "SIGMA.THETA")
  # SO2
  # SOLAR RAD
  names <- fun.temp(names, "SOLAR RAD", "K")
  # SPECIFIC HUMIDITY
  names <- fun.temp(names, "SPECIFIC HUMIDITY", "SHU")
  # T0C
  names <- fun.temp(names, "T0C", "TEMP")
  # TDAY
  # TEMPERATURE
  names <- fun.temp(names, "TEMPERATURE", "TEMP")
  # TEMPERATURE (C)
  names <- fun.temp(names, "TEMPERATURE (C)", "TEMP")
  # TEMPERATURE JUMP ACROSS BOUNDARY LAYER TOP
  names <- fun.temp(
    names,
    "TEMPERATURE JUMP ACROSS BOUNDARY LAYER TOP",
    "DELTA.THETA"
  )
  # THOUR
  # TEMPERATURE OVER LAND MINUS SEA SURFACE TEMPERATURE
  names <- fun.temp(
    names,
    "TEMPERATURE OVER LAND MINUS SEA SURFACE TEMPERATURE",
    "DELTA.T"
  )
  # Time(s)
  # X(m)
  names <- fun.temp(names, "Time(s)", "Time")
  # TSEA
  # TYEAR
  # U
  names <- fun.temp(names, "U", "WS")
  # UG
  names <- fun.temp(names, "UG", "WS.G")
  # UGSTAR
  names <- fun.temp(names, "UGSTAR", "WS.GSTAR")
  # USTAR
  names <- fun.temp(names, "USTAR", "WS.STAR")
  # WIND DIRN
  names <- fun.temp(names, "WIND DIRN", "WD")
  # WIND DIRECTION (DEGREES)
  names <- fun.temp(names, "WIND DIRECTION (DEGREES)", "WD")
  # WIND HEIGHT
  names <- fun.temp(names, "WIND HEIGHT", "WIND.HEIGHT")
  # WIND MEASUREMENT HEIGHT
  names <- fun.temp(names, "WIND MEASUREMENT HEIGHT", "WIND.HEIGHT")
  # WIND SPEED
  names <- fun.temp(names, "WIND SPEED", "WS")
  # WSTAR
  # X(m)
  names <- fun.temp(names, "X(m)", "X")
  # Y(m)
  names <- fun.temp(names, "Y(m)", "Y")
  # YEAR
  # Z(m)
  names <- fun.temp(names, "Z(m)", "Z")
  # Z0(D)
  names <- fun.temp(names, "Z0(D)", "Z0.DISP")
  names <- fun.temp(names, "Z0 (D)", "Z0.DISP")
  # Z0(DISP)
  names <- fun.temp(names, "Z0(DISP)", "Z0.DISP")
  names <- fun.temp(names, "Z0 (DISP)", "Z0.DISP")
  # Z0(M)
  names <- fun.temp(names, "Z0(M)", "Z0.MET")
  names <- fun.temp(names, "Z0 (M)", "Z0.MET")
  # Z0(MET)
  names <- fun.temp(names, "Z0(MET)", "Z0.MET")
  names <- fun.temp(names, "Z0 (MET)", "Z0.MET")

  ########
  # outputs
  ########
  invisible(names)
}
