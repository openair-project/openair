# Find the dominant time interval in a vector of dates/times

Inspect a vector of Date or POSIXt objects and return a human-readable
description of the most common difference between consecutive unique,
sorted timestamps. Small floating-point noise is rounded and common
intervals (1 sec, 1 min, 1 hour, 1 day, 1 month, 1 year) are detected.
For uncommon intervals the function returns the interval in seconds
(e.g. "15 sec").

## Usage

``` r
find.time.interval(dates, return.seconds = FALSE)
```

## Arguments

- dates:

  A vector of Date or POSIXt timestamps.

## Value

A character string describing the detected interval.
