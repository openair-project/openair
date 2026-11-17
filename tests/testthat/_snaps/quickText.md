# pollutant names are formatted

    Code
      show_quicktext(c("no2", "NO2", "no", "nox", "NOx", "NOX", "nh3", "NH3", "co",
        "co2", "CO2", "nmhc", "o3", "O3", "ozone", "so2", "SO2", "h2s", "H2S", "ch4",
        "CH4"))
    Output
      "no2"
        expression: expression(paste("NO"[2]))
        plotmath:   paste("NO"[2])
      "NO2"
        expression: expression(paste("NO"[2]))
        plotmath:   paste("NO"[2])
      "no"
        expression: expression(paste("NO"))
        plotmath:   paste("NO")
      "nox"
        expression: expression(paste("NO"[x]))
        plotmath:   paste("NO"[x])
      "NOx"
        expression: expression(paste("NO"[x]))
        plotmath:   paste("NO"[x])
      "NOX"
        expression: expression(paste("NO"[x]))
        plotmath:   paste("NO"[x])
      "nh3"
        expression: expression(paste("NH"[3]))
        plotmath:   paste("NH"[3])
      "NH3"
        expression: expression(paste("NH"[3]))
        plotmath:   paste("NH"[3])
      "co"
        expression: expression(paste("CO"))
        plotmath:   paste("CO")
      "co2"
        expression: expression(paste("CO"[2]))
        plotmath:   paste("CO"[2])
      "CO2"
        expression: expression(paste("CO"[2]))
        plotmath:   paste("CO"[2])
      "nmhc"
        expression: expression(paste("NMHC"))
        plotmath:   paste("NMHC")
      "o3"
        expression: expression(paste("O"[3]))
        plotmath:   paste("O"[3])
      "O3"
        expression: expression(paste("O"[3]))
        plotmath:   paste("O"[3])
      "ozone"
        expression: expression(paste("O"[3]))
        plotmath:   paste("O"[3])
      "so2"
        expression: expression(paste("SO"[2]))
        plotmath:   paste("SO"[2])
      "SO2"
        expression: expression(paste("SO"[2]))
        plotmath:   paste("SO"[2])
      "h2s"
        expression: expression(paste("H"[2] * "S"))
        plotmath:   paste("H"[2] * "S")
      "H2S"
        expression: expression(paste("H"[2] * "S"))
        plotmath:   paste("H"[2] * "S")
      "ch4"
        expression: expression(paste("CH"[4]))
        plotmath:   paste("CH"[4])
      "CH4"
        expression: expression(paste("CH"[4]))
        plotmath:   paste("CH"[4])

# particulate matter names are formatted

    Code
      show_quicktext(c("pm1", "PM1", "pm4", "PM4", "pm10", "PM10", "pm25", "PM25",
        "pm2.5", "PM2.5", "pmc", "PMc", "pmcoarse", "PMcoarse", "pmf", "PMf",
        "pmfine", "PMfine", "pmtot", "PMtot"))
    Output
      "pm1"
        expression: expression(paste("PM"[1]))
        plotmath:   paste("PM"[1])
      "PM1"
        expression: expression(paste("PM"[1]))
        plotmath:   paste("PM"[1])
      "pm4"
        expression: expression(paste("PM"[4]))
        plotmath:   paste("PM"[4])
      "PM4"
        expression: expression(paste("PM"[4]))
        plotmath:   paste("PM"[4])
      "pm10"
        expression: expression(paste("PM"[10]))
        plotmath:   paste("PM"[10])
      "PM10"
        expression: expression(paste("PM"[10]))
        plotmath:   paste("PM"[10])
      "pm25"
        expression: expression(paste("PM"[2.5]))
        plotmath:   paste("PM"[2.5])
      "PM25"
        expression: expression(paste("PM"[2.5]))
        plotmath:   paste("PM"[2.5])
      "pm2.5"
        expression: expression(paste("PM"[2.5]))
        plotmath:   paste("PM"[2.5])
      "PM2.5"
        expression: expression(paste("PM"[2.5]))
        plotmath:   paste("PM"[2.5])
      "pmc"
        expression: expression(paste("PM"[coarse]))
        plotmath:   paste("PM"[coarse])
      "PMc"
        expression: expression(paste("PM"[coarse]))
        plotmath:   paste("PM"[coarse])
      "pmcoarse"
        expression: expression(paste("PM"[coarse]))
        plotmath:   paste("PM"[coarse])
      "PMcoarse"
        expression: expression(paste("PM"[coarse]))
        plotmath:   paste("PM"[coarse])
      "pmf"
        expression: expression(paste("PM"[fine]))
        plotmath:   paste("PM"[fine])
      "PMf"
        expression: expression(paste("PM"[fine]))
        plotmath:   paste("PM"[fine])
      "pmfine"
        expression: expression(paste("PM"[fine]))
        plotmath:   paste("PM"[fine])
      "PMfine"
        expression: expression(paste("PM"[fine]))
        plotmath:   paste("PM"[fine])
      "pmtot"
        expression: expression(paste("PM"[total]))
        plotmath:   paste("PM"[total])
      "PMtot"
        expression: expression(paste("PM"[total]))
        plotmath:   paste("PM"[total])

# meteorological names are formatted

    Code
      show_quicktext(c("ws", "WS", "wd", "WD", "air_temp", "rh"))
    Output
      "ws"
        expression: expression(paste("wind spd."))
        plotmath:   paste("wind spd.")
      "WS"
        expression: expression(paste("wind spd."))
        plotmath:   paste("wind spd.")
      "wd"
        expression: expression(paste("wind dir."))
        plotmath:   paste("wind dir.")
      "WD"
        expression: expression(paste("wind dir."))
        plotmath:   paste("wind dir.")
      "air_temp"
        expression: expression(paste("temperature"))
        plotmath:   paste("temperature")
      "rh"
        expression: expression(paste("relative humidity"))
        plotmath:   paste("relative humidity")

# units are formatted

    Code
      show_quicktext(c("ug/m3", "ug.m-3", "ug m-3", "ugm-3", "mg/m3", "ng/m3", "g/m3",
        "cm-3", "m/s", "m.s-1", "m s-1", "m/s2", "km2", "g/km", "g/s", "kW/t",
        "g/hour", "g/hr", "g/kg", "km/hr", "km/h", "km/hour", "km/hr/s", "km/h/s",
        "umol/m2", "umol/m2/s"))
    Output
      "ug/m3"
        expression: expression(paste(mu * "g m"^-3))
        plotmath:   paste(mu * "g m"^-3)
      "ug.m-3"
        expression: expression(paste(mu * "g m"^-3))
        plotmath:   paste(mu * "g m"^-3)
      "ug m-3"
        expression: expression(paste(mu * "g m"^-3))
        plotmath:   paste(mu * "g m"^-3)
      "ugm-3"
        expression: expression(paste(mu * "g m"^-3))
        plotmath:   paste(mu * "g m"^-3)
      "mg/m3"
        expression: expression(paste("mg m"^-3))
        plotmath:   paste("mg m"^-3)
      "ng/m3"
        expression: expression(paste("ng m"^-3))
        plotmath:   paste("ng m"^-3)
      "g/m3"
        expression: expression(paste("g m"^-3))
        plotmath:   paste("g m"^-3)
      "cm-3"
        expression: expression(paste("cm"^-3))
        plotmath:   paste("cm"^-3)
      "m/s"
        expression: expression(paste("m s"^-1))
        plotmath:   paste("m s"^-1)
      "m.s-1"
        expression: expression(paste("m s"^-1))
        plotmath:   paste("m s"^-1)
      "m s-1"
        expression: expression(paste("m s"^-1))
        plotmath:   paste("m s"^-1)
      "m/s2"
        expression: expression(paste("m s"^-2))
        plotmath:   paste("m s"^-2)
      "km2"
        expression: expression(paste("km"^2))
        plotmath:   paste("km"^2)
      "g/km"
        expression: expression(paste("g km"^-1))
        plotmath:   paste("g km"^-1)
      "g/s"
        expression: expression(paste("g s"^-1))
        plotmath:   paste("g s"^-1)
      "kW/t"
        expression: expression(paste("kW t"^-1))
        plotmath:   paste("kW t"^-1)
      "g/hour"
        expression: expression(paste("g hour"^-1))
        plotmath:   paste("g hour"^-1)
      "g/hr"
        expression: expression(paste("g hour"^-1))
        plotmath:   paste("g hour"^-1)
      "g/kg"
        expression: expression(paste("g kg"^-1))
        plotmath:   paste("g kg"^-1)
      "km/hr"
        expression: expression(paste("km hr"^-1))
        plotmath:   paste("km hr"^-1)
      "km/h"
        expression: expression(paste("km hr"^-1))
        plotmath:   paste("km hr"^-1)
      "km/hour"
        expression: expression(paste("km hr"^-1))
        plotmath:   paste("km hr"^-1)
      "km/hr/s"
        expression: expression(paste("km hr"^-1 * " s"^-1))
        plotmath:   paste("km hr"^-1 * " s"^-1)
      "km/h/s"
        expression: expression(paste("km hr"^-1 * " s"^-1))
        plotmath:   paste("km hr"^-1 * " s"^-1)
      "umol/m2"
        expression: expression(paste(mu * "mol m"^-2))
        plotmath:   paste(mu * "mol m"^-2)
      "umol/m2/s"
        expression: expression(paste(mu * "mol m"^-2 * " s"^-1))
        plotmath:   paste(mu * "mol m"^-2 * " s"^-1)

# symbols are formatted

    Code
      show_quicktext(c("dgrC", "degreeC", "deg. C", "degreesC", "degrees", "Delta",
        "delta", "r2", "R2", "tau "))
    Output
      "dgrC"
        expression: expression(paste(degree * "C"))
        plotmath:   paste(degree * "C")
      "degreeC"
        expression: expression(paste(degree * "C"))
        plotmath:   paste(degree * "C")
      "deg. C"
        expression: expression(paste(degree * "C"))
        plotmath:   paste(degree * "C")
      "degreesC"
        expression: expression(paste(degree * "C"))
        plotmath:   paste(degree * "C")
      "degrees"
        expression: expression(paste(degree))
        plotmath:   paste(degree)
      "Delta"
        expression: expression(paste(Delta))
        plotmath:   paste(Delta)
      "delta"
        expression: expression(paste(Delta))
        plotmath:   paste(Delta)
      "r2"
        expression: expression(paste("R"^2))
        plotmath:   paste("R"^2)
      "R2"
        expression: expression(paste("R"^2))
        plotmath:   paste("R"^2)
      "tau "
        expression: expression(paste(tau, " "))
        plotmath:   paste(tau, " ")

# combinations and plain text are formatted

    Code
      show_quicktext(c("my no2 label", "pm10 [ ug.m-3 ]", "pm2.5 (ug/m3)", "o3 (ppb)",
        "no2 ug/m3", "temp degrees", "wind m/s", "site A", "it's no2", "no2\nug/m3"))
    Output
      "my no2 label"
        expression: expression(paste("my ", "NO"[2], " label"))
        plotmath:   paste("my ", "NO"[2], " label")
      "pm10 [ ug.m-3 ]"
        expression: expression(paste("PM"[10], " [ ", mu * "g m"^-3, " ]"))
        plotmath:   paste("PM"[10], " [ ", mu * "g m"^-3, " ]")
      "pm2.5 (ug/m3)"
        expression: expression(paste("PM"[2.5], " (", mu * "g m"^-3, ")"))
        plotmath:   paste("PM"[2.5], " (", mu * "g m"^-3, ")")
      "o3 (ppb)"
        expression: expression(paste("O"[3], " (ppb)"))
        plotmath:   paste("O"[3], " (ppb)")
      "no2 ug/m3"
        expression: expression(paste("NO"[2], " ", mu * "g m"^-3))
        plotmath:   paste("NO"[2], " ", mu * "g m"^-3)
      "temp degrees"
        expression: expression(paste("temp ", degree))
        plotmath:   paste("temp ", degree)
      "wind m/s"
        expression: expression(paste("wind ", "m s"^-1))
        plotmath:   paste("wind ", "m s"^-1)
      "site A"
        expression: expression(paste("site A"))
        plotmath:   paste("site A")
      "it's no2"
        expression: expression(paste("it's ", "NO"[2]))
        plotmath:   paste("it's ", "NO"[2])
      "no2\nug/m3"
        expression: expression(displaystyle(atop(displaystyle(paste("NO"[2])), displaystyle(paste(mu *     "g m"^-3)))))
        plotmath:   paste("NO"[2], " ", mu * "g m"^-3)

# names are not formatted inside longer words

    Code
      show_quicktext(c("year2020", "no3", "NO3", "pm100", "Morocco data", "no2conc",
        "no2_roadside", "roadside_no2", "pm10_ugm3", "Mean no2, o3 and co", "no data"))
    Output
      "year2020"
        expression: expression(paste("year2020"))
        plotmath:   paste("year2020")
      "no3"
        expression: expression(paste("NO"[3]))
        plotmath:   paste("NO"[3])
      "NO3"
        expression: expression(paste("NO"[3]))
        plotmath:   paste("NO"[3])
      "pm100"
        expression: expression(paste("pm100"))
        plotmath:   paste("pm100")
      "Morocco data"
        expression: expression(paste("Morocco data"))
        plotmath:   paste("Morocco data")
      "no2conc"
        expression: expression(paste("no2conc"))
        plotmath:   paste("no2conc")
      "no2_roadside"
        expression: expression(paste("NO"[2], "_roadside"))
        plotmath:   paste("NO"[2], "_roadside")
      "roadside_no2"
        expression: expression(paste("roadside_", "NO"[2]))
        plotmath:   paste("roadside_", "NO"[2])
      "pm10_ugm3"
        expression: expression(paste("PM"[10], "_ugm3"))
        plotmath:   paste("PM"[10], "_ugm3")
      "Mean no2, o3 and co"
        expression: expression(paste("Mean ", "NO"[2], ", ", "O"[3], " and ", "CO"))
        plotmath:   paste("Mean ", "NO"[2], ", ", "O"[3], " and ", "CO")
      "no data"
        expression: expression(paste("NO", " data"))
        plotmath:   paste("NO", " data")

# spaces between formatted names and units are kept

    Code
      show_quicktext(c("ug/m3 no2", "Delta no2", "no2 o3", "co2 g/km", "m/s ws",
        "km/h no2", "Delta no2 (ug/m3)"))
    Output
      "ug/m3 no2"
        expression: expression(paste(mu * "g m"^-3, " ", "NO"[2]))
        plotmath:   paste(mu * "g m"^-3, " ", "NO"[2])
      "Delta no2"
        expression: expression(paste(Delta, " ", "NO"[2]))
        plotmath:   paste(Delta, " ", "NO"[2])
      "no2 o3"
        expression: expression(paste("NO"[2], " ", "O"[3]))
        plotmath:   paste("NO"[2], " ", "O"[3])
      "co2 g/km"
        expression: expression(paste("CO"[2], " ", "g km"^-1))
        plotmath:   paste("CO"[2], " ", "g km"^-1)
      "m/s ws"
        expression: expression(paste("m s"^-1, " ", "wind spd."))
        plotmath:   paste("m s"^-1, " ", "wind spd.")
      "km/h no2"
        expression: expression(paste("km hr"^-1, " ", "NO"[2]))
        plotmath:   paste("km hr"^-1, " ", "NO"[2])
      "Delta no2 (ug/m3)"
        expression: expression(paste(Delta, " ", "NO"[2], " (", mu * "g m"^-3, ")"))
        plotmath:   paste(Delta, " ", "NO"[2], " (", mu * "g m"^-3, ")")

# any number of newlines are stacked as lines

    Code
      show_quicktext(c("Line 1\nLine 2", "a\nb\nc",
        "Mean no2\n(ug/m3)\n2020\nroadside", "no2\n", "\nno2"))
    Output
      "Line 1\nLine 2"
        expression: expression(displaystyle(atop(displaystyle(paste("Line 1")), displaystyle(paste("Line 2")))))
        plotmath:   paste("Line 1 Line 2")
      "a\nb\nc"
        expression: expression(displaystyle(atop(displaystyle(paste("a")), displaystyle(atop(displaystyle(paste("b")),     displaystyle(paste("c")))))))
        plotmath:   paste("a b c")
      "Mean no2\n(ug/m3)\n2020\nroadside"
        expression: expression(displaystyle(atop(displaystyle(paste("Mean ", "NO"[2])),     displaystyle(atop(displaystyle(paste("(", mu * "g m"^-3,         ")")), displaystyle(atop(displaystyle(paste("2020")),         displaystyle(paste("roadside")))))))))
        plotmath:   paste("Mean ", "NO"[2], " (", mu * "g m"^-3, ") 2020 roadside")
      "no2\n"
        expression: expression(displaystyle(atop(displaystyle(paste("NO"[2])), displaystyle(paste()))))
        plotmath:   paste("NO"[2], " ")
      "\nno2"
        expression: expression(displaystyle(atop(displaystyle(paste()), displaystyle(paste("NO"[2])))))
        plotmath:   paste(" ", "NO"[2])

# adjacent symbols are formatted

    Code
      show_quicktext(c("Delta Delta", "degrees degrees", "ug/m3 ug/m3", "tau tau"))
    Output
      "Delta Delta"
        expression: expression(paste(Delta, " ", Delta))
        plotmath:   paste(Delta, " ", Delta)
      "degrees degrees"
        expression: expression(paste(degree, " ", degree))
        plotmath:   paste(degree, " ", degree)
      "ug/m3 ug/m3"
        expression: expression(paste(mu * "g m"^-3, " ", mu * "g m"^-3))
        plotmath:   paste(mu * "g m"^-3, " ", mu * "g m"^-3)
      "tau tau"
        expression: expression(paste(tau, " ", tau))
        plotmath:   paste(tau, " ", tau)

# short names that are common words only match on their own

    Code
      show_quicktext(c("co", "co (ppm)", "(co)", "[co]", "co/nox", "co, no2",
        "co: hourly", "co-located", "co.", "co_roadside", "no", "no. 5", "no-data",
        "NO2 and no"))
    Output
      "co"
        expression: expression(paste("CO"))
        plotmath:   paste("CO")
      "co (ppm)"
        expression: expression(paste("CO", " (ppm)"))
        plotmath:   paste("CO", " (ppm)")
      "(co)"
        expression: expression(paste("(", "CO", ")"))
        plotmath:   paste("(", "CO", ")")
      "[co]"
        expression: expression(paste("[", "CO", "]"))
        plotmath:   paste("[", "CO", "]")
      "co/nox"
        expression: expression(paste("CO", "/", "NO"[x]))
        plotmath:   paste("CO", "/", "NO"[x])
      "co, no2"
        expression: expression(paste("CO", ", ", "NO"[2]))
        plotmath:   paste("CO", ", ", "NO"[2])
      "co: hourly"
        expression: expression(paste("CO", ": hourly"))
        plotmath:   paste("CO", ": hourly")
      "co-located"
        expression: expression(paste("co-located"))
        plotmath:   paste("co-located")
      "co."
        expression: expression(paste("co."))
        plotmath:   paste("co.")
      "co_roadside"
        expression: expression(paste("co_roadside"))
        plotmath:   paste("co_roadside")
      "no"
        expression: expression(paste("NO"))
        plotmath:   paste("NO")
      "no. 5"
        expression: expression(paste("no. 5"))
        plotmath:   paste("no. 5")
      "no-data"
        expression: expression(paste("no-data"))
        plotmath:   paste("no-data")
      "NO2 and no"
        expression: expression(paste("NO"[2], " and ", "NO"))
        plotmath:   paste("NO"[2], " and ", "NO")

# Delta can prefix names but not start words

    Code
      show_quicktext(c("Delta", "DeltaNO2", "delta_no2", "Delta2", "Deltaville",
        "deltas"))
    Output
      "Delta"
        expression: expression(paste(Delta))
        plotmath:   paste(Delta)
      "DeltaNO2"
        expression: expression(paste(Delta, "NO2"))
        plotmath:   paste(Delta, "NO2")
      "delta_no2"
        expression: expression(paste(Delta, "_", "NO"[2]))
        plotmath:   paste(Delta, "_", "NO"[2])
      "Delta2"
        expression: expression(paste(Delta, "2"))
        plotmath:   paste(Delta, "2")
      "Deltaville"
        expression: expression(paste("Deltaville"))
        plotmath:   paste("Deltaville")
      "deltas"
        expression: expression(paste("deltas"))
        plotmath:   paste("deltas")

# names are not formatted when followed by a decimal part

    Code
      show_quicktext(c("pm1.5", "pm2.5", "pm1.", "pm10. data"))
    Output
      "pm1.5"
        expression: expression(paste("pm1.5"))
        plotmath:   paste("pm1.5")
      "pm2.5"
        expression: expression(paste("PM"[2.5]))
        plotmath:   paste("PM"[2.5])
      "pm1."
        expression: expression(paste("PM"[1], "."))
        plotmath:   paste("PM"[1], ".")
      "pm10. data"
        expression: expression(paste("PM"[10], ". data"))
        plotmath:   paste("PM"[10], ". data")

# units are not formatted when followed by a letter

    Code
      show_quicktext(c("speed (m/sec)", "km/hours", "g/second", "ug/m3s", "km/s",
        "kg/m3", "m/s_mean", "degreesF"))
    Output
      "speed (m/sec)"
        expression: expression(paste("speed (m/sec)"))
        plotmath:   paste("speed (m/sec)")
      "km/hours"
        expression: expression(paste("km/hours"))
        plotmath:   paste("km/hours")
      "g/second"
        expression: expression(paste("g/second"))
        plotmath:   paste("g/second")
      "ug/m3s"
        expression: expression(paste("ug/m3s"))
        plotmath:   paste("ug/m3s")
      "km/s"
        expression: expression(paste("k", "m s"^-1))
        plotmath:   paste("k", "m s"^-1)
      "kg/m3"
        expression: expression(paste("k", "g m"^-3))
        plotmath:   paste("k", "g m"^-3)
      "m/s_mean"
        expression: expression(paste("m s"^-1, "_mean"))
        plotmath:   paste("m s"^-1, "_mean")
      "degreesF"
        expression: expression(paste(degree, "F"))
        plotmath:   paste(degree, "F")

# unknown formats and format arguments error

    Code
      quickText("no2", format = "nope")
    Condition
      Error in `quickText()`:
      ! `format` must be one of "expression", "plotmath", or "html", not "nope".

# HTML is formatted

    Code
      cat(quickText(c("no2 (ug/m3)", "pm2.5\nroadside", "Mean o3\n(ppb)\n2020",
        "Delta o3", "temp degreesC", "co2 g/km", "km/hr/s", "ws m/s", "r2 = 0.9",
        "tau", "PMcoarse", "h2s", "umol/m2/s", "site A", ""), format = "html"), sep = "\n")
    Output
      NO<sub>2</sub> (µg m<sup>-3</sup>)
      PM<sub>2.5</sub><br>roadside
      Mean O<sub>3</sub><br>(ppb)<br>2020
      Δ O<sub>3</sub>
      temp °C
      CO<sub>2</sub> g km<sup>-1</sup>
      km hr<sup>-1</sup> s<sup>-1</sup>
      wind spd. m s<sup>-1</sup>
      R<sup>2</sup> = 0.9
      τ
      PM<sub>coarse</sub>
      H<sub>2</sub>S
      µmol m<sup>-2</sup> s<sup>-1</sup>
      site A
      

# invalid HTML arguments and expressions error

    Code
      quickText(expression(NO[2]), format = "html")
    Condition
      Error in `quickText()`:
      ! `quickText()` can't convert expressions to the "html" format.

