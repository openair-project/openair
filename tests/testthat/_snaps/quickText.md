# pollutant names are formatted

    Code
      show_quicktext(c("no2", "NO2", "no", "nox", "NOx", "NOX", "nh3", "NH3", "co",
        "co2", "CO2", "nmhc", "o3", "O3", "ozone", "so2", "SO2", "h2s", "H2S", "ch4",
        "CH4"))
    Output
      "no2"
        parse = TRUE:  expression(paste("NO"[2]))
        parse = FALSE: paste("NO"[2])
      "NO2"
        parse = TRUE:  expression(paste("NO"[2]))
        parse = FALSE: paste("NO"[2])
      "no"
        parse = TRUE:  expression(paste("NO"))
        parse = FALSE: paste("NO")
      "nox"
        parse = TRUE:  expression(paste("NO"[x]))
        parse = FALSE: paste("NO"[x])
      "NOx"
        parse = TRUE:  expression(paste("NO"[x]))
        parse = FALSE: paste("NO"[x])
      "NOX"
        parse = TRUE:  expression(paste("NO"[x]))
        parse = FALSE: paste("NO"[x])
      "nh3"
        parse = TRUE:  expression(paste("NH"[3]))
        parse = FALSE: paste("NH"[3])
      "NH3"
        parse = TRUE:  expression(paste("NH"[3]))
        parse = FALSE: paste("NH"[3])
      "co"
        parse = TRUE:  expression(paste("CO"))
        parse = FALSE: paste("CO")
      "co2"
        parse = TRUE:  expression(paste("CO"[2]))
        parse = FALSE: paste("CO"[2])
      "CO2"
        parse = TRUE:  expression(paste("CO"[2]))
        parse = FALSE: paste("CO"[2])
      "nmhc"
        parse = TRUE:  expression(paste("NMHC"))
        parse = FALSE: paste("NMHC")
      "o3"
        parse = TRUE:  expression(paste("O"[3]))
        parse = FALSE: paste("O"[3])
      "O3"
        parse = TRUE:  expression(paste("O"[3]))
        parse = FALSE: paste("O"[3])
      "ozone"
        parse = TRUE:  expression(paste("O"[3]))
        parse = FALSE: paste("O"[3])
      "so2"
        parse = TRUE:  expression(paste("SO"[2]))
        parse = FALSE: paste("SO"[2])
      "SO2"
        parse = TRUE:  expression(paste("SO"[2]))
        parse = FALSE: paste("SO"[2])
      "h2s"
        parse = TRUE:  expression(paste("H"[2] * "S"))
        parse = FALSE: paste("H"[2] * "S")
      "H2S"
        parse = TRUE:  expression(paste("H"[2] * "S"))
        parse = FALSE: paste("H"[2] * "S")
      "ch4"
        parse = TRUE:  expression(paste("CH"[4]))
        parse = FALSE: paste("CH"[4])
      "CH4"
        parse = TRUE:  expression(paste("CH"[4]))
        parse = FALSE: paste("CH"[4])

# particulate matter names are formatted

    Code
      show_quicktext(c("pm1", "PM1", "pm4", "PM4", "pm10", "PM10", "pm25", "PM25",
        "pm2.5", "PM2.5", "pmc", "PMc", "pmcoarse", "PMcoarse", "pmf", "PMf",
        "pmfine", "PMfine", "pmtot", "PMtot"))
    Output
      "pm1"
        parse = TRUE:  expression(paste("PM"[1]))
        parse = FALSE: paste("PM"[1])
      "PM1"
        parse = TRUE:  expression(paste("PM"[1]))
        parse = FALSE: paste("PM"[1])
      "pm4"
        parse = TRUE:  expression(paste("PM"[4]))
        parse = FALSE: paste("PM"[4])
      "PM4"
        parse = TRUE:  expression(paste("PM"[4]))
        parse = FALSE: paste("PM"[4])
      "pm10"
        parse = TRUE:  expression(paste("PM"[10]))
        parse = FALSE: paste("PM"[10])
      "PM10"
        parse = TRUE:  expression(paste("PM"[10]))
        parse = FALSE: paste("PM"[10])
      "pm25"
        parse = TRUE:  expression(paste("PM"[2.5]))
        parse = FALSE: paste("PM"[2.5])
      "PM25"
        parse = TRUE:  expression(paste("PM"[2.5]))
        parse = FALSE: paste("PM"[2.5])
      "pm2.5"
        parse = TRUE:  expression(paste("PM"[2.5]))
        parse = FALSE: paste("PM"[2.5])
      "PM2.5"
        parse = TRUE:  expression(paste("PM"[2.5]))
        parse = FALSE: paste("PM"[2.5])
      "pmc"
        parse = TRUE:  expression(paste("PM"[coarse]))
        parse = FALSE: paste("PM"[coarse])
      "PMc"
        parse = TRUE:  expression(paste("PM"[coarse]))
        parse = FALSE: paste("PM"[coarse])
      "pmcoarse"
        parse = TRUE:  expression(paste("PM"[coarse]))
        parse = FALSE: paste("PM"[coarse])
      "PMcoarse"
        parse = TRUE:  expression(paste("PM"[coarse]))
        parse = FALSE: paste("PM"[coarse])
      "pmf"
        parse = TRUE:  expression(paste("PM"[fine]))
        parse = FALSE: paste("PM"[fine])
      "PMf"
        parse = TRUE:  expression(paste("PM"[fine]))
        parse = FALSE: paste("PM"[fine])
      "pmfine"
        parse = TRUE:  expression(paste("PM"[fine]))
        parse = FALSE: paste("PM"[fine])
      "PMfine"
        parse = TRUE:  expression(paste("PM"[fine]))
        parse = FALSE: paste("PM"[fine])
      "pmtot"
        parse = TRUE:  expression(paste("PM"[total]))
        parse = FALSE: paste("PM"[total])
      "PMtot"
        parse = TRUE:  expression(paste("PM"[total]))
        parse = FALSE: paste("PM"[total])

# meteorological names are formatted

    Code
      show_quicktext(c("ws", "WS", "wd", "WD", "air_temp", "rh"))
    Output
      "ws"
        parse = TRUE:  expression(paste("wind spd."))
        parse = FALSE: paste("wind spd.")
      "WS"
        parse = TRUE:  expression(paste("wind spd."))
        parse = FALSE: paste("wind spd.")
      "wd"
        parse = TRUE:  expression(paste("wind dir."))
        parse = FALSE: paste("wind dir.")
      "WD"
        parse = TRUE:  expression(paste("wind dir."))
        parse = FALSE: paste("wind dir.")
      "air_temp"
        parse = TRUE:  expression(paste("temperature"))
        parse = FALSE: paste("temperature")
      "rh"
        parse = TRUE:  expression(paste("relative humidity"))
        parse = FALSE: paste("relative humidity")

# units are formatted

    Code
      show_quicktext(c("ug/m3", "ug.m-3", "ug m-3", "ugm-3", "mg/m3", "ng/m3", "g/m3",
        "cm-3", "m/s", "m.s-1", "m s-1", "m/s2", "km2", "g/km", "g/s", "kW/t",
        "g/hour", "g/hr", "g/kg", "km/hr", "km/h", "km/hour", "km/hr/s", "km/h/s",
        "umol/m2", "umol/m2/s"))
    Output
      "ug/m3"
        parse = TRUE:  expression(paste(mu * "g m"^-3))
        parse = FALSE: paste(mu * "g m"^-3)
      "ug.m-3"
        parse = TRUE:  expression(paste(mu * "g m"^-3))
        parse = FALSE: paste(mu * "g m"^-3)
      "ug m-3"
        parse = TRUE:  expression(paste(mu * "g m"^-3))
        parse = FALSE: paste(mu * "g m"^-3)
      "ugm-3"
        parse = TRUE:  expression(paste(mu * "g m"^-3))
        parse = FALSE: paste(mu * "g m"^-3)
      "mg/m3"
        parse = TRUE:  expression(paste("mg m"^-3))
        parse = FALSE: paste("mg m"^-3)
      "ng/m3"
        parse = TRUE:  expression(paste("ng m"^-3))
        parse = FALSE: paste("ng m"^-3)
      "g/m3"
        parse = TRUE:  expression(paste("g m"^-3))
        parse = FALSE: paste("g m"^-3)
      "cm-3"
        parse = TRUE:  expression(paste("cm"^-3))
        parse = FALSE: paste("cm"^-3)
      "m/s"
        parse = TRUE:  expression(paste("m s"^-1))
        parse = FALSE: paste("m s"^-1)
      "m.s-1"
        parse = TRUE:  expression(paste("m s"^-1))
        parse = FALSE: paste("m s"^-1)
      "m s-1"
        parse = TRUE:  expression(paste("m s"^-1))
        parse = FALSE: paste("m s"^-1)
      "m/s2"
        parse = TRUE:  expression(paste("m s"^-2))
        parse = FALSE: paste("m s"^-2)
      "km2"
        parse = TRUE:  expression(paste("km"^2))
        parse = FALSE: paste("km"^2)
      "g/km"
        parse = TRUE:  expression(paste("g km"^-1))
        parse = FALSE: paste("g km"^-1)
      "g/s"
        parse = TRUE:  expression(paste("g s"^-1))
        parse = FALSE: paste("g s"^-1)
      "kW/t"
        parse = TRUE:  expression(paste("kW t"^-1))
        parse = FALSE: paste("kW t"^-1)
      "g/hour"
        parse = TRUE:  expression(paste("g hour"^-1))
        parse = FALSE: paste("g hour"^-1)
      "g/hr"
        parse = TRUE:  expression(paste("g hour"^-1))
        parse = FALSE: paste("g hour"^-1)
      "g/kg"
        parse = TRUE:  expression(paste("g kg"^-1))
        parse = FALSE: paste("g kg"^-1)
      "km/hr"
        parse = TRUE:  expression(paste("km hr"^-1))
        parse = FALSE: paste("km hr"^-1)
      "km/h"
        parse = TRUE:  expression(paste("km hr"^-1))
        parse = FALSE: paste("km hr"^-1)
      "km/hour"
        parse = TRUE:  expression(paste("km hr"^-1))
        parse = FALSE: paste("km hr"^-1)
      "km/hr/s"
        parse = TRUE:  expression(paste("km hr"^-1 * " s"^-1))
        parse = FALSE: paste("km hr"^-1 * " s"^-1)
      "km/h/s"
        parse = TRUE:  expression(paste("km hr"^-1 * " s"^-1))
        parse = FALSE: paste("km hr"^-1 * " s"^-1)
      "umol/m2"
        parse = TRUE:  expression(paste(mu * "mol m"^-2))
        parse = FALSE: paste(mu * "mol m"^-2)
      "umol/m2/s"
        parse = TRUE:  expression(paste(mu * "mol m"^-2 * " s"^-1))
        parse = FALSE: paste(mu * "mol m"^-2 * " s"^-1)

# symbols are formatted

    Code
      show_quicktext(c("dgrC", "degreeC", "deg. C", "degreesC", "degrees", "Delta",
        "delta", "r2", "R2", "tau "))
    Output
      "dgrC"
        parse = TRUE:  expression(paste(degree * "C"))
        parse = FALSE: paste(degree * "C")
      "degreeC"
        parse = TRUE:  expression(paste(degree * "C"))
        parse = FALSE: paste(degree * "C")
      "deg. C"
        parse = TRUE:  expression(paste(degree * "C"))
        parse = FALSE: paste(degree * "C")
      "degreesC"
        parse = TRUE:  expression(paste(degree * "C"))
        parse = FALSE: paste(degree * "C")
      "degrees"
        parse = TRUE:  expression(paste(degree))
        parse = FALSE: paste(degree)
      "Delta"
        parse = TRUE:  expression(paste(Delta))
        parse = FALSE: paste(Delta)
      "delta"
        parse = TRUE:  expression(paste(Delta))
        parse = FALSE: paste(Delta)
      "r2"
        parse = TRUE:  expression(paste("R"^2))
        parse = FALSE: paste("R"^2)
      "R2"
        parse = TRUE:  expression(paste("R"^2))
        parse = FALSE: paste("R"^2)
      "tau "
        parse = TRUE:  expression(paste(tau, " "))
        parse = FALSE: paste(tau, " ")

# combinations and plain text are formatted

    Code
      show_quicktext(c("my no2 label", "pm10 [ ug.m-3 ]", "pm2.5 (ug/m3)", "o3 (ppb)",
        "no2 ug/m3", "temp degrees", "wind m/s", "site A", "it's no2", "no2\nug/m3"))
    Output
      "my no2 label"
        parse = TRUE:  expression(paste("my ", "NO"[2], " label"))
        parse = FALSE: paste("my ", "NO"[2], " label")
      "pm10 [ ug.m-3 ]"
        parse = TRUE:  expression(paste("PM"[10], " [ ", mu * "g m"^-3, " ]"))
        parse = FALSE: paste("PM"[10], " [ ", mu * "g m"^-3, " ]")
      "pm2.5 (ug/m3)"
        parse = TRUE:  expression(paste("PM"[2.5], " (", mu * "g m"^-3, ")"))
        parse = FALSE: paste("PM"[2.5], " (", mu * "g m"^-3, ")")
      "o3 (ppb)"
        parse = TRUE:  expression(paste("O"[3], " (ppb)"))
        parse = FALSE: paste("O"[3], " (ppb)")
      "no2 ug/m3"
        parse = TRUE:  expression(paste("NO"[2], " ", mu * "g m"^-3))
        parse = FALSE: paste("NO"[2], " ", mu * "g m"^-3)
      "temp degrees"
        parse = TRUE:  expression(paste("temp ", degree))
        parse = FALSE: paste("temp ", degree)
      "wind m/s"
        parse = TRUE:  expression(paste("wind ", "m s"^-1))
        parse = FALSE: paste("wind ", "m s"^-1)
      "site A"
        parse = TRUE:  expression(paste("site A"))
        parse = FALSE: paste("site A")
      "it's no2"
        parse = TRUE:  expression(paste("it's ", "NO"[2]))
        parse = FALSE: paste("it's ", "NO"[2])
      "no2\nug/m3"
        parse = TRUE:  expression(displaystyle(atop(displaystyle(paste("NO"[2])), displaystyle(paste(mu *     "g m"^-3)))))
        parse = FALSE: paste("NO"[2], " ", mu * "g m"^-3)

# names are not formatted inside longer words

    Code
      show_quicktext(c("year2020", "no3", "NO3", "pm100", "Morocco data", "no2conc",
        "no2_roadside", "roadside_no2", "pm10_ugm3", "Mean no2, o3 and co", "no data"))
    Output
      "year2020"
        parse = TRUE:  expression(paste("year2020"))
        parse = FALSE: paste("year2020")
      "no3"
        parse = TRUE:  expression(paste("NO"[3]))
        parse = FALSE: paste("NO"[3])
      "NO3"
        parse = TRUE:  expression(paste("NO"[3]))
        parse = FALSE: paste("NO"[3])
      "pm100"
        parse = TRUE:  expression(paste("pm100"))
        parse = FALSE: paste("pm100")
      "Morocco data"
        parse = TRUE:  expression(paste("Morocco data"))
        parse = FALSE: paste("Morocco data")
      "no2conc"
        parse = TRUE:  expression(paste("no2conc"))
        parse = FALSE: paste("no2conc")
      "no2_roadside"
        parse = TRUE:  expression(paste("NO"[2], "_roadside"))
        parse = FALSE: paste("NO"[2], "_roadside")
      "roadside_no2"
        parse = TRUE:  expression(paste("roadside_", "NO"[2]))
        parse = FALSE: paste("roadside_", "NO"[2])
      "pm10_ugm3"
        parse = TRUE:  expression(paste("PM"[10], "_ugm3"))
        parse = FALSE: paste("PM"[10], "_ugm3")
      "Mean no2, o3 and co"
        parse = TRUE:  expression(paste("Mean ", "NO"[2], ", ", "O"[3], " and ", "CO"))
        parse = FALSE: paste("Mean ", "NO"[2], ", ", "O"[3], " and ", "CO")
      "no data"
        parse = TRUE:  expression(paste("NO", " data"))
        parse = FALSE: paste("NO", " data")

# spaces between formatted names and units are kept

    Code
      show_quicktext(c("ug/m3 no2", "Delta no2", "no2 o3", "co2 g/km", "m/s ws",
        "km/h no2", "Delta no2 (ug/m3)"))
    Output
      "ug/m3 no2"
        parse = TRUE:  expression(paste(mu * "g m"^-3, " ", "NO"[2]))
        parse = FALSE: paste(mu * "g m"^-3, " ", "NO"[2])
      "Delta no2"
        parse = TRUE:  expression(paste(Delta, " ", "NO"[2]))
        parse = FALSE: paste(Delta, " ", "NO"[2])
      "no2 o3"
        parse = TRUE:  expression(paste("NO"[2], " ", "O"[3]))
        parse = FALSE: paste("NO"[2], " ", "O"[3])
      "co2 g/km"
        parse = TRUE:  expression(paste("CO"[2], " ", "g km"^-1))
        parse = FALSE: paste("CO"[2], " ", "g km"^-1)
      "m/s ws"
        parse = TRUE:  expression(paste("m s"^-1, " ", "wind spd."))
        parse = FALSE: paste("m s"^-1, " ", "wind spd.")
      "km/h no2"
        parse = TRUE:  expression(paste("km hr"^-1, " ", "NO"[2]))
        parse = FALSE: paste("km hr"^-1, " ", "NO"[2])
      "Delta no2 (ug/m3)"
        parse = TRUE:  expression(paste(Delta, " ", "NO"[2], " (", mu * "g m"^-3, ")"))
        parse = FALSE: paste(Delta, " ", "NO"[2], " (", mu * "g m"^-3, ")")

# any number of newlines are stacked as lines

    Code
      show_quicktext(c("Line 1\nLine 2", "a\nb\nc",
        "Mean no2\n(ug/m3)\n2020\nroadside", "no2\n", "\nno2"))
    Output
      "Line 1\nLine 2"
        parse = TRUE:  expression(displaystyle(atop(displaystyle(paste("Line 1")), displaystyle(paste("Line 2")))))
        parse = FALSE: paste("Line 1 Line 2")
      "a\nb\nc"
        parse = TRUE:  expression(displaystyle(atop(displaystyle(paste("a")), displaystyle(atop(displaystyle(paste("b")),     displaystyle(paste("c")))))))
        parse = FALSE: paste("a b c")
      "Mean no2\n(ug/m3)\n2020\nroadside"
        parse = TRUE:  expression(displaystyle(atop(displaystyle(paste("Mean ", "NO"[2])),     displaystyle(atop(displaystyle(paste("(", mu * "g m"^-3,         ")")), displaystyle(atop(displaystyle(paste("2020")),         displaystyle(paste("roadside")))))))))
        parse = FALSE: paste("Mean ", "NO"[2], " (", mu * "g m"^-3, ") 2020 roadside")
      "no2\n"
        parse = TRUE:  expression(displaystyle(atop(displaystyle(paste("NO"[2])), displaystyle(paste()))))
        parse = FALSE: paste("NO"[2], " ")
      "\nno2"
        parse = TRUE:  expression(displaystyle(atop(displaystyle(paste()), displaystyle(paste("NO"[2])))))
        parse = FALSE: paste(" ", "NO"[2])

# adjacent symbols are formatted

    Code
      show_quicktext(c("Delta Delta", "degrees degrees", "ug/m3 ug/m3", "tau tau"))
    Output
      "Delta Delta"
        parse = TRUE:  expression(paste(Delta, " ", Delta))
        parse = FALSE: paste(Delta, " ", Delta)
      "degrees degrees"
        parse = TRUE:  expression(paste(degree, " ", degree))
        parse = FALSE: paste(degree, " ", degree)
      "ug/m3 ug/m3"
        parse = TRUE:  expression(paste(mu * "g m"^-3, " ", mu * "g m"^-3))
        parse = FALSE: paste(mu * "g m"^-3, " ", mu * "g m"^-3)
      "tau tau"
        parse = TRUE:  expression(paste(tau, " ", tau))
        parse = FALSE: paste(tau, " ", tau)

# short names that are common words only match on their own

    Code
      show_quicktext(c("co", "co (ppm)", "(co)", "[co]", "co/nox", "co, no2",
        "co: hourly", "co-located", "co.", "co_roadside", "no", "no. 5", "no-data",
        "NO2 and no"))
    Output
      "co"
        parse = TRUE:  expression(paste("CO"))
        parse = FALSE: paste("CO")
      "co (ppm)"
        parse = TRUE:  expression(paste("CO", " (ppm)"))
        parse = FALSE: paste("CO", " (ppm)")
      "(co)"
        parse = TRUE:  expression(paste("(", "CO", ")"))
        parse = FALSE: paste("(", "CO", ")")
      "[co]"
        parse = TRUE:  expression(paste("[", "CO", "]"))
        parse = FALSE: paste("[", "CO", "]")
      "co/nox"
        parse = TRUE:  expression(paste("CO", "/", "NO"[x]))
        parse = FALSE: paste("CO", "/", "NO"[x])
      "co, no2"
        parse = TRUE:  expression(paste("CO", ", ", "NO"[2]))
        parse = FALSE: paste("CO", ", ", "NO"[2])
      "co: hourly"
        parse = TRUE:  expression(paste("CO", ": hourly"))
        parse = FALSE: paste("CO", ": hourly")
      "co-located"
        parse = TRUE:  expression(paste("co-located"))
        parse = FALSE: paste("co-located")
      "co."
        parse = TRUE:  expression(paste("co."))
        parse = FALSE: paste("co.")
      "co_roadside"
        parse = TRUE:  expression(paste("co_roadside"))
        parse = FALSE: paste("co_roadside")
      "no"
        parse = TRUE:  expression(paste("NO"))
        parse = FALSE: paste("NO")
      "no. 5"
        parse = TRUE:  expression(paste("no. 5"))
        parse = FALSE: paste("no. 5")
      "no-data"
        parse = TRUE:  expression(paste("no-data"))
        parse = FALSE: paste("no-data")
      "NO2 and no"
        parse = TRUE:  expression(paste("NO"[2], " and ", "NO"))
        parse = FALSE: paste("NO"[2], " and ", "NO")

# Delta can prefix names but not start words

    Code
      show_quicktext(c("Delta", "DeltaNO2", "delta_no2", "Delta2", "Deltaville",
        "deltas"))
    Output
      "Delta"
        parse = TRUE:  expression(paste(Delta))
        parse = FALSE: paste(Delta)
      "DeltaNO2"
        parse = TRUE:  expression(paste(Delta, "NO2"))
        parse = FALSE: paste(Delta, "NO2")
      "delta_no2"
        parse = TRUE:  expression(paste(Delta, "_", "NO"[2]))
        parse = FALSE: paste(Delta, "_", "NO"[2])
      "Delta2"
        parse = TRUE:  expression(paste(Delta, "2"))
        parse = FALSE: paste(Delta, "2")
      "Deltaville"
        parse = TRUE:  expression(paste("Deltaville"))
        parse = FALSE: paste("Deltaville")
      "deltas"
        parse = TRUE:  expression(paste("deltas"))
        parse = FALSE: paste("deltas")

# names are not formatted when followed by a decimal part

    Code
      show_quicktext(c("pm1.5", "pm2.5", "pm1.", "pm10. data"))
    Output
      "pm1.5"
        parse = TRUE:  expression(paste("pm1.5"))
        parse = FALSE: paste("pm1.5")
      "pm2.5"
        parse = TRUE:  expression(paste("PM"[2.5]))
        parse = FALSE: paste("PM"[2.5])
      "pm1."
        parse = TRUE:  expression(paste("PM"[1], "."))
        parse = FALSE: paste("PM"[1], ".")
      "pm10. data"
        parse = TRUE:  expression(paste("PM"[10], ". data"))
        parse = FALSE: paste("PM"[10], ". data")

# units are not formatted when followed by a letter

    Code
      show_quicktext(c("speed (m/sec)", "km/hours", "g/second", "ug/m3s", "km/s",
        "kg/m3", "m/s_mean", "degreesF"))
    Output
      "speed (m/sec)"
        parse = TRUE:  expression(paste("speed (m/sec)"))
        parse = FALSE: paste("speed (m/sec)")
      "km/hours"
        parse = TRUE:  expression(paste("km/hours"))
        parse = FALSE: paste("km/hours")
      "g/second"
        parse = TRUE:  expression(paste("g/second"))
        parse = FALSE: paste("g/second")
      "ug/m3s"
        parse = TRUE:  expression(paste("ug/m3s"))
        parse = FALSE: paste("ug/m3s")
      "km/s"
        parse = TRUE:  expression(paste("k", "m s"^-1))
        parse = FALSE: paste("k", "m s"^-1)
      "kg/m3"
        parse = TRUE:  expression(paste("k", "g m"^-3))
        parse = FALSE: paste("k", "g m"^-3)
      "m/s_mean"
        parse = TRUE:  expression(paste("m s"^-1, "_mean"))
        parse = FALSE: paste("m s"^-1, "_mean")
      "degreesF"
        parse = TRUE:  expression(paste(degree, "F"))
        parse = FALSE: paste(degree, "F")

