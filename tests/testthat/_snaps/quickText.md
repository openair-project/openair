# pollutant names are formatted

    Code
      show_quicktext(c("no2", "NO2", "no", "nox", "NOx", "NOX", "nh3", "NH3", "co",
        "co2", "CO2", "nmhc", "o3", "O3", "ozone", "so2", "SO2", "h2s", "H2S", "ch4",
        "CH4"))
    Output
      "no2"
        parse = TRUE:  expression(paste("NO"[2]))
        parse = FALSE: paste( 'NO' [2] )
      "NO2"
        parse = TRUE:  expression(paste("NO"[2]))
        parse = FALSE: paste( 'NO' [2] )
      "no"
        parse = TRUE:  expression(paste("NO"))
        parse = FALSE: paste('NO')
      "nox"
        parse = TRUE:  expression(paste("NO"[x]))
        parse = FALSE: paste( 'NO' [x] )
      "NOx"
        parse = TRUE:  expression(paste("NO"[x]))
        parse = FALSE: paste( 'NO' [x] )
      "NOX"
        parse = TRUE:  expression(paste("NO"[x]))
        parse = FALSE: paste( 'NO' [x] )
      "nh3"
        parse = TRUE:  expression(paste("NH"[3]))
        parse = FALSE: paste( 'NH' [3] )
      "NH3"
        parse = TRUE:  expression(paste("NH"[3]))
        parse = FALSE: paste( 'NH' [3] )
      "co"
        parse = TRUE:  expression(paste("CO"))
        parse = FALSE: paste( 'CO' )
      "co2"
        parse = TRUE:  expression(paste("CO"[2]))
        parse = FALSE: paste( 'CO' [2] )
      "CO2"
        parse = TRUE:  expression(paste("CO"[2]))
        parse = FALSE: paste( 'CO' [2] )
      "nmhc"
        parse = TRUE:  expression(paste("NMHC"))
        parse = FALSE: paste( 'NMHC' )
      "o3"
        parse = TRUE:  expression(paste("O"[3]))
        parse = FALSE: paste( 'O' [3] )
      "O3"
        parse = TRUE:  expression(paste("O"[3]))
        parse = FALSE: paste( 'O' [3] )
      "ozone"
        parse = TRUE:  expression(paste("O"[3]))
        parse = FALSE: paste( 'O' [3] )
      "so2"
        parse = TRUE:  expression(paste("SO"[2]))
        parse = FALSE: paste( 'SO' [2] )
      "SO2"
        parse = TRUE:  expression(paste("SO"[2]))
        parse = FALSE: paste( 'SO' [2] )
      "h2s"
        parse = TRUE:  expression(paste("H"[2] * "S"))
        parse = FALSE: paste( 'H' [2] * 'S')
      "H2S"
        parse = TRUE:  expression(paste("H"[2] * "S"))
        parse = FALSE: paste( 'H' [2] * 'S')
      "ch4"
        parse = TRUE:  expression(paste("CH"[4]))
        parse = FALSE: paste( 'CH' [4] )
      "CH4"
        parse = TRUE:  expression(paste("CH"[4]))
        parse = FALSE: paste( 'CH' [4] )

# particulate matter names are formatted

    Code
      show_quicktext(c("pm1", "PM1", "pm4", "PM4", "pm10", "PM10", "pm25", "PM25",
        "pm2.5", "PM2.5", "pmc", "PMc", "pmcoarse", "PMcoarse", "pmf", "PMf",
        "pmfine", "PMfine", "pmtot", "PMtot"))
    Output
      "pm1"
        parse = TRUE:  expression(paste("PM"[1]))
        parse = FALSE: paste( 'PM' [1] )
      "PM1"
        parse = TRUE:  expression(paste("PM"[1]))
        parse = FALSE: paste( 'PM' [1] )
      "pm4"
        parse = TRUE:  expression(paste("PM"[4]))
        parse = FALSE: paste( 'PM' [4] )
      "PM4"
        parse = TRUE:  expression(paste("PM"[4]))
        parse = FALSE: paste( 'PM' [4] )
      "pm10"
        parse = TRUE:  expression(paste("PM"[10]))
        parse = FALSE: paste( 'PM' [10] )
      "PM10"
        parse = TRUE:  expression(paste("PM"[10]))
        parse = FALSE: paste( 'PM' [10] )
      "pm25"
        parse = TRUE:  expression(paste("PM"[2.5]))
        parse = FALSE: paste( 'PM' [2.5] )
      "PM25"
        parse = TRUE:  expression(paste("PM"[2.5]))
        parse = FALSE: paste( 'PM' [2.5] )
      "pm2.5"
        parse = TRUE:  expression(paste("PM"[2.5]))
        parse = FALSE: paste( 'PM' [2.5] )
      "PM2.5"
        parse = TRUE:  expression(paste("PM"[2.5]))
        parse = FALSE: paste( 'PM' [2.5] )
      "pmc"
        parse = TRUE:  expression(paste("PM"[coarse]))
        parse = FALSE: paste( 'PM' [coarse] )
      "PMc"
        parse = TRUE:  expression(paste("PM"[coarse]))
        parse = FALSE: paste( 'PM' [coarse] )
      "pmcoarse"
        parse = TRUE:  expression(paste("PM"[coarse]))
        parse = FALSE: paste( 'PM' [coarse] )
      "PMcoarse"
        parse = TRUE:  expression(paste("PM"[coarse]))
        parse = FALSE: paste( 'PM' [coarse] )
      "pmf"
        parse = TRUE:  expression(paste("PM"[fine]))
        parse = FALSE: paste( 'PM' [fine] )
      "PMf"
        parse = TRUE:  expression(paste("PM"[fine]))
        parse = FALSE: paste( 'PM' [fine] )
      "pmfine"
        parse = TRUE:  expression(paste("PM"[fine]))
        parse = FALSE: paste( 'PM' [fine] )
      "PMfine"
        parse = TRUE:  expression(paste("PM"[fine]))
        parse = FALSE: paste( 'PM' [fine] )
      "pmtot"
        parse = TRUE:  expression(paste("PM"[total]))
        parse = FALSE: paste( 'PM' [total] )
      "PMtot"
        parse = TRUE:  expression(paste("PM"[total]))
        parse = FALSE: paste( 'PM' [total] )

# meteorological names are formatted

    Code
      show_quicktext(c("ws", "WS", "wd", "WD", "air_temp", "rh"))
    Output
      "ws"
        parse = TRUE:  expression(paste("wind spd."))
        parse = FALSE: paste( 'wind spd.' )
      "WS"
        parse = TRUE:  expression(paste("wind spd."))
        parse = FALSE: paste( 'wind spd.' )
      "wd"
        parse = TRUE:  expression(paste("wind dir."))
        parse = FALSE: paste( 'wind dir.' )
      "WD"
        parse = TRUE:  expression(paste("wind dir."))
        parse = FALSE: paste( 'wind dir.' )
      "air_temp"
        parse = TRUE:  expression(paste("temperature"))
        parse = FALSE: paste( 'temperature' )
      "rh"
        parse = TRUE:  expression(paste("relative humidity"))
        parse = FALSE: paste( 'relative humidity' )

# units are formatted

    Code
      show_quicktext(c("ug/m3", "ug.m-3", "ug m-3", "ugm-3", "mg/m3", "ng/m3", "g/m3",
        "cm-3", "m/s", "m.s-1", "m s-1", "m/s2", "km2", "g/km", "g/s", "kW/t",
        "g/hour", "g/hr", "g/kg", "km/hr", "km/h", "km/hour", "km/hr/s", "km/h/s",
        "umol/m2", "umol/m2/s"))
    Output
      "ug/m3"
        parse = TRUE:  expression(paste(mu * "g m"^-3))
        parse = FALSE: paste( mu * 'g m' ^-3)
      "ug.m-3"
        parse = TRUE:  expression(paste(mu * "g m"^-3))
        parse = FALSE: paste( mu * 'g m' ^-3)
      "ug m-3"
        parse = TRUE:  expression(paste(mu * "g m"^-3))
        parse = FALSE: paste( mu * 'g m' ^-3)
      "ugm-3"
        parse = TRUE:  expression(paste(mu * "g m"^-3))
        parse = FALSE: paste( mu * 'g m' ^-3)
      "mg/m3"
        parse = TRUE:  expression(paste("m" * "g m"^-3))
        parse = FALSE: paste( 'm' * 'g m' ^-3)
      "ng/m3"
        parse = TRUE:  expression(paste("n" * "g m"^-3))
        parse = FALSE: paste( 'n' * 'g m' ^-3)
      "g/m3"
        parse = TRUE:  expression(paste("g m"^-3))
        parse = FALSE: paste( 'g m' ^-3)
      "cm-3"
        parse = TRUE:  expression(paste("cm"^-3))
        parse = FALSE: paste( 'cm' ^-3)
      "m/s"
        parse = TRUE:  expression(paste("m s"^-1))
        parse = FALSE: paste( 'm s' ^-1)
      "m.s-1"
        parse = TRUE:  expression(paste("m s"^-1))
        parse = FALSE: paste( 'm s' ^-1)
      "m s-1"
        parse = TRUE:  expression(paste("m s"^-1))
        parse = FALSE: paste( 'm s' ^-1)
      "m/s2"
        parse = TRUE:  expression(paste("m s"^-2))
        parse = FALSE: paste( 'm s' ^-2)
      "km2"
        parse = TRUE:  expression(paste("km"^2))
        parse = FALSE: paste( 'km' ^2)
      "g/km"
        parse = TRUE:  expression(paste("g km"^-1))
        parse = FALSE: paste( 'g km' ^-1)
      "g/s"
        parse = TRUE:  expression(paste("g s"^-1))
        parse = FALSE: paste( 'g s' ^-1)
      "kW/t"
        parse = TRUE:  expression(paste("kW t"^-1))
        parse = FALSE: paste( 'kW t' ^-1)
      "g/hour"
        parse = TRUE:  expression(paste("g hour"^-1))
        parse = FALSE: paste( 'g hour' ^-1)
      "g/hr"
        parse = TRUE:  expression(paste("g hour"^-1))
        parse = FALSE: paste( 'g hour' ^-1)
      "g/kg"
        parse = TRUE:  expression(paste("g kg"^-1))
        parse = FALSE: paste( 'g kg' ^-1)
      "km/hr"
        parse = TRUE:  expression(paste("km hr"^-1))
        parse = FALSE: paste( 'km hr' ^-1)
      "km/h"
        parse = TRUE:  expression(paste("km hr"^-1))
        parse = FALSE: paste( 'km hr' ^-1)
      "km/hour"
        parse = TRUE:  expression(paste("km hr"^-1 * "our"))
        parse = FALSE: paste( 'km hr' ^-1 *'our')
      "km/hr/s"
        parse = TRUE:  expression(paste("km hr"^-1 * " s"^-1))
        parse = FALSE: paste( 'km hr' ^-1 * ' s' ^-1)
      "km/h/s"
        parse = TRUE:  expression(paste("km hr"^-1 * " s"^-1))
        parse = FALSE: paste('km hr' ^-1 * ' s' ^-1)
      "umol/m2"
        parse = TRUE:  expression(paste(mu * "mol m"^-2))
        parse = FALSE: paste( mu * 'mol m' ^-2)
      "umol/m2/s"
        parse = TRUE:  expression(paste(mu * "mol m"^-2 * " s"^-1))
        parse = FALSE: paste( mu * 'mol m' ^-2 * ' s' ^-1)

# symbols are formatted

    Code
      show_quicktext(c("dgrC", "degreeC", "deg. C", "degreesC", "degrees", "Delta",
        "delta", "r2", "R2", "tau "))
    Output
      "dgrC"
        parse = TRUE:  expression(paste(degree * "C"))
        parse = FALSE: paste( degree * 'C' )
      "degreeC"
        parse = TRUE:  expression(paste(degree * "C"))
        parse = FALSE: paste( degree * 'C' )
      "deg. C"
        parse = TRUE:  expression(paste(degree * "C"))
        parse = FALSE: paste( degree * 'C' )
      "degreesC"
        parse = TRUE:  expression(paste(degree * "C"))
        parse = FALSE: paste( degree * 'C' )
      "degrees"
        parse = TRUE:  expression(paste(degree))
        parse = FALSE: paste( degree)
      "Delta"
        parse = TRUE:  expression(paste(Delta))
        parse = FALSE: paste( Delta)
      "delta"
        parse = TRUE:  expression(paste(Delta))
        parse = FALSE: paste( Delta)
      "r2"
        parse = TRUE:  expression(paste("R"^2))
        parse = FALSE: paste('R' ^2)
      "R2"
        parse = TRUE:  expression(paste("R"^2))
        parse = FALSE: paste('R' ^2)
      "tau "
        parse = TRUE:  expression(paste(tau))
        parse = FALSE: paste( tau )

# combinations and plain text are formatted

    Code
      show_quicktext(c("my no2 label", "pm10 [ ug.m-3 ]", "pm2.5 (ug/m3)", "o3 (ppb)",
        "no2 ug/m3", "temp degrees", "wind m/s", "site A", "it's no2", "no2\nug/m3"))
    Output
      "my no2 label"
        parse = TRUE:  expression(paste("my NO"[2] * " label"))
        parse = FALSE: paste('my NO' [2] * ' label')
      "pm10 [ ug.m-3 ]"
        parse = TRUE:  expression(paste("PM"[10] * " [ " * mu * "g m"^-3 * " ]"))
        parse = FALSE: paste( 'PM' [10] * ' [ ' * mu * 'g m' ^-3 *' ]')
      "pm2.5 (ug/m3)"
        parse = TRUE:  expression(paste("PM"[2.5] * " (" * mu * "g m"^-3 * ")"))
        parse = FALSE: paste( 'PM' [2.5] * ' (' * mu * 'g m' ^-3 *')')
      "o3 (ppb)"
        parse = TRUE:  expression(paste("O"[3] * " (ppb)"))
        parse = FALSE: paste( 'O' [3] * ' (ppb)')
      "no2 ug/m3"
        parse = TRUE:  expression(paste("NO"[2] ~ mu * "g m"^-3))
        parse = FALSE: paste( 'NO' [2] ~ mu * 'g m' ^-3)
      "temp degrees"
        parse = TRUE:  expression(paste("temp " * degree))
        parse = FALSE: paste('temp ' * degree)
      "wind m/s"
        parse = TRUE:  expression(paste("wind m s"^-1))
        parse = FALSE: paste('wind m s' ^-1)
      "site A"
        parse = TRUE:  expression(paste("site A"))
        parse = FALSE: paste('site A')
      "it's no2"
        parse = TRUE:  expression(paste("it's NO"[2]))
        parse = FALSE: paste('it\'s NO' [2] )
      "no2\nug/m3"
        parse = TRUE:  expression(paste(atop("NO"[2] * "", "" * mu * "g m"^-3)))
        parse = FALSE: paste( 'NO' [2] * ' ' * mu * 'g m' ^-3)

# names are not formatted inside longer words

    Code
      show_quicktext(c("year2020", "no3", "NO3", "pm100", "Morocco data", "no2conc",
        "no2_roadside", "roadside_no2", "pm10_ugm3", "Mean no2, o3 and co", "no data"))
    Output
      "year2020"
        parse = TRUE:  expression(paste("year2020"))
        parse = FALSE: paste('year2020')
      "no3"
        parse = TRUE:  expression(paste("NO"[3]))
        parse = FALSE: paste( 'NO' [3] )
      "NO3"
        parse = TRUE:  expression(paste("NO"[3]))
        parse = FALSE: paste( 'NO' [3] )
      "pm100"
        parse = TRUE:  expression(paste("pm100"))
        parse = FALSE: paste('pm100')
      "Morocco data"
        parse = TRUE:  expression(paste("Morocco data"))
        parse = FALSE: paste('Morocco data')
      "no2conc"
        parse = TRUE:  expression(paste("no2conc"))
        parse = FALSE: paste('no2conc')
      "no2_roadside"
        parse = TRUE:  expression(paste("NO"[2] * "_roadside"))
        parse = FALSE: paste( 'NO' [2] * '_roadside')
      "roadside_no2"
        parse = TRUE:  expression(paste("roadside_NO"[2]))
        parse = FALSE: paste('roadside_NO' [2] )
      "pm10_ugm3"
        parse = TRUE:  expression(paste("PM"[10] * "_ugm3"))
        parse = FALSE: paste( 'PM' [10] * '_ugm3')
      "Mean no2, o3 and co"
        parse = TRUE:  expression(paste("Mean NO"[2] * ", O"[3] * " and CO"))
        parse = FALSE: paste('Mean NO' [2] * ', O' [3] * ' and CO' )
      "no data"
        parse = TRUE:  expression(paste("NO data"))
        parse = FALSE: paste('NO data')

# spaces between formatted names and units are kept

    Code
      show_quicktext(c("ug/m3 no2", "Delta no2", "no2 o3", "co2 g/km", "m/s ws",
        "km/h no2", "Delta no2 (ug/m3)"))
    Output
      "ug/m3 no2"
        parse = TRUE:  expression(paste(mu * "g m"^-3 ~ "NO"[2]))
        parse = FALSE: paste( mu * 'g m' ^-3 ~ 'NO' [2] )
      "Delta no2"
        parse = TRUE:  expression(paste(Delta ~ "NO"[2]))
        parse = FALSE: paste( Delta ~ 'NO' [2] )
      "no2 o3"
        parse = TRUE:  expression(paste("NO"[2] ~ "O"[3]))
        parse = FALSE: paste( 'NO' [2] ~ 'O' [3] )
      "co2 g/km"
        parse = TRUE:  expression(paste("CO"[2] ~ "g km"^-1))
        parse = FALSE: paste( 'CO' [2] ~ 'g km' ^-1)
      "m/s ws"
        parse = TRUE:  expression(paste("m s"^-1 ~ "wind spd."))
        parse = FALSE: paste( 'm s' ^-1 ~ 'wind spd.' )
      "km/h no2"
        parse = TRUE:  expression(paste("km hr"^-1 ~ "NO"[2]))
        parse = FALSE: paste( 'km hr' ^-1 ~ 'NO' [2] )
      "Delta no2 (ug/m3)"
        parse = TRUE:  expression(paste(Delta ~ "NO"[2] * " (" * mu * "g m"^-3 * ")"))
        parse = FALSE: paste( Delta ~ 'NO' [2] * ' (' * mu * 'g m' ^-3 *')')

# adjacent symbols are formatted

    Code
      show_quicktext(c("Delta Delta", "degrees degrees", "ug/m3 ug/m3", "tau tau"))
    Output
      "Delta Delta"
        parse = TRUE:  expression(paste(Delta ~ Delta))
        parse = FALSE: paste( Delta ~ Delta)
      "degrees degrees"
        parse = TRUE:  expression(paste(degree ~ degree))
        parse = FALSE: paste( degree ~ degree)
      "ug/m3 ug/m3"
        parse = TRUE:  expression(paste(mu * "g m"^-3 ~ mu * "g m"^-3))
        parse = FALSE: paste( mu * 'g m' ^-3 ~ mu * 'g m' ^-3)
      "tau tau"
        parse = TRUE:  expression(paste(tau ~ tau))
        parse = FALSE: paste( tau ~ tau )

