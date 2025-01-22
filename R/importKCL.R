#' Import data from King's College London networks
#'
#' Function for importing hourly mean data from King's College London networks.
#' Files are imported from a remote server operated by King's College London
#' that provides air quality data files as R data objects.
#'
#' The \code{importKCL} function has been written to make it easy to import
#' data from the King's College London air pollution networks. KCL have
#' provided .RData files (R workspaces) of all individual sites and years for
#' the KCL networks. These files are updated on a weekly basis. This approach
#' requires a link to the Internet to work.
#'
#' There are several advantages over the web portal approach where .csv files
#' are downloaded. First, it is quick to select a range of sites, pollutants
#' and periods (see examples below). Second, storing the data as .RData objects
#' is very efficient as they are about four times smaller than .csv files ---
#' which means the data downloads quickly and saves bandwidth. Third, the
#' function completely avoids any need for data manipulation or setting time
#' formats, time zones etc. Finally, it is easy to import many years of data
#' beyond the current limit of about 64,000 lines. The final point makes it
#' possible to download several long time series in one go. The function also
#' has the advantage that the proper site name is imported and used in
#' \code{openair} functions.
#'
#' The site codes and pollutant names can be upper or lower case. The function
#' will issue a warning when data less than six months old is downloaded, which
#' may not be ratified.
#'
#' The data are imported by stacking sites on top of one another and will have
#' field names \code{date}, \code{site}, \code{code} (the site code) and
#' pollutant(s). Sometimes it is useful to have columns of site data. This can
#' be done using the \code{reshape} function --- see examples below.
#'
#' The situation for particle measurements is not straightforward given the
#' variety of methods used to measure particle mass and changes in their use
#' over time. The \code{importKCL} function imports two measures of PM10 where
#' available. \code{PM10_raw} are TEOM measurements with a 1.3 factor applied
#' to take account of volatile losses. The \code{PM10} data is a current best
#' estimate of a gravimetric equivalent measure as described below. NOTE! many
#' sites have several instruments that measure PM10 or PM2.5. In the case of
#' FDMS measurements, these are given as separate site codes (see below). For
#' example "MY1" will be TEOM with VCM applied and "MY7" is the FDMS data.
#'
#' Where FDMS data are used the volatile and non-volatile components are
#' separately reported i.e. v10 = volatile PM10, v2.5 = volatile PM2.5, nv10 =
#' non-volatile PM10 and nv2.5 = non-volatile PM2.5. Therefore, PM10 = v10 +
#' nv10 and PM2.5 = v2.5 + nv2.5.
#'
#' For the assessment of the EU Limit Values, PM10 needs to be measured using
#' the reference method or one shown to be equivalent to the reference method.
#' Defra carried out extensive trials between 2004 and 2006 to establish which
#' types of particulate analysers in use in the UK were equivalent. These
#' trials found that measurements made using Partisol, FDMS, BAM and SM200
#' instruments were shown to be equivalent to the PM10 reference method.
#' However, correction factors need to be applied to measurements from the
#' SM200 and BAM instruments. Importantly, the TEOM was demonstrated as not
#' being equivalent to the reference method due to the loss of volatile PM,
#' even when the 1.3 correction factor was applied.  The Volatile Correction
#' Model (VCM) was developed for Defra at King's to allow measurements of PM10
#' from TEOM instruments to be converted to reference equivalent; it uses the
#' measurements of volatile PM made using nearby FDMS instruments to correct
#' the measurements made by the TEOM. It passed the equivalence testing using
#' the same methodology used in the Defra trials and is now the recommended
#' method for correcting TEOM measurements (Defra, 2009). VCM correction of
#' TEOM measurements can only be applied after 1st January 2004, when
#' sufficiently widespread measurements of volatile PM became available. The
#' 1.3 correction factor is now considered redundant for measurements of PM10
#' made after 1st January 2004.  Further information on the VCM can be found at
#' \url{http://www.volatile-correction-model.info/}.
#'
#' All PM10 statistics on the LondonAir web site, including the bulletins and
#' statistical tools (and in the RData objects downloaded using
#' \code{importKCL}), now report PM10 results as reference equivalent. For PM10
#' measurements made by BAM and SM200 analysers the applicable correction
#' factors have been applied. For measurements from TEOM analysers the 1.3
#' factor has been applied up to 1st January 2004, then the VCM method has been
#' used to convert to reference equivalent.
#'
#' The meteorological data are meant to represent 'typical' conditions in
#' London, but users may prefer to use their own data. The data provide a an
#' estimate of general meteorological conditions across Greater London. For
#' meteorological species (wd, ws, rain, solar) each data point is formed by
#' averaging measurements from a subset of LAQN monitoring sites that have been
#' identified as having minimal disruption from local obstacles and a long term
#' reliable dataset. The exact sites used varies between species, but include
#' between two and five sites per species. Therefore, the data should represent
#' 'London scale' meteorology, rather than local conditions.
#'
#' While the function is being developed, the following site codes should help
#' with selection. We will also make available other meta data such as site
#' type and location to make it easier to select sites based on other
#' information. Note that these codes need to be refined because only the
#' common species are available for export currently i.e. NOx, NO2, O3, CO,
#' SO2, PM10, PM2.5.
#'
#' \itemize{ \item A30 | Kingston - Kingston Bypass A3 | Roadside \item AD1 |
#' Shoreham-by-Sea | Kerbside \item AR1 | Chichester - Lodsworth | Rural \item
#' AR2 | Wealden - Isfield | Rural \item AS1 | Bath Aethalometer | Urban
#' Background \item BA1 | Basildon - Gloucester Park | Roadside \item BB1 |
#' Broxbourne (Roadside) | Roadside \item BE0 | Belfast - Carbon | Urban
#' Background \item BE1 | Belfast Centre AURN | Urban Background \item BE3 |
#' Belfast Centre Aethalometer | Urban Background \item BE7 | Belfast Centre
#' FDMS trial | Urban Background \item BE8 | Belfast - Nitrate | Urban
#' Background \item BE9 | Belfast - Partisol SO4 | Urban Background \item BF1 |
#' Bedford Stewartby (Rural) | Industrial \item BF3 | Bedford - Kempston |
#' Industrial \item BF4 | Bedford - Prebend Street | Roadside \item BF5 |
#' Bedford - Lurke Street | Roadside \item BG1 | Barking and Dagenham - Rush
#' Green | Suburban \item BG2 | Barking and Dagenham - Scrattons Farm |
#' Suburban \item BG3 | Barking and Dagenham - North Street | Kerbside \item
#' BH0 | Brighton Preston Park AURN | Urban Background \item BH1 | Brighton
#' Roadside | Roadside \item BH2 | Brighton and Hove - Hove Town Hall |
#' Roadside \item BH3 | Brighton and Hove - Foredown Tower | Urban Background
#' \item BH5 | Brighton Mobile (Preston Fire Station) | Roadside \item BH6 |
#' Brighton Mobile (Lewes Road) | Roadside \item BH7 | Brighton Mobile
#' (Gloucester Road) | Roadside \item BH8 | Brighton and Hove - Stanmer Park |
#' Rural \item BH9 | Brighton Mobile Beaconsfield Road | Roadside \item BI1 |
#' Birmingham Tyburn CPC | Urban Background \item BL0 | Camden - Bloomsbury |
#' Urban Background \item BL1 | Bloomsbury AURN SMPS | Urban Background \item
#' BM1 | Ballymena - Ballykeel | Suburban \item BM2 | Ballymena - North Road |
#' Roadside \item BN1 | Barnet - Tally Ho Corner | Kerbside \item BN2 | Barnet
#' - Finchley | Urban Background \item BN3 | Barnet - Strawberry Vale | Urban
#' Background \item BO1 | Ballymoney 1 | Suburban \item BP0 | Westminster -
#' Bridge Place | Urban Background \item BQ5 | Bexley - Manor Road West
#' Gravimetric | Industrial \item BQ6 | Bexley - Manor Road East Gravimetric |
#' Industrial \item BQ7 | Belvedere West | Urban Background \item BQ8 |
#' Belvedere West FDMS | Urban Background \item BT1 | Brent - Kingsbury |
#' Suburban \item BT2 | Brent - Ikea Car Park | Roadside \item BT3 | Brent -
#' Harlesden | Roadside \item BT4 | Brent - Ikea | Roadside \item BT5 | Brent -
#' Neasden Lane | Industrial \item BT6 | Brent - John Keble Primary School |
#' Roadside \item BT7 | Brent - St Marys Primary School | Urban Background
#' \item BW1 | Brentwood - Brentwood Town Hall | Urban Background \item BX0 |
#' Bexley - Belvedere FDMS | Suburban \item BX1 | Bexley - Slade Green |
#' Suburban \item BX2 | Bexley - Belvedere | Suburban \item BX3 | Bexley -
#' Thamesmead | Suburban \item BX4 | Bexley - Erith | Industrial \item BX5 |
#' Bexley - Bedonwell | Suburban \item BX6 | Bexley - Thames Road North FDMS |
#' Roadside \item BX7 | Bexley - Thames Road North | Roadside \item BX8 |
#' Bexley - Thames Road South | Roadside \item BX9 | Bexley - Slade Green FDMS
#' | Suburban \item BY1 | Bromley - Rent Office | Urban Background \item BY4 |
#' Bromley - Tweedy Rd | Roadside \item BY5 | Bromley - Biggin Hill | Suburban
#' \item BY7 | Bromley - Harwood Avenue | Roadside \item CA1 | Crawley
#' Background | Urban Background \item CA2 | Crawley - Gatwick Airport | Urban
#' Background \item CB1 | Chelmsford - Fire Station | Roadside \item CB2 |
#' Chelmsford - Springfield Road | Roadside \item CB3 | Chelmsford - Chignal St
#' James | Urban Background \item CB4 | Chelmsford - Baddow Road | Roadside
#' \item CC1 | Colchester - Lucy Lane South | Roadside \item CC2 | Colchester -
#' Brook Street | Roadside \item CC3 | Colchester - Mersea Road | Roadside
#' \item CD1 | Camden - Swiss Cottage | Kerbside \item CD3 | Camden -
#' Shaftesbury Avenue | Roadside \item CD4 | Camden - St Martins College (NOX
#' 1) | Urban Background \item CD5 | Camden - St Martins College (NOX 2) |
#' Urban Background \item CD7 | Camden - Swiss Cottage Partisol | Kerbside
#' \item CD9 | Camden - Euston Road | Roadside \item CF1 | Cardiff Aethalometer
#' | Urban Background \item CH1 | Cheltenham | Urban Background \item CI1 |
#' Chichester - A27 Chichester Bypass | Roadside \item CI4 | Chichester -
#' Orchard Street | Roadside \item CK1 | Cookstown | Suburban \item CP1 |
#' Castle Point - Canvey Island | Urban Background \item CR2 | Croydon - Purley
#' Way | Roadside \item CR3 | Croydon - Thornton Heath | Suburban \item CR4 |
#' Croydon - George Street | Roadside \item CR5 | Croydon - Norbury | Kerbside
#' \item CR6 | Croydon - Euston Road | Suburban \item CT1 | City of London -
#' Senator House | Urban Background \item CT2 | City of London - Farringdon
#' Street | Kerbside \item CT3 | City of London - Sir John Cass School | Urban
#' Background \item CT4 | City of London - Beech Street | Roadside \item CT6 |
#' City of London - Walbrook Wharf | Roadside \item CT8 | City of London -
#' Upper Thames Street | Roadside \item CY1 | Crystal Palace - Crystal Palace
#' Parade | Roadside \item DC1 | Dacorum 1 Hemel Hempstead (Background) | Urban
#' Background \item DC2 | Dacorum 2 Hemel Hempstead (Background) | Urban
#' Background \item DC3 | High Street Northchurch | Roadside \item DE1 | Derry
#' City - Brandywell | Urban Background \item DE2 | Derry City - Dales Corner |
#' Roadside \item DM1 | Dunmurry Aethalometer | Urban Background \item EA0 |
#' Ealing - Acton Town Hall FDMS | Roadside \item EA1 | Ealing - Ealing Town
#' Hall | Urban Background \item EA2 | Ealing - Acton Town Hall | Roadside
#' \item EA3 | Ealing 3 - A40 East Acton | Roadside \item EA4 | Ealing Mobile -
#' Hamilton Road | Roadside \item EA5 | Ealing Mobile - Southall | Roadside
#' \item EA6 | Ealing - Hanger Lane Gyratory | Roadside \item EA7 | Ealing -
#' Southall | Urban Background \item EA8 | Ealing - Horn Lane | Industrial
#' \item EA9 | Ealing - Court Way | Roadside \item EB1 | Eastbourne -
#' Devonshire Park | Urban Background \item EB3 | Eastbourne - Holly Place |
#' Urban Background \item EH1 | E Herts Throcking (Rural) | Rural \item EH2 |
#' East Herts Sawbridgeworth (Background) | Urban Background \item EH3 | East
#' Herts Sawbridgeworth (Roadside) | Roadside \item EH4 | East Herts Ware |
#' Roadside \item EH5 | East Herts Bishops Stortford | Roadside \item EI0 |
#' Ealing - Greenford | Urban Background \item EI1 | Ealing - Western Avenue |
#' Roadside \item EL1 | Elmbridge - Bell Farm Hersham | Urban Background \item
#' EL2 | Elmbridge - Esher High Street | Roadside \item EL3 | Elmbridge -
#' Hampton Court Parade | Roadside \item EL4 | Elmbridge - Walton High Street |
#' Kerbside \item EN1 | Enfield - Bushhill Park | Suburban \item EN2 | Enfield
#' - Church Street | Roadside \item EN3 | Enfield - Salisbury School | Urban
#' Background \item EN4 | Enfield - Derby Road | Roadside \item EN5 | Enfield -
#' Bowes Primary School | Roadside \item FB1 | Rushmoor - Medway Drive |
#' Roadside \item GB0 | Greenwich and Bexley - Falconwood FDMS | Roadside \item
#' GB6 | Greenwich and Bexley - Falconwood | Roadside \item GL1 | Glasgow
#' Centre | Suburban \item GL4 | Glasgow Centre Aethalometer | Suburban \item
#' GN0 | Greenwich - A206 Burrage Grove | Roadside \item GN2 | Greenwich -
#' Millennium Village | Industrial \item GN3 | Greenwich - Plumstead High
#' Street | Roadside \item GN4 | Greenwich - Fiveways Sidcup Rd A20 | Roadside
#' \item GR4 | Greenwich - Eltham | Suburban \item GR5 | Greenwich - Trafalgar
#' Road | Roadside \item GR7 | Greenwich - Blackheath | Roadside \item GR8 |
#' Greenwich - Woolwich Flyover | Roadside \item GR9 | Greenwich - Westhorne
#' Avenue | Roadside \item HA0 | Harwell - Carbon | Rural \item HA1 | Harwell
#' Rural AURN | Rural \item HA2 | Harwell Rural PARTISOL | Rural \item HA4 |
#' Harwell Rural SMPS | Rural \item HA9 | Harwell - Partisol SO4 | Urban
#' Background \item HF1 | Hammersmith and Fulham - Broadway | Roadside \item
#' HF2 | Hammersmith and Fulham - Brook Green | Urban Background \item HF3 |
#' Hammersmith and Fulham - Scrubs Lane | Kerbside \item HG1 | Haringey -
#' Haringey Town Hall | Roadside \item HG2 | Haringey - Priory Park | Urban
#' Background \item HG3 | Haringey - Bounds Green | Roadside \item HI0 |
#' Hillingdon - Sipson Road | Suburban \item HI1 | Hillingdon - South Ruislip |
#' Roadside \item HI2 | Hillingdon - Hillingdon Hospital | Roadside \item HI3 |
#' Hillingdon - Oxford Avenue | Roadside \item HK4 | Hackney - Clapton | Urban
#' Background \item HK6 | Hackney - Old Street | Roadside \item HL1 | Halifax
#' Aethalometer | Urban Background \item HM1 | Hertsmere Borehamwood 1
#' (Background) | Urban Background \item HM4 | Hertsmere - Borehamwood | Urban
#' Background \item HO1 | Horsham Background | Urban Background \item HO2 |
#' Horsham - Park Way | Roadside \item HO4 | Horsham - Storrington | Roadside
#' \item HO5 | Horsham - Cowfold | Roadside \item HR1 | Harrow - Stanmore |
#' Urban Background \item HR2 | Harrow - Pinner Road | Roadside \item HS1 |
#' Hounslow - Brentford | Roadside \item HS2 | Hounslow - Cranford | Suburban
#' \item HS3 | Hounslow - Brentford | Roadside \item HS4 | Hounslow - Chiswick
#' High Road | Roadside \item HS5 | Hounslow - Brentford | Roadside \item HS6 |
#' Hounslow - Heston Road | Roadside \item HS7 | Hounslow - Hatton Cross |
#' Urban Background \item HS9 | Hounslow - Feltham | Roadside \item HT1 |
#' Hastings - Bulverhythe | Roadside \item HT2 | Hastings - Fresh Fields |
#' Roadside \item HV1 | Havering - Rainham | Roadside \item HV2 | Havering -
#' Harold Hill | Suburban \item HV3 | Havering - Romford | Roadside \item HX0 |
#' Birmingham Tyburn Aethalometer | Urban Background \item IC6 | City of London
#' - Walbrook Wharf Indoor | Roadside \item IG4 | Greenwich - Eltham Ecology
#' Centre Indoor | Urban Background \item IS1 | Islington - Upper Street |
#' Urban Background \item IS2 | Islington - Holloway Road | Roadside \item IS4
#' | Islington - Foxham Gardens | Urban Background \item IS5 | Islington -
#' Duncan Terrace | Roadside \item IS6 | Islington - Arsenal | Urban Background
#' \item IT2 | Tower Hamlets - Mile End Road | Roadside \item KB1 | South
#' Kirkby Aethalometer | Urban Background \item KC0 | North Kensington - Carbon
#' | Urban Background \item KC1 | Kensington and Chelsea - North Ken | Urban
#' Background \item KC2 | Kensington and Chelsea - Cromwell Road | Roadside
#' \item KC3 | Kensington and Chelsea - Knightsbridge | Roadside \item KC4 |
#' Kensington and Chelsea - Kings Road | Roadside \item KC5 | Kensington and
#' Chelsea - Earls Court Rd | Kerbside \item KC7 | Kensington and Chelsea -
#' North Ken FDMS | Urban Background \item KC9 | North Kensington - Partisol
#' SO4 | Urban Background \item KT1 | Kingston - Chessington | Suburban \item
#' KT2 | Kingston - Town Centre | Roadside \item LA1 | Luton Airport | Urban
#' Background \item LB1 | Lambeth - Christchurch Road | Roadside \item LB2 |
#' Lambeth - Vauxhall Cross | Roadside \item LB3 | Lambeth - Loughborough Junct
#' | Urban Background \item LB4 | Lambeth - Brixton Road | Kerbside \item LB5 |
#' Lambeth - Bondway Interchange | Roadside \item LB6 | Lambeth - Streatham
#' Green | Urban Background \item LH0 | Hillingdon - Harlington | Urban
#' Background \item LH2 | Heathrow Airport | Urban Background \item LL1 |
#' Lullington Heath Rural AURN | Rural \item LN1 | Luton - Challney Community
#' College | Urban Background \item LS1 | Lewes - Telscombe Cliffs | Roadside
#' \item LS2 | Lewes - Commercial Square | Roadside \item LS4 | Newhaven -
#' Denton School | Urban Background \item LW1 | Lewisham - Catford | Urban
#' Background \item LW2 | Lewisham - New Cross | Roadside \item LW3 | Lewisham
#' - Mercury Way | Industrial \item MA1 | Manchester Piccadilly CPC | Urban
#' Background \item MA2 | Manchester Piccadilly | Urban Background \item MD1 |
#' Mid Beds Biggleswade (Roadside) | Roadside \item MD2 | Mid Beds Silsoe
#' (Rural) | Rural \item MD3 | Central Beds - Sandy | Roadside \item MD4 |
#' Central Beds - Marston Vale | Rural \item ME1 | Merton - Morden Civic Centre
#' | Roadside \item MP1 | Marchwood Power - Marchwood | Industrial \item MP2 |
#' Marchwood Power - Millbrook Rd Soton | Industrial \item MR3 | Marylebone
#' Road Aethalometer | Kerbside \item MV1 | Mole Valley - Leatherhead | Rural
#' \item MV2 | Mole Valley - Lower Ashtead | Suburban \item MV3 | Mole Valley -
#' Dorking | Urban Background \item MW1 | Windsor and Maidenhead - Frascati Way
#' | Roadside \item MW2 | Windsor and Maidenhead - Clarence Road | Roadside
#' \item MW3 | Windsor and Maidenhead - Ascot | Rural \item MY0 | Marylebone
#' Road - Carbon | Kerbside \item MY1 | Westminster - Marylebone Road |
#' Kerbside \item MY7 | Westminster - Marylebone Road FDMS | Kerbside \item NA5
#' | Newtownabbey- Mallusk | Urban Background \item NA6 | Newtownabbey- Shore
#' Road | Roadside \item NE2 | Port Talbot TEOM and CPC | Urban Background
#' \item NF1 | New Forest - Holbury | Industrial \item NF2 | New Forest -
#' Fawley | Industrial \item NF3 | New Forest - Ringwood | Urban Background
#' \item NF4 | New Forest - Totton | Roadside \item NF5 | New Forest -
#' Lyndhurst | Roadside \item NH1 | North Herts Mobile - Baldock 1 | Roadside
#' \item NH2 | North Herts Mobile - Baldock 2 | Roadside \item NH3 | North
#' Herts Mobile - Royston | Urban Background \item NH4 | North Herts -
#' Breechwood Green | Urban Background \item NH5 | North Herts - Baldock
#' Roadside | Roadside \item NH6 | North Herts - Hitchin Library | Roadside
#' \item NK1 | North Kensington - CPC | Urban Background \item NK3 | North
#' Kensington Aethalometer | Urban Background \item NK6 | North Kensington -
#' URG | Urban Background \item NM1 | Newham - Tant Avenue | Urban Background
#' \item NM2 | Newham - Cam Road | Roadside \item NM3 | Newham - Wren Close |
#' Urban Background \item NW1 | Norwich Centre Aethalometer | Urban Background
#' \item OX0 | Oxford Centre Roadside AURN | Urban Background \item OX1 | South
#' Oxfordshire - Henley | Roadside \item OX2 | South Oxfordshire - Wallingford
#' | Roadside \item OX3 | South Oxfordshire - Watlington | Roadside \item OX4 |
#' Oxford St Ebbes AURN | Urban Background \item PO1 | Portsmouth Background
#' AURN | Urban Background \item PT6 | Port Talbot Dyffryn School | Industrial
#' \item RB1 | Redbridge - Perth Terrace | Urban Background \item RB2 |
#' Redbridge - Ilford Broadway | Kerbside \item RB3 | Redbridge - Fullwell
#' Cross | Kerbside \item RB4 | Redbridge - Gardner Close | Roadside \item RB5
#' | Redbridge - South Woodford | Roadside \item RD0 | Reading AURN - New Town
#' | Urban Background \item RD1 | Reading - Caversham Road | Roadside \item RD2
#' | Reading - Kings Road | Roadside \item RD3 | Reading - Oxford Road |
#' Roadside \item RG1 | Reigate and Banstead - Horley | Suburban \item RG2 |
#' Reigate and Banstead - Horley South | Suburban \item RG3 | Reigate and
#' Banstead - Poles Lane | Rural \item RG4 | Reigate and Banstead - Reigate
#' High St | Kerbside \item RHA | Richmond - Lower Mortlake Road | Roadside
#' \item RHB | Richmond - Lower Mortlake Road | Roadside \item RI1 | Richmond -
#' Castelnau | Roadside \item RI2 | Richmond - Barnes Wetlands | Suburban \item
#' RI5 | Richmond Mobile - St Margarets | Kerbside \item RI6 | Richmond Mobile
#' - St Margarets | Kerbside \item RI7 | Richmond Mobile - Richmond Park |
#' Suburban \item RI8 | Richmond Mobile - Richmond Park | Suburban \item RIA |
#' Richmond Mobile - George Street | Kerbside \item RIB | Richmond Mobile -
#' George Street | Kerbside \item RIC | Richmond Mobile - Kew Rd | Kerbside
#' \item RID | Richmond Mobile - Kew Rd | Kerbside \item RIE | Richmond Mobile
#' - Richmond Rd Twickenham | Roadside \item RIF | Richmond Mobile - Richmond
#' Rd Twickenham | Roadside \item RIG | Richmond Mobile - Upper Teddington Rd |
#' Roadside \item RIH | Richmond Mobile - Upper Teddington Rd | Roadside \item
#' RII | Richmond Mobile - Somerset Rd Teddington | Urban Background \item RIJ
#' | Richmond Mobile - Somerset Rd Teddington | Urban Background \item RIK |
#' Richmond Mobile - St. Margarets Grove | Urban Background \item RIL |
#' Richmond Mobile - St. Margarets Grove | Urban Background \item RIM |
#' Richmond Mobile - Petersham Rd Ham | Roadside \item RIN | Richmond Mobile -
#' Petersham Rd Ham | Roadside \item RIO | Richmond Mobile - Stanley Rd
#' Twickenham | Roadside \item RIP | Richmond Mobile - Stanley Rd Twickenham |
#' Roadside \item RIQ | Richmond Mobile - Richmond Rd Twickenham | Roadside
#' \item RIR | Richmond Mobile - Richmond Rd Twickenham | Roadside \item RIS |
#' Richmond Mobile - Lincoln Ave Twickenham | Roadside \item RIU | Richmond
#' Mobile - Mortlake Rd Kew | Roadside \item RIW | Richmond - Upper Teddington
#' Road | Roadside \item RIY | Richmond - Hampton Court Road | Kerbside \item
#' RO1 | Rochford - Rayleigh High Street | Roadside \item RY1 | Rother - Rye
#' Harbour | Rural \item RY2 | Rother - De La Warr Road | Roadside \item SA1 |
#' St Albans - Fleetville | Urban Background \item SB1 | South Beds - Dunstable
#' | Urban Background \item SC1 | Sevenoaks 1 | Suburban \item SD1 |
#' Southend-on-Sea AURN | Urban Background \item SE1 | Stevenage - Lytton Way |
#' Roadside \item SH1 | Southampton Background AURN | Urban Background \item
#' SH2 | Southampton - Redbridge | Roadside \item SH3 | Southampton - Onslow
#' Road | Roadside \item SH4 | Southampton - Bitterne | Urban Background \item
#' SK1 | Southwark - Larcom Street | Urban Background \item SK2 | Southwark -
#' Old Kent Road | Roadside \item SK5 | Southwark - A2 Old Kent Road | Roadside
#' \item SL1 | Sunderland Aethalometer | Urban Background  \item ST1 | Sutton -
#' Robin Hood School | Roadside \item ST2 | Sutton - North Cheam | Urban
#' Background \item ST3 | Sutton - Carshalton | Suburban \item ST4 | Sutton -
#' Wallington | Kerbside \item ST5 | Sutton - Beddington Lane | Industrial
#' \item ST6 | Sutton - Worcester Park | Kerbside \item ST7 | Sutton - Therapia
#' Lane | Industrial \item SU1 | Sussex Mobile10 Stockbridge | Kerbside \item
#' SU2 | Sussex Mobile11 Jct Whitley Rd | Kerbside \item SU3 | Sussex Mobile12
#' Cowfold | Kerbside \item SU4 | Sussex Mobile 13 Newhaven | Roadside \item
#' SU5 | Sussex Mobile 14 Crawley | Roadside \item SU6 | Sussex Mobile15
#' Chichester County Hall | Urban Background \item SU7 | Sussex Mobile 16
#' Warnham | Rural \item SU8 | Sussex Mobile 17 Newhaven Paradise Park |
#' Roadside \item SX1 | Sussex Mobile 1 | Urban Background \item SX2 | Sussex
#' Mobile 2 North Berstead | Roadside \item SX3 | Sussex Mobile 3 | Roadside
#' \item SX4 | Sussex Mobile 4 Adur | Roadside \item SX5 | Sussex Mobile 5
#' Fresh Fields Rd Hastings | Roadside \item SX6 | Sussex Mobile 6 Orchard St
#' Chichester | Roadside \item SX7 | Sussex Mobile 7 New Road Newhaven |
#' Roadside \item SX8 | Sussex Mobile 8 Arundel | Kerbside \item SX9 | Sussex
#' Mobile 9 Newhaven Kerbside | Kerbside \item TD0 | Richmond - National
#' Physical Laboratory | Suburban \item TE0 | Tendring St Osyth AURN | Rural
#' \item TE1 | Tendring - Town Hall | Roadside \item TH1 | Tower Hamlets -
#' Poplar | Urban Background \item TH2 | Tower Hamlets - Mile End Road |
#' Roadside \item TH3 | Tower Hamlets - Bethnal Green | Urban Background \item
#' TH4 | Tower Hamlets - Blackwall | Roadside \item TK1 | Thurrock - London
#' Road (Grays) | Urban Background \item TK2 | Thurrock - Purfleet | Roadside
#' \item TK3 | Thurrock - Stanford-le-Hope | Roadside \item TK8 | Thurrock -
#' London Road (Purfleet) | Roadside \item TR1 | Three Rivers - Rickmansworth |
#' Urban Background \item UT1 | Uttlesford - Saffron Walden Fire Station |
#' Roadside \item UT2 | Uttlesford - Takeley | Urban Background \item UT3 |
#' Uttlesford - Broxted Farm | Rural \item VS1 | Westminster - Victoria Street
#' | Kerbside \item WA1 | Wandsworth - Garratt Lane | Roadside \item WA2 |
#' Wandsworth - Town Hall | Urban Background \item WA3 | Wandsworth -
#' Roehampton | Rural \item WA4 | Wandsworth - High Street | Roadside \item WA6
#' | Wandsworth - Tooting | Roadside \item WA7 | Wandsworth - Putney High
#' Street | Kerbside \item WA8 | Wandsworth - Putney High Street Facade |
#' Roadside \item WA9 | Wandsworth - Putney | Urban Background \item WE0 |
#' Kensington and Chelsea - Pembroke Road | Urban Background \item WF1 |
#' Watford (Roadside) | Roadside \item WF2 | Watford - Watford Town Hall |
#' Roadside \item WH1 | Welwyn Hatfield - Council Offices | Urban Background
#' \item WL1 | Waltham Forest - Dawlish Road | Urban Background \item WL2 |
#' Waltham Forest - Mobile | Roadside \item WL3 | Waltham Forest - Chingford |
#' Roadside \item WL4 | Waltham Forest - Crooked Billet | Kerbside \item WL5 |
#' Waltham Forest - Leyton | Roadside \item WM0 | Westminster - Horseferry Road
#' | Urban Background \item WM3 | Westminster - Hyde Park Partisol | Roadside
#' \item WM4 | Westminster - Charing Cross Library | Roadside \item WM5 |
#' Westminster - Covent Garden | Urban Background \item WM6 | Westminster -
#' Oxford St | Kerbside \item WR1 | Bradford Town Hall Aethalometer | Urban
#' Background \item WT1 | Worthing - Grove Lodge | Kerbside \item XB1 |
#' Bletchley | Rural \item XS1 | Shukri Outdoor | Industrial \item XS2 | Shukri
#' Indoor | Industrial \item XS3 | Osiris mobile | Urban Background \item YH1 |
#' Harrogate Roadside | Roadside \item ZA1 | Ashford Rural - Pluckley | Rural
#' \item ZA2 | Ashford Roadside | Roadside \item ZA3 | Ashford Background |
#' Urban Background \item ZA4 | Ashford M20 Background | Urban Background \item
#' ZC1 | Chatham Roadside - A2 | Roadside \item ZD1 | Dover Roadside - Town
#' Hall | Roadside \item ZD2 | Dover Roadside - Townwall Street | Roadside
#' \item ZD3 | Dover Background - Langdon Cliff | Urban Background \item ZD4 |
#' Dover Background - East Cliff | Urban Background \item ZD5 | Dover Coast
#' Guard Met | Urban Background \item ZD6 | Dover Docks | Industrial \item ZF1
#' | Folkestone Suburban - Cheriton | Suburban \item ZG1 | Gravesham Backgrnd -
#' Northfleet | Urban Background \item ZG2 | Gravesham Roadside - A2 | Roadside
#' \item ZG3 | Gravesham Ind Bgd - Northfleet | Urban Background \item ZH1 |
#' Thanet Rural - Minster | Rural \item ZH2 | Thanet Background - Margate |
#' Urban Background \item ZH3 | Thanet Airport - Manston | Urban Background
#' \item ZH4 | Thanet Roadside - Ramsgate | Roadside \item ZL1 | Luton
#' Background | Urban Background \item ZM1 | Maidstone Meteorological | Urban
#' Background \item ZM2 | Maidstone Roadside - Fairmeadow | Kerbside \item ZM3
#' | Maidstone Rural - Detling | Rural \item ZR1 | Dartford Roadside - St
#' Clements | Kerbside \item ZR2 | Dartford Roadside 2 - Town Centre | Roadside
#' \item ZR3 | Dartford Roadside 3 - Bean Interchange | Roadside \item ZS1 |
#' Stoke Rural AURN | Rural \item ZT1 | Tonbridge Roadside - Town Centre |
#' Roadside \item ZT2 | Tunbridge Wells Background - Town Hall | Urban
#' Background \item ZT3 | Tunbridge Wells Rural - Southborough | Rural \item
#' ZT4 | Tunbridge Wells Roadside - St Johns | Roadside \item ZT5 | Tonbridge
#' Roadside 2 - High St | Roadside \item ZV1 | Sevenoaks - Greatness Park |
#' Urban Background \item ZV2 | Sevenoaks - Bat and Ball | Roadside \item ZW1 |
#' Swale Roadside - Ospringe A2 | Roadside \item ZW2 | Swale Background -
#' Sheerness | Urban Background \item ZW3 | Swale Roadside 2 - Ospringe Street
#' | Roadside \item ZY1 | Canterbury Backgrnd - Chaucer TS | Urban Background
#' \item ZY2 | Canterbury Roadside - St Dunstans | Roadside \item ZY4 |
#' Canterbury St Peters Place | Roadside }
#'
#' @inheritParams importAURN
#' @param site Site code of the network site to import e.g. "my1" is Marylebone
#'   Road. Several sites can be imported with \code{site = c("my1", "kc1")} ---
#'   to import Marylebone Road and North Kensignton for example.
#' @param met Should meteorological data be added to the import data? The
#'   default is \code{FALSE}. If \code{TRUE} wind speed (m/s), wind direction
#'   (degrees), solar radiation and rain amount are available. See details
#'   below.
#' @param meta Append the site type, latitude and longitude of each selected
#'   `site`? Defaults to `FALSE`.
#' @param units By default the returned data frame expresses the units in mass
#'   terms (ug/m3 for NOx, NO2, O3, SO2; mg/m3 for CO). Use \code{units =
#'   "volume"} to use ppb etc. PM10_raw TEOM data are multiplied by 1.3 and
#'   PM2.5 have no correction applied. See details below concerning PM10
#'   concentrations.
#' @param progress Show a progress bar when many sites/years are being imported?
#'   Defaults to `TRUE`.
#' @param extra Not currently used.
#' @export
#' @return Returns a data frame of hourly mean values with date in POSIXct
#'   class and time zone GMT.
#' @author David Carslaw and Ben Barratt
#' @family import functions
#' @examples
#' ## import all pollutants from Marylebone Rd from 1990:2009
#' \dontrun{mary <- importKCL(site = "my1", year = 2000:2009)}
#'
#' ## import nox, no2, o3 from Marylebone Road and North Kensignton for 2000
#' \dontrun{thedata <- importKCL(site = c("my1", "kc1"), year = 2000,
#' pollutant = c("nox", "no2", "o3"))}
#'
#' ## import met data too...
#' \dontrun{my1 <- importKCL(site = "my1", year = 2008, met = TRUE)}
importKCL <-
  function(site = "my1",
           year = 2009,
           pollutant = "all",
           met = FALSE,
           units = "mass",
           extra = FALSE,
           meta = FALSE,
           to_narrow = FALSE,
           progress = TRUE) {
    ## get rid of R check annoyances
    sites <- NULL
    v10 <- NULL
    v2.5 <- NULL

    site <- toupper(site)

    ## rows with these site codes
    ## this preserves order of site names
    con <-
      url((
        paste(
          "http://www.londonair.org.uk/r_data/",
          "sites",
          ".RData",
          sep = ""
        )
      ))
    load(con)
    close(con)

    id <-
      sapply(site, function(x)
        which(sites$SiteCode %in% toupper(x)))
    site.name <- sites$SiteName[id]

    ## RData files to import
    files <- lapply(site, function(x)
      paste(x, "_", year, sep = ""))
    files <- do.call(c, files)

    loadData <- function(x) {
      tryCatch({
        fileName <-
          paste("http://www.londonair.org.uk/r_data/",
                x,
                ".RData",
                sep = "")
        con <- url(fileName)
        load(con)

        ## need to check the date starts at start of year...
        start <- ISOdatetime(
          year = as.numeric(format(x$date[1], "%Y")),
          month = 1,
          day = 1,
          hour = 0,
          min = 0,
          sec = 0,
          tz = "GMT"
        )

        if (x$date[1] != start) {
          ## add first row
          x1 <- data.frame(date = start, site = x$site[1])
          x <- bind_rows(x1, x)
        }

        x <- date.pad(x, type = "site") ## pad out missing dates
        x
      },
      error = function(ex) {
        warning(x, "does not exist - ignoring that one.")
        NULL
      },
      finally = {
        close(con)
      })
    }

    if (progress)
      progress <- "Importing Air Quality Data"
    thedata <-
      purrr::map(files, loadData, .progress = progress) %>%
      purrr::list_rbind()

    if (is.null(thedata)) {
      warning("No data to import - check site codes and year.", call. = FALSE)
      return()
    }

    if (nrow(thedata) < 1) {
      warning("No data to import - check site codes and year.", call. = FALSE)
      return()
    }

    thedata$code <- thedata$site

    thedata$site <-
      factor(thedata$site, labels = site.name, levels = site)

    ## change names
    names(thedata) <- tolower(names(thedata))

    ## if particular pollutants have been selected
    if (!missing(pollutant)) {
      if (pollutant != "all") {
        thedata <- thedata[, c("date", pollutant, "site", "code")]
      }
    }

    ## change units to mass units, use values in ugm3Conversion table
    if (units == "mass") {
      if ("nox" %in% names(thedata))
        thedata$nox <- thedata$nox * 1.91
      if ("no2" %in% names(thedata))
        thedata$no2 <- thedata$no2 * 1.91
      if ("o3" %in% names(thedata))
        thedata$o3 <- thedata$o3 * 2.00
      if ("so2" %in% names(thedata))
        thedata$so2 <- thedata$so2 * 2.66
      if ("co" %in% names(thedata))
        thedata$co <- thedata$co * 1.16
      if ("pm10_raw" %in% names(thedata))
        thedata$pm10_raw <- thedata$pm10_raw * 1.30

      unitMessage <-
        "NOTE - mass units are used \nug/m3 for NOx, NO2, SO2, O3; mg/m3 for CO\nPM10_raw is raw data multiplied by 1.3\n"
    }

    ## rename PM volatile/non volatile components if present

    if ("pmfr" %in% names(thedata)) {
      thedata <- rename(thedata, v10 = pmfr)
      thedata <- transform(thedata, v10 = -1 * v10)
    }

    if ("p2fr" %in% names(thedata)) {
      thedata <- rename(thedata, v2.5 = p2fr)
      thedata <- transform(thedata, v2.5 = -1 * v2.5)
    }

    if ("pmfb" %in% names(thedata))
      thedata <- rename(thedata, nv10 = pmfb)
    if ("p2fb" %in% names(thedata))
      thedata <- rename(thedata, nv2.5 = p2fb)


    if (units != "mass") {
      if ("pm10" %in% names(thedata))
        thedata$pm10_raw <- thedata$pm10_raw * 1.30
      unitMessage <-
        "NOTE - volume units are used \nppbv for NOx, NO2, SO2, O3; ppmv for CO\nPM10_raw is raw data multiplied by 1.3\n"
    }

    ## don't add additional species
    if (!extra) {
      theNames <- c(
        "date",
        "co",
        "nox",
        "no2",
        "no",
        "o3",
        "so2",
        "pm10",
        "pm10_raw",
        "pm25",
        "v10",
        "v2.5",
        "nv10",
        "nv2.5",
        "code",
        "site"
      )
      thedata <- thedata[, which(names(thedata) %in% theNames)]
    }

    if (is.null(nrow(thedata))) {
      return()
    }

    ## warning about recent, possibly unratified data
    timeDiff <-
      difftime(Sys.time(), max(thedata$date), units = "days")
    if (timeDiff < 180) {
      warning("Some of the more recent data may not be ratified.")
    }

    if (met) {
      ## merge met data
      load(url(
        paste(
          "http://www.londonair.org.uk/r_data/",
          "metData",
          ".RData",
          sep = ""
        )
      ))
      # closeAllConnections()
      thedata <- merge(thedata, met, by = "date")
    }

    ## make sure it is in GMT
    attr(thedata$date, "tzone") <- "GMT"
    thedata <- thedata[order(thedata$site, thedata$date),]

    message(unitMessage)

    # add meta data
    if (meta) {
      meta_data <- importMeta(source = "kcl")
      # suppress warnings about factors
      thedata <-
        suppressWarnings(inner_join(thedata, meta_data, by = c("code", "site")))
    }

    if (to_narrow) {
      if (meta) {
        thedata <-
          pivot_longer(thedata,
                       -c(date, site, code, latitude, longitude, site.type),
                       names_to = "pollutant") %>%
          arrange(site, code, pollutant, date)
      } else {
        thedata <-
          pivot_longer(thedata,-c(date, site, code), names_to = "pollutant") %>%
          arrange(site, code, pollutant, date)
      }
    }

    return(as_tibble(thedata))
  }
