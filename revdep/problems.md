# openairmaps (0.9.1)

* GitHub: <https://github.com/davidcarslaw/openairmaps>
* Email: <mailto:jack.davison@ricardo.com>
* GitHub mirror: <https://github.com/cran/openairmaps>

Run `revdepcheck::revdep_details(, "openairmaps")` for more info

## Newly broken

*   checking Rd cross-references ... WARNING
     ```
     Missing link(s) in Rd file 'windroseMap.Rd':
       '[openair:drawOpenKey]{drawOpenKey()}'
     
     See section 'Cross-references' in the 'Writing R Extensions' manual.
     ```

# ReturnCurves (1.0.1)

* GitHub: <https://github.com/lidiamandre/ReturnCurves>
* Email: <mailto:lidia.andre@unamur.be>
* GitHub mirror: <https://github.com/cran/ReturnCurves>

Run `revdepcheck::revdep_details(, "ReturnCurves")` for more info

## Newly broken

*   checking for missing documentation entries ... WARNING
     ```
     Undocumented S4 methods:
       generic 'plot' and siglist 'adf_est.class'
       generic 'plot' and siglist 'adf_gof.class'
       generic 'plot' and siglist 'marggpd.class'
       generic 'plot' and siglist 'margtransf.class'
       generic 'plot' and siglist 'rc_est.class'
       generic 'plot' and siglist 'rc_gof.class'
       generic 'plot' and siglist 'rc_unc.class'
     All user-level objects in a package (including S4 classes and methods)
     should have documentation entries.
     See chapter 'Writing R documentation files' in the 'Writing R
     Extensions' manual.
     ```

# signifinder (1.12.0)

* GitHub: <https://github.com/CaluraLab/signifinder>
* Email: <mailto:stefania.pirrotta@phd.unipd.it>

Run `revdepcheck::revdep_details(, "signifinder")` for more info

## Newly broken

*   checking running R code from vignettes ...
     ```
       'signifinder.Rmd' using 'UTF-8'... failed
      ERROR
     Errors in running code in vignettes:
     when running code in 'signifinder.Rmd'
       ...
     
     > set.seed(21)
     
     > heatmapSignPlot(data = ovse, whichSign = highest_correlated, 
     +     clusterBySign = paste0("ConsensusOV_Chen_", c("IMR", "DIF", 
     +         "PRO", "M ..." ... [TRUNCATED] 
     
       When sourcing 'signifinder.R':
     Error: invalid 'yscale' in viewport
     Execution halted
     ```

## In both

*   R CMD check timed out


