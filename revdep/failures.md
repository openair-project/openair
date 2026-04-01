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


