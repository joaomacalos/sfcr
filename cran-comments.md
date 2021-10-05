## Resubmission
This is a resubmission.

Change requested:
  
*  Thanks, we see:
  
     Found the following (possibly) invalid URLs:
       URL: https://app.travis-ci.com/joaomacalos/sfcr.svg?branch=main
  (moved to https://api.travis-ci.com/joaomacalos/sfcr.svg?branch=main)
         From: README.md
         Status: 301
         Message: Moved Permanently
         
Problem fixed.


## Test environments
* local Ubuntu 20.04.1 LTS, R 4.0.3
* local Windows 10 x64, R 4.0.3

## R CMD check results (Local Ubuntu 20.04.1)
There were no ERRORs or WARNINGs or NOTEs.

## R CMD check results (Local Windows 10x64)
There were no ERRORs or WARNINGs. 
  
## Existing CRAN checks issues:
There was a NOTE being raised at https://cran.rstudio.com//web/checks/check_results_sfcr.html:
checking LazyData ... NOTE
  'LazyData' is specified without a 'data' directory
  
I fixed this issue by removing the line `LazyData: true` from the DESCRIPTION file since the sfcr package has no data/ directory.
