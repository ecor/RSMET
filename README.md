# RSMET
It is an R Plug-in to Get Real-Time Meteorological Data in SMET Format containing a set of functions and methods to manage snow and weather local timeseries as provided by MeteoIO (http://models.slf.ch/docserver/meteoio/html/index.html,http://www.slf.ch/ueber/organisation/schnee_permafrost/projekte/MeteoIO/index_EN,https://www.openhub.net/p/MeteoIO). MeteoIO is a C/C++ OPen source library which "has been designed to accomodate both the needs of carefully crafted simulations for a specific purpose/study and for the needs of operational simulations that run automatically and unattended". It is integrated in physical spatially-distributed models and tackles several issues with weather input/output data. Here is a SMET S4 opject is defined and can be imported from/ exported to a SMET ini files of MeteoIO , allowing interoperability from R to MeteoIO and other SMET-compliant software.  

See DESCRIPTION file for moredetails/
Maintainer: Emanuele Cordano <emanuele.cordano@gmail.com> (www.rendena100.eu)
License: GPL (>= 2)
Type: R Package
Author: Emanuele Cordano
Maintainer: Emanuele Cordano <emanuele.cordano@gmail.com> (www.rendena100.eu)

## Installation

RSMET can be directly installed from R console: 
```{r eval=FALSE,echo=TRUE}
devtools::install_github("ecor/RSMET")
```
Otherwise package can be installed manually after clone or download.

## Documentation of SMET format

See the documents by Mathias Bavay for SMET specification whose a copy is here contained: https://github.com/ecor/RSMET/blob/master/inst/SMET_specifications.pdf


