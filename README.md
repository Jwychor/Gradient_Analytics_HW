# Gradient Analytics Analysis
An R and Rmarkdown package analyzing data for Gradient Analytics.

## Installation
The following code will install the package on your machine.

##### NOTE: This may take a long time given some of the computation times.
```
if(!'remotes' %in% installed.packages()){
  install.packages('remotes', dependencies = TRUE)
}
if(!'rlang' %in% installed.packages()){
  remotes::install_github('r-lib/rlang', dependencies = TRUE)
}
remotes::install_github("Jwychor/Gradient_Analytics_HW",
                         build_vignettes = T)
```
## Vignette
To view the vignette in your browser, run the following:
```
browseVignettes('GAnalysis')
```
Alternatively, view the vignette on Rpubs [here.](https://rpubs.com/jwychor/GAnalytics)
