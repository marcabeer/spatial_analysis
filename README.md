# Spatial Analysis
## Marc A. Beer
This repository contains custom functions and scripts for processing and analyzing spatial data.

## Scripts
- <strong>nearest_nonNA.R</strong>: Precise geographic coordinates may point to locations not underlain by environmental data in a raster, for example due to recording error, due to low spatial resolution at geographic boundaries (e.g., coasts), or simply because environmental data have not been measured at those locations. <i>When sensible</i>, one may wish to identify the raster cell(s) containing non-missing data that is closest to the geographic coordinate(s) of interest. This script contains a function that does just that, along with an example using simulated data.
