# neonSoilFlux
neonSoilFlux is a repository code to acquire, tidy, and compute soil respiration fluxes at NEON sites using the flux-gradient method. 

An online applet for visualization at select NEON sites is found [here](https://jmzobitz.shinyapps.io/NEON-soil-fluxes/)

## Installation 
Installation in R through:

- CRAN: `install.packages(neonSoilFlux)`
- Github using the devtools package:

`devtools::install_github("jmzobitz/neonSoilFlux", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"),force=TRUE)`

If you encounter problems with code in this repository, feel free to post an [issue](https://github.com/jmzobitz/neonSoilFlux/issues).

## Usage
To analyze fluxes once the package is installed requires a two step process:

1. Load up the tidyverse and lubridate libraries (`library(tidyverse)` and `library(lubridate)`.  We are heavily making use of `dplyr` and `purrr`, so this should get you covered.

2. First acquire the NEON data, following conventions of `loadByProduct` function in the `neonUtilities` package.
` out_env_data <- acquire_neon_data(site_name = 'SJER',
                  download_date = '2021-06',
                  ) `

3. Then process and compute fluxes.
` out_fluxes <- compute_neon_flux(input_site_env = out_env_data$site_data,
                  input_site_megapit = out_env_data$site_megapit
                  ) `


You now have a data frame of computed fluxes.

More detailed usage information is given in the vignette `using-neonSoilFlux` (`vignette(using-neonSoilFlux)`).

## Credits & Acknowledgements
The package `neonSoilFlux` was funded with support from the National Science Foundation, grant \#2017829. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation. 

The National Ecological Observatory Network is a program sponsored by the National Science Foundation and operated under cooperative agreement by Battelle. This material is based in part upon work supported by the National Science Foundation through the NEON Program.

### Code Contributions / Changelog:
- (pre-2021): Initial code developed by Edward Ayres (eayres@battelleecology.org)
- 2021: Initial package development and testing by Zobitz and undergraduate researchers Lajntxiag Lee and Kathleen O'Rourke (both Augsburg University)
- 2022: Ongoing refinement and testing by undergraduate researchers at the following institutions:
  * Ridwan Abdi, Kebba Janeh, Nevin Lor, Diana Ortiz Alvarracin, Dijone Mehmeti, Ali Musa, Barbara Sabino Pina, Ly Xiong, Xeng Yang (Augsburg University)
  * Courtney Leung (Northwestern University)
 - 2024: Refinement to include a gapfilling routine from Zoey Werbin at Boston University as well as additional calculations of the soil flux via different approaches in Maier, M., and H. Schack-Kirchner. 2014. “Using the Gradient Method to Determine Soil Gas Flux: A Review.” Agricultural and Forest Meteorology 192–193 (July): 78–95. https://doi.org/10.1016/j.agrformet.2014.03.006.
 - 05.2024 Renaming of pacakge to neonSoilFlux from NEONSoils and associated updates to prepare for CRAN submission.


## License
GNU Affero General Public License Version 3, 19 November 2007

## Disclaimer
Information and documents contained within this repository are available as-is. Codes or documents, or their use, may not be supported or maintained under any program or service and may not be compatible with data currently available from the NEON Data Portal.

<!-- badges: start -->
[![R-CMD-check](https://github.com/jmzobitz/neonSoilFlux/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jmzobitz/neonSoilFlux/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
