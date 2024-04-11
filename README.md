# NEONSoils
NEONSoils is a repository code to acquire, tidy, and compute soil respiration fluxes at NEON sites using the flux-gradient method. 

**This is an early prototype package and should not be used for research purposes until scientifically validated.**

An online applet for visualization at select NEON sites is found [here](https://jmzobitz.shinyapps.io/NEON-soil-fluxes/)

## Installation 
Installation in R is done through the devtools package:

`devtools::install_github("jmzobitz/NEONSoils", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"),force=TRUE)`

## Usage
To analyze fluxes once the package is installed requires a two step process:

1. Load up the tidyverse and lubridate libraries (`library(tidyverse)` and `library(lubridate)`.  We are heavily making use of `dplyr` and `purrr`, so this should get you covered.

2. First acquire the NEON data, following conventions of `loadByProduct` function in the `neonUtilities` package.
` acquire_neon_data(site_name ="SJER",`
                  download_date = "2020-06",
                  data_file_name = "my-file-2020-06.Rda") `

3. Then process and compute fluxes.
` compute_neon_flux(input_file_name = "my-file-2020-06.Rda",
                                      out_flux_file_name = "my-file-flux-2020-06.Rda") `

If you encounter problems with code in this repository, feel free to post an [issue](https://github.com/jmzobitz/NEONSoils/issues).

## Credits & Acknowledgements
This is a project solely funded by the National Science Foundation, NSF# 2017829. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.

### Code Contributions / Changelog:
- (pre-2021): Initial code developed by Edward Ayres (eayres@battelleecology.org)
- 2021: Initial package development and testing by Zobitz and undergraduate researchers Lajntxiag Lee and Kathleen O'Rourke (both Augsburg University)
- 2022: Ongoing refinement and testing by undergraduate researchers at the following institutions:
  * Ridwan Abdi, Kebba Janeh, Nevin Lor, Diana Ortiz Alvarracin, Dijone Mehmeti, Ali Musa, Barbara Sabino Pina, Ly Xiong, Xeng Yang (Augsburg University)
  * Courtney Leung (Northwestern University)
 - 2024: Refinement to include a gapfilling routine from Zoey Werbin at Boston University as well as additional calculations of the soil flux via different approaches in Maier, M., and H. Schack-Kirchner. 2014. “Using the Gradient Method to Determine Soil Gas Flux: A Review.” Agricultural and Forest Meteorology 192–193 (July): 78–95. https://doi.org/10.1016/j.agrformet.2014.03.006.


## License
GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

## Disclaimer
Information and documents contained within this repository are available as-is. Codes or documents, or their use, may not be supported or maintained under any program or service and may not be compatible with data currently available from the NEON Data Portal.
