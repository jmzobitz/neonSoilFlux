# NEONSoils
NEONSoils is a repository hosting packages and code to analyze and process soil respiration fluxes at NEON sites.

## Installation 
Installation in R is done through the devtools package:

`devtools::install_github("jmzobitz/NEONSoils", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"),force=TRUE)`

## Usage
To analyze fluxes once the package is installed requires a two step process:

1. First acquire the NEON data, following conventions of `loadByProduct` function in the `neonUtilities` package.
` acquire_neon_data(site_name ="SJER",
                  start_date = "2020-06",
                  end_date = "2020-06",
                  file_name = "my-file-2020-06.Rda") `

2. Then process and compute fluxes.
` out_fluxes_jan <- compute_neon_flux("my-file-2020-06.Rda") `

If you encounter problems with code in this repository, feel free to post an [issue](https://github.com/jmzobitz/NEONSoils/issues).

## Credits & Acknowledgements
This is a project solely funded by the National Science Foundation. Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.

## License
GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

## Disclaimer
Information and documents contained within this repository are available as-is. Codes or documents, or their use, may not be supported or maintained under any program or service and may not be compatible with data currently available from the NEON Data Portal.
