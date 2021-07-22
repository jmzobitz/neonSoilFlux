library(tidyverse)
#Test here
### Just compare studies we have that match the latitude and longitude by NEON core sites

neon_sites <- read_csv('neonTerrestrialSites.csv') %>%
  mutate(n_studies_srdb = 0,n_studies_cosore=0)

srdb <- read_csv('../SRDB_V4_1578/data/srdb-data-V4.csv')


### Load in COSORE database:
cosore <- readRDS("../cosore-0.2.0/cosore_data.RDS")
csr_table <- function(cosore, table_name) {

  extract <- function(x, table_name) {
    if(is.null(x[[table_name]])) { return(NULL) }
    # Add an identifier field so we can track things as tables get combined
    x[[table_name]]$CSR_DATASET <- x$description$CSR_DATASET
    x[[table_name]]
  }

  dplyr::bind_rows(lapply(cosore, extract, table_name = table_name))
}

cosore_description <- csr_table(cosore, "description")


for(i in seq_along(neon_sites)) {

  n_vals <- srdb %>% filter(abs(Longitude-neon_sites$Longitude[i])<0.25 & abs(Latitude-neon_sites$Latitude[i])<0.25)  %>% summarize(tot=n()) %>% pull(tot)

  neon_sites$n_studies_srdb[i] <- n_vals

  n_vals <- cosore_description %>% filter(abs(CSR_LONGITUDE-neon_sites$Longitude[i])<0.25 & abs(CSR_LATITUDE-neon_sites$Latitude[i])<0.25)  %>% summarize(tot=n()) %>% pull(tot)

  neon_sites$n_studies_cosore[i] <- n_vals


}


site_summary <- neon_sites %>% select(`Site Name`,n_studies_srdb,n_studies_cosore) %>% arrange(desc(n_studies_srdb))
