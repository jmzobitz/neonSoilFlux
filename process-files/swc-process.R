# 7/25/21
# Code to process a graph of soil water content with the computed flux data

# Download and access soil water data from NEON

# Name of the file where you have the data saved for a particular NEON timeperiod
file_name <- "process-files/my-file.Rda"


# I am assuming that you have already computed the fluxes for the timeperiod - I saved them with the variable out_fluxes

# I want to get the names of the horizontal positions where I computed the fluxes
horizontal_position <- unique(out_fluxes$horizontalPosition)

# Just get the soil water content, filtering on the horizontal locations where the fluxes are computed
swc <- site_swc$SWS_30_minute %>%
  filter(horizontalPosition %in% horizontal_position)

# Make a plot of the soil water content.  We are going to facet this plot by the horizontal position
swc %>%
  ggplot(aes(x=startDateTime,y=VSWCMean,color=verticalPosition)) + geom_line() + facet_grid(horizontalPosition ~ .)

