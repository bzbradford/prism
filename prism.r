
# For handling weather data acquired from http://prism.oregonstate.edu/explorer/bulk.php
# Combines prism csvs into single file

library(tidyverse)


# GDD functions -----------------------------------------------------------

# simple min/max
gdd_simple <- function(tmin, tmax, lower = 50, upper = 86) {
    pmax(0, (pmax(tmin, lower) + pmin(tmax, upper)) / 2 - lower)
  }

gdd_sine <- function(min, max, base, upper) {
  if (length(min) > 1 || length(max) > 1) stop("Function must be called with mapply for use with vectors")
  if (min > max) stop("Min cannot be greater than Max")
  if (base > upper) stop("Base cannot be greater than Upper")
  
  # min and max > upper
  if (min >= upper) return(upper - base)
  
  # min and max < lower
  if (max <= base) return(0)
  
  average = (min + max) / 2
  
  # min and max between base and upper
  if (max <= upper && min >= base) return(average - base)
  
  alpha = (max - min) / 2
  
  # min < base, max between base and upper
  if (max <= upper && min < base) {
    base_radians = asin((base - average) / alpha)
    a = average - base
    b = pi / 2 - base_radians
    c = alpha * cos(base_radians)
    return((1 / pi) * (a * b + c))
  }
  
  # max > upper and min between base and upper
  if (max > upper && min >= base) {
    upper_radians = asin((upper - average) / alpha)
    a = average - base
    b = upper_radians + pi / 2
    c = upper - base
    d = pi / 2 - upper_radians
    e = alpha * cos(upper_radians)
    return((1 / pi) * (a * b + c * d - e))
  }
  
  # max > upper and min < base
  if (max > upper && min < base) {
    base_radians = asin((base - average) / alpha)
    upper_radians = asin((upper - average) / alpha)
    a = average - base
    b = upper_radians - base_radians
    c = alpha * (cos(base_radians) - cos(upper_radians))
    d = upper - base
    e = pi / 2 - upper_radians
    return((1 / pi) * ((a * b + c) + (d * e)))
  }
}



# Load climate data -------------------------------------------------------

# read in PRISM climate data
prism_in <- 
  list.files("antigo", full.names = T) %>%
  map_dfr(read_csv, skip = 10) %>%
  rename(
    "tmin_f" = `tmin (degrees F)`,
    "tmax_f" = `tmax (degrees F)`
  ) %>%
  select(c(Name, Longitude, Latitude, Date, tmin_f, tmax_f))



# Compute GDD accumulations -----------------------------------------------

# simple method
df_simple <-
  prism_in %>%
  mutate(
    Year = format(Date, "%Y"),
    Day = as.numeric(format(Date, "%j"))) %>%
  group_by(Year) %>%
  arrange(Date) %>%
  mutate(GDD = gdd_simple(tmin_f, tmax_f, 39, 86)) %>%
  mutate(GDD_cum = cumsum(GDD))


# sine method
df_sine <-
  prism_in %>%
  mutate(
    Year = format(Date, "%Y"),
    Day = as.numeric(format(Date, "%j"))) %>%
  group_by(Year) %>%
  arrange(Date) %>%
  mutate(GDD = mapply(gdd_sine, tmin_f, tmax_f, 39, 86)) %>%
  mutate(GDD_cum = cumsum(GDD))


# both methods
df_both <-
  prism_in %>%
  mutate(
    Year = format(Date, "%Y"),
    Day = as.numeric(format(Date, "%j"))) %>%
  group_by(Year) %>%
  arrange(Date) %>%
  mutate(
    GDD_simple = gdd_simple(tmin_f, tmax_f, 39, 86),
    GDD_sine = mapply(gdd_sine, tmin_f, tmax_f, 39, 86),
    GDD_simple_cum = cumsum(GDD_simple),
    GDD_sine_cum = cumsum(GDD_sine))



# Plot GDDs ---------------------------------------------------------------

df_both %>%
  ggplot(aes(x = Day, color = Year, group = Year)) +
  geom_ribbon(aes(ymin = GDD_sine_cum, ymax = GDD_simple_cum), alpha = .75) +
  labs(
    x = "Day of year", y = "GDD accumulation, simple (top) vs sine (bottom)",
    title = "Degree-day accumulation")

# compare simple vs sine
df_both %>%
  ggplot(aes(x = Day)) +
  geom_line(aes(y = GDD_simple_cum), color = "black", size = 1) +
  geom_line(aes(y = GDD_sine_cum), color = "red", size = 1) +
  facet_wrap(~ Year) +
  labs(
    x = "Day of year", y = "GDD accumulation",
    title = "Degree-day accumulation method comparison (simple = black, sine = red)")

