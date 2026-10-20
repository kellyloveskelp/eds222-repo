library(tidyverse)
library(ggplot2)

# Read in the csv
soil <- read.csv('labs/data/exide-soil-cores.csv')

# 2. Distribution of soil core lead measurements
# Create a histogram to visualize the distribution of pb_ppm across all samples. 
# How does it compare to the California (80 ppm) and EPA (200 ppm) 
# thresholds for lead in residential soil?

soil_plot <- ggplot(soil, aes(pb_ppm)) +
  geom_histogram() +
  xlim(0, 1000) +
  geom_vline(xintercept = 80, color = 'red', linewidth = 0.5) +
  geom_vline(xintercept = 200, color = 'blue', linewidth = 0.5)
soil_plot

# Peak count of pb_ppm falls between 80 and 200 ppm
# However, significant sum of counts above 200 ppm

# 3. Distribution of property-level mean measurements
# Calculate the mean pb_ppm for each property. 
# What’s the distribution of these property-level means? 
# How does it compare to the distribution of individual soil cores from question 2?

property_mean <- soil |> 
  group_by(property_id) |> 
  mutate(pb_mean = mean(pb_ppm))

property_plot <- ggplot(property_mean, aes(pb_mean)) +
  geom_histogram() +
  xlim(0, 1000) +
  geom_vline(xintercept = 80, color = 'red', linewidth = 0.5) +
  geom_vline(xintercept = 200, color = 'blue', linewidth = 0.5)
property_plot

# Distribution is skewed right, demonstrating a higher average ppm than the sum.

# 4. Property-level variability
# How variable are soil core lead measurements at the property level? 
# Plot the distribution of property-level standard deviations of pb_ppm. 
# What does your plot suggest about the reliability of soil lead measurements?

property_stdev <- soil |> 
  group_by(property_id) |> 
  mutate(pb_stdev = sd(pb_ppm))

property_dev <- ggplot(property_stdev, aes(pb_stdev)) +
  geom_histogram() +
  xlim(0, 1000) +
  geom_vline(xintercept = 80, color = 'red', linewidth = 0.5) +
  geom_vline(xintercept = 200, color = 'blue', linewidth = 0.5)
property_dev


# 5. Lead measurements by location
# What is the distribution of pb_ppm for drip line samples (location_code == "DL") versus everything else (the yard samples)? 
# How much higher do drip line measurements appear to be relative to yard measurements?

drip_line <- soil |> 
  filter(location_code == "DL")

yard_samples <- soil |> 
  filter(!location_code == "DL")

ggplot(drip_line, aes(pb_ppm)) +
  geom_histogram() +
  xlim(0, 1000) +
  geom_vline(xintercept = 80, color = 'red', linewidth = 0.5) +
  geom_vline(xintercept = 200, color = 'blue', linewidth = 0.5)

ggplot(yard_samples, aes(pb_ppm)) +
  geom_histogram() +
  xlim(0, 1000) +
  geom_vline(xintercept = 80, color = 'red', linewidth = 0.5) +
  geom_vline(xintercept = 200, color = 'blue', linewidth = 0.5)

# 6. Drip line vs. yard, at the property level
# Part 5 looked at the aggregate distribution across all samples. 
# Now let’s look at drip line vs. yard within each property.

# a. Create a new column called is_dripline: 
# a logical column that’s TRUE for drip line locations and FALSE otherwise.

# b. Group the data by property and is_dripline, 
# and calculate the mean soil lead concentration in each group.

# c. For each property, calculate the difference between the mean drip line measurement 
# and the mean non-drip-line (yard) measurement. Hint: pivoting will help!

# d. Plot the distribution of the property-level drip line/yard differences.