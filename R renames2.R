install.packages("ggplot2")
install.packages("tidyverse")
install.packages("readxl")
install.packages("dplyr")
install.packages("vctrs")
install.packages("tidyr")
install.packages("RColorBrewer")
install.packages("vegan")
install.packages("RVAideMemoire")
library(RColorBrewer)
library(tidyr)
library(ggplot2)
library(vegan)
library(tidyverse)
library(readxl)
library(dplyr)
library(vctrs)
library(vegan)
library(RVAideMemoire)
library(RColorBrewer)
library(tidyr)
library(ggplot2)
library(vegan)
library(tidyverse)
library(readxl)
library(dplyr)
library(vctrs)
library(vegan)
library(RVAideMemoire)


# In this code, North refers to the northern tundra heathlands of Lundy Island.
# South refers to the southern lowland heathlands of Lundy Island.



# Creating a graph to compare fungal community compositions at the class level.

test <- read_csv("2neworderclass_data.csv", col_types = cols( #Reading in the data of all classes and there proportions
  class = col_character(),
  .default = col_number()
))


dat <- test %>% # Pivoting the data longer so it can be assessed appropriately.
  pivot_longer(
    cols = -class, # "Class" is the column to keep as is.
    names_to = "sampleid", # Naming the column of sample locations as "sampleid".
    values_to = "count" # Naming the proportion of fungi for each sample location as "count".
    
  )
head(dat) #View dat.


dat2 <- dat %>%
  mutate(location = substr(sampleid, 1, 1)) #Extracting the first letter of each sample ID to form the column "location".



dat3 <- dat %>% # Extracting the second letter of the sample ID to label the location of each sample as either "North" or "South".
  mutate(
    location = substr(sampleid, 1, 1),
    map = case_when(
  
      
      TRUE ~ "Unknown" # Default case if the letter does not match any of the specified ones; none were found.
    )
  )

dat3 %>%
  ggplot(aes(x = sampleid, y = count)) + # Creating the first plot; classes are in alphabetical order rather than size order.
  facet_grid(~ location + map, scales = "free_x", space = "free_x") +
  geom_bar(aes(fill = class), stat = "identity", position = "fill", width = 1) +
  scale_fill_brewer(palette = "Greens")

# Define the first 12 colours.
custom_palette <- c(
  "lightblue", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#599999",
  "#66C2A5", "#FC8D62", "#8DA0CB" # creating a custom palette for the graph
)

# Creating 22 grey scale colours for smaller classes.
grayscale_colors <- gray.colors(22, start = 0.1, end = 0.9)

# Combine the colour palettes of custom colours and grey scale colours.
custom_palette <- c(custom_palette, grayscale_colors)

# Print the custom palette to ensure colours are in the correct order.
print(custom_palette)


dat3 %>%
  ggplot(aes(x = sampleid, y = count)) +
  facet_grid(~ location + map, scales = "free_x", space = "free_x") +
  geom_bar(aes(fill = class), stat = "identity", position = "fill", width = 1) +
  scale_fill_manual(values = custom_palette) +
  theme_minimal() # Visualise how the new custom plot looks.


# Remove rows with NA values in the "count" column.
dat3_clean <- dat3 %>%
  filter(!is.na(count))

# Check the summary again to confirm that NA values have been removed.
summary(dat3_clean$count)

# Summarise the counts for each phylum within each sample ID.
phyl_count_df <- dat3_clean %>%
  # Group the data by 'sampleid' and 'class' (representing phylum or class).
  group_by(sampleid, class) %>%
  # Summarise the counts for each combination of 'sampleid' and 'class'.
  summarise(phyl_count = sum(count), .groups = 'drop') %>%
  # Drop the grouping after summarising.
  ungroup()

# Print the summarised data frame showing total counts for each phylum per sample.
print(phyl_count_df)

# Calculate the frequency of each phylum within each sample.
phyl_freq_df <- phyl_count_df %>%
  # Group the data by 'sampleid' to compute relative frequencies within each sample.
  group_by(sampleid) %>%
  # Calculate the frequency of each phylum by dividing its count by the total count for that sample.
  mutate(phyl_freq = phyl_count / sum(phyl_count)) %>%
  # Ensure that the grouping is dropped after the mutation.
  ungroup()

# Print the data frame with frequency calculations for each phylum per sample.
print(phyl_freq_df)

# Calculate the mean frequencies.
mean_freqs <- phyl_freq_df %>%
  ungroup() %>%
  group_by(class) %>%
  summarise(mean = mean(phyl_freq, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(mean))

# Print the result.
print(mean_freqs)


# Change the `max.print` option temporarily to display all rows.
print(mean_freqs, n = 100)



phyla_order <- c("unclassified",  #order the phyla in order of size
                 "Lecanoromycetes",
                 "Agaricomycetes",
                 "Eurotiomycetes",
                 "Leotiomycetes",
                 "Archaeorhizomycetes",
                 "Sordariomycetes",
                 "Mortierellomycetes",
                 "Dothideomycetes",
                 "Tremellomycetes",
                 "Microbotryomycetes",
                 "Umbelopsidomycetes",
                 "Endogonomycetes",
                 "Pezizomycetes",
                 "Mucoromycetes",
                 "Cystobasidiomycetes",
                 "Rhizophydiomycetes",
                 "Atractiellomycetes",
                 "Chytridiomycetes",
                 "Malasseziomycetes",
                 "Spizellomycetes",
                 "Rozellidea",
                 "Tritirachiomycetes",
                 "Orbiliomycetes",
                 "Agaricostilbomycetes",
                 "Saccharomycetes",
                 "Pucciniomycetes",
                 "Glomeromycetes",
                 "Basidiobolomycetes",
                 "Candelariomycetes",
                 "Arthoniomycetes",
                 "Geoglossomycetes",
                 "Olpidiomycetes"
)


dat4 <- dat3 %>%
  mutate(class = factor(class, levels = phyla_order))
dat4



dat4 %>%
  ggplot(aes(x = sampleid, y = count)) + # Final plot with correct colours and in size order.
  facet_grid(~ location + map, scales = "free_x", space = "free_x") +
  geom_bar(aes(fill = class), stat = "identity", position = "fill", width = 1) +
  scale_fill_manual(values = custom_palette) +
  theme_minimal()




dat4 %>%  # The same plot but with alterations to text, number, and axis sizes.
  ggplot(aes(x = sampleid, y = count)) +
  facet_grid(~ location + map, scales = "free_x", space = "free_x") +
  geom_bar(aes(fill = class), stat = "identity", position = "fill", width = 1) +
  scale_fill_manual(values = custom_palette) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 20),
    axis.text.y = element_text(size = 20),  # Adjust y-axis text size.
    axis.title.x = element_text(size = 24), # Change x-axis label font size.
    axis.title.y = element_text(size = 24), # Change y-axis label font size.
    strip.text = element_text(size = 24),   # Change facet strip text size.
    legend.key.size = unit(1, 'cm'),
    legend.text = element_text(size = 20)
  ) +
  labs(
    x = "Sample Name",
    y = "Proportion of fungi within the community"
  )




####### All Compared: North vs South - Soil


test <- read_csv("sp_data.csv", col_types = cols(
  speices = col_character(),
  .default = col_number()
))  # Reading in the community composition data for lichen, soil, and roots.

dat <- test %>% #transforming the data
  pivot_longer(
    cols = -speices, # # Assuming 'speices' is the column to keep as is.
    
    names_to = "sampleid",
    values_to = "count"
  )
head(dat)



dat3 <- dat %>%
  mutate(
    location = substr(sampleid, 1, 1), # Creating a new column called 'map', and taking the third character from 'sample ID' to correspond each sample to either North or South.
    
    map = case_when(
      substr(sampleid, 3, 3) == "C" ~ "Lowland",
      substr(sampleid, 3, 3) == "S" ~ "Lowland",
      substr(sampleid, 3, 3) == "N" ~ "Tundra",
      
      TRUE ~ "Unknown" # Default case if the letter does not match any of the specified ones; no unknowns were found.
    )
  )

dat3 <- dat3[!grepl("^L", dat3$sampleid), ]  # Removing the lichen community.
dat3 <- dat3[!grepl("^R", dat3$sampleid), ] # Removing the roots community.



# Now only the soil community remains.

aggregated_data <- dat3 %>%
  group_by(speices, sampleid, map) %>% # Group the data by the specified columns: species, sample ID, and map.
  summarise(count = sum(count)) %>%
  ungroup()   # Remove the grouping structure to return to a regular data frame.



# Pivot the data to have values as column headers.
lresult <- aggregated_data %>%
  pivot_wider(names_from = speices, values_from = count, values_fill = list(count = 0))


lresult <- lresult[, !colnames(lresult) %in% "NA"]# There is an NA column for some reason and it needs to be removed.

# Conducting an NMDS and a PERMANOVA for soil.



perm_data <- lresult[, 3:ncol(lresult)] # Extract all columns from the 3rd to the last column of 'lresult' and assign it to 'perm_data'.
perm_data_trans <- sqrt(perm_data) # Apply the square root function to each element in 'perm_data'.

nmds_results <- metaMDS(perm_data_trans, distance = "bray") # NMDS with Bray-Curtis dissimilarity as the distance measure.

nmds_scores <- scores(nmds_results) #Extracting scores.
plot(nmds_results)

perm_dist <- vegdist(perm_data_trans, method = "bray")# The 'vegdist' function calculates dissimilarity or distance between observations.

# Perform a beta-diversity dispersion analysis to assess the variation in dissimilarity within groups.
# 'perm_dist' is the dissimilarity matrix calculated using the Bray-Curtis index.



dispersionperamiter <- betadisper(perm_dist, group = lresult$map, type = "centroid")
plot(dispersionperamiter)
soilplot <- dispersionperamiter

# Perform pairwise comparisons with 9999 permutations.
pairwise_permanova <- pairwise.perm.manova(perm_dist, lresult$map, nperm = 9999)

# View the PERMANOVA results.
print(pairwise_permanova)

plot(soilplot)


####### ALL COMPARED NORTH VS SOUTH - Lichen




test <- read_csv("sp_data.csv", col_types = cols(
  speices = col_character(),
  .default = col_number()
)) # Reading in the community composition data for lichen, soil, and roots, this is done for a second time
# as to reset.

dat <- test %>% #Transforming the data
  pivot_longer(
    cols = -speices, # Assuming 'speices' is the column to keep as is.
    names_to = "sampleid",
    values_to = "count"
  )
head(dat)



dat3 <- dat %>% # Creating a new column called 'map', and taking the third character from 'sample ID' to correspond each sample to either North or South.
  
  mutate(
    location = substr(sampleid, 1, 1),
    map = case_when(
      substr(sampleid, 3, 3) == "C" ~ "Lowland",
      substr(sampleid, 3, 3) == "S" ~ "Lowland",
      substr(sampleid, 3, 3) == "N" ~ "Tundra",
      
      TRUE ~ "Unknown"  # Default case if the letter does not match any of the specified ones; no unknowns were found.
      
    )
  )

dat3 <- dat3[!grepl("^S", dat3$sampleid), ]   # Removing the soil community.

dat3 <- dat3[!grepl("^R", dat3$sampleid), ]   # Removing the roots community.

#Now all that is left is lichen.


aggregated_data <- dat3 %>%
  group_by(speices, sampleid, map) %>% # Group the data by the specified columns: species, sample ID, and map.
  summarise(count = sum(count)) %>%
  ungroup()  # Remove the grouping structure to return to a regular data frame.

# Pivot the data to have values as column headers.
lresult <- aggregated_data %>%
  pivot_wider(names_from = speices, values_from = count, values_fill = list(count = 0))


# Conducting an NMDS and a PERMANOVA for lichen.



lresult <- lresult[, !colnames(lresult) %in% "NA"]

perm_data <- lresult[, 3:ncol(lresult)]  # Extract all columns from the 3rd to the last column of 'lresult' and assign it to 'perm_data'.
perm_data_trans <- sqrt(perm_data) # Apply the square root function to each element in 'perm_data'.


nmds_results <- metaMDS(perm_data_trans, distance = "bray") # NMDS with Bray-Curtis dissimilarity as the distance measure.

nmds_scores <- scores(nmds_results) #Extracting scores.
plot(nmds_results)


perm_dist <- vegdist(perm_data_trans, mthod = "bray") # The 'vegdist' function calculates dissimilarity or distance between observations.

# Perform a beta-diversity dispersion analysis to assess the variation in dissimilarity within groups.
# 'perm_dist' is the dissimilarity matrix calculated using the Bray-Curtis index.


dispersionperamiter <- betadisper(perm_dist, group = lresult$map, type = "centroid")
plot(dispersionperamiter) 
lichenplot <- dispersionperamiter





# Perform pairwise comparisons with 9999 permutations.
pairwise_permanova <- pairwise.perm.manova(perm_dist, lresult$map, nperm = 9999)

# View the PERMANOVA results.
print(pairwise_permanova)












####### ALL COMPARED NORTH VS SOUTH - Root




test <- read_csv("sp_data.csv", col_types = cols(
  speices = col_character(),
  .default = col_number()
)) # Reading in the community composition data for lichen, soil, and roots, this is done for a second time
# as to reset.

dat <- test %>%  #Transforming the data.
  pivot_longer(
    cols = -speices,# Assuming 'speices' is the column to keep as is.
    names_to = "sampleid",
    values_to = "count"
  )
head(dat)



dat3 <- dat %>% # Creating a new column called 'map', and taking the third character from 'sample ID' to correspond each sample to either North or South.
  
  mutate(
    location = substr(sampleid, 1, 1),
    map = case_when(
      substr(sampleid, 3, 3) == "C" ~ "Lowland",
      substr(sampleid, 3, 3) == "S" ~ "Lowland",
      substr(sampleid, 3, 3) == "N" ~ "Tundra",
      
      TRUE ~ "Unknown" # Default case if the letter does not match any of the specified ones; no unknowns were found.
      
    )
  )

dat3 <- dat3[!grepl("^S", dat3$sampleid), ] #Removing the soil community.
dat3 <- dat3[!grepl("^L", dat3$sampleid), ] #Removing the lichen community.



aggregated_data <- dat3 %>%
  group_by(speices, sampleid, map) %>% # Group the data by the specified columns: species, sample ID, and map.
  summarise(count = sum(count)) %>%
  ungroup() # Remove the grouping structure to return to a regular data frame.


# Pivot the data to have values as column headers.
lresult <- aggregated_data %>%
  pivot_wider(names_from = speices, values_from = count, values_fill = list(count = 0))

# Conducting an NMDS and a PERMANOVA for roots.


# Assuming your data frame is called 'data5'
lresult <- lresult[, !colnames(lresult) %in% "NA"]

perm_data <- lresult[, 3:ncol(lresult)] # Extract all columns from the 3rd to the last column of 'lresult' and assign it to 'perm_data'.
perm_data_trans <- sqrt(perm_data) # Apply the square root function to each element in 'perm_data'.


nmds_results <- metaMDS(perm_data_trans, distance = "bray")  # NMDS with Bray-Curtis dissimilarity as the distance measure.
nmds_scores <- scores(nmds_results) #Extracting scores.
plot(nmds_results)

perm_dist <- vegdist(perm_data_trans, mthod = "bray") # The 'vegdist' function calculates dissimilarity or distance between observations.

# Perform a beta-diversity dispersion analysis to assess the variation in dissimilarity within groups.
# 'perm_dist' is the dissimilarity matrix calculated using the Bray-Curtis index.

# Calculate the dispersion parameter using the betadisper function.
dispersionperamiter <- betadisper(perm_dist, group = lresult$map, type = "centroid")
plot(dispersionperamiter) # Plot the dispersion parameter object.
rootplot <- dispersionperamiter # Store the dispersion parameter object in a new variable called 'rootplot'.

# Perform pairwise comparisons with 9999 permutations.
pairwise_permanova <- pairwise.perm.manova(perm_dist, lresult$map, nperm = 9999)

# View the  PERMANOVA result.
print(pairwise_permanova)

par(mfrow = c(1, 3)) #Change  how graphs are viewed to 1x3.

#plot all three results together.
plot(lichenplot)
plot(rootplot)
plot(soilplot)


#Make plots within the same limits.
plot(lichenplot, xlim = c(-0.4, 0.4), ylim = c(-0.4, 0.4))
plot(rootplot, xlim = c(-0.4, 0.4), ylim = c(-0.4, 0.4))
plot(soilplot, xlim = c(-0.4, 0.4), ylim = c(-0.4, 0.4))


par(mfrow = c(1, 1)) #Change back to 1x1.


# Assuming you have already calculated dispersion parameter.
dispersionperamiter <- betadisper(perm_dist, group = lresult$map, type = "centroid")

# Plotting the dispersion parameter with increased axis labels and numbers.
plot(dispersionperamiter, cex.axis = 2, cex.lab = 2)







# Assuming you have already calculated dispersion parameter.
dispersionperamiter <- betadisper(perm_dist, group = lresult$map, type = "centroid")

# Plotting the dispersion parameter with increased axis labels, numbers, and point labels.
plot(dispersionperamiter, cex.axis = 2, cex.lab = 2, cex.main = 2, cex.sub = 2, label = FALSE)

# Retrieve the coordinates for the labels.
coords <- dispersionperamiter$centroids

# Adding custom labels with increased size for specific labels.
text(coords, labels = rownames(coords), cex = 1.5)

# Identify specific labels and replot them with increased size.
north_coords <- coords[rownames(coords) == "north", ]
south_coords <- coords[rownames(coords) == "south", ]

# Add the specific labels with a larger size.
text(north_coords, labels = "north", cex = 4)
text(south_coords, labels = "south", cex = 4)








# Checking to see if metals play a significant difference between North and South.


# Testing for organic matter proportions. 


# Input data from the data collected.
North <- c(80.89, 76.42, 84.51, 93.75, 81.85)
South <- c(92.33, 94.12, 94.10, 87.77, 89.18)

# Combine data into a single vector
values <- c(North, South)

# Combine the data into a single vector.
group <- factor(rep(c("North", "South"), each = 5))

# Create a data frame.
data <- data.frame(values, group)

# Perform an ANOVA.
anova_result <- aov(values ~ group, data = data)

# Print the ANOVA summary.
summary(anova_result)

# Run a post-hoc test on the ANOVA results using Tukey's Honest Significant Difference (HSD) method.
tukey_result <- TukeyHSD(anova_result)

# Perform pairwise comparisons using the Benjamini-Hochberg (BH) method to adjust for multiple testing.
pairwise_results <- pairwise.t.test(data$values, data$group, p.adjust.method = "BH")

# Print the results of the pairwise comparisons.
print(pairwise_results)



