library(dplyr)

# Read the KM data, adjust according to the previous context and remove specific columns
KM_A <- read.csv("gogn/coverkm.csv", check.names = FALSE) %>%
  select(-c(7:9))  # Assuming removal of these columns as indicated before
KM_A <-KM_A |> 
  select(-c(species,Type)) |> 
  group_by(km, Stadur, Reitur, Fluor,Brennisteinn) %>%
  distinct() %>%
  ungroup()

# Read the main data set
data_A <- read.csv("gogn/hreinsad.csv", check.names = FALSE)

# Join KM_A data with the main dataset data_A on 'Reitur', and then perform subsequent operations
joined_data_A <- data_A %>%
  left_join(KM_A, by = "Reitur") %>%  # Perform the join on 'Reitur'
  mutate(species = tolower(species)) %>%  # Convert species names to lowercase
  filter(!species %in% c("ber klöpp", "dauður mosi")) %>%  # Exclude certain species
  filter(!Type %in% c("Grænþörungar", "Cyanobacteria")) %>%  # Exclude certain types
  filter(!is.na(Stadur))  # Exclude rows where 'Stadur' is NA

# Optionally, inspect the data to ensure the join and filters are applied correctly
print(head(joined_data_A))
summary(joined_data_A)

# --- Code Segment B1 ---
KM_B <- read.csv("gogn/coverkm.csv", check.names = FALSE) %>%
  select(-c(7:9))  # Removing columns 7 to 9 as indicated

# Read the main data set
data_B <- read.csv("gogn/hreinsad.csv", check.names = FALSE)

# Join KM_B data with the main dataset data_B on 'Reitur', 'Type', and 'species'
joined_data_B <- data_B %>%
  left_join(KM_B, by = c("Reitur", "Type", "species")) %>%  # Joining on multiple keys
  select(species, Type, Reitur, km, everything()) %>%  # Selecting specific columns to keep
  filter(!Reitur %in% c("R1", "R2", "R4", "R5", "R6", "R9", "R10", "R15", "R28", "R29", "R53", "R54", "R55", "R57", "R58", "R59", "R61", "R62")) %>%  # Excluding specific Reitur values
  filter(!species %in% c("Ber klöpp", "Dauður mosi"))  # Excluding specific species

# Optionally, you can add inspection points here to check the data
print(head(joined_data_B))
summary(joined_data_B)

# Compare structures
str(joined_data_A)
str(joined_data_B)
identical(joined_data_A,joined_data_B)

beitiA <- joined_data_A |> filter(Stadur == "Beitistaðaholt")
beitiB <- joined_data_B |> filter(Stadur == "Beitistaðaholt")
identical(unique(beitiA$species),unique(tolower(beitiB$species)))




####### A #########
# Filter joined_data_A based on specific criteria and clean up species names
filtered_data_A <- joined_data_A %>%
  filter(Stadur %in% i) %>%  # Ensure data is filtered by the current location 'i'
  filter(!Type %in% c("Grænþörungar", "Cyanobacteria")) %>%  # Exclude certain types
  mutate(species = tolower(species)) %>%  # Standardize species names to lowercase
  filter(!species %in% c("ber klöpp", "dauður mosi")) %>%  # Exclude certain species
  select(where(~ !all(is.na(.))))  # Select columns where not all entries are NA

# Dynamically identify existing year columns after filtering
year_columns <- c('1976', '1997', '2006', '2011', '2014', '2017', '2020', '2023')
existing_year_columns <- year_columns[year_columns %in% colnames(filtered_data_A)]

# Pivot data to longer format only including existing year columns
jd_long_A <- filtered_data_A %>%
  pivot_longer(cols = existing_year_columns, names_to = "Year", values_to = "Coverage")

# Filter out entries with NA in 'Coverage'
jd_long_Engin_NA_A <- jd_long_A[!is.na(jd_long_A$Coverage),]



#########    B #########
# Filter joined_data_B based on specific criteria
filtered_data_B <- joined_data_B %>%
  filter(Stadur %in% i) %>%  # Ensure data is filtered by the current location 'i'
  filter(!Type %in% c("Grænþörungar", "Cyanobacteria")) %>%  # Exclude certain types
  select(where(~ !all(is.na(.))))  # Select columns where not all entries are NA

# Dynamically identify existing year columns after filtering
year_columns <- c('1976', '1997', '1999', '2006', '2011', '2014', '2017', '2020', '2023')
existing_year_columns <- year_columns[year_columns %in% colnames(filtered_data_B)]

# Pivot data to longer format only including existing year columns
jd_long_B <- filtered_data_B %>%
  pivot_longer(cols = existing_year_columns, names_to = "Year", values_to = "Coverage")

# Filter out entries with NA in 'Coverage'
jd_long_Engin_NA_B <- jd_long_B[!is.na(jd_long_B$Coverage),]





########  A ##########
species_counts_A <- jd_long_Engin_NA_A %>%
  group_by(Reitur, Year) %>%  # Removed 'Mynd' as it was not defined in your data pipeline
  summarise(SpeciesCount = n_distinct(species), .groups = 'drop')

# Calculate mean species per year
mean_species_per_year_A <- species_counts_A %>%
  group_by(Year) %>%
  summarise(
    Mean = mean(SpeciesCount),
    SE = sd(SpeciesCount) / sqrt(n())  # Using standard error calculation
  ) %>%
  mutate(Type = "Tegundafjöldi") %>% 
  select(Type, Year, Mean, SE)

# Summarize coverage data using dplyr
df_A <- jd_long_Engin_NA_A %>%
  group_by(Type, Year, Stadur, Reitur) %>%
  summarise(Summa = sum(Coverage, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Type, Year) %>%
  summarise(
    Mean = mean(Summa, na.rm = TRUE),
    SE = sd(Summa, na.rm = TRUE) / sqrt(n())  # Adjusted to use dplyr only
  )

# Summarize overall coverage across all areas within years
df_summary_A <- df_A %>%
  group_by(Year) %>%
  summarise(
    Mean = sum(Mean, na.rm = TRUE),
    SE = sd(Mean, na.rm = TRUE) / sqrt(n())  # Standard error calculation
  ) %>%
  mutate(Type = "Heildarþekja") %>%
  select(Type, Year, Mean, SE)

# Combine all data frames
combined_df_A <- bind_rows(df_summary_A, mean_species_per_year_A)
df_with_heildarþekja_A <- bind_rows(df_A, combined_df_A)

# Assuming you might want to output or further process df_with_heildarþekja_A
print(df_with_heildarþekja_A)





#########  B  ###########

# Calculate distinct species counts and average per year for B
species_counts_B <- jd_long_Engin_NA_B %>%
  mutate(species = tolower(species)) %>%
  group_by(Reitur, Year) %>%
  summarise(SpeciesCount = n_distinct(species), .groups = 'drop') %>%
  group_by(Year) %>%
  summarise(
    Mean = mean(SpeciesCount),
    SE = sd(SpeciesCount) / sqrt(n())
  ) %>%
  mutate(Type = "Tegundafjöldi") %>%
  select(Type, Year, Mean, SE)

# Calculate mean coverage by Type and Year
df_B <- jd_long_B %>%
  group_by(Type, Year, Reitur) %>%
  summarise(Summa = sum(Coverage, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Type, Year) %>%
  summarise(
    Mean = mean(Summa, na.rm = TRUE),
    SE = sd(Summa, na.rm = TRUE) / sqrt(n())
  )

# Calculate total coverage for all Types within each Year
df_summary_B <- jd_long_B %>%
  group_by(Year, Reitur) %>%
  summarise(Summa = sum(Coverage, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Year) %>%
  summarise(
    Mean = mean(Summa, na.rm = TRUE),
    SE = sd(Summa, na.rm = TRUE) / sqrt(n())
  ) %>%
  mutate(Type = "Heildarþekja") %>%
  select(Type, Year, Mean, SE)

# Combine all data frames
df_with_heildarþekja_B <- bind_rows(df_B, df_summary_B, species_counts_B)

# Ensure Type is ordered as desired
df_with_heildarþekja_B$Type <- factor(
  df_with_heildarþekja_B$Type,
  levels = c("Mosar", "Blað- og runnfléttur", "Hrúðurfléttur", "Heildarþekja", "Tegundafjöldi")
)

# Optionally, print or view the final structured data frame
print(df_with_heildarþekja_B)




# Install the gridExtra package if you haven't already
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  install.packages("gridExtra")
}

library(gridExtra)
library(ggplot2)

# Create the plots
plot_A <- ggplot(df_with_heildarþekja_A, aes(x = Type, y = Mean, fill = Year)) +
  geom_bar(stat = "identity", color = "black", size = .6, position = position_dodge(width = 0.75)) +
  theme_minimal() +
  labs(title = "Data A: Coverage by Type and Year",
       x = "Type",
       y = "Mean Coverage") +
  theme(plot.title = element_text(hjust = 0.5))  # Centering the title

plot_B <- ggplot(df_with_heildarþekja_B, aes(x = Type, y = Mean, fill = Year)) +
  geom_bar(stat = "identity", color = "black", size = .6, position = position_dodge(width = 0.75)) +
  theme_minimal() +
  labs(title = "Data B: Coverage by Type and Year",
       x = "Type",
       y = "Mean Coverage") +
  theme(plot.title = element_text(hjust = 0.5))  # Centering the title

# Arrange the plots side by side
grid.arrange(plot_A, plot_B, ncol = 2)
