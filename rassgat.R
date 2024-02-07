
library(dplyr)

# Define year columns to check
year_cols <- c('1976', '1997', '2006', '2011', '2014', '2017', '2020', '2023', '1999')

# Create a function to check for rows without numerical values for each year within each Reitur
check_numerical_values <- function(data, year_cols) {
  results <- data %>%
    select(Reitur, all_of(year_cols)) %>%
    pivot_longer(cols = -Reitur, names_to = "Year", values_to = "Value") %>%
    group_by(Reitur, Year) %>%
    summarize(HasValue = any(!is.na(Value) & Value > 0), .groups = 'drop') %>%
    filter(!HasValue)
  
  return(results)
}

# Apply the function to the jd dataframe
results <- check_numerical_values(jd, year_cols)

# Print results
print(results)



results_df <-as.data.frame(table(results))[,c(1,2,4)]
colnames(results_df) <-  c("Reitur","Year","Exclude") 
rass <- results_df[order(results_df$Reitur) & results_df$Exclude==0,]
include <- rass[,1:2]
include$identifier <- paste(include$Reitur, include$Year, sep = "_")


jd_long <- jd %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), 
               names_to = "Year", 
               values_to = "Value", 
               names_prefix = "") %>%
  mutate(Year = as.numeric(Year))
jd_long$identifier <- paste(jd_long$Reitur, jd_long$Year, sep = "_")

jd_filtered <- jd_long %>%
     filter(identifier %in% include$identifier) |> 
  filter(Type %in% "Blað- og runnfléttur") |> # setja filter fyrir myndir hér
  filter(!species %in% "Ber klöpp") |> 
  filter(!Year %in% c( '1976', '1997', '2006', '2011', '2014', '2023', '1999')) |>
  mutate(PointType = case_when(
    !is.na(Fluor) ~ "Fluor",
    !is.na(Brennisteinn) ~ "Brennisteinn",
    TRUE ~ "Other")) 

jd_wide_filtered <- jd_filtered %>%
     pivot_wider(names_from = Year, values_from = Value)

jd_fyrir_plot <-jd_wide_filtered |> 
  group_by(km,Stadur,Reitur,PointType) |>
  summarise(M17=sum(`2017`, na.rm = T),M20=sum(`2020`, na.rm = T ),CoverageChange=M20-M17, .groups = 'drop')

ggplot(jd_fyrir_plot, aes(x = km, y = CoverageChange, color = PointType, shape = PointType)) +
  geom_point() + # Plot all points with conditional shapes and colors
  geom_smooth(data = subset(jd_fyrir_plot, PointType == "Other"), method = "lm", se = T, aes(group = 1, color = "Other")) + # Only draw line for "Other"
  scale_shape_manual(values = c("Fluor" = 17, "Brennisteinn" = 15, "Other" = 1)) + # 17 is triangle, 15 is square
  scale_color_manual(values = c("Fluor" = "green", "Brennisteinn" = "red", "Other" = "black")) +
  theme_minimal() +
  labs(title = "Coverage Change vs. Distance for Point Type 'Other'",
       x = "Distance from Source (km)",
       y = "Coverage Change",
       shape = "Point Type",
       color = "Point Type") +
  guides(color = guide_legend(override.aes = list(shape = 1)), shape = guide_legend(override.aes = list(color = "black"))) 












# Assuming you have a way to identify which combinations to exclude, you filter them here
# For example, if 'results' had 'Reitur' and 'Year' to exclude
# This step assumes results is adjusted to the correct format
# jd_filtered <- jd_long %>%
#   filter(!(paste(Reitur, Year, sep = "_") %in% results$identifier))

# If you need to pivot back to wide format after filtering
# jd_wide_filtered <- jd_filtered %>%
#   pivot_wider(names_from = Year, values_from = Value)

# Note: The above is a conceptual approach. You'll need to adjust it based on actual conditions and data structure.


#













































# Assuming 'jd' is your dataframe
# Loading necessary library
library(dplyr)

# Define year columns to check
year_cols <- c('1976', '1997', '2006', '2011', '2014', '2017', '2020', '2023', '1999')

# Create a function to check for rows without numerical values for each year within each Reitur
check_numerical_values <- function(data, year_cols) {
  results <- data %>%
    select(Reitur, all_of(year_cols)) %>%
    pivot_longer(cols = -Reitur, names_to = "Year", values_to = "Value") %>%
    group_by(Reitur, Year) %>%
    summarize(HasValue = any(!is.na(Value) & Value > 0), .groups = 'drop') %>%
    filter(!HasValue)
  
  return(results)
}

# Apply the function to the jd dataframe
results <- check_numerical_values(jd, year_cols)

# Print results
print(results, n=250)

# Assuming 'results' contains the Reitur and Year combinations to exclude
# Assuming 'jd' is your original dataframe

# First, create an identifier in both jd and results to match rows
jd$identifier <- paste(jd$Reitur, jd$Year, sep = "_")
results$identifier <- paste(results$Reitur, results$Year, sep = "_")

# Then, filter out those identifiers from jd
jd_filtered <- jd[!jd$identifier %in% results$identifier, ]

# Finally, you can remove the temporary identifier column if you don't need it anymore
jd_filtered$identifier <- NULL

# jd_filtered now contains your data with the specified Reitur and year combinations removed

