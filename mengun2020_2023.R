data <- read.csv("gogn/hreinsad.csv", check.names = F)
KM <- read.csv("gogn/coverkm.csv", check.names = F)
KM <-  KM[, -c(7:9)]
joined_data <- data %>%
  left_join(KM, by = c("Reitur", "Type", "species"))  %>% # Getur komið upp villa, stroka þá út  , "Type", "species"
  select(species, Type, Reitur, km, everything())



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
  filter(!Year %in% c( '1976', '1997', '2006', '2011', '2014', '2017', '1999')) |>
  mutate(PointType = case_when(
    !is.na(Fluor) ~ "Fluor",
    !is.na(Brennisteinn) ~ "Brennisteinn",
    TRUE ~ "Other")) |> 
  group_by(Reitur) %>%
  filter(all(c(2023, 2020) %in% unique(Year))) %>%
  ungroup()

jd_wide_filtered <- jd_filtered %>%
  pivot_wider(names_from = Year, values_from = Value)

jd_fyrir_plot <-jd_wide_filtered |> 
  group_by(km,Stadur,Reitur,PointType) |>
  summarise(M23=sum(`2023`, na.rm = T),M20=sum(`2020`, na.rm = T ),CoverageChange=M23-M20, .groups = 'drop') |> 
  mutate(PointType = case_when(
    PointType=="Fluor" ~ "Innan þynningarsvæðis flúors",
    PointType=="Brennisteinn" ~ "Innan þynningarsvæðis brennisteins",
    TRUE ~ "Utan þynningarsvæðis iðnaðarsvæðisins")) 



# Define colors from Dark2 palette
library(RColorBrewer)
colors_dark2 <- brewer.pal(3, "Dark2")

# Adjust your ggplot code
mynd1 <- ggplot(jd_fyrir_plot, aes(x = km, y = CoverageChange, color = PointType, shape = PointType)) +
  geom_point(size = 3) + # Increase point size
  geom_smooth(data = subset(jd_fyrir_plot, PointType == "Utan þynningarsvæðis iðnaðarsvæðisins"), method = "lm", se = TRUE, aes(group = 1, color = "Utan þynningarsvæðis iðnaðarsvæðisins"),show.legend = FALSE) +
  scale_shape_manual(values = c("Innan þynningarsvæðis flúors" = 17, "Innan þynningarsvæðis brennisteins" = 15, "Utan þynningarsvæðis iðnaðarsvæðisins" = 1)) +
  scale_color_manual(values = c("Innan þynningarsvæðis flúors" = colors_dark2[1], "Innan þynningarsvæðis brennisteins" = colors_dark2[2], "Utan þynningarsvæðis iðnaðarsvæðisins" = colors_dark2[3])) +
  theme_minimal()  +
  theme(legend.position = c(0.75, 0.15)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5)) + # Add frame around the plot area
  labs(title = "Blað- og runnfléttur",
       x = "Fjarlægð frá Grundartanga (km)",
       y = "Þekjubreyting 2020-2023 (prósentustig)",
       shape = "Reitir", # Change legend title for shapes
       color = "Reitir") + # Change legend title for colors
  guides(color = guide_legend(override.aes = list(shape = c(17,15,1))), # Ensure legend matches plot symbols
         shape = guide_legend(override.aes = list(color = colors_dark2))) # Match colors in legend

mynd1 <- mynd1 + theme(plot.title = element_text(size = 22), 
                       axis.title = element_text(size = 16),
                       axis.text = element_text(size = 16),
                       legend.text = element_text(size = 16)) # Increase legend text

ggsave(filename = "mynd1.png", plot = mynd1, width = 11.7, height = 8.3, dpi = 300, units = "in", bg = 'white')






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




#Mosar


jd_filtered <- jd_long %>%
  filter(identifier %in% include$identifier) |> 
  filter(Type %in% "Mosar") |> # setja filter fyrir myndir hér
  filter(!species %in% "Ber klöpp") |> 
  filter(!Year %in% c( '1976', '1997', '2006', '2011', '2014', '2017', '1999')) |>
  mutate(PointType = case_when(
    !is.na(Fluor) ~ "Fluor",
    !is.na(Brennisteinn) ~ "Brennisteinn",
    TRUE ~ "Other")) |> 
  group_by(Reitur) %>%
  filter(all(c(2023, 2020) %in% unique(Year))) %>%
  ungroup()

jd_wide_filtered <- jd_filtered %>%
  pivot_wider(names_from = Year, values_from = Value)

jd_fyrir_plot <-jd_wide_filtered |> 
  group_by(km,Stadur,Reitur,PointType) |>
  summarise(M23=sum(`2023`, na.rm = T),M20=sum(`2020`, na.rm = T ),CoverageChange=M23-M20, .groups = 'drop') |> 
  mutate(PointType = case_when(
    PointType=="Fluor" ~ "Innan þynningarsvæðis flúors",
    PointType=="Brennisteinn" ~ "Innan þynningarsvæðis brennisteins",
    TRUE ~ "Utan þynningarsvæðis iðnaðarsvæðisins")) 



# Define colors from Dark2 palette
library(RColorBrewer)
colors_dark2 <- brewer.pal(3, "Dark2")

# Adjust your ggplot code
mynd2 <- ggplot(jd_fyrir_plot, aes(x = km, y = CoverageChange, color = PointType, shape = PointType)) +
  geom_point(size = 3) + # Increase point size
  geom_smooth(data = subset(jd_fyrir_plot, PointType == "Utan þynningarsvæðis iðnaðarsvæðisins"), method = "lm", se = TRUE, aes(group = 1, color = "Utan þynningarsvæðis iðnaðarsvæðisins"),show.legend = FALSE) +
  scale_shape_manual(values = c("Innan þynningarsvæðis flúors" = 17, "Innan þynningarsvæðis brennisteins" = 15, "Utan þynningarsvæðis iðnaðarsvæðisins" = 1)) +
  scale_color_manual(values = c("Innan þynningarsvæðis flúors" = colors_dark2[1], "Innan þynningarsvæðis brennisteins" = colors_dark2[2], "Utan þynningarsvæðis iðnaðarsvæðisins" = colors_dark2[3])) +
  theme_minimal()  +
  theme(legend.position = c(0.75, 0.15)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5)) + # Add frame around the plot area
  labs(title = "Mosar",
       x = "Fjarlægð frá Grundartanga (km)",
       y = "Þekjubreyting 2020-2023 (prósentustig)",
       shape = "Reitir", # Change legend title for shapes
       color = "Reitir") + # Change legend title for colors
  guides(color = guide_legend(override.aes = list(shape = c(17,15,1))), # Ensure legend matches plot symbols
         shape = guide_legend(override.aes = list(color = colors_dark2))) # Match colors in legend

mynd2 <- mynd2 + theme(plot.title = element_text(size = 22), 
                       axis.title = element_text(size = 16),
                       axis.text = element_text(size = 16),
                       legend.text = element_text(size = 16)) # Increase legend text

ggsave(filename = "mynd2.png", plot = mynd2, width = 11.7, height = 8.3, dpi = 300, units = "in", bg = 'white')
































