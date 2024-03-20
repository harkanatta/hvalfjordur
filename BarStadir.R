library(tidyverse)
library(glue)
library(scales)

colors_dark2 <- brewer.pal(8, "Accent")
KM <- read.csv("gogn/coverkm.csv", check.names = F)
KM <-  KM[,-c(7:9)]
data <- read.csv("gogn/hreinsad.csv", check.names = F)

joined_data <- data %>%
  left_join(KM, by = c("Reitur", "Type", "species")) %>% # Ef þessi villa kemur upp þá Error in `left_join()`:  ! Join columns must be present in data.✖ Problem with `Type`, `species`, `2020`, and `2023`. Þá gæti verið að plyr pakkinn sé í gangi og að þvælast fyrir
  select(species, Type, Reitur, km, everything()) |>
  #select(-c(`1999`)) |> 
  filter(!Reitur %in% c("R1", "R2", "R4", "R5", "R6", "R9", "R10", "R15", "R28", "R29", "R53", "R54", "R55", "R57", "R58", "R59", "R61", "R62")) |> 
  #mutate(Type = if_else(species == "Ber klöpp", "Ber klöpp", Type)) |> 
  filter(!species %in% "Ber klöpp") 

#########
### lúppa fyrir myndir allra staða - stöplarit: "Mosar","Blað- og runnfléttur", "Hrúðurfléttur","Heildarþekja",  "Tegundafjöldi"
#########

for (i in unique(joined_data$Stadur)[9]) {
  
  filtered_data <- joined_data %>%
    filter(Stadur %in% i & !Type %in% c("Grænþörungar", "Cyanobacteria")) %>%
    select(where(~ !all(is.na(.))))
  
  # Dynamically identify year columns that exist after filtering
  year_columns <- c('1976', '1997','1999', '2006', '2011', '2014', '2017', '2020', '2023')
  existing_year_columns <- year_columns[year_columns %in% colnames(filtered_data)]
  
  # Pivot longer only with existing year columns
  jd_long <- filtered_data %>%
    pivot_longer(cols = existing_year_columns, names_to = "Year", values_to = "Coverage")

jd_long_Engin_NA <- jd_long[!is.na(jd_long$Coverage),]

# Heildarfjöldi tegunda á ári í hverjum reit. Passa að hafa ekki plyr pakkann í gangi.
# Fjöldi tegunda í reit á ári
species_counts <- jd_long_Engin_NA %>%
  group_by(Reitur, Year) %>% 
  summarise(SpeciesCount = n_distinct(species), .groups = 'drop') %>%
  # Meðalfjöldi tegunda í reit á ári
  group_by(Year) %>%
  summarise(Mean = mean(SpeciesCount), SE = sd(SpeciesCount)) |> 
  mutate(Type = "Tegundafjöldi")|> 
  select(Type, Year, Mean, SE)

# Meðalþekja reita eftir Type
df <- jd_long |> 
  group_by(Type,Year,Reitur) %>% 
  summarise( Summa = sum(Coverage, na.rm = TRUE), .groups = 'drop') |> 
  group_by(Type,Year) %>% 
  summarise(Mean=mean(Summa, na.rm=TRUE),SE = sd(Summa, na.rm=TRUE), .groups = 'drop')

# heildarþekja (öll Type) allra reita innan ára
df_summary <- jd_long |> 
  group_by(Year,Reitur) %>% 
  summarise( Summa = sum(Coverage, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Year) %>% 
  summarise(Mean=mean(Summa, na.rm=TRUE),SE = sd(Summa, na.rm = TRUE) / sqrt(length(Summa)), .groups = 'drop') %>%
  mutate(Type = "Heildarþekja") |> 
  select(Type, Year, Mean, SE)


df_with_heildarþekja <- bind_rows(df, df_summary, species_counts)
# Ensure Type is ordered as desired
df_with_heildarþekja$Type <- factor(df_with_heildarþekja$Type, levels = c("Mosar","Blað- og runnfléttur", "Hrúðurfléttur","Heildarþekja",  "Tegundafjöldi"))

# Plot
p1 <-
  ggplot(df_with_heildarþekja, aes(x = Type, y = Mean, fill = Year)) +
  geom_bar(
    stat = "identity",
    color = "black",
    size = .6,
    position = position_dodge(width = 0.75)
  ) +
   # geom_errorbar(
   #   aes(ymin = Mean - SE, ymax = Mean + SE),
   #   width = .2,
   #   position = position_dodge(width = 0.75)
   # ) +
  scale_fill_manual(values = colors_dark2) +  # Use the Dark2 palette
  scale_y_continuous(
    labels = label_dollar(prefix = "", suffix = " \n%"),
    # Example secondary axis (e.g., double the Mean for illustration)
    sec.axis = sec_axis( ~ . , name = "Meðaltegundafjöldi")
  ) +
  labs(title = "",
       y = "Meðalþekja", x = "") +
  scale_x_discrete(
    "",
    labels = c(
      "Mosar" = "Mosar",
      "Blað- og runnfléttur" = "Blað- og\nrunnfléttur",
      "Hrúðurfléttur" = "Hrúðurfléttur",
      "Heildarþekja" = "Heildarþekja",
      "Tegundafjöldi" = "Tegundafjöldi"
    )
  ) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.background = element_rect(fill = "white", color = NA)) +
  annotate(
    "text",
    x = 2,
    y = Inf,
    label = "Álfholt",
    hjust = 1,
    vjust = 1,
    size = 8,
    angle = 0
  ) +
  theme(text = element_text(family = "Arial Unicode MS")) # Ensure the font supports "Á"

p1 <-
  p1 + theme(
    axis.title = element_text(size = 16),
    # Increase axis titles
    axis.text = element_text(size = 16),
    # Increase axis text
    legend.text = element_text(size = 16)
  ) # Increase legend text

Filename <-  glue("./myndir/",i,"-","{paste(unique(filtered_data$Reitur), collapse='_')}.png")
ggsave(filename = Filename, plot = p1, width = 11.7, height = 8.3, dpi = 300, units = "in")


}
p1


