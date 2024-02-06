# Mynd 4

library(tidyverse)

data <- read.csv("gogn/hreinsad.csv", check.names = F) 

dflong <- data %>%
  filter(!Reitur %in% c("R1", "R2", "R4", "R5", "R6", "R9", "R10", "R15", "R28", "R29", "R53", "R54", "R55", "R57", "R58", "R59", "R61", "R62") &
           !species %in% "Ber klöpp" &
           !Type %in% c("Grænþörungar", "Cyanobacteria")) |> 
  #select(-c(`1976`, `1997`, `2006`, `2011`, `1999`,`2014`, `2017`)) |> 
  select(-c(`1999`)) |> 
  pivot_longer(cols = c(`2020`, `2023`,`1976`, `1997`, `2006`, `2011`, `2014`, `2017`), 
               names_to = "Year", 
               values_to = "Coverage")

dflong_Engin_NA <- dflong[!is.na(dflong$Coverage),]


# Heildarfjöldi tegunda á ári í hverjum reit. Passa að hafa ekki plyr pakkann í gangi.
pkg <- "package:plyr"
detach(pkg, character.only = TRUE)

species_counts <- dflong_Engin_NA %>%
  group_by(Reitur, Year) %>%
  summarise(SpeciesCount = n_distinct(species), .groups = 'drop')

# Meðalfjöldi tegunda í reit á ári
mean_species_per_year <- species_counts %>%
  group_by(Year) %>%
  summarise(Mean = mean(SpeciesCount)) |> 
  mutate(Type = "Tegundafjöldi")

#ddply(dflong_Engin_NA,.(Year, species), summarise, Fjoldi = length(species))
library(plyr)
df <- dflong_Engin_NA |> 
  ddply(.(Type,Year,Reitur), summarize,  Summa = sum(Coverage, na.rm = TRUE)) |> 
  ddply(.(Type,Year),summarize,Mean=mean(Summa, na.rm=TRUE)) 

#Leggja saman heildarþekju allra reita innan ára
df_summary <- df |> 
  ddply(.(Year),summarize,Mean=sum(Mean, na.rm=TRUE)) 
sums_row <- df_summary %>%
  mutate(Type = "Heildarþekja") |> 
  select(Type, Year, Mean)

combined_df <- bind_rows(sums_row, mean_species_per_year)

df_with_heildarþekja <- bind_rows(df, combined_df)


library(scales)

# Ensure Type is ordered as desired
df_with_heildarþekja$Type <- factor(df_with_heildarþekja$Type, levels = c("Mosar","Blað- og runnfléttur", "Hrúðurfléttur","Heildarþekja",  "Tegundafjöldi"))

# Plot
p1 <- ggplot(df_with_heildarþekja, aes(x = Type, y = Mean)) +
  geom_bar(aes(fill = Year), stat = "identity", color="black", linewidth =.6, position="dodge") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = label_dollar(prefix = "", suffix = " \n%"),
                     # Example secondary axis (e.g., double the Mean for illustration)
                     sec.axis = sec_axis(~ . , name = "Meðaltegundafjöldi")) +
  labs(title = "", 
       caption = "Meðalþekja mosa, blað- og runnfléttna auk meðalheildarþekju og meðaltegundafjölda í öllum föstum reitum sem metnir hafa verið síðan 1976 (alls x reitir)",
       y = "Meðalþekja", x = "") +
  theme_minimal()
