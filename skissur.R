



library(readxl)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyverse)

# slóðin
excel_path <- "gogn/loka.xlsx" # Replace with your file path

# Allir reitir
sheet_names <- excel_sheets(excel_path)

# Function sem les öll sheet, breytir í character og skilar data frame
read_sheet <- function(sheet_name) {
  read_excel(excel_path, sheet = sheet_name) |>
    mutate(across(everything(), as.character))
}

# innlestur
combined_data <- lapply(sheet_names, read_sheet) |> bind_rows()

# Næstu línur eru fyrir hreinsun
{
# Athuga með skrítnar sellur úr excel
selected_columns <- combined_data[, c(4:12)]
# Function sem skoðar hvort gildi eru non-numeric
is_non_numeric <- function(x) {
  !grepl("^\\d+\\.?\\d*$", x)
}

# skoða öll gildi í ára-dálkum (4:12)
non_numeric_values <- apply(selected_columns, 2, function(column) {
  column[sapply(column, is_non_numeric)]
})

# Skoða grilluð gildi
print(non_numeric_values)


# Function til að sjá í hvaða línum í gögnunum grilluðu gildin eru 
get_row_indices_non_na <- function(column_name) {
  non_numeric_vals <- non_numeric_values[[column_name]]
  non_na_non_numeric_vals <- non_numeric_vals[!is.na(non_numeric_vals)]
  which(combined_data[[column_name]] %in% non_na_non_numeric_vals)
}

# Sækja línunúmerin með grilluðu gildunum (Apply this function to each column and combine indices)
all_indices_non_na <- unique(unlist(lapply(names(non_numeric_values), get_row_indices_non_na)))

# Gera nýtt data frame með grilluðu gildunum (Create a new data frame with only the rows with non-numeric and non-NA values)
new_data_frame_non_na <- combined_data[all_indices_non_na, ]
print(new_data_frame_non_na)


hreinsad <- combined_data |> 
  mutate(
    `1997` = case_when(
      `1997` == "<1" ~ "1",
      `1997` == "x" ~ "0.5",
      TRUE ~ `1997`)) |> 
  mutate(
    `2006` = case_when(
      `2006` == "0,5*" ~ "0.5",
      `2006` == "*" ~ "0.01",
      TRUE ~ `2006`)) |> 
  mutate(
    `2014` = case_when(
      `2014` == "x" ~ "0.5",
      TRUE ~ `2014`))

hreinsad[, 4:ncol(hreinsad)] <- lapply(4:ncol(hreinsad), function(x) as.numeric(hreinsad[[x]]))
}

# Fjarlægð
KM <- read.csv("gogn/km.csv")
  
FogS <- read.csv("gogn/Reitir_myndir_lykill.csv", encoding = "latin1")
FogS$Fluor <- ifelse(is.na(FogS$Fluor)==TRUE,0,1)
FogS$Brennisteinn <- ifelse(is.na(FogS$Brennisteinn)==TRUE,0,1)


# myndir

data <- hreinsad
data <- data %>%
  mutate(CoverageChange = `2023` - `2020`)



# Assuming your data is in a dataframe named 'data'
# and that your data columns are of the correct type (numeric for years, character for others)

# Calculate average coverage for each 'Type' for the years 2014, 2017, and 2020
average_coverage_by_type <- data |>
  filter(Type %in% c("Mosar", "Blað- og runnfléttur", "Hrúðurfléttur")) |>
  group_by(Type) |>
  summarise(
    Average_2014 = mean(`2014`, na.rm = TRUE),
    Average_2017 = mean(`2017`, na.rm = TRUE),
    Average_2020 = mean(`2020`, na.rm = TRUE)
  )

# Calculate average total coverage and average species count for 2014, 2017, and 2020
average_total_coverage <- data |>
  summarise(
    Total_Average_2014 = mean(`2014`, na.rm = TRUE),
    Total_Average_2017 = mean(`2017`, na.rm = TRUE),
    Total_Average_2020 = mean(`2020`, na.rm = TRUE)
  )

average_species_count <- data |>
  summarise(Average_Species_Count = n())

# Combine the results
combined_results <- list(
  Coverage_by_Type = average_coverage_by_type,
  Total_Coverage = average_total_coverage,
  Species_Count = average_species_count
)

# Print results
print(combined_results)

# Note: Make sure your data columns are correctly named and formatted.





joined_data <- data %>%
  left_join(KM, by = "Reitur") %>%
  select(species, Type, Reitur, km, everything())

jd <- joined_data %>%
  left_join(FogS, by = "Reitur") %>%
  select(species, Type, Reitur, km, Fluor, Brennisteinn, everything())




# Endurmóta gögnin í long-format fyrir ggröf
data_long <- data |>
  filter(!Reitur %in% c("R1", "R2", "R4", "R5", "R6", "R9", "R10", "R15", "R28", "R29", "R53", "R54", "R55", "R57", "R58", "R59", "R61", "R62")) |> 
  select(-c(`1976`, `1997`, `2006`, `2011`, `1999`)) |>
  pivot_longer(cols = c(`2014`, `2017`, `2020`, `2023`), 
               names_to = "Year", 
               values_to = "Coverage")
data_long$Year <- as.numeric(gsub("`", "", data_long$Year))  # Clean year names


data_long <- jd |>
  filter(!Reitur %in% c("R1", "R2", "R4", "R5", "R6", "R9", "R10", "R15", "R28", "R29", "R53", "R54", "R55", "R57", "R58", "R59", "R61", "R62")) |>
  pivot_longer(cols = c(`2020`, `2023`), 
               names_to = "Year", 
               values_to = "Coverage") |> 
  select(-c(`1976`, `1997`, `2006`, `2011`, `1999`,`2014`, `2017`))
data_long$Year <- as.numeric(gsub("`", "", data_long$Year))  # Clean year names



data_long <- data_long %>%
  mutate(
    species = as.character(species),
    Type = as.factor(Type),
    Reitur = as.factor(Reitur),
    km = as.numeric(km),
    Fluor = as.logical(Fluor),
    Brennisteinn = as.logical(Brennisteinn),
    CoverageChange = as.numeric(CoverageChange),
    Stadur = as.character(Stadur),
    Mynd = as.character(Mynd),
    Year = as.factor(Year),
    Coverage = as.numeric(Coverage)
  )

library(plyr)
df <- data_long %>% 
  filter(Type %in% c("Mosar", "Blað- og runnfléttur", "Hrúðurfléttur") &
           Fluor==FALSE & Brennisteinn==FALSE) |>
  ddply(.(Year, Type, Stadur, km, Fluor, Brennisteinn),summarise, N=mean(CoverageChange, na.rm = T )) %>% 
  arrange(N)

ggplot(df, aes(x = km, y = N, fill = Stadur)) +
  geom_line(stat = "identity", position = position_dodge()) +
  labs(title = "Þekjuhlutfalls hvert ár",
       x = "'",
       y = "Þekju") +
  theme_minimal()


# 1. mynnd
ggplot(df, aes(x = km, y = N)) +
  geom_point(stat = "identity", position = position_dodge()) +
  labs(title = "Coverage Change in 2020 and 2023 by Place",
       x = "Place",
       y = "Coverage Change (N)",
       fill = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rota

### skissur

#fjarlægð Skoða breytingu frá 20 til 23 gera aðhvarfslínu fyrir punkta utan S og F
df <- joined_data |> 
  select(-c(`1976`, `1997`, `2006`, `2011`, `1999`)) |>
  pivot_longer(cols = c(`2014`, `2017`, `2020`, `2023`), 
               names_to = "Year", 
               values_to = "Coverage")
df$Year <- as.numeric(gsub("`", "", df$Year))  # Clean year names
  

df <- data_long %>% 
  filter(Type %in% c("Mosar", "Blað- og runnfléttur", "Hrúðurfléttur")) |>
  mutate(Year = factor(Year),
         Reitur = factor(Reitur),
         Type = factor(Type)) %>% 
  ddply(.(Year, Type),summarise, N=mean(Coverage, na.rm = T )) %>% 
  arrange(N)

ggplot(df, aes(x = Year, y = N, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Þekjuhlutfalls hvert ár",
       x = "'Year Ár",
       y = "Þekju") +
  theme_minimal()
###


# Plotting
library(ggplot2)
library(dplyr)
library(hrbrthemes) # for stylish themes and typography

# Filtering the data for specific types and converting years to a factor
filtered_data <- data_long |>
  filter(Type %in% c("Mosar", "Blað- og runnfléttur", "Hrúðurfléttur")) |>
  mutate(Year = as.factor(Year))

# Preparing the data for the bar chart
average_coverage_by_year_and_type <- filtered_data |>
  group_by(Year, Type) |>
  summarise(Average_Coverage = mean(Coverage, na.rm = TRUE))

# Creating the bar chart
ggplot(average_coverage_by_year_and_type, aes(x = Type, y = Average_Coverage, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d() + # Vibrant color palette
  theme_ipsum_rc(grid = "Y", base_size = 14) + # Elegant theme
  labs(
    title = "Average Coverage by Type Over Years",
    subtitle = "Bar chart in Cedric Shearer's style",
    x = "",
    y = "Meðalþekja (%)",
    caption = "3. mynd. Meðalþekja mosa, blað- og runnfléttna, hrúðurfléttna auk meðalheildarþekju og meðaltegundafjölda í öllum föstum reitum sem metnir voru 2014, 2017 og 2020 (alls 58 reitir)."
  ) +
  theme(
    legend.position = "top",
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10)
  )

# Ensure hrbrthemes is installed for this styling.


















bib$AUTHOR <- lapply(bib$AUTHOR, function(x) x |>
                       enc2native() |>
                       format_reverse() |>
                       format_period() |>
                       parse_names())


cross_ls  <- purrr::cross(list(iris = split(afli, afli$hofn),
                               geom = list('bar', 'box')))







# Melting the data for easier plotting with ggplot
data_long <- data |>
  #select(-c(`1976`, `1997`,  `1999`, `2006`, `2011`)) |>
  pivot_longer(cols = c(`1976`, `1997`,  `1999`, `2006`, `2011`, `2014`, `2017`, `2020`, `2023`), 
               names_to = "Year", 
               values_to = "Coverage")
data_long$Year <- as.numeric(gsub("`", "", data_long$Year))  # Clean year names
lapply(split(data, data$Reitur), function(x) prop.table(x$n))


data_long <- read.csv("C:/Users/valty/Documents/vinna/nnv/hvalfj/gogn/langt.csv")

library(vegan)
dca_result <- decorana(data_long)

# Plotting the DCA results
plot(dca_result)













dflong <- hreinsad %>%
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


#óþarfi
{
df_wider <- df_with_heildarþekja |> 
  pivot_wider(
    names_from = Year,  # Use Year as column names
    values_from = Mean  # Use Mean as the values in the table
  )


sums_row <- df %>% 
  summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  mutate(Type = "Heildarþekja")

# Bind this new row to the original dataframe
df_wider_with_sums <- bind_rows(df_wider, sums_row)

# Step 2: Pivot longer
df_longer <- df_wider_with_sums %>%
  pivot_longer(
    cols = -Type,  # Exclude the Type column from pivoting
    names_to = "Year",
    values_to = "Mean"
  )

# View the result
print(df_longer)
}

p1 <- df_with_heildarþekja%>%
  ggplot(aes(x = Type, y = Mean)) +
  geom_bar(aes(fill = Year), stat = "identity", color="black", linewidth =.6,position="dodge")  +
  xlab("") + ylab("") + labs(fill = "", title = "Hei", caption = "Meðalþekja mosa, blað- og runnfléttna auk meðalheildarþekju og meðaltegundafjölda í öllum föstum reitum sem metnir hafa verið síðan 1976 (alls x reitir") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::label_dollar(prefix = "", suffix = " \n%"))








library(ggplot2)
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

 ggsave("mynd4.png", plot = p1, width = 11.7, height = 8.3, dpi = 300, units = "in")

 
 
 
 
 
 # Assuming you have the 'ggplot2' package installed
 # If not, you can install it by uncommenting the following line:
 # install.packages("ggplot2")
 
 library(ggplot2)
 
 # Your data frame will be similar to this structure
 rass <- data.frame(
   Year = c(1976, 1997, 2006, 2011, 2014, 2017, 2020, 2023),
   Coverage = c(3.791796, 3.632789, 3.524017, 3.560057, 4.273623, 4.891759, 4.068379, 4.814438)
 )
 
 # Plotting
 ggplot(rass, aes(x = Year, y = Coverage)) +
   geom_line() + # Add a line
   geom_point() + # Add points at each year
   theme_minimal() + # Use a minimal theme
   labs(title = "Trend of Lichen Coverage Over Time",
        x = "Year",
        y = "Average Coverage") +
   scale_x_continuous(breaks = data$Year) # Ensure all years are included on the x-axis
 

 
 
 
 
 #########
 #Mynd scatter
 ##########
 
 
 library(tidyverse)
 
 data <- read.csv("gogn/hreinsad.csv", check.names = F) 
 
 joined_data <- data %>%
   left_join(KM, by = "Reitur") %>%
   select(species, Type, Reitur, km, everything())
 
 jd <- joined_data %>%
   left_join(FogS, by = "Reitur") %>%
   select(species, Type, Reitur, km, Fluor, Brennisteinn, everything())
 


 
   jd_long <-jd %>%
     #left_join(KM, by = c("Reitur", "Type", "species","2020","2023")) |> 
     select(-c(`1999`,`1976`, `1997`, `2006`, `2011`, `2014`, `2023`)) |> 
     filter(!Reitur %in% c("R1", "R2", "R4", "R5", "R6", "R9", "R10", "R15", "R28",
                           "R29", "R53", "R54", "R55", "R57", "R58", "R59", "R61", "R62") &
              Type %in% "Blað- og runnfléttur") |>
     filter(!species %in% "Ber klöpp") |> 
     #filter(!is.na(`2017`)) |> 
     #filter(!is.na(`2020`)) |> 
     mutate(#`2017` = case_when(
       # is.na(`2017`) ~ 0,
       # TRUE ~ `2017`),
       # `2020` = case_when(
       # is.na(`2020`) ~ 0,
       # TRUE ~ `2020`),
       PointType = case_when(
       Fluor==1 ~ "Fluor",
       Brennisteinn==1 ~ "Brennisteinn",
       TRUE ~ "Other")) |> 
     group_by(km,Stadur,Reitur,PointType) |>
     summarise(M17=sum(`2017`),M20=sum(`2020` ),CoverageChange=M20-M17, .groups = 'drop')#|> 
     # group_by(km,Stadur,Reitur,PointType,CoverageChange) |>
     # summarise(CoverageChange=mean(CoverageChange, na.rm = T), .groups = 'drop') #|> 
     # group_by(km,Fluor,Brennisteinn,Stadur,PointType) |> 
     # summarise(CoverageChange=mean(CoverageChange, na.rm = T), .groups = 'drop')
      

 
 # Assuming jd_long has already been mutated to include the PointType column as previously described
 
 # Plot with conditional aesthetics and a single line for "Other"
 ggplot(jd_long, aes(x = km, y = CoverageChange, color = PointType, shape = PointType)) +
   geom_point() + # Plot all points with conditional shapes and colors
   geom_smooth(data = subset(jd_long, PointType == "Other"), method = "lm", se = T, aes(group = 1, color = "Other")) + # Only draw line for "Other"
   scale_shape_manual(values = c("Fluor" = 17, "Brennisteinn" = 15, "Other" = 1)) + # 17 is triangle, 15 is square
   scale_color_manual(values = c("Fluor" = "green", "Brennisteinn" = "red", "Other" = "black")) +
   theme_minimal() +
   labs(title = "Coverage Change vs. Distance for Point Type 'Other'",
        x = "Distance from Source (km)",
        y = "Coverage Change",
        shape = "Point Type",
        color = "Point Type") +
   guides(color = guide_legend(override.aes = list(shape = 1)), shape = guide_legend(override.aes = list(color = "black"))) # Adjust legend to reflect the aesthetics accurately
 
 # Note: The aes(group = 1) in geom_smooth ensures that the line is considered as a single group, important for datasets with multiple groups.
 
 
 

 filtered_data <- rass %>%
   group_by(Reitur) %>%
   filter(all(c(2017, 2020) %in% unique(Year))) %>%
   ungroup()
 
 # View the filtered data
 print(filtered_data)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 KM <- read.csv("gogn/coverkm.csv", check.names = F)
 KM <-  KM[, -c(7:9)]
 data <- read.csv("gogn/hreinsad.csv", check.names = F) 
 FogS <- FogS <- read.csv("gogn/Reitir_myndir_lykill.csv", encoding = "latin1")
 
 plot_data <- data %>%
   mutate(CoverageChange = (`2023` - `2020`)/`2020`) %>%
   left_join(KM, by = c("Reitur", "Type", "species")) %>%
   select(species, Type, Stadur, Reitur, km, Fluor, Brennisteinn, everything()) %>%
   filter(!Reitur %in% c("R1", "R2", "R4", "R5", "R6", "R9", "R10", "R15", "R28", "R29", "R53", "R54", "R55", "R57", "R58", "R59", "R61", "R62") &
            !is.na(CoverageChange) &
            Type %in% "Blað- og runnfléttur") %>%
   mutate(PointType = case_when(
     Fluor == 1 ~ "Fluor",
     Brennisteinn == 1 & Fluor == 0 ~ "Brennisteinn",
     TRUE ~ "Other"
   )) %>%
   select(-c(`1976`, `1997`, `2006`, `2011`, `1999`,`2014`, `2017`)) %>%
   ddply(.(km, Stadur, Reitur, PointType), summarize, CoverageChange=mean(CoverageChange, na.rm=TRUE))
 
 
 # Define colors from Dark2 palette
 colors_dark2 <- brewer.pal(3, "Dark2")
 
 # Adjust your ggplot code with updated legend
 ggplot(plot_data, aes(x = km, y = CoverageChange, color = PointType, shape = PointType)) +
   geom_point(size = 3) + # Increase point size
   geom_smooth(data = subset(plot_data, PointType == "Other"), method = "lm", se = TRUE, aes(group = 1, color = "Other")) +
   scale_shape_manual(values = c("Fluor" = 17, "Brennisteinn" = 15, "Other" = 1)) +
   scale_color_manual(values = c("Fluor" = colors_dark2[1], "Brennisteinn" = colors_dark2[2], "Other" = colors_dark2[3])) +
   theme_minimal() +
   theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + # Add frame around the plot area
   labs(title = "Blað- og runnfléttur",
        x = "Fjarlægð frá Grundartanga (km)",
        y = "Þekjubreyting 2020-2023 (prósentustig)",
        shape = "Reitir", # Change legend title for shapes
        color = "Reitir") + # Change legend title for colors
   guides(color = guide_legend(override.aes = list(shape = c(17,15,1))), # Ensure legend matches plot symbols
          shape = guide_legend(override.aes = list(color = colors_dark2))) # Match colors in legend
 
 
 
 
 
 

 
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
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 


 joined_data <- data %>%
   left_join(KM, by = c("Reitur", "Type", "species"))  %>% # Getur komið upp villa, stroka þá út  , "Type", "species"
   select(species, Type, Reitur, km, everything())
 
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
 
  jd <- joined_data
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
   filter(!Year %in% c( '1976', '1997', '2006', '2011', '2014', '2017', '1999')) |>
   mutate(PointType = case_when(
     Fluor == 1 ~ "Fluor",
     Brennisteinn == 1 & Fluor == 0 ~ "Brennisteinn",
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

 
 ggplot(jd_fyrir_plot, aes(x = km, y = CoverageChange, color = PointType, shape = PointType)) +
   geom_point(size = 3) + # Increase point size
   geom_smooth(data = subset(jd_fyrir_plot, PointType == "Utan þynningarsvæðis iðnaðarsvæðisins"), method = "lm", se = TRUE, aes(group = 1, color = "Utan þynningarsvæðis iðnaðarsvæðisins")) +
   scale_shape_manual(values = c("Innan þynningarsvæðis flúors" = 17, "Innan þynningarsvæðis brennisteins" = 15, "Utan þynningarsvæðis iðnaðarsvæðisins" = 1)) +
   scale_color_manual(values = c("Innan þynningarsvæðis flúors" = colors_dark2[1], "Innan þynningarsvæðis brennisteins" = colors_dark2[2], "Utan þynningarsvæðis iðnaðarsvæðisins" = colors_dark2[3])) +
   theme_minimal() +
   theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + # Add frame around the plot area
   labs(title = "Blað- og runnfléttur",
        x = "Fjarlægð frá Grundartanga (km)",
        y = "Þekjubreyting 2020-2023 (prósentustig)",
        shape = "Reitir", # Change legend title for shapes
        color = "Reitir") + # Change legend title for colors
   guides(color = guide_legend(override.aes = list(shape = c(17,15,1))), # Ensure legend matches plot symbols
          shape = guide_legend(override.aes = list(color = colors_dark2))) # Match colors in legend
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 library(dplyr)
 library(tidyr)
 library(ggplot2)
 library(RColorBrewer)
 
 # Join datasets and immediately filter out unnecessary rows and columns
 joined_data <- data %>%
   left_join(KM, by = c("Reitur", "Type", "species")) %>%
   select(species, Type, Reitur, km, everything()) %>%
   filter(Type == "Blað- og runnfléttur", !species %in% "Ber klöpp")
 
 # Convert to long format and filter based on years directly
 jd_long <- joined_data %>%
   pivot_longer(cols = starts_with("19") | starts_with("20"), names_to = "Year", values_to = "Value", names_prefix = "") %>%
   mutate(Year = as.numeric(Year)) %>%
   filter(Year %in% c(2020, 2023) & !is.na(Value)) # Focusing on years directly relevant to the plot |> 

 
 # Assign PointType with a single mutate call
 jd_long <- jd_long %>%
   mutate(PointType = case_when(
     Fluor == 1 ~ "Innan þynningarsvæðis flúors",
     Brennisteinn == 1 & Fluor == 0 ~ "Innan þynningarsvæðis brennisteins",
     TRUE ~ "Utan þynningarsvæðis iðnaðarsvæðisins"
   ))
 
 # Calculate Coverage Change for each Reitur and Year directly
 jd_summary <- jd_long %>%
   group_by(km, Stadur, Reitur, PointType, Year) %>%
   summarise(Coverage = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
   pivot_wider(names_from = Year, values_from = Coverage, names_prefix = "M") %>%
   mutate(CoverageChange = M2023 - M2020)
 
 # Define colors from Dark2 palette
 colors_dark2 <- brewer.pal(3, "Dark2")
 
 # Plotting
 ggplot(jd_summary, aes(x = km, y = CoverageChange, color = PointType, shape = PointType)) +
   geom_point(size = 3) +
   geom_smooth(data = filter(jd_summary, PointType == "Utan þynningarsvæðis iðnaðarsvæðisins"), 
               method = "lm", se = TRUE, aes(group = 1, color = "Utan þynningarsvæðis iðnaðarsvæðisins")) +
   scale_shape_manual(values = c(17, 15, 1)) +
   scale_color_manual(values = colors_dark2) +
   theme_minimal() +
   theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) +
   labs(title = "Blað- og runnfléttur",
        x = "Fjarlægð frá Grundartanga (km)",
        y = "Þekjubreyting 2020-2023 (prósentustig)",
        shape = "Reitir", 
        color = "Reitir") +
   guides(color = guide_legend(override.aes = list(shape = c(17,15,1))),
          shape = guide_legend(override.aes = list(color = colors_dark2)))
 
 # Note: Adjust shape and color scales as necessary to match your specific plot needs.
 