



library(readxl)
library(dplyr)
library(purrr)

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
}

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


# Fjarlægð
KM <- read_excel("gogn/R1_2023.xlsx", sheet = "Results")
  
FogS <- read.csv("gogn/Reitir_myndir_lykill.csv", encoding = "latin1")
FogS$Fluor <- ifelse(is.na(FogS$Fluor)==TRUE,0,1)
FogS$Brennisteinn <- ifelse(is.na(FogS$Brennisteinn)==TRUE,0,1)


# myndir

data <- hreinsad
data <- data |>
  mutate(CoverageChange = `2023` - `2020`)


# Load necessary library
library(dplyr)

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



library(ggplot2)
library(tidyverse)

joined_data <- data |>
  left_join(KM, by = "Reitur") |>
  select(species, Type, Reitur, km, everything())

jd <- joined_data |>
  left_join(FogS, by = "Reitur") |>
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
  select(-c(`1976`, `1997`, `2006`, `2011`, `1999`)) |>
  pivot_longer(cols = c(`2014`, `2017`, `2020`, `2023`), 
               names_to = "Year", 
               values_to = "Coverage")
data_long$Year <- as.numeric(gsub("`", "", data_long$Year))  # Clean year names


library(dplyr)

data_long <- data_long |>
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
df <- data_long |> 
  filter(Type %in% c("Mosar", "Blað- og runnfléttur", "Hrúðurfléttur")) |>
  ddply(.(Year, Type, Stadur, KM, Fluor, Brennisteinn),summarise, N=mean(CoverageChange, na.rm = T )) |> 
  arrange(N)

ggplot(df, aes(x = KM, y = N, fill = Stadur)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Þekjuhlutfalls hvert ár",
       x = "'Year Ár",
       y = "Þekju") +
  theme_minimal()


### skissur

#fjarlægð Skoða breytingu frá 20 til 23 gera aðhvarfslínu fyrir punkta utan S og F
df <- joined_data |> 
  select(-c(`1976`, `1997`, `2006`, `2011`, `1999`)) |>
  pivot_longer(cols = c(`2014`, `2017`, `2020`, `2023`), 
               names_to = "Year", 
               values_to = "Coverage")
df$Year <- as.numeric(gsub("`", "", df$Year))  # Clean year names
  

df <- data_long |> 
  filter(Type %in% c("Mosar", "Blað- og runnfléttur", "Hrúðurfléttur")) |>
  mutate(Year = factor(Year),
         Reitur = factor(Reitur),
         Type = factor(Type)) |> 
  ddply(.(Year, Type),summarise, N=mean(Coverage, na.rm = T )) |> 
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
