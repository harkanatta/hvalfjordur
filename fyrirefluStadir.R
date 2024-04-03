

KM <- read.csv("gogn/coverkm.csv", check.names = F)
KM <-  KM[, -c(7:9)]|> 
  select(-c(species,Type)) |> 
  group_by(km, Stadur, Reitur, Fluor,Brennisteinn) %>%
  distinct() %>%
  ungroup()
data <- read.csv("gogn/hreinsad.csv", check.names = F) 

joined_data <- data %>%
  left_join(KM, by = c("Reitur")) %>%
  select(species, Type, Reitur, km, everything()) %>%
  mutate(species = tolower(species)) |> 
  filter(!species %in% "ber klöpp" & !species %in% "dauður mosi") |> 
  filter(!Type %in% c("Grænþörungar", "Cyanobacteria")) |> 
  filter(!is.na(Stadur))

tabeList <- list()
for (i in unique(joined_data$Stadur)) {

filtered_data <- joined_data %>%
  filter(Stadur %in% i & !Type %in% c("Grænþörungar", "Cyanobacteria")) |> 
  mutate(species = tolower(species)) |> 
  filter(!species %in% "ber klöpp" & !species %in% "dauður mosi") |> 
  select(where(~ !all(is.na(.))))

# Dynamically identify year columns that exist after filtering
year_columns <- c('1976', '1997', '2006', '2011', '2014', '2017', '2020', '2023')
existing_year_columns <- year_columns[year_columns %in% colnames(filtered_data)]

# Pivot longer only with existing year columns
jd_long <- filtered_data %>%
  pivot_longer(cols = existing_year_columns, names_to = "Year", values_to = "Coverage")

# Assuming jd_long is what you intended to use instead of jd_long_Engin_NA
jd_long_Engin_NA <- jd_long[!is.na(jd_long$Coverage),]

# Heildarfjöldi tegunda á ári í hverjum reit. Passa að hafa ekki plyr pakkann í gangi.
if ("package:plyr" %in% search()) {
  # Detach 'plyr' if it is loaded
  detach("package:plyr", unload = TRUE, character.only = TRUE)
}

species_counts <- jd_long_Engin_NA %>%
  group_by(Reitur, Year, Mynd) %>%
  summarise(SpeciesCount = n_distinct(species), .groups = 'drop')

mynd = unique(species_counts$Mynd)

# Meðalfjöldi tegunda í reit á ári
mean_species_per_year <- species_counts %>%
  group_by(Year) %>%
  summarise(Mean = mean(SpeciesCount), SE = sd(SpeciesCount)) |> 
  mutate(Type = "Tegundafjöldi")|> 
  select(Type, Year, Mean, SE)

#ddply(jd_long_Engin_NA,.(Year, species), summarise, Fjoldi = length(species))
library(plyr)
df <- jd_long_Engin_NA |> 
  ddply(.(Type,Year,Stadur,Reitur), summarize,  Summa = sum(Coverage, na.rm = TRUE)) |> 
  ddply(.(Type,Year),summarize,Mean=mean(Summa, na.rm=TRUE),SE = sd(Summa, na.rm = TRUE) / sqrt(length(Summa))) 

#Leggja saman heildarþekju allra reita innan ára
df_summary <- df |> 
  ddply(.(Year),summarize,BreytaStaf=sum(Mean, na.rm=TRUE),SE = sd(Mean, na.rm = TRUE) / sqrt(length(Mean))) |> 
  dplyr::rename(Mean=BreytaStaf)
sums_row <- df_summary %>%
  mutate(Type = "Heildarþekja") |> 
  select(Type, Year, Mean, SE)

combined_df <- bind_rows(sums_row, mean_species_per_year)

df_with_heildarþekja <- bind_rows(df, combined_df)



# Ensure Type is ordered as desired
#df_with_heildarþekja$Type <- factor(df_with_heildarþekja$Type, levels = c("Mosar","Blað- og runnfléttur", "Hrúðurfléttur"#,"Heildarþekja",  "Tegundafjöldi")) 


TableforExcel <-  df_with_heildarþekja %>%
  mutate(Type = factor(Type, levels = c("Mosar", "Blað- og runnfléttur", "Hrúðurfléttur", "Heildarþekja", "Tegundafjöldi"))) %>%
  arrange(Type) %>%
  select(-SE) %>%
  pivot_wider(names_from = Year, values_from = Mean)

TableTitle <- paste(mynd,i, sep = " - ")

tabeList[[TableTitle]] <- TableforExcel
}

# Correct extraction of numeric parts from the names
numeric_part <- as.numeric(sub("^([0-9]+).*", "\\1", names(tabeList)))

# Order the names by the numeric part
sorted_indices <- order(numeric_part)

# Reorder the list based on the sorted indices
sorted_tableList <- tabeList[sorted_indices]

# Now 'sorted_tableList' is sorted by the numeric part of the names

library(openxlsx)

# Create a new workbook
wb <- createWorkbook()

# Add a worksheet to the workbook
addWorksheet(wb, "Toflur")

# Function to add table to Excel with title
add_table_to_excel <- function(wb, sheet, table_title, table_data, start_row = 1) {
  # Write the table title
  writeData(wb, sheet, table_title, startCol = 1, startRow = start_row, colNames = FALSE)
  
  # Write the table data directly below the title
  writeData(wb, sheet, table_data, startRow = start_row + 1)
  
  # Calculate the new start row for the next table
  # Assuming there are two empty rows after each table
  new_start_row <- start_row + nrow(table_data) + 3
  
  return(new_start_row)
}

# Initialize the start row for the first table
start_row <- 1

# Write each table from the sorted list to the Excel sheet
for (i in seq_along(sorted_tableList)) {
  table_title <- names(sorted_tableList)[i]
  table_data <- sorted_tableList[[i]]
  start_row <- add_table_to_excel(wb, "Toflur", table_title, table_data, start_row)
}

# Save the workbook to a file
saveWorkbook(wb, "SortedTablesB.xlsx", overwrite = TRUE)
