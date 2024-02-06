
library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(plyr)
#

library(plyr)
  df <- hreinsad %>%
  mutate(CoverageChange = `2023` - `2020`) %>%
  left_join(KM, by = "Reitur") %>%
  left_join(FogS, by = "Reitur") %>%
  select(species, Type, Reitur, km, Fluor, Brennisteinn, everything()) |>
    filter(!Reitur %in% c("R1", "R2", "R4", "R5", "R6", "R9", "R10", "R15", "R28", "R29", "R53", "R54", "R55", "R57", "R58", "R59", "R61", "R62")) |> 
    select(-c(`1976`, `1997`, `2006`, `2011`, `1999`,`2014`, `2017`))
  write.csv(df,"gogn/coverkm.csv", row.names = F, fileEncoding = "UTF-8")
  
  jd <- read.csv("gogn/coverkm.csv", check.names = F) 
  jd |> 
  ddply(.(km, Reitur, Fluor, Brennisteinn), summarize, CoverageChange=mean(CoverageChange, na.rm=T))  %>%
  ggplot(aes(x = km, y = CoverageChange)) +
  geom_point() +  # Change to geom_line() if a line plot is preferred
  labs(title = "Mean Coverage by km and Reitur",
       x = "km",
       y = "Mean Coverage") +
  theme_minimal()


data_long <- jd |>
  pivot_longer(cols = c(`2020`, `2023`), 
               names_to = "Year", 
               values_to = "Coverage") |> 
  select(-c(`1976`, `1997`, `2006`, `2011`, `1999`,`2014`, `2017`))

ddply(data_long,.(km, Stadur), summarize, Coverage=mean(Coverage, na.rm=T)) |> 
  ggplot(aes(x = km, y = Coverage)) +
  geom_point() +  # Change to geom_line() if a line plot is preferred
  labs(title = "Mean Coverage by km and Stadur",
       x = "km",
       y = "Mean Coverage") +
  theme_minimal()










# Mosar




# Assuming df is your data frame
#plot_data <- df[df$Type=="Blað- og runnfléttur",] %>%
  plot_data <- df[df$Type=="Mosar",] %>%
  ddply(.(km, Reitur, Fluor, Brennisteinn), summarize, CoverageChange=mean(CoverageChange, na.rm=TRUE))

# Fit a linear model to the subset where Fluor and Brennisteinn are both 0
lm_data <- subset(plot_data, Fluor == 0 & Brennisteinn == 0)
fit <- lm(CoverageChange ~ km, data = lm_data)

# Create the scatter plot with adjusted legends
p <- ggplot(plot_data, aes(x = km, y = CoverageChange)) +
  geom_point(aes(shape = case_when(Fluor == 1 ~ 17,
                                   Brennisteinn == 1 & Fluor == 0 ~ 19,
                                   TRUE ~ 16),
                 color = case_when(Fluor == 1 ~ "Fluor",
                                   Brennisteinn == 1 & Fluor == 0 ~ "Brennisteinn",
                                   TRUE ~ "Black")),
             size = 3) +
  scale_shape_identity() +
  scale_color_manual(values = c("Fluor" = "green", "Brennisteinn" = "red", "Black" = "black"),
                     name = "Reitir",
                     labels = c("Fluor", "Brennisteinn", "Other")) +
  labs(title = "Mean Coverage by km and Reitur",
       x = "km",
       y = "Mean Coverage") +
  theme_minimal()

# Add the regression line from the linear model
p + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "blue")


















stats_by_reitur <- hreinsad %>%
  filter(!Reitur %in% c("R1", "R2", "R4", "R5", "R6", "R9", "R10", "R15", "R28", "R29", "R53", "R54", "R55", "R57", "R58", "R59", "R61", "R62")) |> 
  select(-c(`1976`, `1997`, `2006`, `2011`, `1999`,`2014`, `2017`)) |> 
  pivot_longer(cols = c(`2020`, `2023`), 
               names_to = "Year", 
               values_to = "Coverage") |> 
  ddply(.(Year,Reitur), summarize,  Mean = mean(Coverage, na.rm = TRUE),
        SD = sd(Coverage, na.rm = TRUE),
        N = sum(!is.na(Coverage)),
        Error = SD / sqrt(N)) |> 
  mutate(
    ReiturNumber = as.numeric(gsub("R", "", Reitur))  # Extract numeric part
  ) |> 
  arrange(ReiturNumber)


library(ggplot2)
library(dplyr)
library(hrbrthemes)

# Assuming stats_by_reitur is already prepared and ordered
# Dodge position for side-by-side placement
dodge <- position_dodge(width = 0.2)

# Plotting with side-by-side points and error bars for each year
my_plot <- ggplot(stats_by_reitur, aes(x = factor(Reitur, levels = unique(Reitur)), y = Mean, group = Year, color = Year, shape=Year)) +
  geom_point(position = dodge, size = 3) +
  geom_errorbar(aes(ymin = Mean - Error, ymax = Mean + Error), width = 0.2, position = dodge) +
  scale_color_manual(values = c("2020" = "blue", "2023" = "red"), name = "Year") +
  theme_minimal() +
  labs(
    title = "Breytileiki þekju innan reita Reitur",
    x = "Reitur",
    y = "Mean Coverage with Error Bars"
  ) +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

# Ensure the dataset contains both 2020 and 2023 before plotting





# Summa reita á hverju ári fyrir hvern flokk
stats_by_reitur <- hreinsad %>%
  filter(!Reitur %in% c("R1", "R2", "R4", "R5", "R6", "R9", "R10", "R15", "R28", "R29", "R53", "R54", "R55", "R57", "R58", "R59", "R61", "R62") &
           !species %in% "Ber klöpp" &
           !Type %in% c("Grænþörungar", "Cyanobacteria")) |> 
  #select(-c(`1976`, `1997`, `2006`, `2011`, `1999`,`2014`, `2017`)) |> 
  pivot_longer(cols = c(`2020`, `2023`,`1976`, `1997`, `2006`, `2011`, `1999`,`2014`, `2017`), 
               names_to = "Year", 
               values_to = "Coverage") |> 
  ddply(.(Type,Year,Reitur), summarize,  Summa = sum(Coverage, na.rm = TRUE),
        Summa = mean(Coverage, na.rm = TRUE),.drop=F) |> 
  mutate(
    ReiturNumber = as.numeric(gsub("R", "", Reitur))  # Extract numeric part
  ) |> 
  arrange(ReiturNumber)

stats_by_reitur %>%
  ggplot(aes(x = Type, y = Summa)) +
  geom_bar(aes(fill = Year), stat = "identity", color="black", linewidth =1,position="dodge")  +
  xlab("") + ylab("") + labs(fill = "", title = "Landanir í Húnaflóa 2022", caption = "(Gögn fengin af vef Fiskistofu (fiskistofa.is))") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::label_dollar(prefix = "", suffix = " \nTonn"))
