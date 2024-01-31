
library(tidyverse)
library(dplyr)


library(plyr)
  df <- hreinsad %>%
  mutate(CoverageChange = `2023` - `2020`) %>%
  left_join(KM, by = "Reitur") %>%
  left_join(FogS, by = "Reitur") %>%
  select(species, Type, Reitur, km, Fluor, Brennisteinn, everything()) |>
    filter(!Reitur %in% c("R1", "R2", "R4", "R5", "R6", "R9", "R10", "R15", "R28", "R29", "R53", "R54", "R55", "R57", "R58", "R59", "R61", "R62")) |> 
    select(-c(`1976`, `1997`, `2006`, `2011`, `1999`,`2014`, `2017`))
  write.csv(df,"gogn/coverkm.csv", row.names = F, fileEncoding = "UTF-8")
  
  read.csv("gogn/coverkm.csv", check.names = F)  |> 
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













library(ggplot2)
library(dplyr)
library(plyr)

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
