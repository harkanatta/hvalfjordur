library(tidyverse)
library(glue)
library(scales)

colors_dark2 <- brewer.pal(9, "Accent")
KM <- read.csv("gogn/coverkm.csv", check.names = F)
KM <-  KM[,-c(7:9)]
data <- read.csv("gogn/hreinsad.csv", check.names = F)

joined_data <- data %>%
  left_join(KM, by = c("Reitur", "Type", "species")) %>% # Ef þessi villa kemur upp þá Error in `left_join()`:  ! Join columns must be present in data.✖ Problem with `Type`, `species`, `2020`, and `2023`. Þá gæti verið að plyr pakkinn sé í gangi og að þvælast fyrir
  select(species, Type, Reitur, km, everything()) |>
  select(-c(`1997`)) |> 
  filter(!Reitur %in% c("R1", "R2", "R4", "R5", "R6", "R9", "R10", "R15", "R28", "R29", "R53", "R54",
                        "R55", "R57", "R58", "R59", "R61", "R62") &
          species %in% "Ramalina subfarinacea" )


#dput(joined_data)
mynd <- structure(list(species = c("Ramalina subfarinacea", "Ramalina subfarinacea", 
                           "Ramalina subfarinacea"), Type = c("Blað- og runnfléttur", "Blað- og runnfléttur", "Blað- og runnfléttur"),
               Reitur = c("R33","R56", "R60"), km = c(1.4, 2.3, 2.4), `1976` = c(20, NA, NA), 
               `2006` = c(6, 9, 1), `2011` = c(5, 1, 0.5), `2014` = c(3,1, 0.5), `2017` = c(10, 2, 1), 
               `2020` = c(15, 3, 1), `2023` = c(17,8, 2), `1999` = c(1, 20, 1), 
               Fluor = c(1L, 0L, 0L), Brennisteinn = c(1L,1L, 1L), Stadur = c("Stekkjarás", "Akrafjall", "Akrafjall"),
               Mynd = c("7. mynd", "13. mynd", "13. mynd")), class = "data.frame", row.names = c(NA,-3L))|> 
pivot_longer(cols = starts_with("19") | starts_with("20"), 
             names_to = "Year", 
             values_to = "Value", 
             names_prefix = "")

p1 <- ggplot(mynd, aes(fill = factor(Year), y = Value, x = Reitur)) + 
  geom_bar(stat = "identity",color = "black",size = .6,position = position_dodge(width = 0.75)) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".",suffix = " \n%")) +
  labs(x = "Year", y = "Value", fill = "Location", title = "Value by Year and Location") +
  scale_fill_manual(values = colors_dark2) + 
  labs(title = "",  y = "Þekja", x = "") +
  ggtitle("Klettastrý")+
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.background = element_rect(fill = "white", color = NA)) +
  theme(axis.title = element_text(size = 16), # Increase axis titles
        axis.text = element_text(size = 16), # Increase axis text
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 18)) # Increase legend text

#ath í R_2023, reit 33 er ártalið 1997 með 1% klettastrý. Er það talan fyrir árið 1999 í mynd 37 í skýrslunni?
Filename <-  glue("./myndir/","mynd37",".png")
ggsave(filename = Filename, plot = p1, width = 11.7, height = 8.3, dpi = 300, units = "in")


