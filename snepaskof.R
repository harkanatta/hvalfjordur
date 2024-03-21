library(tidyverse)
library(glue)
library(scales)

colors_dark2 <- brewer.pal(9, "Set1")
KM <- read.csv("gogn/coverkm.csv", check.names = F)
KM <-  KM[,-c(7:9)]
data <- read.csv("gogn/hreinsad.csv", check.names = F)


######################
#innan þynningarsvæðis
######################

joined_data <- data %>%
  left_join(KM, by = c("Reitur", "Type", "species")) %>% # Ef þessi villa kemur upp þá Error in `left_join()`:  ! Join columns must be present in data.✖ Problem with `Type`, `species`, `2020`, and `2023`. Þá gæti verið að plyr pakkinn sé í gangi og að þvælast fyrir
  select(species, Type, Reitur, km, everything()) |>
  #select(-c(`1999`)) |> 
  filter(!Reitur %in% c("R1", "R2", "R4", "R5", "R6", "R9", "R10", "R15", "R28", "R29", "R53", "R54",
                        "R55", "R57", "R58", "R59", "R61", "R62") &
           species %in% "Parmelia saxatilis" ) |> 
  pivot_longer(cols = starts_with("19") | starts_with("20"), 
               names_to = "Year", 
               values_to = "Value", 
               names_prefix = "") |> 
  filter(Reitur %in% c("R11", "R33", "R34", "R56", "R60"))



p1 <- ggplot(joined_data, aes(fill = factor(Year), y = Value, x = Reitur)) + 
  geom_bar(stat = "identity",color = "black",size = .6,position = position_dodge(width = 0.75)) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".",suffix = " \n%")) +
  labs(x = "Year", y = "Value", fill = "Location", title = "Value by Year and Location") +
  scale_fill_manual(values = colors_dark2) + 
  labs(title = "",  y = "Þekja", x = "") +
  ggtitle("Snepaskóf - innan þynningarsvæðis")+
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.background = element_rect(fill = "white", color = NA)) +
  theme(axis.title = element_text(size = 16), # Increase axis titles
        axis.text = element_text(size = 16), # Increase axis text
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 18)) # Increase legend text

#ath í R_2023, reit 33 er ártalið 1997 með 1% klettastrý. Er það talan fyrir árið 1999 í mynd 37 í skýrslunni?
Filename <-  glue("./myndir/","mynd38",".png")
ggsave(filename = Filename, plot = p1, width = 11.7, height = 8.3, dpi = 300, units = "in")


#####################
#Utan þynningarsvæðis
#####################

joined_data <- data %>%
  left_join(KM, by = c("Reitur", "Type", "species")) %>% # Ef þessi villa kemur upp þá Error in `left_join()`:  ! Join columns must be present in data.✖ Problem with `Type`, `species`, `2020`, and `2023`. Þá gæti verið að plyr pakkinn sé í gangi og að þvælast fyrir
  select(species, Type, Reitur, km, everything()) |>
  #select(-c(`1999`)) |> 
  filter(species %in% "Parmelia saxatilis" ) |> 
  pivot_longer(cols = starts_with("19") | starts_with("20"), 
               names_to = "Year", 
               values_to = "Value", 
               names_prefix = "") |> 
  filter(Reitur %in% c("R12","R13","R14","R16","R21","R30","R32","R37","R42","R44","R45"))


library('patchwork')

p1 <- ggplot(joined_data, aes(fill = factor(Year), y = Value, x = Reitur)) + 
  geom_bar(stat = "identity",color = "black",size = .6,position = position_dodge(width = 0.75)) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".",suffix = " \n%")) +
  labs(x = "Year", y = "Value", fill = "Location", title = "Value by Year and Location") +
  scale_fill_manual(values = colors_dark2) + 
  labs(title = "",  y = "Þekja", x = "") +
  ggtitle("Snepaskóf - utan þynningarsvæðis")+
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.background = element_rect(fill = "white", color = NA)) +
  theme(axis.title = element_text(size = 16), # Increase axis titles
        axis.text = element_text(size = 16), # Increase axis text
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 18),
        legend.position = "none") # Increase legend text


joined_dataB <- data %>%
  left_join(KM, by = c("Reitur", "Type", "species")) %>% # Ef þessi villa kemur upp þá Error in `left_join()`:  ! Join columns must be present in data.✖ Problem with `Type`, `species`, `2020`, and `2023`. Þá gæti verið að plyr pakkinn sé í gangi og að þvælast fyrir
  select(species, Type, Reitur, km, everything()) |>
  #select(-c(`1999`)) |> 
  filter(species %in% "Parmelia saxatilis" ) |> 
  pivot_longer(cols = starts_with("19") | starts_with("20"), 
               names_to = "Year", 
               values_to = "Value", 
               names_prefix = "") |> 
  filter(Reitur %in% c("R47","R48","R49","R51","R52","R64","R67","R69","R70","R72","R73"))

p2 <- ggplot(joined_dataB, aes(fill = factor(Year), y = Value, x = Reitur)) + 
  geom_bar(stat = "identity",color = "black",size = .6,position = position_dodge(width = 0.75)) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".",suffix = " \n%")) +
  labs(x = "Year", y = "Value", fill = "Location", title = "Value by Year and Location") +
  scale_fill_manual(values = colors_dark2) + 
  labs(title = "",  y = "Þekja", x = "") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.background = element_rect(fill = "white", color = NA)) +
  theme(axis.title = element_text(size = 16), # Increase axis titles
        axis.text = element_text(size = 16), # Increase axis text
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 18))

stacked_plots <- p1 / p2

# Display the stacked plots
stacked_plots

Filename <-  glue("./myndir/","mynd39",".png")
ggsave(filename = Filename, plot = stacked_plots, width = 11.7, height = 8.3, dpi = 300, units = "in")

