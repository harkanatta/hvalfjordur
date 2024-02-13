readLines("mynd40.txt")

mynd <- read.table("mynd40.txt", header = TRUE, sep = "\t", fileEncoding = "UTF-8") %>% 
  pivot_longer(cols = c("hraunbreyskja","snepaskóf" ), names_to = "Gerd", values_to = "Heild" )

ggplot(mynd, aes(x = Gerd, y = Heild, fill = µg.S.g)) +
  geom_bar(
    stat = "identity",
    color = "black",
    size = .6,
    position = position_dodge(width = 0.75)
  ) 


mynd <- read.table("mynd41.txt", header = TRUE, sep = "\t", fileEncoding = "UTF-8") %>% 
  pivot_longer(cols = c("hraunbreyskja","snepaskóf" ), names_to = "Gerd", values_to = "Heild" )

ggplot(mynd, aes(x = Gerd, y = Heild, fill = µg.F.g)) +
  geom_bar(
    stat = "identity",
    color = "black",
    size = .6,
    position = position_dodge(width = 0.75)
  ) 


mynd <- read.table("mynd42.txt", header = TRUE, sep = "\t", fileEncoding = "UTF-8", check.names = F) %>% 
  pivot_longer(cols = starts_with("19") | starts_with("20"), 
               names_to = "Year", 
               values_to = "Value", 
               names_prefix = "") %>%
  mutate(Year = as.numeric(Year))

ggplot(mynd, aes(x = `µg S/g`, y = Value, fill = Year)) +
  geom_bar(
    stat = "identity",
    color = "black",
    size = .6,
    position = position_dodge(width = 0.75)
  )  +
  scale_fill_manual(values = colors_dark2)
