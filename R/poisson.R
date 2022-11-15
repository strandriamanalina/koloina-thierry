# Library
library(tidyverse)
library(readxl)
library(stringr)


# data
data <- read_xlsx(here::here("data", "Crabe", "Db_classe de taile pss 2021.xlsx"), trim_ws = TRUE)

data_class <- data %>% 
  mutate(Pourcentage = str_replace(Pourcentage, ",", "."), 
         Pourcentage = parse_number(Pourcentage)/100) %>% # extraction du nombre
  group_by(Taille) %>% 
  summarise(Pourcentage = mean(Pourcentage)) # Moyenne par classe

theme_set(theme_light())

data_class %>%
  mutate(min = parse_number(Taille)) %>%
  arrange(min) %>%
  mutate(Taille = as.factor(Taille)) %>% 
  ggplot(aes(x = fct_reorder(Taille, min), y = Pourcentage)) +
  geom_col() +
  geom_line(size = 1, color="red", group = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Pourcentage",
       x = "Classe") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

# ggsave(here::here("graph.png"), plot = graph)
