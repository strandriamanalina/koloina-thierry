# Library
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)



# data
poisson <- read_xlsx(here::here("data", "données Mahafaly Poisson et poulpe_Danny.xlsx"), trim_ws = TRUE, sheet = 1) %>% 
  clean_names()

poulpe <- read_xlsx(here::here("data", "données Mahafaly Poisson et poulpe_Danny.xlsx"), trim_ws = TRUE, sheet = 2) %>% 
  clean_names()



# cleaning data
poisson_data <- poisson %>% 
  select(date, village, poids_kg, nombre_de_personne_sur_une_pirogue, famille, classe_de_taille, moyenne) %>% 
  mutate(date = dmy(date),
         poids_kg = parse_number(poids_kg),
         moyenne = parse_number(moyenne),
         capture_unite = poids_kg/nombre_de_personne_sur_une_pirogue,
         year = year(date)
  )


poulpe_data <- poulpe %>% 
  select(date, village, poids_kg, nombre_de_personne_sur_une_pirogue, famille, classe_de_taille, moyenne)


# Réalisation de graphe
theme_set(theme_light())

# Par année
evolution_annuelle <- poisson_data %>%
  group_by(year) %>% 
  summarise(moyenne_annuelle = mean(poids_kg)) %>% 
  ggplot(aes(year, moyenne_annuelle)) +
  geom_line()

# Par village
taille_par_village <- poisson_data %>%
  mutate(village = as.factor(village)) %>% 
  group_by(village) %>% 
  summarise(moyenne = mean(poids_kg)) %>%
  top_n(20) %>% 
  ggplot(aes(fct_reorder(village, moyenne, .desc = TRUE), moyenne)) +
  geom_col() +
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1)) +
  labs(x = "Village",
       y = "poids moyen")
  

# Par an par village // Il faut réfléchir à une graphe qui permet de comaprer tous les villages
poisson_data %>%
  group_by(village, year) %>% 
  summarise(moyenne = mean(poids_kg)) %>% 
  ggplot(aes(year, moyenne)) +
  geom_line() +
  facet_wrap(~village)

poisson_data %>%
  group_by(village, year) %>% 
  summarise(moyenne = mean(poids_kg)) %>% 
  ggplot(aes(year, moyenne, color = village)) +
  geom_line()

# Mensuration par village
poisson_data %>% 
  group_by(village) %>% 
  count(classe_de_taille) %>%
  mutate(start_class = parse_number(classe_de_taille, na = "Non classe")) %>% 
  ggplot(aes(x = fct_reorder(classe_de_taille, start_class), y = taille)) +
  geom_col()

# Famille



# ggsave(here::here("graph.png"), plot = graph)
