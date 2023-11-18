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
         cpue = poids_kg/nombre_de_personne_sur_une_pirogue,
         year = year(date)
  )


poulpe_data <- poulpe %>% 
  select(date, village, poids_kg, nombre_de_personne_sur_une_pirogue, famille, classe_de_taille, moyenne) %>% 
  mutate(date = dmy(date),
         poids_kg = parse_number(poids_kg),
         moyenne = parse_number(moyenne),
         cpue = poids_kg/nombre_de_personne_sur_une_pirogue,
         year = year(date)
  )


# Réalisation de graphe
theme_set(theme_light())

# Par année
evolution_annuelle <- poisson_data %>%
  filter(village %in% selection_village) %>% 
  group_by(year) %>% 
  summarise(moyenne_annuelle = mean(cpue)) %>% 
  ggplot(aes(year, moyenne_annuelle)) +
  geom_line(size = 1.5) +
  labs(x = "Year",
       y = "CPUE (kg/fisher/trip)")

evolution_poulpe <- poulpe_data %>%
  filter(village %in% selection_village) %>% 
  group_by(year) %>% 
  summarise(moyenne_annuelle = mean(cpue)) %>% 
  ggplot(aes(year, moyenne_annuelle)) +
  geom_line(size = 1.5) +
  labs(x = "Year",
       y = "CPUE (kg/fisher/trip)")

# ggsave(here::here("plot","evolution_annuelle.png"), plot = evolution_annuelle, width = 200, height = 125, units = "mm")
# ggsave(here::here("plot","evolution_poulpe.png"), plot = evolution_poulpe, width = 200, height = 125, units = "mm")

# Par village

selection_village <- c("MAROMENA", "BEFASY", "AMBOHIBOLA", "BESAMBAY", "ITAMPOLO", "BEHELOKE", "TARIBOLY", "AMBOLA", "ANDOMOTSE")

cpue <- poisson_data %>%
  filter(village %in% selection_village) %>% 
  group_by(village,year) %>% 
  summarise(moyenne = mean(cpue)) %>%
  ggplot(aes(year, moyenne)) +
  geom_line() +
  facet_wrap(~village) +
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1)) +
  labs(x = "Year",
       y = "CPUE (kg/fisher/trip)")

# ggsave(here::here("plot","cpue.png"), plot = cpue, width = 200, height = 125, units = "mm")


# Mensuration par village
mensuration_2015 <- poisson_data %>% 
  filter(year ==  2015) %>%
  filter(classe_de_taille != "Non classe") %>% 
  filter(village %in% selection_village) %>% 
  count(classe_de_taille) %>%
  mutate(pourcentage = (n/sum(n))) %>% 
  mutate(start_class = parse_number(classe_de_taille, na = "Non classe")) %>% 
  mutate(classe_de_taille = as.factor(classe_de_taille)) %>% 
  ggplot(aes(x = fct_reorder(classe_de_taille, start_class), y = pourcentage)) +
  geom_col() +
  geom_line(size = 1, color = "red", group = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage",
       x = "length class",
       title = "2015") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

# ggsave(here::here("plot","mensuration_2020.png"), plot = mensuration_2020, width = 200, height = 125, units = "mm")


mensuration_2016 <- poisson_data %>% 
  filter(year ==  2016) %>%
  filter(classe_de_taille != "Non classe") %>% 
  filter(village %in% selection_village) %>% 
  count(classe_de_taille) %>%
  mutate(pourcentage = (n/sum(n))) %>% 
  mutate(start_class = parse_number(classe_de_taille, na = "Non classe")) %>% 
  mutate(classe_de_taille = as.factor(classe_de_taille)) %>% 
  ggplot(aes(x = fct_reorder(classe_de_taille, start_class), y = pourcentage)) +
  geom_col() +
  geom_line(size = 1, color = "red", group = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage",
       x = "length class",
       title = "2016") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

mensuration_2017 <- poisson_data %>% 
  filter(year ==  2017) %>%
  filter(classe_de_taille != "Non classe") %>% 
  filter(village %in% selection_village) %>% 
  count(classe_de_taille) %>%
  mutate(pourcentage = (n/sum(n))) %>% 
  mutate(start_class = parse_number(classe_de_taille, na = "Non classe")) %>% 
  mutate(classe_de_taille = as.factor(classe_de_taille)) %>% 
  ggplot(aes(x = fct_reorder(classe_de_taille, start_class), y = pourcentage)) +
  geom_col() +
  geom_line(size = 1, color = "red", group = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage",
       x = "length class",
       title = "2017") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

mensuration_2018 <- poisson_data %>% 
  filter(year ==  2018) %>%
  filter(classe_de_taille != "Non classe") %>% 
  filter(village %in% selection_village) %>% 
  count(classe_de_taille) %>%
  mutate(pourcentage = (n/sum(n))) %>% 
  mutate(start_class = parse_number(classe_de_taille, na = "Non classe")) %>% 
  mutate(classe_de_taille = as.factor(classe_de_taille)) %>% 
  ggplot(aes(x = fct_reorder(classe_de_taille, start_class), y = pourcentage)) +
  geom_col() +
  geom_line(size = 1, color = "red", group = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage",
       x = "length class",
       title = "2018") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

mensuration_2019 <- poisson_data %>% 
  filter(year ==  2019) %>%
  filter(classe_de_taille != "Non classe") %>% 
  filter(village %in% selection_village) %>% 
  count(classe_de_taille) %>%
  mutate(pourcentage = (n/sum(n))) %>% 
  mutate(start_class = parse_number(classe_de_taille, na = "Non classe")) %>% 
  mutate(classe_de_taille = as.factor(classe_de_taille)) %>% 
  ggplot(aes(x = fct_reorder(classe_de_taille, start_class), y = pourcentage)) +
  geom_col() +
  geom_line(size = 1, color = "red", group = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage",
       x = "length class",
       title = "2019") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

mensuration_2020 <- poisson_data %>% 
  filter(year ==  2020) %>%
  filter(classe_de_taille != "Non classe") %>% 
  filter(village %in% selection_village) %>% 
  count(classe_de_taille) %>%
  mutate(pourcentage = (n/sum(n))) %>% 
  mutate(start_class = parse_number(classe_de_taille, na = "Non classe")) %>% 
  mutate(classe_de_taille = as.factor(classe_de_taille)) %>% 
  ggplot(aes(x = fct_reorder(classe_de_taille, start_class), y = pourcentage)) +
  geom_col() +
  geom_line(size = 1, color = "red", group = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage",
       x = "length class",
       title = "2020") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

# Famille

famille_2015 <- poisson_data %>% 
  filter(year ==  2015) %>%
  filter(!(famille %in% c("non defini", "A identifier"))) %>% 
  filter(classe_de_taille != "Non classe") %>% 
  filter(village %in% selection_village) %>% 
  count(famille) %>%
  mutate(pourcentage = n/sum(n)) %>% 
  mutate(famille = as.factor(famille)) %>% 
  arrange(desc(n)) %>% 
  top_n(20, n) %>% 
  ggplot(aes(fct_reorder(famille, pourcentage), pourcentage)) +
  geom_col() +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  labs(x = "family",
       y = "percentage",
       title = "2015") +
  scale_y_continuous(labels = scales::percent)

# ggsave(here::here("plot","famille_2020.png"), plot = famille_2020, width = 200, height = 125, units = "mm")


famille_2016 <- poisson_data %>% 
  filter(year ==  2016) %>%
  filter(!(famille %in% c("non defini", "A identifier"))) %>% 
  filter(classe_de_taille != "Non classe") %>% 
  filter(village %in% selection_village) %>% 
  count(famille) %>%
  mutate(pourcentage = n/sum(n)) %>% 
  mutate(famille = as.factor(famille)) %>% 
  arrange(desc(n)) %>% 
  top_n(20, n) %>% 
  ggplot(aes(fct_reorder(famille, pourcentage), pourcentage)) +
  geom_col() +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  labs(x = "family",
       y = "percentage",
       title = "2016") +
  scale_y_continuous(labels = scales::percent)

famille_2017 <- poisson_data %>% 
  filter(year ==  2017) %>%
  filter(!(famille %in% c("non defini", "A identifier"))) %>% 
  filter(classe_de_taille != "Non classe") %>% 
  filter(village %in% selection_village) %>% 
  count(famille) %>%
  mutate(pourcentage = n/sum(n)) %>% 
  mutate(famille = as.factor(famille)) %>% 
  arrange(desc(n)) %>% 
  top_n(20, n) %>% 
  ggplot(aes(fct_reorder(famille, pourcentage), pourcentage)) +
  geom_col() +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  labs(x = "family",
       y = "percentage",
       title = "2017") +
  scale_y_continuous(labels = scales::percent)

famille_2018 <- poisson_data %>% 
  filter(year ==  2018) %>%
  filter(!(famille %in% c("non defini", "A identifier"))) %>% 
  filter(classe_de_taille != "Non classe") %>% 
  filter(village %in% selection_village) %>% 
  count(famille) %>%
  mutate(pourcentage = n/sum(n)) %>% 
  mutate(famille = as.factor(famille)) %>% 
  arrange(desc(n)) %>% 
  top_n(20, n) %>% 
  ggplot(aes(fct_reorder(famille, pourcentage), pourcentage)) +
  geom_col() +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  labs(x = "family",
       y = "percentage",
       title = "2018") +
  scale_y_continuous(labels = scales::percent)

famille_2019 <- poisson_data %>% 
  filter(year ==  2019) %>%
  filter(!(famille %in% c("non defini", "A identifier"))) %>% 
  filter(classe_de_taille != "Non classe") %>% 
  filter(village %in% selection_village) %>% 
  count(famille) %>%
  mutate(pourcentage = n/sum(n)) %>% 
  mutate(famille = as.factor(famille)) %>% 
  arrange(desc(n)) %>% 
  top_n(20, n) %>% 
  ggplot(aes(fct_reorder(famille, pourcentage), pourcentage)) +
  geom_col() +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  labs(x = "family",
       y = "percentage",
       title = "2019") +
  scale_y_continuous(labels = scales::percent)


famille_2020 <- poisson_data %>% 
  filter(year ==  2020) %>%
  filter(!(famille %in% c("non defini", "A identifier"))) %>% 
  filter(classe_de_taille != "Non classe") %>% 
  filter(village %in% selection_village) %>% 
  count(famille) %>%
  mutate(pourcentage = n/sum(n)) %>% 
  mutate(famille = as.factor(famille)) %>% 
  arrange(desc(n)) %>% 
  top_n(20, n) %>% 
  ggplot(aes(fct_reorder(famille, pourcentage), pourcentage)) +
  geom_col() +
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  labs(x = "family",
       y = "percentage",
       title = "2020") +
  scale_y_continuous(labels = scales::percent)
  
