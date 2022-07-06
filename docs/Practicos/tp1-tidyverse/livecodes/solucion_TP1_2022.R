library(tidyverse)

summer_tbl <- read_csv(file = "data/summer.csv")
winter_tbl <- read_csv(file = "data/winter.csv")
dictionary_tbl <- read_csv(file = "data/dictionary.csv")

# 1 ####
summer_por_disciplina_tbl <- summer_tbl %>%
  group_by(Year, City, Sport, Country, Discipline, Event, Gender, Medal) %>%
  summarise()

winter_por_disciplina_tbl <- winter_tbl %>%
  group_by(Year, City, Sport, Country, Discipline, Event, Gender, Medal) %>%
  summarise()

# 2 ####
oro_por_pais <- summer_por_disciplina_tbl %>%
  filter(Medal == "Gold") %>%
  group_by(Country) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))

oro_por_pais

# 3 ####
medal_por_pais <- summer_por_disciplina_tbl %>%
  group_by(Country, Medal) %>%
  summarise(Total = n())
medal_por_pais

# 4 ####
muchos_oro_por_pais <- summer_por_disciplina_tbl %>%
  filter(Medal == "Gold") %>%
  group_by(Country) %>%
  summarise(Total = n()) %>%
  filter(Total > 5)
muchos_oro_por_pais

# 5 ####
country_to_filter <- c("CAN", "URS")
medallas_hockey_tbl <- winter_por_disciplina_tbl %>%
  filter(Discipline == "Ice Hockey", Medal == "Gold", Gender == "Men") %>%
  filter(Country %in% country_to_filter) %>%
  group_by(Country) %>%
  summarise(Total = n())
medallas_hockey_tbl

# 6 ####
nombres_natacion_tbl <- summer_tbl %>%
  filter(Country == "ARG", Sport == "Aquatics") %>%
  select(-c("City", "Gender", "Country", "Sport"))
  select(c("Year", "Athlete", "Discipline", "Event", "Medal"))

nombres_natacion_tbl

# 7 ####
total_por_disciplina <- winter_por_disciplina_tbl %>%
  bind_rows(summer_por_disciplina_tbl)

top_3_tbl <- total_por_disciplina %>%
  filter(Medal == "Gold") %>%
  group_by(Country) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total)) %>%
  slice_head(n = 3)

write_csv(file = "data/prueba.csv", x = top_3_tbl)

# 8 ####
medallas_per_million_tbl <- winter_por_disciplina_tbl %>%
  bind_rows(summer_por_disciplina_tbl) %>%
  group_by(Country) %>%
  summarise(Total = n()) %>%
  rename("Code" = "Country") %>%
  inner_join(dictionary_tbl) %>%
  drop_na(Population) %>%
  mutate(medals_por_millon = Total/Population * 1000000) %>%
  select(c("Country", "medals_por_millon")) %>%
  arrange(desc(medals_por_millon))

medallas_per_million_tbl
