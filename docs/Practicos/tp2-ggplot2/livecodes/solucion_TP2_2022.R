library(tidyverse)

summer_tbl <- read_csv("tp1-tidyverse/data/summer.csv")

summer_por_disciplina_tbl <- summer_tbl %>%
  group_by(Year, City, Sport, Country, Discipline, Event, Gender, Medal) %>%
  summarise() # No hace ninguna operación

winter_tbl <- read_csv("tp1-tidyverse/data/winter.csv")

winter_por_disciplina_tbl <- winter_tbl %>%
  group_by(Year, City, Sport, Country, Discipline, Event, Gender, Medal) %>%
  summarise() # No hace ninguna operación

dictionary_tbl <- read_csv("tp1-tidyverse/data/dictionary.csv")

# 1 ####
oro_por_pais <- summer_por_disciplina_tbl %>%
  filter(Medal == "Gold") %>%
  group_by(Country) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total)) %>%
  slice_head(n = 10)

ggplot(data = oro_por_pais,
       aes(x = reorder(Country, Total),
           y = Total)) +
  geom_col(color = "white",
           fill = "#1380A1") +
  geom_point(aes(size = Total)) +
  coord_flip() +
  labs(x = "País",
       y = "Medallas de oro acumuladas",
       title = "Top 10 países con más medallas de oro aculumadas") +
  theme_minimal()

# 2 ####
summer_por_disciplina_tbl %>%
  filter(Medal == "Gold") %>%
  group_by(Country) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total)) %>%
  ggplot(aes(x = Total)) +
  geom_histogram(color = "white",
                 fill = "#1380A1") +
  theme_minimal()

# 3 ####
summer_por_disciplina_tbl %>%
  group_by(Country, Medal) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total)) %>%
  ggplot(aes(x = Total, fill = Medal)) +
  geom_histogram(position = "dodge", binwidth = 100) +
  scale_fill_manual(breaks = c("Gold", "Silver", "Bronze"),
                    values = c("#BC9B69", "#C4C5C7", "#B88748")) +
  theme_minimal() +
  theme(legend.position = "top")

# 4 ####
summer_por_disciplina_tbl %>%
  group_by(Country, Medal) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total)) %>%
  ggplot(aes(x = Total, fill = Medal)) +
  geom_histogram(position = "dodge", binwidth = 20) +
  facet_grid(Medal ~ .) +
  scale_fill_manual(breaks = c("Gold", "Silver", "Bronze"),
                    values = c("#BC9B69", "#C4C5C7", "#B88748")) +
  theme_minimal() +
  theme(legend.position = "top")

# 5 ####
winter_por_disciplina_tbl %>%
  filter(Medal == "Gold") %>%
  group_by(Country, Year) %>%
  summarise(Total = n()) %>%
  ggplot(aes(x = Total)) +
  geom_histogram(fill = "#BC9B69",
                 position = "dodge",
                 binwidth = 4) +
  theme_minimal() +
  facet_wrap(. ~ Year) +
  theme(legend.position = "top")

# 6 ####
summer_por_disciplina_tbl %>%
  bind_rows(winter_por_disciplina_tbl) %>%
  filter(Medal == "Gold") %>%
  group_by(Country) %>%
  summarise(Total = n()) %>%
  rename("Code" = "Country") %>%
  inner_join(dictionary_tbl) %>%
  drop_na() %>%
  ggplot(aes(x = log(`GDP per Capita`), y = log(Total))) +
  geom_point(color = "#1380A1") +
  labs(x = "log(PBI per cápita)",
       y = "log(Medallas de oro acumuladas)",
       title = "Medallas de oro acumuladas vs. PBI per cápita") +
  theme_minimal()

# 6 ####
summer_por_disciplina_tbl %>%
  bind_rows(winter_por_disciplina_tbl) %>%
  group_by(Country, Medal) %>%
  summarise(Total = n()) %>%
  rename("Code" = "Country") %>%
  inner_join(dictionary_tbl) %>%
  drop_na() %>%
  ggplot(aes(x = log(`GDP per Capita`), y = log(Total), color = Medal)) +
  geom_point(size = 1, alpha = .5) +
  #facet_wrap(. ~ Medal) +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "PBI per cápita",
       y = "Medallas de oro acumuladas",
       color = NULL,
       title = "Medallas de oro acumuladas vs. PBI per cápita") +
  scale_color_manual(breaks = c("Gold", "Silver", "Bronze"),
                     values = c("#BC9B69", "#C4C5C7", "#392916")) +
  theme_minimal() +
  theme(legend.position = "top")



