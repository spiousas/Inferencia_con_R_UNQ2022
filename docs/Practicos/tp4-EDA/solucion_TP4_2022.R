library(tidyverse)
library(here)

# leer los datos
netflix <- read_csv("docs/Practicos/tp4-EDA/data/NetflixOriginals.csv")
#netflix <- read_csv(here("docs/Practicos/tp4-EDA/data/NetflixOriginals.csv"))
spec(netflix)

# 1 - Mirar los datos ----------------------------------------------------------
#
# Usemos las funciones `summary()`, `str()` y `glimpse()` para ver qué estructura
# y qué tipos de variables tiene nuestro dataset
#
# - ¿Tiene valores `NA`?
# - ¿Alguna de las variables no es del tipo que corresponde?
# - ¿Hay algún valor sospechoso?

summary(netflix)
str(netflix)
glimpse(netflix)

library(lubridate)
a <- mdy("August 5, 2019")
b <- dmy("13-8-2021")
b - a

netflix <- netflix %>%
  mutate(Premiere = mdy(Premiere))
str(netflix)
summary(netflix)

netflix %>% drop_na()

netflix %>% mutate(`IMDB Score` = replace_na(`IMDB Score`, -1)) %>%
  summary()

netflix %>% mutate(`IMDB Score` = replace_na(`IMDB Score`, mean(`IMDB Score`, na.rm = TRUE))) %>%
  summary()

netflix <- netflix %>% drop_na()

# Una vez corregidos los problemas del dataset imprimir un resumen usando la función `skim()` del paquete *{skimr}*.

library(skimr)
skim(netflix)

# ¿Cuáles son los tres géneros con más estrenos?
netflix %>% count(Genre) %>% arrange(desc(n)) %>% slice_head(n = 3)

# ¿Y los tres idiomas con más estrenos?
netflix %>% count(Language) %>% arrange(desc(n)) %>% slice_head(n = 3)

# 2 - Variación ----------------------------------------------------------------
#
# Exploremos con un simple gráfico de barras la cantidad de películas de cada género.
#
# - ¿Cuántos géneros hay?
# - ¿Cómo es la distribución por género?

netflix %>%
  count(Genre, name = "Frecuencia") %>%
  filter(Frecuencia > 5) %>%
  ggplot(aes(x = reorder(Genre, Frecuencia), y = Frecuencia)) +
  geom_col() +
  coord_flip()

table(netflix$Language)

# Ahora miremos la distribución de duraciones y de rating de **IMDB** ¿Qué podemos decir al respecto?

netflix %>%
  ggplot(aes(x = Runtime)) +
  geom_histogram(bins = 20)


netflix %>%
  ggplot(aes(x = `IMDB Score`)) +
  geom_histogram(binwidth = 1)

# Veamos cómo se distribuyen los ratings de **IMDB** para los géneros *Drama* y *Comedy*.

generos <- c("Drama", "Comedy")
netflix %>% filter(Genre %in% generos) %>%
  ggplot(aes(x = `IMDB Score`,
             color = Genre)) +
  geom_freqpoly() +
  theme_light()

netflix %>% filter(Genre %in% generos) %>% group_by(Genre) %>% summarise(media = mean(`IMDB Score`))

# Por último: ¿Qué pasa con las distribuciones de duraciones para *Comedy* y *Documentary*?

generos <- c("Documentary", "Comedy")
netflix %>% filter(Genre %in% generos) %>%
  ggplot(aes(x = Runtime,
             color = Genre)) +
  geom_freqpoly() +
  theme_light()

# 3 - Covariación --------------------------------------------------------------
#
# Utilizando un boxplot veamos si hay alguna relación entre las películas de los géneros *Comedy*, *Drama* y *Documentary* y su rating de **IMDB**.

generos <- c("Documentary", "Comedy", "Drama")
netflix %>% filter(Genre %in% generos) %>%
  ggplot(aes(x = Genre, y = `IMDB Score`, color = Genre)) +
  geom_boxplot() +
  #coord_flip() +
  theme_light()

# Luego, usando la función `geom_tile()` miremos la cantidad de muestras para las combinaciones de los tres géneros y los tres idiomas con más estrenos.

generos <- c("Documentary", "Comedy", "Drama")
idiomas <- c("English", "Hindi", "Spanish")
netflix %>%
  filter(Genre %in% generos) %>%
  filter(Language %in% idiomas) %>%
  count(Genre, Language, name = "Frecuencia") %>%
  ggplot(aes(x = Genre, y = Language, fill = Frecuencia)) +
  geom_tile() +
  geom_text(aes(label = Frecuencia), size = 3, color = "white") +
  scale_fill_viridis_c() +
  theme_minimal()

# Ahora vamos a ver la covariación entre dos variables continuas. Vemos si existe alguna relación entre la fecha de estreno y el rating de **IMDB**.

netflix %>%
  ggplot(aes(x = Premiere, y = `IMDB Score`)) +
  geom_point(alpha = 0.8, color = "lightblue") +
  geom_smooth(method = lm) +
  theme_minimal()

# ¿Y si nos quedamos con los tres géneros más populares y lo vemos por género?

generos <- c("Documentary", "Comedy", "Drama")

netflix %>%
  filter(Genre %in% generos) %>%
  ggplot(aes(x = Premiere, y = `IMDB Score`, color = Genre)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = lm, se = FALSE) +
  theme_minimal()

# Por último, utilicemos la función `ggpairs()` de *{GGally}* para ver las distribuciones y correlaciones de todas las variables numéricas de `netflix`.

library(GGally)
netflix %>%
  select(c("Premiere", "Runtime", "IMDB Score")) %>%
  ggpairs() +
  theme_minimal()


# 4 - Outliers -----------------------------------------------------------------
#
# Usemos la librería *{Routliers}* para ver si tenemos *outliers* univariados en las variables `Runtime` (duración) y `IMDB Score` (rating de IMDB)

library(Routliers)
outliers_runtime <- outliers_mad(x = netflix$Runtime)
outliers_runtime
outliers_runtime$outliers
outliers_runtime$outliers_pos
outliers_runtime$limits[1]
outliers_runtime$limits[2]

plot_outliers_mad(outliers_runtime,
                  x = netflix$Runtime)

netflix %>% filter(between(Runtime, outliers_runtime$limits[1], outliers_runtime$limits[2]))

# ¿Qué podemos decir de los *outliers* de `Runtime`? ¿Los podemos categorizar de alguna forma? ¿Nos pueden dar alguna información sobre las producciones de **Netflix**?

# ¿Y sobre los *outliers* de rating de **IMDB**?
outliers_imdb <- outliers_mad(x = netflix$`IMDB Score`)
outliers_imdb

netflix %>% filter(`IMDB Score`<outliers_imdb$limits[1])

# Por último, analicemos si hay outliers multivariados en ambas variables.
outliers_multi <- outliers_mcd(x = cbind(netflix$Runtime,
                                         netflix$`IMDB Score`))
outliers_multi

plot_outliers_mcd(outliers_multi,
                  x = cbind(netflix$Runtime,
                            netflix$`IMDB Score`))

