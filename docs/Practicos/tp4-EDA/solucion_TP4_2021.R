library(tidyverse)
library(here)

# leer los datos
netflix <- read_csv(here("docs/Practicos/tp4-EDA/data/NetflixOriginals.csv"))

# 1 - Mirar los datos ----------------------------------------------------------
#
# Usemos las funciones `summary()`, `str()` y `glimpse()` para ver qué estructura
# y qué tipos de variables tiene nuestro dataset
#
# - ¿Tiene valores `NA`?
# - ¿Alguna de las variables no es del tipo que corresponde?
# - ¿Hay algún valor sospechoso?
#
# Una vez corregidos los problemas del dataset imprimir un resumen usando la función `skim()` del paquete *{skimr}*.
#
# ¿Cuáles son los tres géneros con más estrenos?
#
# ¿Y los tres idiomas con más estrenos?

summary(netflix)
str(netflix)
glimpse(netflix)

netflix %>% drop_na()

netflix %>%
  mutate(`IMDB Score` = replace_na(`IMDB Score`, 0)) %>%
  summary()

netflix %>%
  mutate(`IMDB Score` = replace_na(`IMDB Score`, mean(`IMDB Score`, na.rm = TRUE))) %>%
  summary()

netflix <- netflix %>% drop_na()

library(lubridate)
a <- dmy("13-8-2021")
b <- mdy("August 5, 2019")
a-b

# Mencionar el tema de las variables que deberían ser categóricas y son ordinales (ID de sujeto o animal)
as.character(netflix$Runtime)

netflix <- netflix %>%
  mutate(Premiere = mdy(Premiere))
glimpse(netflix)

library(skimr)
skim(netflix)
# Hablar de cómo ya sabe que la variable date es una fecha
# Mencionar el tema de los n_unique y cómo eso nos puede ayudar a entender un dataset

netflix %>%
  count(Genre) %>%
  arrange(desc(n)) %>%
  slice_head(n = 3)
# Hablar de los idiomas compuestos

netflix %>%
  count(Language, name = "Cantidad") %>%
  arrange(desc(Cantidad)) %>%
  slice_head(n = 3)

# 2 - Variación ----------------------------------------------------------------
#
# Exploremos con un simple gráfico de barras la cantidad de películas de cada género.
#
# - ¿Cuántos géneros hay?
# - ¿Cómo es la distribución por género?
#
# Ahora miremos la distribución de duraciones y de rating de **IMDB** ¿Qué podemos decir al respecto?
#
# Veamos cómo se distribuyen los ratings de **IMDB** para los géneros *Drama* y *Comedy*.
#
# Por último: ¿Qué pasa con las distribuciones de duraciones para *Comedy* y *Documentary*?

netflix %>%
  group_by(Genre) %>%
  summarise(N = n()) %>%
  ggplot(aes(x = Genre,
             y = N)) +
  geom_col()

netflix %>%
  group_by(Genre) %>%
  summarise(N = n()) %>%
  ggplot(aes(x = Genre,
             y = N)) +
  geom_col() +
  coord_flip() +
  theme(axis.text.y = element_text(size = 5))

netflix %>%
  group_by(Genre) %>%
  summarise(N = n()) %>%
  ggplot(aes(x = reorder(Genre, N),
             y = N)) +
  geom_col() +
  coord_flip() +
  theme(axis.text.y = element_text(size = 5))

netflix %>%
  group_by(Genre) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(Genre, N),
             y = N)) +
  geom_col() +
  coord_flip()

table(netflix$Genre)

netflix %>%
  ggplot(aes(x = `IMDB Score`)) +
  geom_histogram(bins = 10) +
  theme_minimal()

netflix %>%
  ggplot(aes(x = Runtime)) +
  geom_histogram() +
  theme_minimal()

netflix %>%
  ggplot(aes(x = Premiere)) +
  geom_histogram() +
  theme_minimal()

generos <- c("Drama", "Comedy")
netflix %>%
  filter(Genre %in% generos) %>%
  ggplot(aes(x = `IMDB Score`,
             color = Genre)) +
  geom_freqpoly() +
  theme_minimal()

netflix %>%
  filter(Genre %in% generos) %>%
  group_by(Genre) %>%
  summarise(mean_rating = mean(`IMDB Score`),
            sd_rating = sd(`IMDB Score`))

generos <- c("Documentary", "Comedy")
netflix %>%
  filter(Genre %in% generos) %>%
  ggplot(aes(x = Runtime,
             color = Genre)) +
  geom_freqpoly() +
  theme_minimal()

netflix %>%
  filter(Genre %in% generos) %>%
  group_by(Genre) %>%
  summarise(mean_rating = mean(`IMDB Score`),
            sd_rating = sd(`IMDB Score`))

# 3 - Covariación --------------------------------------------------------------
#
# Utilizando un boxplot veamos si hay alguna relación entre las películas de los géneros *Comedy*, *Drama* y *Documentary* y su rating de **IMDB**.
#
# Luego, usando la función `geom_tile()` miremos la cantidad de muestras para las combinaciones de los tres géneros y los tres idiomas con más estrenos.
#
# Ahora vamos a ver la covariación entre dos variables continuas. Vemos si existe alguna relación entre la fecha de estreno y el rating de **IMDB**.
#
# ¿Y si nos quedamos con los tres géneros más populares y lo vemos por género?
#
# Por último, utilicemos la función `ggpairs()` de *{GGally}* para ver las distribuciones y correlaciones de todas las variables numéricas de `netflix`.

generos <- c("Drama", "Comedy", "Documentary")
netflix %>%
  filter(Genre %in% generos) %>%
  ggplot(aes(y = `IMDB Score`,
             x = Genre,
             color = Genre)) +
  geom_boxplot() +
  scale_fill_viridis_b() +
  theme_minimal()

# Hablar de qué simboliza cada cosa en un boxplot

idiomas <- c("English", "Spanish", "Hindi")
netflix %>%
  filter(Genre %in% generos) %>%
  filter(Language %in% idiomas) %>%
  count(Genre, Language) %>%
  ggplot(aes(y = Language,
             x = Genre,
             fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal()

netflix %>%
  filter(Genre %in% generos) %>%
  filter(Language %in% idiomas) %>%
  count(Genre, Language) %>%
  ggplot(aes(y = Language,
             x = Genre,
             fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), color = "white") +
  scale_fill_viridis_c() +
  theme_minimal()


# Hablar de para qué sirve el tile. Si por una combinación de factores nos mandamos alguna cagada.

netflix %>%
  ggplot(aes(x = Premiere,
             y = `IMDB Score`)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = lm) +
  theme_minimal()

netflix %>%
  filter(Genre %in% generos) %>%
  ggplot(aes(x = Premiere,
             y = `IMDB Score`,
             color = Genre)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = lm,
              se = FALSE) +
  theme_minimal()

library(GGally)
netflix %>%
  select(all_of(c("Premiere", "Runtime", "IMDB Score"))) %>%
  ggpairs() +
  theme_minimal()

library(corrplot)
mydata.cor = cor(netflix %>%
                   select_if(is.numeric))
corrplot(mydata.cor, tl.cex = .6)

# 4 - Outliers -----------------------------------------------------------------
#
# Usemos la librería *{Routliers}* para ver si tenemos *outliers* univariados en las variables `Runtime` (duración) y `IMDB Score` (rating de IMDB)
#
# ¿Qué podemos decir de los *outliers* de `Runtime`? ¿Los podemos categorizar de alguna forma? ¿Nos pueden dar alguna información sobre las producciones de **Netflix**?
#
# ¿Y sobre los *outliers* de rating de **IMDB**?
#
# Por último, analicemos si hay outliers multivariados en ambas variables.

library(Routliers)

# Otliers:
#   Error: Errores de tipeo, etc.
#   Sistemáticos: Valores extremos relacionados con alguna estructura de nuestros datos que no tuvimos en cuenta en el diseño
#   De distribución: Simplemente valores extremos

median(netflix$Runtime)

outliers_runtime <- outliers_mad(x = netflix$Runtime)
outliers_runtime

plot_outliers_mad(outliers_runtime,
                  x = netflix$Runtime)

netflix %>% filter(Runtime>outliers_runtime$limits[1] & Runtime<outliers_runtime$limits[2])

netflix %>%
  filter(Runtime < outliers_runtime$limits[1]) %>%
  count(Genre) %>%
  arrange(desc(n))

netflix %>%
  filter(Runtime > outliers_runtime$limits[2])

outliers_IMDB <- outliers_mad(x = netflix$`IMDB Score`)
outliers_IMDB

plot_outliers_mad(outliers_IMDB,
                  x = netflix$`IMDB Score`)

netflix %>%
  filter(`IMDB Score` < outliers_IMDB$limits[1])

# Ejemplo de outlier multivariado, persona de 1.9 metros de alturay de 55 Kg de
outliers_multi <- outliers_mcd(x = cbind(netflix$Runtime,
                                         netflix$`IMDB Score`))
outliers_multi

plot_outliers_mcd(outliers_multi,
                  x = cbind(netflix$Runtime,
                            netflix$`IMDB Score`))

