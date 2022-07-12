# ---- header ----

# importación de bibliotecas
library(tidyverse)
library(magrittr) # magrittr me permite usar nuevos pipes: %<>%, %$%


# limpio el entorno
rm(list = ls())


# ---- ejercicio 1 -----
N = 50
set.seed(101)
realizaciones = base::sample(x=0:1, size=N, replace=T, prob=c(0.5, 0.5))

realizaciones


datos = tibble(id=1:N, valor=realizaciones)

datos

# ---- ítem a ----

# aplicar etiquetas
datos %<>% mutate(valor_label = case_when(valor==1 ~ "cara",
                                          valor==0 ~ "ceca"))

datos

# ---- ítem b ----

# frecuencia absoluta
histograma = datos %>% count(valor)

# frecuencia relativa
histograma %<>% mutate(f = n/N)

histograma

# ---- ítem c ----

# graficar

histograma %>% ggplot(aes(x=valor, y=f)) + geom_col() + # frec relat
  # límites eje y entre 0 y 1
  scale_y_continuous(limits = c(0,1))

histograma %>% ggplot(aes(x=valor, y=n)) + geom_col() + # frec abs
  # limites eje y entre 0 y N
  scale_y_continuous(limits = c(0,N))

# ---- ítem d ----
# usar N = 5000 o más grande

# ---- ítem e ----

# suma parcial
datos %<>% mutate(suma_parcial = cumsum(valor))


# ---- ítem f ----

# graficar la suma parcial
datos %>% ggplot(aes(x=id, y=suma_parcial)) + geom_line()

# ---- ítem g ----

# frecuencia relativa parcial (a medida que incorporo datos)
datos %>%
  # filter(id > 100) %>%
  ggplot(aes(x=id, y=suma_parcial/id)) +
  geom_line() +
  geom_hline(yintercept =0.5, linetype = "dashed")

# suma parcial

#  todo en un pipe
datos %>%
  mutate(suma_parcial = cumsum(valor)) %>%
  ggplot(aes(x = id, y = suma_parcial)) + geom_line()

# de la otra manera: guardo los datos primero y luego grafico
datos %<>%
  mutate(suma_parcial = cumsum(valor))

datos %>%
  ggplot(aes(x = id, y = suma_parcial)) + geom_line()

# grafico
datos %>% ggplot(aes(x = id, y = suma_parcial))

# frecuencia relativa parcial

datos %<>% mutate( f_parcial = suma_parcial / id)

datos %>%
  filter(id > 1000) %>%
  ggplot(aes(x = id, y = f_parcial)) + geom_line()

