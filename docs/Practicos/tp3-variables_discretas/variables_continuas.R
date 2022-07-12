# ---- header ----

# importación de bibliotecas
library(tidyverse)
library(magrittr)
library(purrr)


# limpio el entorno
rm(list = ls())


# ---- generar datos ----
N = 50
set.seed(101)

# --- elegir el caso que más nos guste ---
# caso uniforme (entre 0 y 1)
realizaciones = runif(n=N, min=0, max=1)

# caso gaussiano
# mu    = 100
# sigma = 10
# realizaciones = rnorm(n=N, mean=mu, sd=sigma)

# ponerlos en un tibble
datos = tibble(id=1:N, value=realizaciones)

# ---- ítem a ----

# graficarlos
ggplot(datos, aes(x=id, y=value)) + geom_point()


# ---- ítem b ----

# histograma

# usando case_when:
datos %>%
  mutate(aux = case_when(value  > 0.5 ~ 1,
                         value <= 0.5 ~ 0)) %>%
  count(aux)

# usando hist:
histograma = datos %>%
  # obtengo el histograma gracias a la función hist
  with(hist(value,
            # breaks = c(0, 0.5, 1),
            breaks = seq(from=0, to=1, by=0.5), # para tener más bines cambiando fácilmente el código
            plot = FALSE)) %$%
  # almaceno la salida en un tibble
  tibble(from    = head(breaks, -1),
         to      = tail(breaks, -1),
         mids    = mids,
         counts  = counts,  # frecuencia absoluta
         density = density) # densidad

histograma

# graficarlo
histograma %>%
  # uso mids para centrar las columnas
  ggplot(aes(x = mids, y = counts)) +
  geom_col(color='black', fill='white') #width = 0.5)


# (para quien tenga curiosidad, así funciona hist:)

hist(datos$value, breaks = seq(from=0, to=1, by=0.1))

datos %$% hist(value, breaks = seq(from=0, to=1, by=0.1))  # pipe $

datos %>% hist(value, breaks = seq(from=0, to=1, by=0.1))  # pipe tradicional no funciona


# ---- ítem c ----

# frecuencia relativa: es la probabilidad dentro de un rango de valores, se
# estima usando la proporción entre frecuencia absoluta y cantidad total de
# datos
histograma %<>%
  mutate(f = counts / N)

# graficarla
histograma %>%
  ggplot(aes(x = mids, y = f)) +
  geom_col(color='black', fill='white') #width = 0.5)


# (cálculo de la densidad:
# recordar analogía con un cuerpo sólido: densidad*volumen = masa total
# volumen del cuerpo -> longitud del bin
# masa total -> probabilidad)

histograma %>% mutate(my_dens = f / (to - from)) # no almaceno este tibble ya que es redundante,
                                                 # es solo para chequear el cálculo


# ---- ítem d ----

# densidad teorica

# uniforme
densidad <-
  # cambiando los parámetros de seq elijo la resolución y límites del eje x:
  tibble(x = seq(from=-1, to=2, by=0.01)) %>%
  # mutate para evaluar dunif en los valores del eje x:
  mutate(pdf = dunif(x, min=0, max=1))

# gaussiana
# densidad <-
#   tibble(x = seq(from=50, to=150, by=0.01)) %>%
#   mutate(pdf = dnorm(x, mean=mu, sd=sigma))

densidad

# grafico usando dos datasets diferentes con dos geoms diferentes
ggplot() +
  # dataset histograma -> geom_col
  geom_col(data = histograma, aes(x = mids, y=density), color='black', fill='white') + #width = 0.5,
  # dataset densidad -> geom_line
  geom_line(data = densidad, aes(x = x, y = pdf), color = 'orange')

# ---- ítem e ----

# probabilidad acumulada experimental
histograma %<>% mutate(p = cumsum(f))
histograma

# teorica

# uniforme
densidad %<>% mutate(p = punif(x, min=0, max=1))
densidad

# guassiana
# densidad %<>% mutate(p = pnorm(x, mean=mu, sd=sigma))
# densidad

# graficarla
ggplot() +
  # en este caso, uso la columna to para el eje x (para la densidad usaba mids): ¿por qué?
  geom_line(data = histograma, aes(x = to, y = p), color = 'black') +
  #geom_col(data = histograma, aes(x = mids, y = p), color = 'black') +
  #geom_step(data = histograma, aes(x = mids, y = p), color = 'black') +
  geom_line(data = densidad, aes(x = x, y = p), color = 'orange')

# ---- ítem f ----
# usar N grande (5000)

# ---- Ejercicio 2 ----
# correr el código para el caso gaussiano



