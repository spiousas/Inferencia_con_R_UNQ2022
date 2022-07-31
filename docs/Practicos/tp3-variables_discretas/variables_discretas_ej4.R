# tutorial para tener en cuenta: https://adv-r.hadley.nz/functionals.html

# ---- header ----
# importación de bibliotecas (aka paquetes, librerías)
library(tidyverse)  # nuestro caballito de batalla
library(magrittr)   # necesario para extender pipes

rm(list = ls())


# ---- parametros ----
par <- list(p = 0.1,  # probabilidad de cara <------------------------------ este parámetro no está incluído dentro del código
            N = 10,   # cantidad de monedas tiradas en cada experimento
        N_rep = 050)  # cantidad de repeticiones del experimento

# parámetros alternativos
par <- list(p = 0.5,
            N = seq(from = 10, to = 100, by = 10),
            #N = 10,
        N_rep = c(50, 5000)
        #N_rep = 500
        )

par

# puedo acceder a los parámetros usando $:
# par$p
# par$N
# par$N_rep


# guardo los parámetros en un tibble (que luego
# usaré para almacenar las simulaciones)
sims = par %>% cross_df() %>%
  # agrego un id para cada caso (innecesario por ahora)
  mutate(id = 1:n(), .before =1)

sims


# ---- empieza la historia ----

# 1. genero un vector de datos (suelto)
sims %>%
  slice(1) %$%
  base::sample(x    = 0:1,
               size = N*N_rep,
               prob = c(p, 1-p),
            replace = T)

# esta es una forma de generar datos anidados dentro del tibble
# (aunque no son tibbles), es necesario usar list para armar la
# nueva columna
sims %>%
  group_by(id) %>% # <------------------------------ probar comentando group_by
  mutate(data = list(base::sample(x    = 0:1,
                                  size = N*N_rep,
                                  prob = c(1-p, p),
                               replace = T))) %$%
  data

# puedo usar pipes dentro del mutate (obtengo mismo resultado)
sims %>%
  group_by(id) %>%
  mutate(data = base::sample(x    = 0:1,
                             size = N*N_rep,
                             prob = c(1-p, p),
                          replace = T) %>%
           list() ) %$%
  data

# obtengo un tibble anidado (uso tibble antes de list)
sims %<>%
  group_by(id) %>%
  mutate(data = base::sample(x    = 0:1,
                             size = N*N_rep,
                             prob = c(1-p, p),
                          replace = T) %>%
           # guardo los datos en una columna llamada values.
           # el punto refiere a los datos pasados a través
           # del pipe previo
           tibble(values = .) %>%
           list()
           )

# para ver al tibble anidado:
sims$data[[1]]   # debo usar corchetes ya que la columna data es una lista
                 # el índice 1 indica el primer (y único) elemento de la lista

# si quiero los datos en forma de vector
sims$data[[1]]$values

# usando pipes
sims %$%           # pipe de exposición (gracias a magrittr)
  data %>%
  '[['(1) %$%      # función '[[' para extraer elementos, luego pipe $
  values

# si tengo más de un tibble anidado
# sims$data[[2]]


# 2. ahora voy a modificar el tibble anidado para incorporar la
#    estructura de las tiradas

sims %<>%
  mutate(data = pmap(list(N, N_rep, data),
                     ~ data[[1]] %>%
                       mutate(id      = 1:(N*N_rep),               # id del dato, es el nro de fila
                              # rep permite repetir secuencias de dos maneras diferentes
                              # usando each o times según el caso
                              id_rep  = rep(1:N_rep, each = N),    # id de la repeticion, va de 1 a N_rep
                              id_coin = rep(1:N, times = N_rep),   # id de la moneda dentro de una repetición
                              .before = 1)
                    ))

# verificar la estructura de id's usando View()
sims$data[[1]] %>% View()

# si tengo más de un tibble anidado (usando la función walk)
# walk(sims$data, View)       # sin pipes
# sims %$% walk(data, View)   # con pipes


# podria haber incluido la estructura al generar el tibble anidado
sims %>%
  select(-data) %>%
  group_by(id) %>%
  mutate(data = tibble(id      = 1:(N*N_rep),
                       id_rep  = rep(1:N_rep, each = N),
                       id_coin = rep(1:N, times = N_rep),
                       values  = base::sample(x    = 0:1,
                                              size = N*N_rep,
                                              prob = c(1-p, p),
                                           replace = T)
                       ) %>%
           list()) %$%
  data[[1]]


# 3. ahora voy a extraer información de cada experimento simulado

# suma total de todas las tiradas (sin considerar estructura)
sims %<>%
  mutate(suma = pmap(list(data),
                     ~ data[[1]] %$%
                         sum(values)) %>%
                  unlist())
sims

# promedio de todas las tiradas (sin considerar estructura)
sims %<>%
  mutate(media = pmap(list(data),
                      ~ data[[1]] %$%
                        mean(values)) %>%
           unlist())
sims

# distribucion de p estimada dentro de cada experimento repetido
sims %<>%
  mutate(distrib_p = pmap(list(N, data),
                          ~ data[[1]] %>%
                              group_by(id_rep) %>%
                              summarise(p = sum(values) / N)  # <--- ¿podía usar mean?
                          ))

walk(sims$distrib_p, print)

# puedo realizar un histograma sobre dicha distribución

# usando hist:
walk(sims$distrib_p, ~ hist(.$p))

# (puedo probar con ggplot también)


# puedo obtener el desvío estándar de esa distribución (caracterizo su ancho)
sims %<>%
  mutate(desvio = pmap(list(distrib_p),
                       ~ distrib_p[[1]] %$%
                           sd(p)) %>%
                    unlist())
sims

# ahora puedo correr todo el código desde arriba, usando nuevos
# parámetros

par <- list(p = 0.5,                               # probabilidad de cara
            N = seq(from = 10, to = 100, by = 10), # cantidad de monedas tiradas en cada experimento
        N_rep = c(50, 500))                        # cantidad de repeticiones del experimento

# estos parámetros me permiten estudiar diferentes condiciones
# para mi experimentos, en este caso, diferentes cantidades de
# monedas tiradas en cada experimento
par

# luego puedo graficar el desvío de la distribución de p estimados
# en función de los distintos parámetros que simulé

sims %>%
  # algunos pipes para acomodar el dataset antes de graficar
  select(-data, -distrib_p) %>%
  mutate(N_rep = factor(N_rep)) %>%
  # acá comienza el gráfico
  ggplot(aes(x = N, y = desvio, group=N_rep)) + geom_line(aes(color=N_rep)) + geom_point()

