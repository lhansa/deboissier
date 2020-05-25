library(tidyverse)

# Init --------------------------------------------------------------------

exp_sin <- 3
exp_cos <- 4

valores_eje_x <- seq(0, 2*pi, by = 0.1)

# Funciones ---------------------------------------------------------------

f1 <- function(x) sin(x)^exp_sin + cos(x)^exp_cos
f2 <- function(x) -sin(x)
f3 <- function(x) sin(3 * x^2)
# f4 <- function(x) sin(10 * x) ^ 2 - cos(10 * x) ^ 2
Fprueba <- function(x) sin(x^2) ^ 2 - cos(x^2) ^2

ggplot(df_plot, aes(x = eje_x, y = Fprueba(eje_x))) + 
  geom_line() + 
  coord_polar()

lista_funciones <- ls(pattern = "^f[[:digit:]]")

# Data frame --------------------------------------------------------------

df_evaluacion_funciones <- map_dfc(lista_funciones, function(funcion){
  funcion_eval <- eval(as.symbol(funcion))
  c(funcion = funcion_eval(valores_eje_x))
}) %>% 
  set_names(lista_funciones)

df_plot <- tibble(eje_x) %>% 
  mutate(ind = row_number()) %>% 
  bind_cols(df_evaluacion_funciones) %>% 
  pivot_longer(-c(ind, eje_x), values_to = "y")

# Parámetros de dibujo ----------------------------------------------------

# TODO Esto no funciona
alphas <- seq(1, 0.5, along.with = unique(df_plot$name))
names(alphas) <- unique(df_plot$name)

df_plot <- df_plot %>% 
  mutate(trans = alphas[name])

# Dibujo ------------------------------------------------------------------

ggplot(df_plot) + 
  geom_line(aes(eje_x, y, group = name, alpha = trans), 
            col = "yellow", size = 1) + 
  coord_polar() + 
  theme_void() +
  theme(panel.background = element_rect(fill = "purple"), 
        legend.position = "none")
