library(tidyverse)

exp_sin <- 3
exp_cos <- 4

eje_x <- seq(-2*pi, 2*pi, by = 0.1)

f1 <- function(x) sin(x)^exp_sin + cos(x)^exp_cos
f2 <- function(x) -sin(x)

df_plot <- tibble(x1 = eje_x, 
                  y1 = f1(x1), 
                  y2 = f2(x1)) %>% 
  mutate(ind = row_number()) %>% 
  pivot_longer(-c(ind, x1), values_to = "y")

ggplot(df_plot) + 
  geom_line(aes(x1, y, group = name), col = "yellow", size = 1) + 
  coord_polar() + 
  theme_void() +
  theme(panel.background = element_rect(fill = "purple"))
