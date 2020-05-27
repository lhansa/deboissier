library(tidyverse)
seq(-12, 12, by = 0.04) %>%
  expand.grid(x=., y=.) %>%
  ggplot(aes(x=(x+pi*sin(y)), y=(y+pi*sin(x)))) +
  geom_point(alpha=.05, 
             shape=20, 
             color="red",
             size=0)+theme_void()+
  coord_polar()

# no polar
seq(-12, 12, by = 0.04) %>%
  expand.grid(x=., y=.) %>%
  ggplot(aes(x=(x+pi*sin(y)), y=(y+pi*sin(x)))) +
  geom_point(alpha=.05, 
             shape=20, 
             color="red",
             size=0)+
  theme_void()

# cambio de color
seq(-12, 12, by = 0.04) %>%
  expand.grid(x=., y=.) %>%
  mutate(color_aux = row_number()) %>% 
  ggplot(aes(x=(x+pi*sin(y)), y=(y+pi*sin(x)))) +
  geom_point(aes(color = color_aux), 
             alpha=.05, 
             shape=20, 
             size=0)+
  theme_void()


# cambio de color y polar
seq(-12, 12, by = 0.04) %>%
  expand.grid(x=., y=.) %>%
  mutate(color_aux = row_number()) %>% 
  ggplot(aes(x=(x+pi*sin(y)), y=(y+pi*sin(x)))) +
  geom_point(aes(color = color_aux), 
             alpha=.05, 
             shape=20, 
             size=0)+
  theme_void() + 
  coord_polar() + 
  scale_color_fermenter(palette = "PuRd")

# invertido el color
seq(-12, 12, by = 0.04) %>%
  expand.grid(x=., y=.) %>%
  mutate(color_aux = row_number()) %>% 
  ggplot(aes(x=(x+pi*sin(y)), y=(y+pi*sin(x)))) +
  geom_point(aes(color = color_aux), 
             alpha=.05, 
             shape=20, 
             size=0)+
  theme_void() + 
  coord_polar() + 
  scale_color_fermenter(palette = "RdPu", direction = 1)

# nueva paleta
seq(-12, 12, by = 0.04) %>%
  expand.grid(x=., y=.) %>%
  mutate(color_aux = row_number()) %>% 
  ggplot(aes(x=(x+pi*sin(y)), y=(y+pi*sin(x)))) +
  geom_point(aes(color = color_aux), 
             alpha=.05, 
             shape=20, 
             size=0)+
  theme_void() + 
  coord_polar() + 
  scale_color_fermenter(palette = "RdYlBu")

# cosenos
seq(-12, 12, by = 0.04) %>%
  expand.grid(x=., y=.) %>%
  mutate(color_aux = row_number()) %>% 
  ggplot(aes(x=(x+pi*cos(y)), y=(y+pi*cos(x)))) +
  geom_point(aes(color = color_aux), 
             alpha=.05, 
             shape=20, 
             size=0)+
  theme_void() + 
  coord_polar() + 
  scale_color_fermenter(palette = "RdYlBu")

# senos y cosenos
seq(-12, 12, by = 0.04) %>%
  expand.grid(x=., y=.) %>%
  mutate(color_aux = row_number()) %>% 
  ggplot(aes(x=(x+pi*sin(y)), y=(y+pi*cos(x)))) +
  geom_point(aes(color = color_aux), 
             alpha=.05, 
             shape=20, 
             size=0)+
  theme_void() +
  theme(legend.position = "none") + 
  coord_polar() + 
  scale_color_fermenter(palette = "RdYlBu")

# senos y cosenos
seq(-12, 12, by = 0.04) %>%
  expand.grid(x=., y=.) %>%
  mutate(color_aux = row_number()) %>% 
  ggplot(aes(x=(y+pi*sin(x)), y=(x+pi*cos(y)))) +
  geom_point(aes(color = color_aux), 
             alpha=.05, 
             shape=20, 
             size=0)+
  theme_void() +
  theme(legend.position = "none") + 
  coord_polar() + 
  scale_color_fermenter(palette = "Reds")

