library(tidyverse)

# Un pentágono ------------------------------------------------------------

pentagon <- tibble(
  x    = accumulate(1:4, ~.x+cos(.y*2*pi/5), .init = 0),
  y    = accumulate(1:4, ~.x+sin(.y*2*pi/5), .init = 0),
  xend = accumulate(2:5, ~.x+cos(.y*2*pi/5), .init = cos(2*pi/5)),
  yend = accumulate(2:5, ~.x+sin(.y*2*pi/5), .init = sin(2*pi/5)))

ggplot(pentagon)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()


# Polígono ----------------------------------------------------------------

polygon <- function(n) {
  tibble(
    x    = accumulate(1:(n-1), ~.x+cos(.y*2*pi/n), .init = 0),
    y    = accumulate(1:(n-1), ~.x+sin(.y*2*pi/n), .init = 0),
    xend = accumulate(2:n,     ~.x+cos(.y*2*pi/n), .init = cos(2*pi/n)),
    yend = accumulate(2:n,     ~.x+sin(.y*2*pi/n), .init = sin(2*pi/n)))
}

ggplot(polygon(6))+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

ggplot(polygon(7))+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

ggplot(polygon(8))+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()

ggplot(polygon(9))+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()


# Más segmentos -----------------------------------------------------------

df1 <- polygon(5)

df2 <- df1 %>% 
  mutate(angle = atan2(yend-y, xend-x)+pi/2,
         x = 0.5*x+0.5*xend,
         y = 0.5*y+0.5*yend,
         xend = x+0.2*cos(angle),
         yend = y+0.2*sin(angle)) %>% 
  select(x, y, xend, yend)

df <- bind_rows(df1, df2)

ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()


# Un pentágono interno ----------------------------------------------------

df1 <- polygon(5)
df2 <- df1 %>% mutate(
  angle = atan2(yend-y, xend-x)+pi/2,
               x = 0.5*x+0.5*xend,
               y = 0.5*y+0.5*yend,
               xend = x+0.2*cos(angle),
               yend = y+0.2*sin(angle)) %>% 
  select(x, y, xend, yend)

df3 <- df2 %>% mutate(
  x=xend,
  y=yend,
  xend=lead(x, default=first(x)),
  yend=lead(y, default=first(y))) %>% 
  select(x, y, xend, yend)

df <- df1 %>% 
  bind_rows(df2) %>% 
  bind_rows(df3)

ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()


# Funciones ---------------------------------------------------------------

mid_points <- function(d) {
  d %>% 
    mutate(
      angle=atan2(yend-y, xend-x) + pi/2,
      x=0.5*x+0.5*xend,
      y=0.5*y+0.5*yend,
      xend=x+0.2*cos(angle),
      yend=y+0.2*sin(angle)) %>% 
    select(x, y, xend, yend)
}

con_points <- function(d) {
  d %>% 
    mutate(
      x=xend,
      y=yend,
      xend=lead(x, default=first(x)),
      yend=lead(y, default=first(y))) %>% 
    select(x, y, xend, yend)
}

df1 <- polygon(5)
df2 <- mid_points(df1)
df3 <- con_points(df2)
df4 <- mid_points(df3)
df5 <- con_points(df4)

df <- df1 %>% 
  bind_rows(df2) %>% 
  bind_rows(df3) %>% 
  bind_rows(df4) %>% 
  bind_rows(df5)

ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()


# Iterativo ---------------------------------------------------------------

edges <- 5
niter <- 4

df1 <- polygon(edges)

df <- accumulate(1:niter, 
  .f = function(old, y) {
    if (y %% 2 != 0)
      mid_points(old)
    else
      con_points(old)
  },
  .init = df1
) %>%
  bind_rows()

ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()


# Cualquier ángulo --------------------------------------------------------

mid_points <- function(d, p, a) {
  d %>% mutate(
    angle=atan2(yend-y, xend-x) + a,
    x=p*x+(1-p)*xend,
    y=p*y+(1-p)*yend,
    xend=x+0.2*cos(angle),
    yend=y+0.2*sin(angle)) %>% 
    select(x, y, xend, yend)
}

edges <- 7
niter <- 18

df1 <- polygon(edges)

df <- accumulate(.f = function(old, y) {
  if (y%%2!=0) mid_points(old, 0.3, pi/5) else con_points(old)
},
1:niter,
.init=df1) %>% 
  bind_rows()

ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()


# Cambiar longitud --------------------------------------------------------

mid_points <- function(d, p, a, i, FUN = function(x) x) {
  d %>% mutate(
    angle=atan2(yend-y, xend-x) + a,
    radius=FUN(i),
    x=p*x+(1-p)*xend,
    y=p*y+(1-p)*yend,
    xend=x+radius*cos(angle),
    yend=y+radius*sin(angle)) %>% 
    select(x, y, xend, yend)
}

edges <- 7
niter <- 18

df1 <- polygon(edges)
df <- accumulate(.f = function(old, y) {
  if (y%%2!=0) mid_points(old, 0.3, pi/5, y) else con_points(old)
},
1:niter,
.init=df1) %>% 
  bind_rows()

ggplot(df)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
  coord_equal()+
  theme_void()


# Más iters ---------------------------------------------------------------

edges <- 7
niter <- 250
step <- 2

df1 <- polygon(edges)

df <- accumulate(.f = function(old, y) {
  if (y%%step!=0) mid_points(old, 0.3, pi/5, y) else con_points(old)
},
1:niter,
.init=df1) %>% 
  bind_rows()

ggplot(df)+
  geom_curve(aes(x=x, y=y, xend=xend, yend=yend),
             curvature = 0,
             color="black",
             alpha=0.1)+
  coord_equal()+
  theme(legend.position  = "none",
        panel.background = element_rect(fill="white"),
        plot.background  = element_rect(fill="white"),
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank())
