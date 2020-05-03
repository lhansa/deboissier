library(tidyverse)

# This function creates the segments of the original polygon
polygon <- function(n) {
  tibble(
    x    = accumulate(1:(n-1), ~.x+cos(.y*2*pi/n), .init = 0),
    y    = accumulate(1:(n-1), ~.x+sin(.y*2*pi/n), .init = 0),
    xend = accumulate(2:n,     ~.x+cos(.y*2*pi/n), .init = cos(2*pi/n)),
    yend = accumulate(2:n,     ~.x+sin(.y*2*pi/n), .init = sin(2*pi/n)))
}

# This function creates segments from some mid-point of the edges
mid_points <- function(d, p, a, i, FUN = ratio_f) {
  d %>% mutate(
    angle=atan2(yend-y, xend-x) + a,
    radius=FUN(i),
    x=p*x+(1-p)*xend,
    y=p*y+(1-p)*yend,
    xend=x+radius*cos(angle),
    yend=y+radius*sin(angle)) %>% 
    select(x, y, xend, yend)
}

# This function connect the ending points of mid-segments
con_points <- function(d) {
  d %>% mutate(
    x=xend,
    y=yend,
    xend=lead(x, default=first(x)),
    yend=lead(y, default=first(y))) %>% 
    select(x, y, xend, yend)
}

dibuja_estrella <- function(edges, niter, pond, step, alph, 
                            angle, curv, line_color, back_color, 
                            ratio_f){
  
  # Generation on the fly of the dataset
  df <- accumulate(.f = function(old, y) {
    if (y%%step!=0) mid_points(old, pond, angle, y) else con_points(old)
  }, 1:niter,
  .init=polygon(edges)) %>% 
    bind_rows()
  
  # Plot
  ggplot(df)+
    geom_curve(aes(x=x, y=y, xend=xend, yend=yend),
               curvature = curv,
               color=line_color,
               alpha=alph)+
    coord_equal()+
    theme(legend.position  = "none",
          panel.background = element_rect(fill=back_color),
          plot.background  = element_rect(fill=back_color),
          axis.ticks       = element_blank(),
          panel.grid       = element_blank(),
          axis.title       = element_blank(),
          axis.text        = element_blank())
  
}

dibuja_estrella(
  edges = 3,   # Number of edges of the original polygon
  niter = 250, # Number of iterations
  pond = 0.24,  # Weight to calculate the point on the middle of each edge
  step =  13,  # No of times to draw mid-segments before connect ending points
  alph =  0.25, # transparency of curves in geom_curve
  angle = 0.6, # angle of mid-segment with the edge
  curv = 0.1,   # Curvature of curves
  line_color = "black", # Color of curves in geom_curve
  back_color = "white", # Background of the ggplot
  ratio_f = function(x) {sin(x)} # To calculate the longitude of mid-segments
)

colores <- sample(viridisLite::magma(10), 2)

dibuja_estrella(
  edges = 5,   
  niter = 100, 
  pond = 0.27, 
  step =  14,  
  alph =  0.2, 
  angle = pi/4, 
  curv = 0.5,  
  line_color = colores[1],
  back_color = colores[2],
  ratio_f = function(x) {log(x + 1)}
)

dibuja_estrella(
  edges = 4,   
  niter = 200, 
  pond = 0.27, 
  step =  20,  
  alph =  0.25, 
  angle = pi/3.5, 
  curv = 0.5,  
  line_color = colores[1],
  back_color = colores[2],
  ratio_f = function(x) {1/x}
)

dibuja_estrella(
  edges = 4,   
  niter = 200, 
  pond = 0.27, 
  step =  20,  
  alph =  0.25, 
  angle = 0, 
  curv = 0.4,  
  line_color = colores[1],
  back_color = colores[2],
  ratio_f = function(x) {1/x}
)

dibuja_estrella(
  edges = 12,   
  niter = 200, 
  pond = 0.2, 
  step =  6,  
  alph =  0.2, 
  angle = 0.5, 
  curv = 0.3,  
  line_color = colores[1],
  back_color = colores[2],
  ratio_f = function(x) {1/x}
)

dibuja_estrella(
  edges = 5,   
  niter = 150, 
  pond = 0.2, 
  step =  4,  
  alph =  0.2, 
  angle = pi, 
  curv = 0.6,  
  line_color = "yellow",
  back_color = "grey3",
  ratio_f = function(x) {1/x}
)

