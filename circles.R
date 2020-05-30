# remotes::install_github("thomasp85/ambient")
# remotes::install_github("djnavarro/jasmines")
# remotes::install_github("djnavarro/rosemary")
# 
# rosemary::incantations("img")
# 
# rosemary

library(jasmines)
library(dplyr)

use_seed(1) %>%
  scene_discs(
    rings = 3, points = 5000, size = 5
  ) %>%
  mutate(ind = 1:n()) %>%
  unfold_warp(
    iterations = 1,
    scale = .5, 
    output = "layer" 
  ) %>%
  unfold_tempest(
    iterations = 20,
    scale = .01
  ) %>%
  style_ribbon(
    palette = palette_named("vik"),
    colour = "ind",
    alpha = c(.1,.1),
    background = "oldlace"
  )
