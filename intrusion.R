# https://gist.github.com/djnavarro/e11f24c886a57816b86783becbdd4ef4

library(dplyr)
library(ambient)

intrusion <- function(filename = NULL, shade = "#b3b3ff", seed = 1, 
                      frak_one = 3, frak_two = 2, 
                      pixels_wide = 1000, pixels_high = 1000,
                      colors_used = 1000) {
  
  # transformation functions ------------------------------------------------
  
  stage_one <- function(x, y) {
    fracture(
      noise = gen_worley,
      fractal = ridged,
      octaves = 2, 
      frequency = frak_one,
      value = "distance2", 
      seed = seed, 
      x = x, 
      y = y
    ) %>%
      normalise()
  }
  
  stage_two <- function(x, y) {
    
    curled <- fracture(
      noise = curl_noise,
      generator = gen_worley,
      fractal = fbm,
      octaves = 10, 
      frequency = frak_two, 
      seed = seed + 1, 
      value = "distance2",
      x = x, 
      y = y
    )
    
    fracture(
      noise = gen_simplex,
      fractal = billow,
      octaves = 10, 
      frequency = .1, 
      seed = seed + 2, 
      x = normalize(curled$x), 
      y = normalize(curled$y)
    )
  }
  
  
  
  # helper functions --------------------------------------------------------
  
  indexify <- function(x, n) {
    round(1 + (n - 1) * normalise(x))
  }
  
  colorize <- function(index, n, shade = "#b3b3ff") {
    adj_f <- col2rgb(shade)/255
    palette <- adjustcolor(
      col = gray.colors(n), 
      red.f = adj_f[1], 
      green.f = adj_f[2], 
      blue.f = adj_f[3]
    )
    
    return(palette[index])
  }
  
  
  
  
  # construct image ---------------------------------------------------------
  
  
  image <- long_grid(
    x = seq(0, min(1, pixels_wide/pixels_high), length.out = pixels_wide),
    y = seq(0, min(1, pixels_high/pixels_wide), length.out = pixels_high)
  ) %>%
    mutate(
      a = stage_one(x, y),
      b = stage_two(x + a, y + a),
      i = indexify(b, colors_used),
      v = colorize(i, colors_used, shade)
    )
  
  # write image file --------------------------------------------------------
  
  if(!is.null(filename)){
    png(
      filename = filename,
      width = pixels_wide,
      height = pixels_high,
    )
    op <- par(mar = c(0,0,0,0))
    plot(as.raster(image, value = v))
    dev.off()
    par(op)
  }
  
  
  # invisibly return the image object ---------------------------------------
  
  return(invisible(image))
  
}

intrusion("img/intrusion.png")
