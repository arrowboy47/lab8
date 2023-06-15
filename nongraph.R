library(tidyverse)
library(jasmines)
library(ggforce)


# my turn >:)
# title: 1,2,Jango
# descrip: 1,2, Jango shows us how there can be so much disarray in the world, yet we still try to make sense of it.
# When I first made it I tried to identify a pattern in how it was made. I was able to identify that there were about 3 loops
# made of points of nonagons, but was unable to identify a pattern to explain the deviations from their predicted positions.
# It reminded me of looking at a rock face or something else in nature. Where I feel like there are parts that I understand and can explain, but 
# there is so much more stuff going on that it just seems random.
# explanation: use_seed() will set the seed for the RNG. Setting the value to a speific value will make it so we get the same results every time.
# Next, the scene_bubbles lays out 5 circles made of 20 points each. Unfold_warp will then add some noise to the circles we have. The iteration was set to 10, so it repeat the noise adding process 10 times.
# The unfold loop will then take every point and and turn in into a polygon with n-1 sides, where n is the points argument. We also specify the radius of the points. 
# Finally, the style_ribbon function converts the object to a ggplot2 image using the "ribbon" style. It also sets the color palette of the image to my defined palette, my_palette, and the background color black


my_palette <- c('#d072a6','#ac84b2','#8795bd','#3db7d3','#6ac5c6','#97d3b8','#ffb18a','#f99586','#f27981')

use_seed(1) %>%
  scene_bubbles(n = 5,
                grain = 20) |>
  unfold_warp(iterations = 10) %>%
  unfold_loop(points = 10,
              radius = .05) %>%
  style_ribbon(palette = palette_manual(my_palette), background = "black")







# plot number 2
# Define a function to generate random polygons
generate_random_polygon <- function(id, n_points) {
  data.frame(
    id = id,
    x = cos(seq(0, 2*pi, length.out = n_points)) + rnorm(n_points, sd = 0.1),
    y = sin(seq(0, 2*pi, length.out = n_points)) + rnorm(n_points, sd = 0.1)
  )
}
# Generate a dataset of 10 random polygons
polygons <- bind_rows(lapply(1:10, generate_random_polygon, n_points = 30))
# Generate a color for each polygon
polygons <- polygons %>%
  group_by(id) %>%
  mutate(color = sample(rainbow(10), 1))
# Plot the polygons
p<-ggplot(polygons, aes(x = x, y = y, group = id, fill = color)) +
  geom_polygon(alpha = 0.7, show.legend = FALSE) +
  theme_void() +
  coord_fixed()


# combine the plots
grob <- ggplotGrob(p2)
combined_plot <- p + 
  annotation_custom(grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
combined_plot




