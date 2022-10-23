library (ggplot2)
library(tidyverse)
library(maps)

world_tbl = map_data("world")

world_base <- world_tbl %>%
  ggplot() +
  geom_map (
    aes(long, lat, map_id = region),
    map = world_tbl,
    color = "gray80", fill = "gray30", size = 0.3
  )

world_base +
  coord_map("ortho", orientation = c(30, 98, 0))
