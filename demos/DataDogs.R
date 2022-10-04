library(tidyverse)
library(ggrepel)

dogs <- read_csv(
  "~/Downloads/KIB - Best in Show (public) - Best in show full sheet.csv",
  skip = 4
)
colnames(dogs) <- colnames(read_csv(
  "~/Downloads/KIB - Best in Show (public) - Best in show full sheet.csv",
  skip = 2
))

# Rename variables and remove dogs with insufficient data:
dogs <- dogs %>%
  transmute(
    Dog = `Sources - see bottom right`,
    Category = category,
    Popularity = `POPULAR RATING`,
    DataScore = `datadog score`,
    Size = `size category`,
    Intelligence = ifelse(
      `intelligence category` %in% c("Brightest", 
                                     "Excellent",
                                     "Above average"),
      "clever", 
      "dumb"
    )
  ) %>%
  filter(!is.na(DataScore))

## Geoms and aesthetics ------

# Start with a scatterplot with the desired aesthetics:
ggplot(dogs) +
  geom_point(
    mapping = aes(
      x = DataScore,
      y = Popularity,
      color = Category,
      size = Size,
      shape = Intelligence
    )
  ) 

## Scales ------

# Flip the y-axis:
last_plot() +
  scale_y_reverse()

# Change the color scheme:
last_plot() +
  scale_color_brewer(palette = "Dark2")

# Code intelligence as arrows:
last_plot() +
  scale_shape_manual(values = c(
    "clever" = "\u2192",
    "dumb" = "\u2190"
  ))

# Fix the size scale:
last_plot() +
  scale_size_manual(values = c(
    "small" = 6,
    "medium" = 9,
    "large" = 12
  ))

## Labels and annotations ------

# Add title and axis labels:
last_plot() +
  labs(
    x = "our data score",
    y = "popularity",
    title = "Best in Show: The Ultimate Data Dog"
  )

# Add annotations:
last_plot() +
  geom_text_repel(
    mapping = aes(
      x = DataScore,
      y = Popularity,
      label = Dog,
      color = Category
    ), 
    min.segment.length = Inf)

# Add quadrant labels:
last_plot() +
  annotate(
    "text", 
    label = "Inexplicably Overrated",
    x = min(dogs$DataScore),
    y = min(dogs$Popularity),
    hjust = "left", 
    vjust = "top"
  ) +
  annotate(
    "text", 
    label = "Hot Dogs!",
    x = max(dogs$DataScore),
    y = min(dogs$Popularity),
    hjust = "right", 
    vjust = "top"
  ) +
  annotate(
    "text", 
    label = "The Rightly Ignored",
    x = min(dogs$DataScore),
    y = max(dogs$Popularity),
    hjust = "left", 
    vjust = "bottom"
  ) +
  annotate(
    "text", 
    label = "Overlooked Treasures",
    x = max(dogs$DataScore),
    y = max(dogs$Popularity),
    hjust = "right", 
    vjust = "bottom"
  )

## Theme ------

# Add axes:
last_plot() +
  geom_segment(
    data = data.frame(
      xstart = min(dogs$DataScore),
      ystart = mean(range(dogs$Popularity)),
      xend = max(dogs$DataScore),
      yend = mean(range(dogs$Popularity))
    ),
    mapping = aes(
      x = xstart, xend = xend, y = ystart, yend = yend
    ),
    arrow = arrow(type = "closed")
  ) +
  geom_segment(
    data = data.frame(
      xstart = mean(range(dogs$DataScore)),
      ystart = min(dogs$Popularity),
      xend = mean(range(dogs$DataScore)),
      yend = max(dogs$Popularity)
    ),
    mapping = aes(
      x = xstart, xend = xend, y = ystart, yend = yend
    ),
    arrow = arrow(type = "closed", ends = "both")
  ) 
      
  
# Clear previous theme:
last_plot() + 
  theme_void()

# Move legends:
last_plot() + 
  theme(
    legend.position = "top",
    legend.justification = "right"
  )

# Change order and appearance of guides:
last_plot() +
  guides(
    shape = guide_legend(
      title = "INTELLIGENCE",
      title.position = "top",
      label.position = "bottom",
      order = 1
    ),
    size = guide_legend(
      title = "SIZE",
      title.position = "top",
      label.position = "bottom",
      order = 2
    ),
    color = guide_legend(
      title = " ",
      title.position = "top",
      label.position = "bottom",
      nrow = 1,
      order = 3
    )
  )

ggplot(dogs) +
  geom_point(
    aes(
      x = DataScore,
      y = Popularity,
      color = Category,
      size = Size,
      shape = Intelligence
    )
  ) +
  scale_x_continuous(limits = range(dogs$DataScore) + c(-0.4, 0.4)) +
  scale_y_reverse(limits = rev(range(dogs$Popularity)) + c(20, -20)) +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(
    "clever" = "\u2192",
    "dumb" = "\u2190"
  )) +
  scale_size_manual(values = c(
    "small" = 6,
    "medium" = 9,
    "large" = 12
  )) +
  labs(
    x = "our data score",
    y = "popularity",
    title = "Best in Show: The Ultimate Data Dog"
  ) +
  geom_text_repel(
    aes(
      x = DataScore,
      y = Popularity,
      label = Dog,
      color = Category
    ), 
    size = 3,
    min.segment.length = Inf,
    max.overlaps = 20,
    force_pull = 10
  ) +
  annotate(
    "text", 
    label = "Inexplicably Overrated",
    x = min(dogs$DataScore) - 0.2,
    y = min(dogs$Popularity) - 10,
    hjust = "left", 
    vjust = "top",
    size = 8
  ) +
  annotate(
    "text", 
    label = "Hot Dogs!",
    x = max(dogs$DataScore) + 0.2,
    y = min(dogs$Popularity) - 10,
    hjust = "right", 
    vjust = "top",
    size = 8
  ) +
  annotate(
    "text", 
    label = "The Rightly Ignored",
    x = min(dogs$DataScore) - 0.2,
    y = max(dogs$Popularity) + 10,
    hjust = "left", 
    vjust = "bottom",
    size = 8
  ) +
  annotate(
    "text", 
    label = "Overlooked Treasures",
    x = max(dogs$DataScore) + 0.2,
    y = max(dogs$Popularity) + 10,
    hjust = "right", 
    vjust = "bottom",
    size = 8
  ) +
  annotate(
    "text", 
    label = "popularity",
    x = mean(range(dogs$DataScore)),
    y = min(dogs$Popularity) - 15,
    size = 5
  ) +
  annotate(
    "text", 
    label = "our data score\n(intelligence, costs,\nlongevity, grooming,\nailments, appetite)",
    x = 1.0,
    y = mean(range(dogs$Popularity)),
    vjust = "top",
    size = 5
  ) + 
  geom_segment(
    data = data.frame(
      xstart = min(dogs$DataScore) - 0.2,
      ystart = mean(range(dogs$Popularity)),
      xend = max(dogs$DataScore) + 0.2,
      yend = mean(range(dogs$Popularity))
    ),
    mapping = aes(
      x = xstart, xend = xend, y = ystart, yend = yend
    ),
    arrow = arrow(type = "closed", length = unit(0.35, "cm"))
  ) +
  geom_segment(
    data = data.frame(
      xstart = mean(range(dogs$DataScore)),
      ystart = min(dogs$Popularity) - 10,
      xend = mean(range(dogs$DataScore)),
      yend = max(dogs$Popularity) + 10
    ),
    mapping = aes(
      x = xstart, xend = xend, y = ystart, yend = yend
    ),
    arrow = arrow(type = "closed", ends = "both", length = unit(0.35, "cm"))
  ) +
  theme_void() + 
  theme(
    legend.position = "top",
    legend.justification = "right"
  ) +
  guides(
    shape = guide_legend(
      title = "INTELLIGENCE",
      title.position = "top",
      label.position = "bottom",
      order = 1,
      override.aes = list(size = 8)
    ),
    size = guide_legend(
      title = "SIZE",
      title.position = "top",
      label.position = "bottom",
      order = 2
    ),
    color = guide_legend(
      title = " ",
      title.position = "top",
      label.position = "bottom",
      nrow = 1,
      order = 3,
      override.aes = list(label = "", size = 8)
    )
  ) + 
  theme(
    plot.title = element_text(size = 28),
    legend.spacing = unit(1, "cm"),
    legend.box.margin = margin(r = 20)
  )
