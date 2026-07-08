# Source - https://stackoverflow.com/q/78024867
# Posted by MoonS, modified by community. See post 'Timeline' for change history
# Retrieved 2026-07-08, License - CC BY-SA 4.0

plot_data <- structure(list(from = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, NA, NA, NA, NA), levels = c("Afghanistan", 
"Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan"
), class = "factor"), to = structure(c(1L, 3L, 3L, 3L, 3L, 3L, 
3L, 4L, 4L, 4L, 5L, 5L, 6L, 6L, 2L, 6L, NA, NA, NA, NA), levels = c("Afghanistan", 
"Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan"
), class = "factor"), weight = c(1291072130433.34, 480160896152.234, 
480160896152.234, 480160896152.234, 480160896152.234, 480160896152.234, 
480160896152.234, 3474907531417.02, 3474907531417.02, 3474907531417.02, 
867103764128.709, 867103764128.709, 7791981051421.92, 7791981051421.92, 
133799551.098735, 1102379004.66647, NA, NA, NA, NA), agreement_num = structure(c(NA, 
1L, 5L, 2L, 6L, 3L, 4L, 2L, 1L, 6L, 1L, 6L, 1L, 6L, NA, NA, NA, 
NA, NA, NA), levels = c("51", "58", "133", "135", "176", "224"
), class = "factor")), row.names = c("1", "2", "3", "4", "5", 
"6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", 
"NA", "NA.1", "NA.2", "NA.3"), class = "data.frame")

library(ggplot2)
library(dplyr, warn = FALSE)

plot_data <- plot_data |>
  mutate(
    x = as.numeric(from),
    y = as.numeric(to),
    ymin = y - .5, ymax = y + .5
  ) |>
  mutate(
    n = n(),
    xmin = x + scales::rescale(row_number(),
      from = c(1, unique(n) + 1),
      to = .5 * c(-1, 1)
    ),
    xmax = x + scales::rescale(row_number() + 1,
      from = c(1, unique(n) + 1),
      to = .5 * c(-1, 1)
    ),
    .by = c(from, to)
  )

  coul <- RColorBrewer::brewer.pal(9, "Set3") 

ggplot(plot_data, aes(x = from, y = to, fill = agreement_num)) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  ) +
  scale_fill_manual(values = coul) +
  theme_bw() +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  theme(
    axis.text.x = element_text(
      size = 9, angle = 270,
      hjust = 0, vjust = 0
    ),
    axis.text.y = element_text(size = 9),
    aspect.ratio = 1
  )
