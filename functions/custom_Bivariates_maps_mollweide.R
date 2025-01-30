Bivar_map_Mollweide <- function(x,                                    # Raster to map
                                color_palette,# Color palette
                                main_title,                           # Main title
                                
                                
                                legend_title,             # Legend title
                                legend_breaks,            # Legend tick positions

main_title_cex = 1.5,

xlim = c(-5457, 4953),
ylim = c(-4495, 5705),
axis_cex = 1.4,
xlab = "",
ylab = "",
x_axis_breaks = c(-3930, -2170, -420, 1500, 3050),
y_axis_breaks = c(-3650, -2450, -1220, 0, 1230, 2445, 3660, 4890),
x_axis_labels = c("120°E", "100°E", "80°E", "60°E", "40°E"),
y_axis_labels = c("30°S", "20°S", "10°S", "0°", "10°N", "20°N", "30°N", "40°N"),

legend_title_cex = 1 ,  # Legend title size
legend_title_x = -2900 ,  # Legend title x position
legend_title_y = 0 ,   # Legend title y position
legend_cex = 1.4   ,      # Legend size
legend_location=c(-4100, -3800, -3950, -1500),  # Legend position


scale_bar_position = c(2700, -4100),  # Scale bar position

arrow_scale = 0.45     ,      # North arrow size
arrow_padin = c(0.15, 0.15),  # North arrow position adjustement

facet_letter = ""      ,            # Small case letter for facet
facet_letter_col = "black"   ,      # Color of case letter for facet
facet_letter_cex = 2.2   ,          # Size of small case letter for facet
facet_letter_inset = c(0, -0.008))



{
  # Plot raster background without axis
  image(x, col = color_palette,
        xlim = xlim, ylim = ylim, axes = F,
        xlab = xlab, ylab = ylab)
  title(main = main_title, cex.main = main_title_cex, line = 1)
  
  # Generate axes with manual positioning of ticks
  axis(side = 1, at = x_axis_breaks, labels = x_axis_labels, cex.axis = axis_cex, lwd = 0.2, lwd.ticks = 1, gap.axis = 0, padj = 0.5)
  axis(side = 2, at = y_axis_breaks, labels = y_axis_labels, cex.axis = axis_cex, lwd = 0.2, lwd.ticks = 1, gap.axis = 0)
  
  graphics::text(x = legend_title_x, y = legend_title_y, font = 2, cex = legend_title_cex, label = legend_title)
  
  # Add facet letter
  legend(legend = facet_letter, x = -6300, y =5800, bty = "n", text.col = facet_letter_col,
         text.font = 2, cex = facet_letter_cex, inset = facet_letter_inset)
  scalebar(d = 2000, type = "line", lwd = 4, divs = 4, xy = scale_bar_position, label = c("", "2000 km", ""), adj = c(0.5, -0.8), font = 2, cex = 1.2)
  prettymapr::addnortharrow(scale = arrow_scale, padin = arrow_padin, text.col = "#00000000")
}
