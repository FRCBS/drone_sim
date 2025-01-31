data <- data.frame(
  hospitals = rep(c(5, 10, 15, 20, 25), times = 9),
  reached_scenes = c(
    # Speed 60, Time 45
    1:25,
    # Speed 60, Time 60
    26:50,
    # Speed 60, Time 75
    51:75,
    # Speed 80, Time 45
    76:100,
    # Speed 80, Time 60
    101:125,
    # Speed 80, Time 75
    126:150,
    # Speed 100, Time 45
    151:175,
    # Speed 100, Time 60
    176:200,
    # Speed 100, Time 75
    201:225
  ),
  range_val=factor(rep(c("20km","30km","40km","50km","60km"),each=15,times=3)),
  time = factor(rep(c("45 min", "60 min", "75 min"), each = 25, times = 3)),
  speed = factor(rep(c("60 km/h", "80 km/h", "100 km/h"), each = 75), levels = c("100 km/h", "80 km/h", "60 km/h"))
)

setwd("/Users/erastop1/Documents/Kokoveri") # Update this
load("Kuva_data_frame_new_2.RData")         # Update this
data$speed <- factor(data$speed, levels = c("60 km/h", "80 km/h", "100 km/h"))
data$reached_scenes=100*data$reached_scenes

#subset_data <- data %>%
#  filter(time == "30 min" & speed == "60 km/h")

# Print the subsetted data
#print(subset_data)

library(RColorBrewer)
colors <- brewer.pal(n = length(unique(data$range_val)), name = "Dark2")



## Dashed plot

# Define the color palette from RColorBrewer
colors <- brewer.pal(n = length(unique(data$range_val)), name = "Dark2")


# Create the plot with different line types
p <- ggplot(data, aes(x = hospitals, y = reached_scenes, color = range_val, group = range_val, linetype = range_val)) +
  geom_point(size = 1) +
  geom_line() +  # Add lines between points of the same range_val
  scale_color_manual(values = colors) +  # Apply the custom colors
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "twodash")) +  # Define line types
  facet_grid(speed ~ time) +
  labs(x = "Number of BDCs in use", y = "Reached trauma scenes (%)", color = "Range")
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Scatterplot background
    strip.background = element_rect(fill = "grey70", color = NA),  # Facet header background
    strip.text = element_text(face = "bold")  # Facet header text style
  )

# Print the plot
print(p)


# Create the plot with combined legend for color and linetype
p <- ggplot(data, aes(x = hospitals, y = reached_scenes, color = range_val, group = range_val, linetype = range_val)) +
  geom_point(size = 1) +
  geom_line() +  # Add lines between points of the same range_val
  scale_color_manual(values = colors) +  # Apply the custom colors
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "twodash")) +  # Define line types
  facet_grid(speed ~ time) +
  labs(x = "Number of BDCs in use", y = "Reached trauma scenes (%)", color = "Half-Range", linetype = "Half-Range") +  # Make sure both have the same label
  guides(linetype = guide_legend(override.aes = list(color = colors))) +  # Combine color and linetype into one legend
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Scatterplot background
    strip.background = element_rect(fill = "grey70", color = NA),  # Facet header background
    strip.text = element_text(face = "bold")  # Facet header text style
  )

# Print the plot
print(p)



ggsave(filename = "results/fig4.pdf", width = 180, height = 180, units = "mm", dpi=600)

ggsave(filename = "results/fig4.png", width = 180, height = 180, units = "mm", dpi=600)






