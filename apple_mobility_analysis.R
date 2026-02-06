# Apple Mobility Trends (COVID-19)
# Main analysis script
# Raw data files are not included in this public repository


# Raw Apple Mobility CSV files are not included in this public repository.
# Please download Apple Mobility Trends data and place the files in a local folder.
# Example:
# mob1 <- read_csv("applemobilitytrends-2021-10-10.csv")


# Stack all CVSs into one dataset 
mobility_raw <- bind_rows(mob1, mob2, mob3, mob4)


#### 2. FILTER TO CITY-LEVEL WALKING MOBILITY 
mobility_city_walking <- mobility_raw %>%
  filter(
    geo_type == "city",
    transportation_type == "walking"
  )


#### 3. TIDYING DATE STRUCTURE (CITY-LEVEL WALKING MOBILITY)
date_columns <- colnames(mobility_city_walking)[7:ncol(mobility_city_walking)]

mobility_long <- mobility_city_walking %>%
  pivot_longer(
    cols = all_of(date_columns),
    names_to = "date",
    values_to = "mobility_index"
  ) %>%
  mutate(
    date = as.Date(date)
  ) %>%
  select(country, city, date, mobility_index)

#### 4. FILTER TO YEAR 2020 (COVID MOBILITY YEAR)
mobility_2020 <- mobility_long %>%
  filter(date >= as.Date("2020-01-01"),
         date <= as.Date("2020-12-31"))

range(mobility_long$date)

mobility_2021 <- mobility_long %>%
  filter(
    date >= as.Date("2021-01-01"),
    date <= as.Date("2021-10-10")
  )

#### 5. DEFINE PRE-COVID BASELINE & RESTRICTION PERIOD
baseline_period <- mobility_2020 %>%
  filter(date >= as.Date("2020-01-13"),
         date <= as.Date("2020-02-15"))

response_period <- mobility_2020 %>%
  filter(date >= as.Date("2020-02-16"),
         date <= as.Date("2020-12-31"))


#### 6. GLOBAL MOBILITY LINE GRAPH (INTERACTIVE)
global_baseline <- baseline_period %>%
  group_by(date) %>%
  summarise(global_avg = mean(mobility_index, na.rm = TRUE))

global_response <- response_period %>%
  group_by(date) %>%
  summarise(global_avg = mean(mobility_index, na.rm = TRUE), .groups = "drop")

global_full <- bind_rows(global_baseline, global_response)

p_global <- ggplot(global_full, aes(date, global_avg)) +
  geom_line(color = "hotpink") +
  geom_hline(yintercept = 100, linetype = "dashed") +
  labs(
    title = "Global Walking Mobility in 2020 (City-Level)",
    subtitle = "Pre-COVID baseline vs restriction response period",
    x = "Date",
    y = "Average Mobility Index"
  ) +
  theme_minimal()

ggplotly(p_global)


#### 7. TOP 20 AND BOTTOM 20 CITIES (“COMPLIANCE”)
city_scores <- response_period %>%
  group_by(city) %>%
  summarise(avg_mobility = mean(mobility_index, na.rm = TRUE)) %>%
  arrange(avg_mobility)

bottom_20 <- city_scores %>% slice(1:20)
top_20 <- city_scores %>% slice_tail(n = 20)

bottom_10 <- bottom_20$city[1:10]
top_10 <- top_20$city[11:20]



#### 8. INTERACTIVE LINE GRAPHS: TOP 10 & BOTTOM 10 CITIES
# Bottom 10 cities
mob_bottom10 <- mobility_2020 %>% filter(city %in% bottom_10)

p_bottom10 <- ggplot(mob_bottom10, aes(date, mobility_index, color = city)) +
  geom_line() +
  geom_hline(yintercept = 100, linetype = "dashed") +
  labs(
    title = "Bottom 10 Cities (Lowest Mobility)",
    subtitle = "Strongest mobility reduction during COVID restrictions",
    x = "Date", y = "Mobility Index"
  ) +
  theme_minimal()

ggplotly(p_bottom10)

# Top 10 cities
mob_top10 <- mobility_2020 %>% filter(city %in% top_10)

p_top10 <- ggplot(mob_top10, aes(date, mobility_index, color = city)) +
  geom_line() +
  geom_hline(yintercept = 100, linetype = "dashed") +
  labs(
    title = "Top 10 Cities (Highest Mobility)",
    subtitle = "Cities moving the most during restrictions", 
    x = "Date", y = "Mobility Index"
  ) + 
  theme_minimal()

ggplotly(p_top10)


#### 9. GIS MAPPING: APRIL 2020 MOBILITY (LEAFLET)
# City level walking data is aggregated to country averages for mapping.
world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

mobility_april <- mobility_2020 %>%
  filter(date >= as.Date("2020-04-01"),
         date <= as.Date("2020-04-30")) %>%
  group_by(country) %>%
  summarise(avg_april_mobility = mean(mobility_index, na.rm = TRUE), .groups = "drop") 


world_mobility_sf <- world_sf %>%
  left_join(mobility_april, by = c("name" = "country"))

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = world_mobility_sf$avg_april_mobility
)

leaflet(world_mobility_sf) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(avg_april_mobility), 
    color = "white", weight = 0.5, fillOpacity = 0.8,
    label = ~paste0(name, " (City Walking Avg.): ", round(avg_april_mobility, 1))
  ) %>%
  addLegend(
    pal = pal, values = ~avg_april_mobility,
    title = "City Walking Index<br>April 2020"
  )

#### 10. CORRELATION MATRIX (CITY SIMILARITY)
mob_subset_clean <- mobility_2020 %>%
  group_by(date, city) %>%
  summarise(mobility_index = mean(mobility_index, na.rm =TRUE), .groups = "drop")

mob_wide <- mob_subset_clean %>%
  pivot_wider(names_from = city, values_from = mobility_index) %>%
  arrange(date)

mob_matrix <- mob_wide %>%
  select(-date) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

mob_matrix <- mob_matrix[, colSums(!is.na(mob_matrix)) > 0]

for (j in seq_len(ncol(mob_matrix))) {
  col <- mob_matrix[, j]
  col[is.na(col)] <- mean(col, na.rm = TRUE)
  mob_matrix[,j] <- col
}

cor_mat <- cor(mob_matrix)

#### 11. INTERACTIVE CORRELATION HEATMAP (PLOTLY)
cor_df <- cor_mat %>%
  as.data.frame() %>%
  rownames_to_column("city1") %>%
  pivot_longer(-city1, names_to = "city2", values_to = "corr")

plot_ly(
  x = unique(cor_df$city1),
  y = unique(cor_df$city2),
  z = matrix(cor_df$corr,
             nrow = length(unique(cor_df$city1)),
             ncol = length(unique(cor_df$city2)),
             byrow = TRUE),
  type = "heatmap",
  colorscale = "Viridis"
) %>%
  layout(
    title = "City Mobility Correlation Heatmap (2020)",
    xaxis = list(title = "City", tickangle = 45),
    yaxis = list(title = "City", autorange = "reversed")
  )

#### 12. INTERACTIVE NETWORK ANALYSIS (SIMILARITY NETWORK)
cor_threshold <- 0.6
edges <- cor_mat %>%
  as.data.frame() %>%
  rownames_to_column("city1") %>%
  pivot_longer(-city1, names_to = "city2", values_to = "correlation") %>%
  filter(city1 != city2,
         !is.na(correlation),
         correlation >= cor_threshold)

g <- graph_from_data_frame(edges, directed = FALSE)

communities <- cluster_louvain(g)
V(g)$community <- as.factor(communities$membership)

set.seed(42)
layout_fr <- layout_with_fr(g)

nodes <- data.frame(
  id = V(g)$name,
  label = V(g)$name,
  group = V(g)$community,
  x = layout_fr[, 1] * 200,
  y = layout_fr[, 2] * 200
)

edges_vis <- edges %>%
  rename(from = city1, to = city2, value = correlation) %>%
  mutate(width = (value - cor_threshold) * 5)

visNetwork(nodes, edges_vis, width = "100%", height = "850px") %>%
  visNodes(shape = "dot", scaling = list(min = 20, max = 30)) %>%
  visEdges(color = list(color = "#CCCCCC", highlight = "black")) %>%
  visOptions(
    highlightNearest = TRUE,
    nodesIdSelection = TRUE,
    selectedBy = "group"
  ) %>%
  visLegend() %>%
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) %>%
  visPhysics(
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(
      gravitationalConstant = -120,
      centralGravity = 0.002,
      springLength = 200,
      springConstant = 0.05,
      avoidOverlap = 1
    ),
    stabilization = list(enabled = TRUE, iterations = 300),
    minVelocity = 0.75
  ) %>%
  visPhysics(enabled = FALSE)

