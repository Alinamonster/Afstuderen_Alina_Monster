### Prepare data: assign therapeutic categories to each drug
### Drugs with multiple categories are split into separate rows so that
### each drug-category combination can be analysed individually
drug_to_category <- manual_categories %>%
  select(drug_id, canonical_name, manual_category) %>%
  separate_rows(manual_category, sep = "\\s*\\+\\s*") %>%
  mutate(category = str_squish(manual_category)) %>%
  filter(category != "") %>%
  select(drug_id, canonical_name, category)

### Add therapeutic category information to the main orphan drug dataset
master_with_cats <- master %>%
  left_join(drug_to_category %>% select(drug_id, category), by = "drug_id")

### Summarize approval by therapeutic category and country
### Calculate number of approved orphan drugs per therapeutic area across the countries
ta_data <- master_with_cats %>%
  filter(!is.na(category)) %>%
  group_by(category) %>%
  summarise(
    Brazil         = sum(approved_brazil,    na.rm = TRUE),
    Canada         = sum(approved_canada,    na.rm = TRUE),
    Netherlands    = sum(approved_nl,        na.rm = TRUE),
    Singapore      = sum(approved_singapore, na.rm = TRUE),
    `South Africa` = sum(approved_sa,        na.rm = TRUE),
    Total          = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Total))

### Select major therapeutic categories (at least 9 drugs). Others are grouped
### into "Other small categories" to simplify visualization
top_categories <- ta_data %>%
  filter(Total >= 9) %>%
  pull(category)

ta_grouped <- ta_data %>%
  mutate(category_display = if_else(category %in% top_categories, 
                                    category, 
                                    "Other small categories")) %>%
  group_by(category_display) %>%
  summarise(across(Brazil:`South Africa`, sum), Total = sum(Total), .groups = "drop") %>%
  arrange(desc(Total))

### Reshape dataset into long for heatmap visualisation. Standardise category labels
ta_long <- ta_grouped %>%
  select(-Total) %>%
  pivot_longer(cols = Brazil:`South Africa`, 
               names_to = "Country", 
               values_to = "N_drugs") %>%
  mutate(
    Country = factor(Country, 
                     levels = c("Brazil", "Canada", "Netherlands", "Singapore", "South Africa")),
    category_short = str_replace(category_display, "^Rare ", "") %>%
      str_to_sentence(),
    category_short = factor(category_short, 
                            levels = rev(unique(category_short)))
  )

### Visualise number of approved orphan drugs per therapeutic area and country
fig4 <- ggplot(ta_long, aes(x = Country, y = category_short, fill = N_drugs)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = N_drugs), color = "black", size = 3.5) +
  scale_fill_gradient(low = "#f0f4f8", high = "#1f4d7a", 
                      name = "N drugs\napproved") +
  labs(
    title = "Approved orphan drugs per therapeutic area per country",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "right"
  )

print(fig4)


### Visualise number of available orphan drugs per therapeutic area and country
ta_data_avail <- master_with_cats %>%
  filter(!is.na(category)) %>%
  group_by(category) %>%
  summarise(
    Brazil         = sum(available_brazil,    na.rm = TRUE),
    Canada         = sum(available_canada,    na.rm = TRUE),
    Netherlands    = sum(available_nl,        na.rm = TRUE),
    Singapore      = sum(available_singapore, na.rm = TRUE),
    `South Africa` = sum(available_sa,        na.rm = TRUE),
    Total          = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Total))

### Group smaller availability categories into a single group to improve readability
ta_grouped_avail <- ta_data_avail %>%
  mutate(category_display = if_else(category %in% top_categories, 
                                    category, 
                                    "Other small categories")) %>%
  group_by(category_display) %>%
  summarise(across(Brazil:`South Africa`, sum), Total = sum(Total), .groups = "drop") %>%
  arrange(desc(Total))

### Convert availability data to long format
ta_long_avail <- ta_grouped_avail %>%
  select(-Total) %>%
  pivot_longer(cols = Brazil:`South Africa`, 
               names_to = "Country", 
               values_to = "N_drugs") %>%
  mutate(
    Country = factor(Country, 
                     levels = c("Brazil", "Canada", "Netherlands", "Singapore", "South Africa")),
    category_short = str_replace(category_display, "^Rare ", "") %>%
      str_to_sentence(),
    category_short = factor(category_short, 
                            levels = rev(unique(category_short)))
  )


### Availability heatmap
fig5 <- ggplot(ta_long_avail, aes(x = Country, y = category_short, fill = N_drugs)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = N_drugs), color = "black", size = 3.5) +
  scale_fill_gradient(low = "#fef0f0", high = "#a83232", 
                      name = "N drugs\navailable") +
  labs(
    title = "Available orphan drugs per therapeutic area per country",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "right"
  )

print(fig5)


### Calculate availability percentages by category and country
availability_pct_per_cat <- master_with_cats %>%
  group_by(category) %>%
  summarise(
    Brazil         = round(100 * sum(available_brazil, na.rm = TRUE) / 
                             sum(approved_brazil | available_brazil, na.rm = TRUE), 1),
    Canada         = round(100 * sum(available_canada, na.rm = TRUE) / 
                             sum(approved_canada | available_canada, na.rm = TRUE), 1),
    Netherlands    = round(100 * sum(available_nl, na.rm = TRUE) / 
                             sum(approved_nl | available_nl, na.rm = TRUE), 1),
    Singapore      = round(100 * sum(available_singapore, na.rm = TRUE) / 
                             sum(approved_singapore | available_singapore, na.rm = TRUE), 1),
    `South Africa` = round(100 * sum(available_sa, na.rm = TRUE) / 
                             sum(approved_sa | available_sa, na.rm = TRUE), 1),
    Total_drugs    = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_drugs))

### Filter categories with sufficient sample size (at least 10)
availability_pct_filtered <- availability_pct_per_cat %>%
  filter(Total_drugs >= 10)

### Convert availability percentages to long format
ta_pct_long <- availability_pct_filtered %>%
  select(-Total_drugs) %>%
  pivot_longer(cols = Brazil:`South Africa`, 
               names_to = "Country", 
               values_to = "Pct") %>%
  mutate(
    Country = factor(Country, 
                     levels = c("Brazil", "Canada", "Netherlands", "Singapore", "South Africa")),
    category_short = str_replace(category, "^Rare ", "") %>% 
      str_to_sentence(),
    category_short = factor(category_short, 
                            levels = rev(unique(category_short)))
  )

### Availability percentage heatmap
fig6 <- ggplot(ta_pct_long, aes(x = Country, y = category_short, fill = Pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.0f%%", Pct)), 
            color = "black", size = 3.5) +
  scale_fill_gradient2(low = "#a83232", mid = "#f4e8a8", high = "#2e7d32",
                       midpoint = 50, limits = c(0, 100),
                       name = "Availability (%)") +
  labs(
    title = "Availability of approved orphan drugs per therapeutic area",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "right"
  )

print(fig6)


### Function to calculate pairwise overlap matrices
# Computes pairwise overlap percentages 
make_overlap_matrix <- function(country_list) {
  countries_full <- names(country_list)
  mat <- matrix(NA, nrow = length(countries_full), ncol = length(countries_full),
                dimnames = list(countries_full, countries_full))
  
  for (i in seq_along(countries_full)) {
    for (j in seq_along(countries_full)) {
      a <- country_list[[i]]
      b <- country_list[[j]]
      if (i == j) {
        mat[i, j] <- 100
      } else {
        union_n <- length(union(a, b))
        if (union_n > 0) {
          mat[i, j] <- round(100 * length(intersect(a, b)) / union_n, 1)
        }
      }
    }
  }
  
  as.data.frame(mat) %>%
    tibble::rownames_to_column("Country_A") %>%
    pivot_longer(cols = -Country_A, names_to = "Country_B", values_to = "Overlap")
}

### Approval overlap analysis. Measure similarity in orphan drug approvals between countries
countries_approved <- list(
  Brazil         = master$drug_id[master$approved_brazil],
  Canada         = master$drug_id[master$approved_canada],
  Netherlands    = master$drug_id[master$approved_nl],
  Singapore      = master$drug_id[master$approved_singapore],
  `South Africa` = master$drug_id[master$approved_sa]
)

overlap_app <- make_overlap_matrix(countries_approved) %>%
  mutate(
    Country_A = factor(Country_A, 
                       levels = c("Brazil", "Canada", "Netherlands", "Singapore", "South Africa")),
    Country_B = factor(Country_B, 
                       levels = rev(c("Brazil", "Canada", "Netherlands", "Singapore", "South Africa")))
  )

### Approval overlap heatmap
fig3a <- ggplot(overlap_app, aes(x = Country_A, y = Country_B, fill = Overlap)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.0f%%", Overlap)), color = "black", size = 3.5) +
  scale_fill_gradient(low = "#f0f4f8", high = "#1f4d7a", 
                      limits = c(0, 100), name = "Overlap (%)") +
  labs(
    title = "Approval overlap",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", angle = 30, hjust = 1),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 11),
    legend.position = "right"
  )

### Availability overlap analysis. Measure similarity in orphan drug availability between countries
countries_available <- list(
  Brazil         = master$drug_id[master$available_brazil],
  Canada         = master$drug_id[master$available_canada],
  Netherlands    = master$drug_id[master$available_nl],
  Singapore      = master$drug_id[master$available_singapore],
  `South Africa` = master$drug_id[master$available_sa]
)

overlap_avail <- make_overlap_matrix(countries_available) %>%
  mutate(
    Country_A = factor(Country_A, 
                       levels = c("Brazil", "Canada", "Netherlands", "Singapore", "South Africa")),
    Country_B = factor(Country_B, 
                       levels = rev(c("Brazil", "Canada", "Netherlands", "Singapore", "South Africa")))
  )

### Availability overlap heatmap
fig3b <- ggplot(overlap_avail, aes(x = Country_A, y = Country_B, fill = Overlap)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.0f%%", Overlap)), color = "black", size = 3.5) +
  scale_fill_gradient(low = "#fef0f0", high = "#a83232", 
                      limits = c(0, 100), name = "Overlap (%)") +
  labs(
    title = "Availability overlap",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", angle = 30, hjust = 1),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 11),
    legend.position = "right"
  )

### Combine both heatmaps such that they fit on one figure 
fig3 <- fig3a + fig3b + 
  plot_annotation(
    title = "Pairwise overlap of orphan drugs across countries",
    theme = theme(plot.title = element_text(face = "bold", size = 12))
  )

print(fig3)



### Prepare country-level approval and availability counts
### Calculate total numbers of approved and available orphan drugs for each country.

fig2_data <- tibble(
  Country = c("Brazil", "Canada", "Netherlands", "Singapore", "South Africa"),
  Approved = c(sum(master$approved_brazil),
               sum(master$approved_canada),
               sum(master$approved_nl),
               sum(master$approved_singapore),
               sum(master$approved_sa)),
  Available = c(sum(master$available_brazil),
                sum(master$available_canada),
                sum(master$available_nl),
                sum(master$available_singapore),
                sum(master$available_sa))
) %>%
  pivot_longer(cols = c(Approved, Available), 
               names_to = "Status", 
               values_to = "N_drugs") %>%
  mutate(
    Country = factor(Country, 
                     levels = c("Brazil", "Canada", "Netherlands", "Singapore", "South Africa")),
    Status = factor(Status, levels = c("Approved", "Available"))
  )

### Bar chart: approval vs. availability
### Compare number of approved and available orphan drugs across countries
fig2 <- ggplot(fig2_data, aes(x = Country, y = N_drugs, fill = Status)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = N_drugs), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Approved" = "#1f4d7a", "Available" = "#a83232")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Orphan drug approval and availability per country",
    x = NULL,
    y = "Number of drugs",
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "top"
  )

print(fig2)
