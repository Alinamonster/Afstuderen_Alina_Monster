### Availability Index per country 
availability_index <- tibble(
  Country     = c("NL", "Canada", "Singapore", "Brazil", "SA"),
  N_approved  = c(sum(master$approved_nl),
                  sum(master$approved_canada),
                  sum(master$approved_singapore),
                  sum(master$approved_brazil),
                  sum(master$approved_sa)),
  N_available = c(sum(master$available_nl),
                  sum(master$available_canada),
                  sum(master$available_singapore),
                  sum(master$available_brazil),
                  sum(master$available_sa))
) %>%
  mutate(Availability_pct = round(N_available / N_approved * 100, 1))

print(availability_index)

# Creates lists of orphan drugs available in each country for analysis
countries_available <- list(
  NL = master$drug_id[master$available_nl],
  CA = master$drug_id[master$available_canada],
  SG = master$drug_id[master$available_singapore],
  BR = master$drug_id[master$available_brazil],
  SA = master$drug_id[master$available_sa]
)

### Generate pairwise country combinations. Produces all unique country pairs 
country_names <- names(countries_available)
pairs <- combn(country_names, 2, simplify = FALSE)

### Calculate pairwise availability overlap
availability_overlap <- data.frame(
  Country_A = character(), Country_B = character(),
  Shared = integer(), Union = integer(), Overlap_pct = numeric()
)

### Print output 
for (pair in pairs) {
  a <- countries_available[[pair[1]]]
  b <- countries_available[[pair[2]]]
  shared  <- length(intersect(a, b))
  union_n <- length(union(a, b))
  oi      <- if (union_n == 0) NA else round(100 * shared / union_n, 1)
  availability_overlap <- rbind(availability_overlap, data.frame(
    Country_A = pair[1], Country_B = pair[2],
    Shared = shared, Union = union_n, Overlap_pct = oi
  ))
  cat(sprintf("%s & %s: %d shared / %d union = %.1f%%\n",
              pair[1], pair[2], shared, union_n, oi))
}

# Select all drugs that are available in all five countries and calculate overlap 
avail_all5  <- Reduce(intersect, countries_available)
avail_union <- Reduce(union, countries_available)
avail_oi_all <- round(100 * length(avail_all5) / length(avail_union), 1)

### Print results in numbers
cat("Drugs available in all 5:", length(avail_all5), "\n")
cat("Total unique drugs (union):", length(avail_union), "\n")
cat("5-country availability overlap:", avail_oi_all, "%\n")

### Print results (names)
universal_drugs <- master %>%
  filter(available_nl & available_canada & available_singapore & 
           available_brazil & available_sa) %>%
  pull(canonical_name)
print(universal_drugs)

### Gap analysis
### Calculates difference between the number of countries in which drug is approved 
### and number in which it's available
gap_analysis <- master %>%
  mutate(
    n_approved  = approved_nl + approved_canada + approved_singapore + 
      approved_brazil + approved_sa,
    n_available = available_nl + available_canada + available_singapore + 
      available_brazil + available_sa,
    gap = n_approved - n_available
  ) %>%
  # Includes only drugs approved in at least two countries
  filter(n_approved >= 2)

cat("Drugs approved in ≥2 countries:", nrow(gap_analysis), "\n")
gap_table <- gap_analysis %>% count(gap, sort = TRUE)
print(gap_table)

### Identify drugs with the largest gaps
gap_top <- gap_analysis %>%
  arrange(desc(gap)) %>%
  select(canonical_name, n_approved, n_available, gap) %>%
  head(10)
print(gap_top)

### Rank countries by approval and availability performance
ranking_full <- availability_index %>%
  mutate(
    Rank_approval     = rank(-N_approved, ties.method = "min"),
    Rank_availability = rank(-Availability_pct, ties.method = "min")
  ) %>%
  arrange(Rank_approval)

print(ranking_full)

### Find out what type of drugs are universally approved or not approved anywhere
### Calculate approval count per drug
master_grouped <- master %>%
  mutate(
    n_approved = approved_brazil + approved_canada + approved_nl +
      approved_singapore + approved_sa,
    group = case_when(
      n_approved == 0 ~ "Not approved anywhere",
      n_approved == 5 ~ "Universally approved",
      TRUE            ~ "Other"
    )
  )

# Link therapeutic area to drug
group_categories <- master_grouped %>%
  filter(group %in% c("Not approved anywhere", "Universally approved")) %>%
  select(drug_id, group) %>%
  left_join(drug_to_category %>% select(drug_id, category), by = "drug_id")

# Top therapeutic areas per group
group_categories %>%
  filter(!is.na(category)) %>%
  count(group, category, sort = TRUE) %>%
  group_by(group) %>%
  slice_max(n, n = 5) %>%
  print(n = 20)

### Amount of drugs per group 
master_grouped %>%
  count(group)
# Which % of therapeutic area is universally approved/not approved at all? 
master_grouped %>%
  left_join(drug_to_category %>% select(drug_id, category), by = "drug_id") %>%
  filter(!is.na(category)) %>%
  group_by(category) %>%
  summarise(
    total_in_cat       = n(),
    n_universal        = sum(group == "Universally approved"),
    n_none             = sum(group == "Not approved anywhere"),
    pct_universal      = round(100 * n_universal / total_in_cat, 1),
    pct_none           = round(100 * n_none      / total_in_cat, 1),
    .groups = "drop"
  ) %>%
  filter(total_in_cat >= 10) %>%
  arrange(desc(pct_none)) %>%
  print(n = Inf)







### Voeg designation date toe vanuit de originele bestanden
### Lees de bronnen opnieuw in MET de datum-kolom

FDA_orphan_dates <- read_xlsx("Databases/FDA_orphandesignation.xlsx",
                              col_types = "text") %>%
  filter(`Orphan Designation Status` == "Designated/Approved") %>%
  transmute(
    INGREDIENT = clean_ingredient(`Generic Name`),
    fda_date   = as.Date(as.numeric(`Marketing Approval Date`), origin = "1899-12-30")
  ) %>%
  filter(!is.na(INGREDIENT), INGREDIENT != "") %>%
  group_by(INGREDIENT) %>%
  summarise(fda_date = suppressWarnings(min(fda_date, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(fda_date = if_else(is.infinite(as.numeric(fda_date)), as.Date(NA), fda_date))

EMA_orphan_dates <- read_xlsx("Databases/Approved/Netherlands_approved.xlsx",
                              col_types = "text") %>%
  filter(`Orphan medicine` == "Yes") %>%
  transmute(
    INGREDIENT = clean_ingredient(`Active substance`),
    ema_date   = dmy(`Marketing authorisation date`)
  ) %>%
  filter(!is.na(INGREDIENT), INGREDIENT != "") %>%
  group_by(INGREDIENT) %>%
  summarise(ema_date = suppressWarnings(min(ema_date, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(ema_date = if_else(is.infinite(as.numeric(ema_date)), as.Date(NA), ema_date))

### Combineer tot één designation-datum per ingredient (vroegste van de twee)
designation_dates <- full_join(FDA_orphan_dates, EMA_orphan_dates, by = "INGREDIENT") %>%
  mutate(designation_date = pmin(fda_date, ema_date, na.rm = TRUE))

### Koppel aan de 67 drugs via je orphan_variants_long
### (want master gebruikt drug_id, designation_dates gebruikt INGREDIENT)
not_approved_dates <- master_grouped %>%
  filter(group == "Not approved anywhere") %>%
  select(drug_id) %>%
  left_join(orphan_variants_long, by = "drug_id") %>%
  left_join(designation_dates, by = c("drug_variant" = "INGREDIENT")) %>%
  group_by(drug_id) %>%
  summarise(designation_date = min(designation_date, na.rm = TRUE), .groups = "drop")

### Vat samen
summary(not_approved_dates$designation_date)

not_approved_dates %>%
  mutate(year = year(designation_date)) %>%
  count(year) %>%
  arrange(year) %>%
  print(n = Inf)
