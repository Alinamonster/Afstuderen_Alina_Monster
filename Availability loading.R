### Canada: manual availability (CADTH + ODB combined)
# run once, then comment out
# cadth_ids <- match_country_drug_ids(Canada_available$`Generic Name`)
# drugs_to_check_canada <- orphan_variants_long %>%
#   group_by(drug_id) %>%
#   summarise(canonical_name = first(drug_variant), .groups = "drop") %>%
#   arrange(canonical_name) %>%
#   mutate(
#     available = if_else(drug_id %in% cadth_ids, "Y", ""),
#     source    = if_else(drug_id %in% cadth_ids, "CADTH", "")
#   )
# write.csv(drugs_to_check_canada, "Canada_manual_check.csv", row.names = FALSE)

### Fill in manual and reload, filter for Y only
Canada_manual <- read_csv("Canada_manual_check.csv", show_col_types = FALSE)
canada_available_ids <- Canada_manual %>%
  filter(available == "Y") %>%
  pull(drug_id)

### Brazil: manual availability (RENAME) 
# run once, then comment out
# drugs_to_check_brazil <- orphan_variants_long %>%
#   group_by(drug_id) %>%
#   summarise(canonical_name = first(drug_variant), .groups = "drop") %>%
#   arrange(canonical_name) %>%
#   mutate(available = "")
# write.csv(drugs_to_check_brazil, "Brazil_manual_check.csv", row.names = FALSE)

### Fill in manual and reload, filter for Y only
Brazil_manual <- read_csv("Brazil_manual_check.csv", show_col_types = FALSE)
brazil_available_ids <- Brazil_manual %>%
  filter(available == "Y") %>%
  pull(drug_id)

### Netherlands: manual availability (GVS)
# run once, then comment out
# drugs_to_check_netherlands <- orphan_variants_long %>%
#   group_by(drug_id) %>%
#   summarise(canonical_name = first(drug_variant), .groups = "drop") %>%
#   arrange(canonical_name) %>%
#   mutate(available = "")
# write.csv(drugs_to_check_netherlands, "Netherlands_manual_check.csv", row.names = FALSE)

### Fill in manual and reload, filter for Y only
Netherlands_manual <- read_csv("Netherlands_manual_check.csv", show_col_types = FALSE)
netherlands_available_ids <- Netherlands_manual %>%
  filter(available == "Y") %>%
  pull(drug_id)

### Singapore: manual availability (SDL) 
# run once, then comment out
# drugs_to_check_singapore <- orphan_variants_long %>%
#   group_by(drug_id) %>%
#   summarise(canonical_name = first(drug_variant), .groups = "drop") %>%
#   arrange(canonical_name) %>%
#   mutate(
#     available = if_else(drug_id %in% singapore_available_ids, "Y", ""),
#     source    = if_else(drug_id %in% singapore_available_ids, "SDL", "")
#   )
# write.csv(drugs_to_check_singapore, "Singapore_manual_check.csv", row.names = FALSE)

### Fill in manual and reload, filter for Y only
Singapore_manual <- read_csv2("Singapore_manual_check.csv", show_col_types = FALSE)
singapore_available_ids <- Singapore_manual %>%
  filter(available == "Y") %>%
  pull(drug_id)

### South Africa: handmatige availability (EML)
# run once, then comment out
# drugs_to_check_sa <- orphan_variants_long %>%
#   group_by(drug_id) %>%
#   summarise(canonical_name = first(drug_variant), .groups = "drop") %>%
#   arrange(canonical_name) %>%
#   mutate(
#     available = if_else(drug_id %in% sa_available_ids, "Y", ""),
#     source    = if_else(drug_id %in% sa_available_ids, "EML", "")
#   )
# write.csv(drugs_to_check_sa, "SA_manual_check.csv", row.names = FALSE)

### Fill in manual and reload, filter for Y only
SA_manual <- read_csv2("SA_manual_check.csv", show_col_types = FALSE)
sa_available_ids <- SA_manual %>%
  filter(available == "Y") %>%
  pull(drug_id)


### Print availability counts
cat("Availability (allemaal handmatig):\n")
cat("  NL:       ", length(netherlands_available_ids), "\n")
cat("  Canada:   ", length(canada_available_ids),      "\n")
cat("  Singapore:", length(singapore_available_ids),   "\n")
cat("  Brazil:   ", length(brazil_available_ids),      "\n")
cat("  SA:       ", length(sa_available_ids),          "\n")


### Create variables indicating whether each drug is approved 
### and available in each country
master <- orphan_variants_long %>%
  distinct(drug_id) %>%
  arrange(drug_id) %>%
  mutate(
    approved_nl         = drug_id %in% countries$NL,
    available_nl        = drug_id %in% netherlands_available_ids,
    approved_canada     = drug_id %in% countries$CA,
    available_canada    = drug_id %in% canada_available_ids,
    approved_singapore  = drug_id %in% countries$SG,
    available_singapore = drug_id %in% singapore_available_ids,
    approved_brazil     = drug_id %in% countries$BR,
    available_brazil    = drug_id %in% brazil_available_ids,
    approved_sa         = drug_id %in% countries$SA,
    available_sa        = drug_id %in% sa_available_ids
  ) %>%
  # Adds a drug name for each unique drug_id. Uses the first recorded drug variant
  left_join(
    orphan_variants_long %>%
      group_by(drug_id) %>%
      summarise(canonical_name = first(drug_variant), .groups = "drop"),
    by = "drug_id"
  ) %>%
  relocate(drug_id, canonical_name)
