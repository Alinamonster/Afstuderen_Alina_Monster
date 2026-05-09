# ### Extract unique drug variants and associated conditions
# ### Creates dataset containing unique drug identifiers and variants,
# ### and assign disease conditions using extraction function.
# drug_classifications <- orphan_variants_long %>%
#   distinct(drug_id, drug_variant) %>%
#   mutate(condition = sapply(drug_variant, get_condition))
# 
# ### Import previously curated condition classifications
# #write.csv(drug_classifications, "drug_classifications.csv", row.names = FALSE)
# conditions_old <- read_csv2("drug_classifications.csv", show_col_types = FALSE)
# 
# ### Standardise drug variant names
# conditions_old <- conditions_old %>%
#   mutate(drug_variant = str_squish(tolower(drug_variant)))
# 
# ### Links previously classified conditions to the updated drug dataset
# ### Matches variant to new drug_ids
# conditions_new <- orphan_variants_long %>%
#   left_join(conditions_old %>% select(drug_variant, condition),
#             by = "drug_variant")
# 
# ### Assign one condition per drug
# conditions_per_drug <- conditions_new %>%
#   filter(!is.na(condition)) %>%
#   group_by(drug_id) %>%
#   summarise(condition = first(condition), .groups = "drop")
# 
# 
# ### Load Orphanet classification data and extract disorder names and disease categories
# xml <- read_xml("Databases/linearisation_conditions.xml")
# disorders <- xml_find_all(xml, "//Disorder[OrphaCode]")
# 
# ### Create Orphanet disorder lookup table. Reference table linking disorders to categories
# disorder_lookup <- tibble(
#   orpha_code      = xml_text(xml_find_first(disorders, "OrphaCode")),
#   disorder_name   = xml_text(xml_find_first(disorders, "Name[@lang='en']")),
#   parent_category = xml_text(xml_find_first(disorders,
#                                             ".//TargetDisorder/Name[@lang='en']"))
# )
# 
# ### Display available Orphanet categories
# orphanet_categories <- unique(disorder_lookup$parent_category)
# orphanet_categories <- sort(orphanet_categories[!is.na(orphanet_categories)])
# 
# cat("\nBeschikbare Orphanet categorieën (", length(orphanet_categories), "):\n")
# print(orphanet_categories)
# 
# 
# ### Automatic category matching. Results in sugested_category. That way, there is
# ### a guideline when doing manual work
# # Splits drugs with multiple indications at '+'
# conditions_split <- conditions_per_drug %>%
#   separate_rows(condition, sep = "\\s*\\+\\s*") %>%
#   mutate(
#     condition       = str_squish(condition),
#     condition_lower = fix_spelling(tolower(condition))
#   )
# 
# ### Standardise condition and disorder names to increase matching
# disorder_lookup <- disorder_lookup %>%
#   mutate(disorder_lower = fix_spelling(tolower(disorder_name)))
# 
# ### Exact matching matches conditions to Orphanet disorders using exact matching
# matched_exact <- conditions_split %>%
#   left_join(disorder_lookup %>% select(disorder_lower, parent_category),
#             by = c("condition_lower" = "disorder_lower"))
# 
# cat("\nExact match:", sum(!is.na(matched_exact$parent_category)),
#     "/", nrow(matched_exact),
#     "(", round(100*sum(!is.na(matched_exact$parent_category))/nrow(matched_exact), 1), "%)\n")
# 
# ### When there was no exact matching: Applies substring-based matching to conditions not matched
# unmatched_idx <- which(is.na(matched_exact$parent_category))
# 
# 
# for (i in unmatched_idx) {
#   cond <- matched_exact$condition_lower[i]
#   if (cond == "" || is.na(cond)) next
# 
#   hits <- disorder_lookup %>%
#     filter(str_detect(disorder_lower, fixed(cond))) %>%
#     filter(!is.na(parent_category))
# 
#   ### assigns the most commonly occurring parent category 
#   if (nrow(hits) > 0) {
#     most_common_cat <- hits %>%
#       count(parent_category, sort = TRUE) %>%
#       slice(1) %>%
#       pull(parent_category)
#     matched_exact$parent_category[i] <- most_common_cat
#   }
# }
# 
# cat("Total match na substring:", sum(!is.na(matched_exact$parent_category)),
#     "/", nrow(matched_exact),
#     "(", round(100*sum(!is.na(matched_exact$parent_category))/nrow(matched_exact), 1), "%)\n")
# 
# # Summarises matched categories and conditions for each drug and identify drugs with unmatched conditions
# drug_categories_auto <- matched_exact %>%
#   mutate(
#     cat_or_na = if_else(is.na(parent_category),
#                         paste0("NA: ", condition),
#                         parent_category)
#   ) %>%
#   group_by(drug_id) %>%
#   summarise(
#     all_conditions     = paste(unique(condition), collapse = " + "),
#     suggested_category = paste(unique(cat_or_na), collapse = " + "),
#     n_unmatched        = sum(is.na(parent_category)),
#     .groups = "drop"
#   )
# 
# 
# ### Create manual review template. contains suggestions and empty 
# ### column for manual entry or correction
# manual_template <- conditions_per_drug %>%
#   arrange(drug_id) %>%
#   left_join(
#     orphan_variants_long %>%
#       group_by(drug_id) %>%
#       summarise(canonical_name = first(drug_variant), .groups = "drop"),
#     by = "drug_id"
#   ) %>%
#   left_join(
#     drug_categories_auto %>% select(drug_id, suggested_category),
#     by = "drug_id"
#   ) %>%
#   select(drug_id, canonical_name, condition, suggested_category) %>%
#   mutate(manual_category = "")
# 
# # write_csv(manual_template, "drug_categories_manual.csv")
# 



### Import manually reviewed drug-category classification dataset
manual_categories <- read_csv("drug_categories_manual.csv", show_col_types = FALSE)


### Split drugs assigned to multiple categories (eg "Rare neurologic disease + Rare immune disease")
### such that both categories count
### Create table that links each drug to a category
drug_to_category <- manual_categories %>%
  select(drug_id, canonical_name, manual_category) %>%
  separate_rows(manual_category, sep = "\\s*\\+\\s*") %>%
  mutate(category = str_squish(manual_category)) %>%
  filter(category != "") %>%
  select(drug_id, canonical_name, category)

cat("Drug-category pairs:", nrow(drug_to_category), "\n")
cat("Drugs:               ", n_distinct(drug_to_category$drug_id), "\n")
cat("Unique categories:  ", n_distinct(drug_to_category$category), "\n\n")


### Merge categories with master orphan drug table. Whenever there are several 
### categories, a drug get several rows in the new dataset
master_with_cats <- master %>%
  left_join(drug_to_category %>% select(drug_id, category), by = "drug_id")

### Calculate total number of drugs per category
category_totals <- drug_to_category %>%
  count(category, name = "n_drugs", sort = TRUE)

cat("=== DRUGS PER CATEGORIE (totaal in onze orphan list) ===\n")
print(category_totals, n = Inf)


### Calculate approvals by category and country
approval_per_cat <- master_with_cats %>%
  group_by(category) %>%
  summarise(
    NL        = sum(approved_nl,        na.rm = TRUE),
    Canada    = sum(approved_canada,    na.rm = TRUE),
    Singapore = sum(approved_singapore, na.rm = TRUE),
    Brazil    = sum(approved_brazil,    na.rm = TRUE),
    SA        = sum(approved_sa,        na.rm = TRUE),
    Total     = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Total))

cat("\n=== APPROVAL per CATEGORIE per LAND ===\n")
print(approval_per_cat, n = Inf)


### Calculate availability by category and country
availability_per_cat <- master_with_cats %>%
  group_by(category) %>%
  summarise(
    NL        = sum(available_nl,        na.rm = TRUE),
    Canada    = sum(available_canada,    na.rm = TRUE),
    Singapore = sum(available_singapore, na.rm = TRUE),
    Brazil    = sum(available_brazil,    na.rm = TRUE),
    SA        = sum(available_sa,        na.rm = TRUE),
    Total     = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Total))

cat("\n=== AVAILABILITY per CATEGORIE per LAND ===\n")
print(availability_per_cat, n = Inf)


### Calculate availability percentages by category and country
availability_pct_per_cat <- master_with_cats %>%
  group_by(category) %>%
  summarise(
    NL_pct = round(100 * sum(available_nl, na.rm = TRUE) / 
                     sum(approved_nl | available_nl, na.rm = TRUE), 1),
    Canada_pct = round(100 * sum(available_canada, na.rm = TRUE) / 
                         sum(approved_canada | available_canada, na.rm = TRUE), 1),
    Singapore_pct = round(100 * sum(available_singapore, na.rm = TRUE) / 
                            sum(approved_singapore | available_singapore, na.rm = TRUE), 1),
    Brazil_pct = round(100 * sum(available_brazil, na.rm = TRUE) / 
                         sum(approved_brazil | available_brazil, na.rm = TRUE), 1),
    SA_pct = round(100 * sum(available_sa, na.rm = TRUE) / 
                     sum(approved_sa | available_sa, na.rm = TRUE), 1),
    Total_drugs = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_drugs))

cat("\n=== AVAILABILITY % per CATEGORIE per LAND (union-based) ===\n")
print(availability_pct_per_cat, n = Inf)


### Filter categories with sufficient sample size (at least 10 drugs in a category)
availability_pct_filtered <- availability_pct_per_cat %>%
  filter(Total_drugs >= 10)

cat("\n=== Filtered (≥10 drugs) ===\n")
print(availability_pct_filtered, n = Inf)
