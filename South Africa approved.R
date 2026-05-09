# Load raw data
SA_raw <- readRDS("sahpra_medicines/sahpra_all_medicines.rds")

# Filter registered medicines and select relevant variables
SA_approved <- SA_raw %>%
  filter(status == "Registered") %>%
  select(productName, ingredient, reg_date, status) %>%
  
  # Convert registration dates to Date format
  mutate(
    reg_date = as.Date(reg_date, format = "%Y/%m/%d")
  ) %>%
  
  # Extract and clean ingredient names
  mutate(
    INGREDIENT_RAW = str_extract(ingredient, "(?i)(?:CONTAINS:?\\s+)(.+)"),
    INGREDIENT_RAW = str_remove(INGREDIENT_RAW, "(?i)^CONTAINS:?\\s+"),
    INGREDIENT_RAW = ifelse(is.na(INGREDIENT_RAW), ingredient, INGREDIENT_RAW),
    # Remove equivalence statements
    INGREDIENT_RAW = str_remove_all(INGREDIENT_RAW, "(?i)\\s*equival[ea]nt.*"),
    # Remove dosage units and trailing numbers
    INGREDIENT_RAW = str_remove_all(INGREDIENT_RAW, "\\s+[0-9,\\.]+\\s*(mg|g|%|ml|mcg|μg|iu|units|ccid50?|antigen units?)\\b"),
    INGREDIENT_RAW = str_remove_all(INGREDIENT_RAW, "\\s+[0-9,\\.]+$"),
    # Apply ingredient cleaning function
    INGREDIENT = clean_ingredient(INGREDIENT_RAW),
    INGREDIENT = str_replace_all(INGREDIENT, "\\s+", " "),
    INGREDIENT = str_trim(INGREDIENT)
  ) %>%
  filter(!is.na(INGREDIENT), INGREDIENT != "") %>%
  
  # split on different possible separators
  separate_rows(INGREDIENT, sep = ";|\\+|&&|,|/|\\s+and\\s+") %>%
  mutate(
    INGREDIENT = str_trim(INGREDIENT),
    INGREDIENT = str_remove(INGREDIENT, "^:\\s*"),
    INGREDIENT = str_trim(INGREDIENT)
  ) %>%
  
  # Remove invalid or non-informative ingredient entries
  filter(
    INGREDIENT != "",
    nchar(INGREDIENT) >= 4,                                    # At least 4 chars
    !str_detect(INGREDIENT, "^[0-9\\-&%\\+\\.\\s:]+$"),       # No pure numbers/symbols
    !str_detect(INGREDIENT, "^[0-9&\\-:]"),                   # No starting with number/symbol
    !str_detect(INGREDIENT, "\\?"),                           # No ?
    !str_detect(INGREDIENT, "^(each|tablet|capsule|solution|ml|per)\\b"),  # No dosage words
    !str_detect(INGREDIENT, "\\b(each|tablet|capsule|solution|ml|per)\\b"),  # anywhere in string
    str_detect(INGREDIENT, "[a-z]{3,}"),                       # Must have 3+ letters in a row
    # Filter out common junk words
    !str_detect(INGREDIENT, "^(with|electrolytes|emulsion|solution|oil|refined|anhydrous|hydrated)$")
  ) %>%
  
  group_by(INGREDIENT) %>%
  summarise(
    first_approval = if (all(is.na(reg_date))) {
      as.Date(NA)
    } else {
      min(reg_date, na.rm = TRUE)
    },
    atc_code = NA_character_,
    .groups = "drop"
  ) %>%
  
  # Identify orphan drugs using the master orphan drug list
  mutate(is_orphan = sapply(INGREDIENT, is_orphan_smart, master_orphan_list))

# Show summary
cat("South Africa - Final approved ingredients:", nrow(SA_approved), "\n")
cat("South Africa - Orphan drugs found:", sum(SA_approved$is_orphan), "\n")

# Display first 50 cleaned ingredients to check if they look clean
SA_approved %>%
  arrange(INGREDIENT) %>%
  select(INGREDIENT, is_orphan) %>%
  head(50) %>%
  print()

# Display example orphan drugs
SA_approved %>%
  filter(is_orphan) %>%
  arrange(INGREDIENT) %>%
  select(INGREDIENT, first_approval) %>%
  head(30) %>%
  print()

# Export template for manual approval verification
#generate_approval_template(SA_drug_ids, "SA", "SA_approval_check.csv")

