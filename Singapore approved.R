# Load dataset
Sing_app <- read.csv("Databases/Approved/Singapore_approved.csv")

# Clean approved data
Sing_approved <- Sing_app %>%
  select(product_name, active_ingredients, approval_d, atc_code) %>%
  
  # Standardise ingredient separators
  mutate(
    active_ingredients = str_replace_all(active_ingredients, "(?i)\\s+eqv\\s+", " + "),
    active_ingredients = str_replace_all(active_ingredients, "(?i)\\s+equivalent to\\s+", " + ")
  ) %>%
  
  #  Clean ingredient names and format dates
  mutate(
    INGREDIENT = clean_ingredient(active_ingredients),
    approval_d = as.Date(substr(approval_d, 1, 10)),
    atc_code = na_if(atc_code, "pending")
  ) %>%
  
  # Remove missing ingredient entries
  filter(
    !is.na(INGREDIENT),
    INGREDIENT != ""
  ) %>%
  
  # Split combination products into separate ingredients
  separate_rows(INGREDIENT, sep = ";|\\+|&&|,") %>%
  mutate(INGREDIENT = str_trim(INGREDIENT)) %>%
  filter(INGREDIENT != "") %>%
  
  # Aggregate approval information at ingredient level
  group_by(INGREDIENT) %>%
  summarise(
    first_approval = if (all(is.na(approval_d))) {
      as.Date(NA)
    } else {
      min(approval_d, na.rm = TRUE)
    },
    
    # Retain first non-missing ATC code
    atc_code = {
      atc <- na.omit(atc_code)
      if (length(atc) == 0) NA_character_ else atc[1]
    },
    .groups = "drop"
  ) 

# Display summary statistics
cat("Singapore total:", nrow(Sing_approved), "\n")

# Preview cleaned dataset
head(Sing_approved)

# Generate template for manual fill-in 
#generate_approval_template(Sing_drug_ids,   "Singapore", "Singapore_approval_check.csv")
