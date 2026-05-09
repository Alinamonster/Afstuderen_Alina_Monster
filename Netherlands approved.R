### Load EMA data 
NL_app_ema <- read_xlsx("Databases/Approved/Netherlands_approved.xlsx")

# Filter to authorised human medicines and standardise structure
NL_approved_ema <- NL_app_ema %>%
  filter(Category == "Human", `Medicine status` == "Authorised") %>%
  
  # Select and standardise relevant variables
  transmute(
    INGREDIENT = clean_ingredient(`Active substance`),
    first_approval = as.Date(`Marketing authorisation date`, format = "%d/%m/%Y"),
    atc_code = `ATC code (human)`
  ) %>%
  
  # Handle missing ATC codes
  mutate(atc_code = na_if(atc_code, "")) %>%
  
  # Remove missing or empty ingredient entries
  filter(!is.na(INGREDIENT), INGREDIENT != "") %>%
  
  # Split combination products into separate ingredients
  separate_rows(INGREDIENT, sep = ";|\\+|&&|,") %>%
  
  # Standardise formatting
  mutate(INGREDIENT = str_trim(tolower(INGREDIENT))) %>%
  filter(INGREDIENT != "")


### Load CBG data
NL_app_cbg <- read_delim("Databases/Approved/Netherlands_approved.csv", 
                         delim = "|", show_col_types = FALSE)

# Clean and restructure CBG dataset
NL_approved_cbg <- NL_app_cbg %>%
  
  # Keep only registered medicines (RVG = authorised products)
  filter(str_trim(SOORT) == "RVG") %>%
  
  # Extract ingredient and ATC information
  transmute(
    INGREDIENT = clean_ingredient(str_extract(ATC, "(?<=- ).+$")),
    atc_code = str_extract(ATC, "^[A-Z]\\d{2}[A-Z]{2}\\d{2}"),
    first_approval = as.Date(INSCHRIJVINGSDATUM, format = "%Y/%m/%d")
  ) %>%
  
  # Standardise missing values
  mutate(atc_code = na_if(atc_code, "")) %>%
  
  # Remove invalid entries
  filter(!is.na(INGREDIENT), INGREDIENT != "") %>%
  
  # Split multi-ingredient products
  separate_rows(INGREDIENT, sep = ";|\\+|&&|,") %>%
  
  # Standardise formatting
  mutate(INGREDIENT = str_trim(tolower(INGREDIENT))) %>%
  filter(INGREDIENT != "")

# Combine both datasets 
NL_approved <- bind_rows(NL_approved_ema, NL_approved_cbg) %>%
  
  # Aggregate at ingredient level
  group_by(INGREDIENT) %>%
  summarise(
    
    # Use earliest available approval date
    first_approval = if (all(is.na(first_approval))) {
      as.Date(NA)
    } else {
      min(first_approval, na.rm = TRUE)
    },
    
    # Retain first available ATC code
    atc_code = {
      atc <- na.omit(atc_code)
      if (length(atc) == 0) NA_character_ else atc[1]
    },
    .groups = "drop"
  )

# View total rows
cat("NL_approved (EMA + CBG combined):", nrow(NL_approved), "\n")

# Export for manual approvals
#generate_approval_template(NL_drug_ids,     "Nederland", "Netherlands_approval_check.csv")
