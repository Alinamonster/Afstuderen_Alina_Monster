### Load dataset 
drug   <- read.csv("Databases/Approved/Canada_approved/drug.txt", header = FALSE)
ingred <- read.csv("Databases/Approved/Canada_approved/ingred.txt", header = FALSE)
ther   <- read.csv("Databases/Approved/Canada_approved/ther.txt", header = FALSE)
status <- read.csv("Databases/Approved/Canada_approved/status.txt", header = FALSE)

# Assign column names to imported datasets
names(drug) <- c("DRUG_CODE", "PRODUCT_CATEGORIZATION", "CLASS",
                 "DRUG_IDENTIFICATION_NUMBER", "BRAND_NAME", "DESCRIPTOR",
                 "PEDIATRIC_FLAG", "ACCESSION_NUMBER", "NUMBER_OF_AIS",
                 "LAST_UPDATE_DATE", "AI_GROUP_NO", "CLASS_F",
                 "BRAND_NAME_F", "DESCRIPTOR_F")

names(ingred) <- c("DRUG_CODE", "ACTIVE_INGREDIENT_CODE", "INGREDIENT",
                   "INGREDIENT_SUPPLIED_IND", "STRENGTH", "STRENGTH_UNIT",
                   "STRENGTH_TYPE", "DOSAGE_VALUE", "BASE", "DOSAGE_UNIT",
                   "NOTES", "INGREDIENT_F", "STRENGTH_UNIT_F",
                   "STRENGTH_TYPE_F", "DOSAGE_UNIT_F")

names(ther) <- c("DRUG_CODE", "TC_ATC_NUMBER", "TC_ATC", "TC_AHFS_NUMBER")

names(status) <- c("DRUG_CODE", "CURRENT_STATUS_FLAG", "STATUS",
                   "HISTORY_DATE", "STATUS_F", "LOT_NUMBER", "EXPIRATION_DATE")

# Filter status to current only to get active drugs 
status_current <- status %>%
  mutate(HISTORY_DATE = as.Date(HISTORY_DATE, format = "%d-%b-%Y")) %>%
  filter(CURRENT_STATUS_FLAG == "Y")

# Build dataset
Canada_approved <- drug %>%
  # Merge drug, ingredient, therapeutic, and status information
  left_join(ingred, by = "DRUG_CODE", relationship = "many-to-many") %>%
  left_join(ther, by = "DRUG_CODE", relationship = "many-to-many") %>%
  left_join(status_current, by = "DRUG_CODE") %>%
  
  # Human products were left empty for this column, by selecting this, you wont get 
  # any vetinary products
  filter(PRODUCT_CATEGORIZATION == "") %>%
  select(DRUG_CODE, BRAND_NAME, INGREDIENT, TC_ATC_NUMBER, HISTORY_DATE) %>%
  
  # Clean ingredient names and ATC codes
  mutate(INGREDIENT = clean_ingredient(INGREDIENT),
         TC_ATC_NUMBER = na_if(TC_ATC_NUMBER, "")
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
  
  # Group by ingredient
  group_by(INGREDIENT) %>%
  summarise(
    first_approval = if (all(is.na(HISTORY_DATE))) {
      as.Date(NA)
    } else {
      min(HISTORY_DATE, na.rm = TRUE)
    },
    # Retain first non-missing ATC code
    atc_code = {
      atc <- na.omit(TC_ATC_NUMBER)
      if (length(atc) == 0) NA_character_ else atc[1]
    },
    .groups = "drop"
  ) 

#Summary 
cat("Canada total:", nrow(Canada_approved), "\n")
head(Canada_approved)

# Export template for manual approval 
#generate_approval_template(Canada_drug_ids, "Canada",    "Canada_approval_check.csv")
