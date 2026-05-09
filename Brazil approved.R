### Load Brazilian dataset
Brazil_app <- read_excel("Databases/Approved/Brazil_approved.xls")

### Extract products requiring translation
# Selects only fully registered medicines and keeps unique Portuguese ingredient names
Brazil_to_translate <- Brazil_app %>%
  filter(`Tipo de Regularização` == "REGISTRADO") %>% 
  transmute(
    INGREDIENT_PT = `Princípio Ativo ou Descrição do Medicamento Notificado`
  ) %>%
  filter(!is.na(INGREDIENT_PT), INGREDIENT_PT != "") %>%
  distinct(INGREDIENT_PT)

# Shows number of unique Portuguese ingredient names
cat("Brazil - Portuguese names to translate:", nrow(Brazil_to_translate), "\n")

### Prepare translation 
# Adds empty column for English translations
Brazil_to_translate <- Brazil_to_translate %>%
  mutate(INGREDIENT_EN = NA_character_)

### Translate Portuguese ingredient names to English
# Uses translation function with tracking of API vs pattern-based translations
api_count <- 0
pattern_count <- 0

for (i in 1:nrow(Brazil_to_translate)) {
  
  # Progress reporting every 50 entries
  if (i %% 50 == 0) {
    cat("Progress:", i, "/", nrow(Brazil_to_translate), 
        "- API:", api_count, "Pattern:", pattern_count, "\n")
  }
  
  # Translate each ingredient individually
  Brazil_to_translate$INGREDIENT_EN[i] <- translate_pt_to_en_tracked(
    Brazil_to_translate$INGREDIENT_PT[i]
  )
  
  # Rate limiting to avoid API overload
  Sys.sleep(0.2)
}

### Shows translation method usage
cat("Translated via API:", api_count, 
    "(", round(100*api_count/(api_count+pattern_count), 1), "%)\n")
cat("Translated via Pattern:", pattern_count, 
    "(", round(100*pattern_count/(api_count+pattern_count), 1), "%)\n")

### Build final Brazilian approval dataset
Brazil_approved <- Brazil_to_translate %>%
  mutate(INGREDIENT = clean_ingredient(INGREDIENT_EN)) %>%
  filter(!is.na(INGREDIENT), INGREDIENT != "") %>%
  
  # Split multi-ingredient products
  separate_rows(INGREDIENT, sep = ";|\\+|&&|,|/") %>%
  mutate(INGREDIENT = str_trim(INGREDIENT)) %>%
  filter(INGREDIENT != "") %>%
  
  # Aggregate at ingredient level
  group_by(INGREDIENT) %>%
  summarise(
    first_approval = as.Date(NA),  
    atc_code = NA_character_,
    .groups = "drop"
  )

# Report final dataset size
cat("\nBrazil - Final approved ingredients (after splitting):", nrow(Brazil_approved), "\n")

### Show translation examples
cat("\n=== TRANSLATION EXAMPLES ===\n")
Brazil_to_translate %>%
  select(INGREDIENT_PT, INGREDIENT_EN) %>%
  head(20) %>%
  print()

# Preview final dataset
head(Brazil_approved)

# Export template for manual validation
# generate_approval_template(Brazil_drug_ids, "Brazil", "Brazil_approval_check.csv")