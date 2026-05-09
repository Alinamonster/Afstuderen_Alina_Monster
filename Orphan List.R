FDA_orphan <- read_xlsx("Databases/FDA_orphandesignation.xlsx")

# FDA orphan designation list: keep only approved/designated entries and clean names
FDA_orphan <- FDA_orphan %>%
  filter(`Orphan Designation Status` == "Designated/Approved") %>% 
  transmute(
    INGREDIENT = clean_ingredient(`Generic Name`)
  ) %>%
  filter(!is.na(INGREDIENT), INGREDIENT != "") %>%
  distinct(INGREDIENT)


EMA_orphan <- read_xlsx("Databases/Approved/Netherlands_approved.xlsx")

# EMA orphan medicines: extract orphan-labelled active substances
EMA_orphan <- EMA_orphan %>%
  filter(`Orphan medicine` == "Yes") %>%
  transmute(
    INGREDIENT = clean_ingredient(`Active substance`),
    atc_code = na_if(`ATC code (human)`, "")
  ) %>%
  filter(!is.na(INGREDIENT), INGREDIENT != "") %>%
  distinct(INGREDIENT, .keep_all = TRUE)  


# Combine FDA + EMA orphan lists into a single reference set
master_orphan_list <- c(
  EMA_orphan$INGREDIENT,
  FDA_orphan$INGREDIENT
) %>% unique()



### PUBCHEM API (MOLECULAR WEIGHT CHECK)
# Queries PubChem for molecular weight + formula and classifies size
check_molecular_weight <- function(drug_name) {
  url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/", 
                URLencode(drug_name), 
                "/property/MolecularWeight,MolecularFormula/JSON")
  
  tryCatch({
    response <- GET(url, timeout(10))
    
    if (status_code(response) == 200) {
      data <- content(response, "text", encoding = "UTF-8") %>% fromJSON()
      
      mol_weight <- as.numeric(data$PropertyTable$Properties$MolecularWeight[1])
      mol_formula <- data$PropertyTable$Properties$MolecularFormula[1]
      
      is_small <- if (!is.null(mol_weight) && !is.na(mol_weight)) {
        mol_weight < 1000 
      } else {
        NA  
      }
      
      return(list(
        found = TRUE,
        molecular_weight = mol_weight,
        formula = mol_formula,
        is_small_molecule = is_small
      ))
    } else {
      return(list(
        found = FALSE, 
        molecular_weight = NA, 
        formula = NA, 
        is_small_molecule = NA
      ))
    }
  }, error = function(e) {
    return(list(
      found = FALSE, 
      molecular_weight = NA, 
      formula = NA, 
      is_small_molecule = NA
    ))
  })
}


### PUBCHEM 
cat("Total drugs to check:", length(master_orphan_list), "\n")
cat("Estimated time:", round(length(master_orphan_list) * 0.3 / 60, 1), "minutes\n\n")

# Initialise results table for PubChem output
pubchem_results <- tibble(
  drug = master_orphan_list,
  found = NA,
  molecular_weight = NA,
  formula = NA,
  is_small_molecule = NA
)

# API with progress tracking
for (i in 1:nrow(pubchem_results)) {
  
  if (i %% 50 == 0) {
    cat("Progress:", i, "/", nrow(pubchem_results), 
        "| Found:", sum(pubchem_results$found[1:i], na.rm = TRUE), 
        "| Small mol:", sum(pubchem_results$is_small_molecule[1:i], na.rm = TRUE), "\n")
  }
  
  result <- check_molecular_weight(pubchem_results$drug[i])
  
  pubchem_results$found[i] <- result$found
  pubchem_results$molecular_weight[i] <- result$molecular_weight
  pubchem_results$formula[i] <- result$formula
  pubchem_results$is_small_molecule[i] <- result$is_small_molecule
  
  Sys.sleep(0.3)
}


### BIOLOGICS CLASSIFICATION + MANUAL REVIEW
# pattern for biologic naming conventions
biologics_pattern <- paste(
  "mab$", "umab$", "zumab$", "ximab$", "omab$",
  "cept$", "ceptin$",
  "ase$", "dase$",
  "alfa$", "beta$", "gamma$", "delta$",
  "kin$", "lukine$", "kinra$",
  "cog$", "octocog$", "nonacog$",
  "grastim$", "stim$",
  "irsen$", "ersen$", "mersen$",
  "tropin$", "trophin$",
  "genlecleucel$", "cileucel$", "leucel$",
  "actant$",
  "globulin$", "immune$",
  "antitrypsin$", "proteinase$",
  sep = "|"
)

# Create dataset of compounds that could not be resolved via PubChem
not_found_for_review <- pubchem_results %>%
  filter(!found | is.na(found)) %>%
  mutate(
    name_length = nchar(drug),
    has_biologic_suffix = str_detect(drug, biologics_pattern),
    has_protein_keyword = str_detect(drug, "(?i)protein|globulin|antitrypsin|immune|factor|interferon"),
    
    # classification for review prioritisation
    suggested_class = case_when(
      has_biologic_suffix ~ "biologic_suffix",
      has_protein_keyword ~ "biologic_protein",
      name_length > 50 ~ "biologic_long",
      name_length < 15 ~ "small_mol_short",
      TRUE ~ "UNKNOWN"
    ),
    
    # Pre-fill manual classification where likely biologic
    manual_classification = case_when(
      has_biologic_suffix ~ "N",
      has_protein_keyword ~ "N",
      TRUE ~ ""  # left blank for manual work
    ),
    
    # Suggestions
    review_note = case_when(
      has_biologic_suffix ~ "Auto: biologic suffix",
      has_protein_keyword ~ "Auto: protein keyword",
      name_length > 50 ~ "CHECK: Long name (likely biologic)",
      name_length < 15 ~ "CHECK: Short name (likely small mol)",
      TRUE ~ "CHECK: Manual review needed"
    )
  ) %>%
  select(drug, name_length, suggested_class, manual_classification, review_note) %>%
  arrange(suggested_class, drug)

write.csv(not_found_for_review, "NOT_FOUND_MANUAL_REVIEW_1000.csv", row.names = FALSE)



### MANUAL REVIEW IMPORT 
manual_review <- read.csv("NOT_FOUND_MANUAL_REVIEW_1000.csv", 
                          sep = ";", 
                          stringsAsFactors = FALSE)

# Remove index column if accidentally included
if (names(manual_review)[1] %in% c("X", "") || suppressWarnings(all(!is.na(as.numeric(manual_review[[1]]))))) {
  manual_review <- manual_review[, -1]
}

# Standardize column names after manual editing
names(manual_review) <- c("drug", "name_length", "suggested_class", 
                          "manual_classification", "review_note")

# Check whether manual classification is complete
missing <- manual_review %>%
  filter(manual_classification == "" | is.na(manual_classification))

if (nrow(missing) > 0) {
  cat("WARNING: Some drugs still need classification!\n")
  cat("Missing:", nrow(missing), "drugs\n")
  print(head(missing %>% select(drug, suggested_class), 20))
  cat("\nPlease fill these in before proceeding!\n")
} else {
  cat("All drugs have been classified!\n\n")
  
  # Merge manual classifications with PubChem results
  pubchem_results_final <- pubchem_results %>%
    left_join(
      manual_review %>% select(drug, manual_classification), 
      by = "drug"
    ) %>%
    mutate(
      name_length = nchar(drug),
      has_biologic_suffix = str_detect(drug, biologics_pattern),
      has_protein_keyword = str_detect(drug, "(?i)protein|globulin|antitrypsin|immune|factor|interferon"),
      
      # Final decision logic combining PubChem + manual override
      is_small_molecule_final = case_when(
        found == TRUE & !is.na(is_small_molecule) ~ is_small_molecule,
        !is.na(manual_classification) & manual_classification == "Y" ~ TRUE,
        !is.na(manual_classification) & manual_classification == "N" ~ FALSE,
        TRUE ~ NA
      )
    )
  
  # Summary statistics
  cat("=== FINAL CLASSIFICATION (WITH MANUAL REVIEW) ===\n")
  cat("Small molecules:", sum(pubchem_results_final$is_small_molecule_final, na.rm = TRUE), "\n")
  cat("Biologics:", sum(!pubchem_results_final$is_small_molecule_final, na.rm = TRUE), "\n")
  cat("Still NA:", sum(is.na(pubchem_results_final$is_small_molecule_final)), "\n\n")
  
  # Breakdown of classification sources
  cat("=== BREAKDOWN ===\n")
  cat("PubChem MW < 1000:", 
      sum(pubchem_results_final$found & pubchem_results_final$is_small_molecule_final, na.rm = TRUE), "\n")
  cat("PubChem MW >= 1000:", 
      sum(pubchem_results_final$found & !pubchem_results_final$is_small_molecule_final, na.rm = TRUE), "\n")
  cat("Manual: Small molecule (Y):", 
      sum(!pubchem_results_final$found & pubchem_results_final$is_small_molecule_final, na.rm = TRUE), "\n")
  cat("Manual: Biologic (N):", 
      sum(!pubchem_results_final$found & !pubchem_results_final$is_small_molecule_final, na.rm = TRUE), "\n\n")
  
  # Final orphan drug list (small molecules only)
  master_orphan_list <- pubchem_results_final %>%
    filter(is_small_molecule_final == TRUE) %>%
    pull(drug)
  
  write.csv(data.frame(drug = master_orphan_list), 
            "master_orphan_list.csv", 
            row.names = FALSE)
}


file.choose()

# Load curated final dataset
master_orphan_list_final <- read.csv("master_orphan_list_final.csv", sep = ";", 
                                     stringsAsFactors = FALSE)

# Assign unique drug IDs
master_orphan_list_final <- master_orphan_list_final %>%
  mutate(drug_id = row_number())

# Convert wide variant format to long format
orphan_variants_long <- master_orphan_list_final %>%
  pivot_longer(
    cols = starts_with("variant_"),
    names_to = "variant_slot",
    values_to = "drug_variant"
  ) %>%
  filter(!is.na(drug_variant), drug_variant != "") %>%
  mutate(drug_variant = str_squish(tolower(drug_variant))) %>%
  select(drug_id, drug_variant)

cat("Unique drugs:", n_distinct(orphan_variants_long$drug_id), "\n")
cat("Total variants:", nrow(orphan_variants_long), "\n")