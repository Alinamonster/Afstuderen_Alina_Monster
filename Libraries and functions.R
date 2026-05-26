library(dplyr)
library(readxl)
library(stringr)
library(httr)
library(jsonlite)
library(tidyr)
library(rvest)
library(httr2)
library(tibble)
library(stringdist) 
library(readr)
library(ggplot2)
library(patchwork)
library(lubridate)
library(broom)
library(emmeans)

### Cleaning
# Standardizes ingredient strings: casing, punctuation, salts, dosages, etc.
clean_ingredient <- function(x) {
  x %>%
    str_to_upper() %>%
    str_trim() %>%
    str_remove_all("\\(.*?\\)") %>%
    str_remove(" EQV.*") %>%
    str_remove_all("\\b(SODIUM|MALEATE|HYDROCHLORIDE|ACETATE|TARTRATE|SULFATE|SULPHATE|MESYLATE|CITRATE|PHOSPHATE|SUCCINATE|FUMARATE)\\b") %>%
    str_remove_all("[0-9]+%|[0-9]+ ?MG") %>%
    str_remove_all("[0-9]+\\.?$") %>%
    str_remove_all("@.*") %>%
    str_replace_all("\\s*,\\s*", " + ") %>%
    str_trim() %>%
    tolower()
}

# Splits combination therapies into separate rows per ingredient
split_combinations <- function(df, ingredient_col = "INGREDIENT") {
  df %>%
    separate_rows(!!sym(ingredient_col), sep = ";|\\+|&&|,") %>%
    mutate(
      !!sym(ingredient_col) := str_trim(!!sym(ingredient_col))
    ) %>%
    filter(!!sym(ingredient_col) != "")
}

### BRAZIL

# RxNorm-based fuzzy translation of drug names via API
translate_drug_rxnorm <- function(drug_name) {
  drug_clean <- drug_name %>%
    str_to_upper() %>%
    str_trim()
  
  url <- paste0("https://rxnav.nlm.nih.gov/REST/approximateTerm.json?term=", 
                URLencode(drug_clean))
  
  tryCatch({
    response <- GET(url)
    if (status_code(response) != 200) return(NA_character_)
    
    data <- content(response, "text", encoding = "UTF-8") %>% fromJSON()
    
    if (!is.null(data$approximateGroup$candidate)) {
      candidates <- data$approximateGroup$candidate
      if (length(candidates) > 0 && nrow(candidates) > 0) {
        best_match <- candidates$name[1]
        if (length(best_match) == 0 || is.null(best_match)) {
          return(NA_character_)
        }
        return(best_match)
      }
    }
    return(NA_character_)
  }, error = function(e) {
    return(NA_character_)
  })
}

# Rule-based Portuguese to English ingredient translation with RxNorm validation
translate_pt_to_en_tracked <- function(pt_name) {
  
  clean <- pt_name %>%
    str_to_lower() %>%
    str_trim() %>%
    # Remove Portuguese salt prefixes (systematic stripping)
    str_remove("^acetato de ") %>%
    str_remove("^acetonida de ") %>%
    str_remove("^alendronato de ") %>%
    str_remove("^alginato de ") %>%
    str_remove("^aspartato de ") %>%
    str_remove("^benzoato de ") %>%
    str_remove("^besilato de ") %>%
    str_remove("^bicarbonato de ") %>%
    str_remove("^bissulfato de ") %>%
    str_remove("^brometo de ") %>%
    str_remove("^bromidrato de ") %>%
    str_remove("^carbonato de ") %>%
    str_remove("^cipionato de ") %>%
    str_remove("^citrato de ") %>%
    str_remove("^clavulanato de ") %>%
    str_remove("^cloreto de ") %>%
    str_remove("^cloridrato de ") %>%
    str_remove("^clonixinato de ") %>%
    str_remove("^decanoato de ") %>%
    str_remove("^diaspartato de ") %>%
    str_remove("^dicloridrato de ") %>%
    str_remove("^difosfato de ") %>%
    str_remove("^dimesilato de ") %>%
    str_remove("^dipropionato de ") %>%
    str_remove("^ditartarato de ") %>%
    str_remove("^ditosilato de ") %>%
    str_remove("^divalproato de ") %>%
    str_remove("^dobesilato de ") %>%
    str_remove("^embonato de ") %>%
    str_remove("^enantato de ") %>%
    str_remove("^esilato de ") %>%
    str_remove("^estolato de ") %>%
    str_remove("^etabonato de ") %>%
    str_remove("^etanolato de ") %>%
    str_remove("^fendizoato de ") %>%
    str_remove("^folinato de ") %>%
    str_remove("^fosfato dissódico de ") %>%
    str_remove("^fosfato sódico de ") %>%
    str_remove("^fosfato de ") %>%
    str_remove("^fumarato de ") %>%
    str_remove("^furoato de ") %>%
    str_remove("^fusidato de ") %>%
    str_remove("^gadobenato de ") %>%
    str_remove("^glicinato de ") %>%
    str_remove("^gliconato de ") %>%
    str_remove("^glicerofosfato de ") %>%
    str_remove("^gluconato de ") %>%
    str_remove("^hemifumarato de ") %>%
    str_remove("^hemitartarato de ") %>%
    str_remove("^hexafluoreto de ") %>%
    str_remove("^hialuronato de ") %>%
    str_remove("^hiclato de ") %>%
    str_remove("^hidrobrometo de ") %>%
    str_remove("^hidrogenotartarato de ") %>%
    str_remove("^hidróxido de ") %>%
    str_remove("^ibandronato de ") %>%
    str_remove("^iodeto de ") %>%
    str_remove("^isetionato de ") %>%
    str_remove("^lactato de ") %>%
    str_remove("^lactobionato de ") %>%
    str_remove("^levolisinato de ") %>%
    str_remove("^levomalato de ") %>%
    str_remove("^levomefolato de ") %>%
    str_remove("^lisinato de ") %>%
    str_remove("^malato de ") %>%
    str_remove("^maleato de ") %>%
    str_remove("^mesilato de ") %>%
    str_remove("^metilbrometo de ") %>%
    str_remove("^metilsulfato de ") %>%
    str_remove("^metotrexato de ") %>%
    str_remove("^micofenolato de ") %>%
    str_remove("^mononitrato de ") %>%
    str_remove("^montelucaste de ") %>%
    str_remove("^nitrato de ") %>%
    str_remove("^nitroprusseto de ") %>%
    str_remove("^oleato de ") %>%
    str_remove("^oxalato de ") %>%
    str_remove("^palmitato de ") %>%
    str_remove("^pamoato de ") %>%
    str_remove("^pantotenato de ") %>%
    str_remove("^pertecnetato de ") %>%
    str_remove("^picossulfato de ") %>%
    str_remove("^pidolato de ") %>%
    str_remove("^poliestirenossulfonato de ") %>%
    str_remove("^polissulfato de ") %>%
    str_remove("^propilenoglicolato de ") %>%
    str_remove("^propionato de ") %>%
    str_remove("^sacarato de ") %>%
    str_remove("^salicilato de ") %>%
    str_remove("^subgalato de ") %>%
    str_remove("^succinato sódico de ") %>%
    str_remove("^succinato de ") %>%
    str_remove("^sulfadiazina de ") %>%
    str_remove("^sulfato de ") %>%
    str_remove("^sulfeto de ") %>%
    str_remove("^tartarato de ") %>%
    str_remove("^tosilato de ") %>%
    str_remove("^tricloreto de ") %>%
    str_remove("^trifenatato de ") %>%
    str_remove("^undecilato de ") %>%
    str_remove("^valerato de ") %>%
    str_remove("^valproato de ") %>%
    str_remove("^xinafoato de ") %>%
    # Remove salt suffixes and metal forms
    str_remove(" dissódica?$") %>%
    str_remove(" de sódio$") %>%
    str_remove(" sódica?$") %>%
    str_remove(" sódico$") %>%
    str_remove(" de potássio$") %>%
    str_remove(" potássica?$") %>%
    str_remove(" de cálcio$") %>%
    str_remove(" de magnésio$") %>%
    str_remove(" de zinco$") %>%
    str_remove(" de alumínio$") %>%
    # Hydrate forms cleanup
    str_remove(" sesqui-?idratad[oa]$") %>%
    str_remove(" pentaidratad[oa]$") %>%
    str_remove(" hemipentaidratad[oa]$") %>%
    str_remove(" heptaidratad[oa]$") %>%
    str_remove(" hexaidratad[oa]$") %>%
    str_remove(" tetraidratad[oa]$") %>%
    str_remove(" trihidratad[oa]$") %>%
    str_remove(" diidratad[oa]$") %>%
    str_remove(" monoidratad[oa]$") %>%
    str_remove(" hemi-?idratad[oa]$") %>%
    str_remove(" anidro$") %>%
    str_remove(" anidra$")
  
  # Skip pure elements/metals
  if (clean %in% c("sódio", "potássio", "cálcio", "magnésio", "zinco", "alumínio", 
                   "sodium", "potassium", "calcium", "magnesium", "zinc", "aluminum")) {
    return(NA_character_) 
  }
  
  # Pattern-based linguistic normalization (Portuguese to English roots)
  pattern_result <- clean %>%
    str_replace("^talidom", "thalidom") %>%
    str_replace("^teofil", "theophyll") %>%
    str_replace("^hidroxi", "hydroxy") %>%
    str_replace("^hidro", "hydro") %>%
    str_replace("^feno", "pheno") %>%
    str_replace("^fos", "phos") %>%
    str_replace("^cef", "cef") %>%
    str_replace("^xanto", "xantho") %>%
    str_replace("ftal", "phthal") %>%
    str_replace("ácid", "acid") %>%
    str_replace("ureia$", "urea") %>%
    str_replace("ato$", "ate") %>%
    str_replace("ila$", "il") %>%
    str_replace("ina$", "ine") %>%
    str_replace("ida$", "ide") %>%
    str_replace("ona$", "one") %>%
    str_replace("ico$", "ic") %>%
    str_replace("ano$", "an") %>%
    str_replace("eno$", "ene") %>%
    str_replace("ose$", "ose") %>%
    str_replace("zol$", "zole") %>%
    str_replace("idina$", "idine") %>%
    str_replace_all("ç", "c") %>%
    str_replace_all("ã", "a") %>%
    str_replace_all("õ", "o") %>%
    str_replace_all("á", "a") %>%
    str_replace_all("é", "e") %>%
    str_replace_all("í", "i") %>%
    str_replace_all("ó", "o") %>%
    str_replace_all("ú", "u")
  
  # External validation using RxNorm API
  rxnorm_result <- translate_drug_rxnorm(pattern_result)
  
  if (length(rxnorm_result) > 0 && !is.na(rxnorm_result)) {
    rxnorm_lower <- tolower(rxnorm_result)
    
    # Similarity filter: avoid false-positive mappings
    edit_dist <- adist(pattern_result, rxnorm_lower)[1, 1]
    rel_dist  <- edit_dist / max(nchar(pattern_result), nchar(rxnorm_lower), 1)
    
    if (rel_dist <= 0.4) {
      api_count <<- api_count + 1
      return(rxnorm_lower)
    }
  }
  
  pattern_count <<- pattern_count + 1
  return(pattern_result)
}


### ORPHAN DETECTION

# Smart rule-based orphan drug identification
is_orphan_smart <- function(ingredient, orphan_list) {
  
  # Direct match
  if (ingredient %in% orphan_list) {
    return(TRUE)
  }
  
  # Handle combination therapies
  combos <- orphan_list[str_detect(orphan_list, "\\+|;|&&|,|/")]
  
  for (combo in combos) {
    components <- str_split(combo, "\\+|;|&&|,|/")[[1]] %>%
      str_trim() %>%
      tolower()
    
    if (ingredient %in% components) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}


### CONDITION MAPPING
# Retrieves condition linked to a drug variant
get_condition <- function(drug_variant) {
  matches <- iris_clean %>%
    filter(drug == drug_variant)
  
  if (nrow(matches) == 0) return(NA_character_)
  
  cond <- matches %>%
    pull(`Condition (MedDRA)`) %>%
    na_if("") %>%
    na.omit() %>%
    unique()
  
  if (length(cond) == 0) NA_character_ else paste(cond, collapse = " + ")
}



### COUNTRY MATCHING HELPERS
# Splits multi-component drug definitions into normalized parts
split_components <- function(s) {
  parts <- str_split(s, ";|/|\\+|\\s+and\\s+")[[1]]
  parts <- str_squish(parts)
  parts <- parts[parts != ""]
  parts
}

# Matches drug requirements against country ingredient sets
match_country_drug_ids <- function(country_ingredients) {
  country_set <- country_ingredients %>%
    na.omit() %>%
    tolower() %>%
    str_squish() %>%
    unique()
  country_set <- country_set[country_set != ""]
  
  matches <- vapply(drug_requirements$components, function(comps) {
    length(comps) > 0 && all(comps %in% country_set)
  }, logical(1))
  
  unique(drug_requirements$drug_id[matches])
}

# Flags which ingredients contribute to matched orphan drugs
flag_orphan_ingredients <- function(df, matched_ids) {
  country_set <- df$INGREDIENT %>%
    na.omit() %>% tolower() %>% str_squish()
  
  contributing <- drug_requirements %>%
    filter(drug_id %in% matched_ids) %>%
    pull(components) %>%
    unlist() %>%
    unique()
  
  df %>%
    mutate(
      ingredient_norm = str_squish(tolower(INGREDIENT)),
      is_orphan = ingredient_norm %in% contributing
    ) %>%
    select(-ingredient_norm)
}

# similarity index (percentage overlap)
overlap_index <- function(a, b) {
  intersection <- length(intersect(a, b))
  union_n <- length(union(a, b))
  if (union_n == 0) return(NA)
  round(100 * intersection / union_n, 1)
}

# Standardises spelling differences (UK/US + medical variants)
fix_spelling <- function(x) {
  x %>%
    str_replace_all("leukaemia", "leukemia") %>%
    str_replace_all("tumour", "tumor") %>%
    str_replace_all("haemato", "hemato") %>%
    str_replace_all("oedema", "edema") %>%
    str_replace_all("oesophag", "esophag") %>%
    str_replace_all("anaemia", "anemia") %>%
    str_replace_all("paediatr", "pediatr")
}



### OUTPUT UTILITIES
# Generates country-specific approval template for manual curation
generate_approval_template <- function(country_drug_ids, country_name, output_file) {
  template <- orphan_variants_long %>%
    group_by(drug_id) %>%
    summarise(canonical_name = first(drug_variant), .groups = "drop") %>%
    arrange(canonical_name) %>%
    mutate(
      approved = if_else(drug_id %in% country_drug_ids, "Y", ""),
      source   = if_else(drug_id %in% country_drug_ids, "auto", "")
    )
  
  write_csv(template, output_file)
  cat("✓", country_name, ":", output_file, "\n")
  cat("   Pre-filled Y (auto-match):", sum(template$approved == "Y"), "\n")
  cat("   Still to review:", sum(template$approved == ""), "\n\n")
}

# Loads manually approved drug list
load_approval <- function(file) {
  read_csv2(file, show_col_types = FALSE) %>%
    filter(approved == "Y") %>%
    pull(drug_id)
}