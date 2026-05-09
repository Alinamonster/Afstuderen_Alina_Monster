# load all countries approvals
NL_drug_ids     <- load_approval("Netherlands_approval_check.csv")
Canada_drug_ids <- load_approval("Canada_approval_check.csv")
Sing_drug_ids   <- load_approval("Singapore_approval_check.csv")
Brazil_drug_ids <- load_approval("Brazil_approval_check.csv")
SA_drug_ids     <- load_approval("SA_approval_check.csv")

# get list length per country 
cat("  Netherlands: ", length(NL_drug_ids),     "\n")
cat("  Canada:      ", length(Canada_drug_ids), "\n")
cat("  Singapore:   ", length(Sing_drug_ids),   "\n")
cat("  Brazil:      ", length(Brazil_drug_ids), "\n")
cat("  South Africa:", length(SA_drug_ids),     "\n")

### create list of all countries with ids for overlap analys
countries <- list(
  NL = NL_drug_ids,
  CA = Canada_drug_ids,
  SG = Sing_drug_ids,
  BR = Brazil_drug_ids,
  SA = SA_drug_ids
)

### Pairwise overlap 
country_names <- names(countries)
pairs <- combn(country_names, 2, simplify = FALSE)
pairwise_results <- data.frame(
  Country_A = character(), Country_B = character(),
  Shared = integer(), Union = integer(), Overlap_pct = numeric()
)
for (pair in pairs) {
  a <- countries[[pair[1]]]
  b <- countries[[pair[2]]]
  shared  <- length(intersect(a, b))
  union_n <- length(union(a, b))
  oi      <- round(100 * shared / union_n, 1)
  pairwise_results <- rbind(pairwise_results, data.frame(
    Country_A = pair[1], Country_B = pair[2],
    Shared = shared, Union = union_n, Overlap_pct = oi
  ))
  cat(sprintf("%s & %s: %d shared / %d union = %.1f%%\n",
              pair[1], pair[2], shared, union_n, oi))
}

### 5-country overlap
all_five  <- Reduce(intersect, countries)
all_union <- Reduce(union, countries)
overlap_all5 <- round(100 * length(all_five) / length(all_union), 1)

cat("Drugs approved in all 5:", length(all_five), "\n")
cat("Total unique drugs (union):", length(all_union), "\n")
cat("5-country overlap index:", overlap_all5, "%\n")

### Country ranking for approved drugs
cat("\n=== COUNTRY RANKING (by n approved) ===\n")
ranking <- data.frame(
  Country = country_names,
  N_approved = sapply(countries, length)
) %>% arrange(desc(N_approved))
print(ranking)