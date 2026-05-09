
## Done with Claude for quick overview of all previousely calculated numbers
### ── ORPHAN LIST COMPOSITION ────────────────────────────────────────────────

cat("── ORPHAN LIST COMPOSITION ──\n")
cat("  EMA orphans (raw):                ", nrow(EMA_orphan), "\n")
cat("  FDA orphans (raw):                ", nrow(FDA_orphan), "\n")
cat("  Combined raw orphan list:         ", length(unique(c(EMA_orphan$INGREDIENT, FDA_orphan$INGREDIENT))), "\n")
cat("  Small molecules (after PubChem):  ", nrow(pubchem_results_final %>% filter(is_small_molecule_final)), "\n")
cat("  Variants in lookup:               ", nrow(orphan_variants_long), "\n")
cat("  Unique canonical drugs (drug_ids):", n_distinct(orphan_variants_long$drug_id), "\n\n")

### ── APPROVAL PER COUNTRY ───────────────────────────────────────────────────

cat("── APPROVAL PER COUNTRY ──\n")
cat(sprintf("  %-12s %s\n", "Country", "N approved"))
cat(sprintf("  %-12s %d\n", "NL",        sum(master$approved_nl)))
cat(sprintf("  %-12s %d\n", "Canada",    sum(master$approved_canada)))
cat(sprintf("  %-12s %d\n", "Singapore", sum(master$approved_singapore)))
cat(sprintf("  %-12s %d\n", "Brazil",    sum(master$approved_brazil)))
cat(sprintf("  %-12s %d\n\n", "SA",      sum(master$approved_sa)))

### ── AVAILABILITY PER COUNTRY ───────────────────────────────────────────────

cat("── AVAILABILITY PER COUNTRY ──\n")
cat(sprintf("  %-12s %-12s %-13s %s\n", "Country", "N approved", "N available", "Availability %"))
for (i in seq_len(nrow(availability_index))) {
  row <- availability_index[i, ]
  cat(sprintf("  %-12s %-12d %-13d %.1f%%\n",
              row$Country, row$N_approved, row$N_available, row$Availability_pct))
}
cat("\n")

### ── COUNTRY RANKING (DUAL CRITERIA) ────────────────────────────────────────

cat("── COUNTRY RANKING (DUAL CRITERIA) ──\n")
cat(sprintf("  %-12s %-15s %s\n", "Country", "Rank approval", "Rank availability"))
for (i in seq_len(nrow(ranking_full))) {
  row <- ranking_full[i, ]
  cat(sprintf("  %-12s %-15d %d\n",
              row$Country, row$Rank_approval, row$Rank_availability))
}
cat("\n")

### ── PAIRWISE APPROVAL OVERLAP ──────────────────────────────────────────────

cat("── PAIRWISE APPROVAL OVERLAP (%) ──\n")
for (i in seq_len(nrow(pairwise_results))) {
  row <- pairwise_results[i, ]
  cat(sprintf("  %s & %s: %d shared / %d union = %.1f%%\n",
              row$Country_A, row$Country_B, row$Shared, row$Union, row$Overlap_pct))
}
cat(sprintf("\n  All 5 countries: %d shared / %d union = %.1f%%\n\n",
            length(all_five), length(all_union), overlap_all5))

### ── PAIRWISE AVAILABILITY OVERLAP ──────────────────────────────────────────

cat("── PAIRWISE AVAILABILITY OVERLAP (%) ──\n")
for (i in seq_len(nrow(availability_overlap))) {
  row <- availability_overlap[i, ]
  cat(sprintf("  %s & %s: %d shared / %d union = %.1f%%\n",
              row$Country_A, row$Country_B, row$Shared, row$Union, row$Overlap_pct))
}
cat(sprintf("\n  All 5 countries: %d shared / %d union = %.1f%%\n\n",
            length(avail_all5), length(avail_union), avail_oi_all))

### ── UNIVERSAL DRUGS (AVAILABLE IN ALL 5) ───────────────────────────────────

universal_drugs <- master %>%
  filter(available_nl & available_canada & available_singapore & 
           available_brazil & available_sa) %>%
  pull(canonical_name)

cat("── UNIVERSALLY AVAILABLE DRUGS (in all 5 countries) ──\n")
cat("  Total:", length(universal_drugs), "\n")
for (drug in universal_drugs) cat("  -", drug, "\n")
cat("\n")

### ── APPROVAL-AVAILABILITY GAP ──────────────────────────────────────────────

cat("── APPROVAL-AVAILABILITY GAP ──\n")
cat("  Drugs approved in ≥2 countries:", nrow(gap_analysis), "\n")
cat("  Gap distribution (n_approved - n_available):\n")
for (i in seq_len(nrow(gap_table))) {
  row <- gap_table[i, ]
  cat(sprintf("    gap = %2d:  %d drugs\n", row$gap, row$n))
}
cat("\n  Top 10 drugs with largest gap:\n")
for (i in seq_len(nrow(gap_top))) {
  row <- gap_top[i, ]
  cat(sprintf("    %-25s approved in %d, available in %d (gap=%d)\n",
              row$canonical_name, row$n_approved, row$n_available, row$gap))
}
cat("\n")


### ── Summary table: approval and availability status ───────────────────
summary_table <- master %>%
  mutate(
    n_approved  = approved_nl + approved_canada + approved_singapore +
      approved_brazil + approved_sa,
    n_available = available_nl + available_canada + available_singapore +
      available_brazil + available_sa,
    gap = n_approved - n_available,
    status = case_when(
      n_approved == 0                        ~ "Not approved in any country",
      n_approved == 1                        ~ "Approved in 1 country only",
      n_approved >= 2 & n_available == 5     ~ "Universally available (all 5)",
      n_approved == 5 & n_available < 5      ~ "Universally approved, not fully available",
      n_approved >= 2 & n_available == 0     ~ "Approved ≥2 countries, unavailable everywhere",
      n_approved >= 2 & gap == 0             ~ "Approved ≥2, fully available where approved",
      n_approved >= 2 & gap >= 1             ~ "Approved ≥2, partial availability gap",
      TRUE                                   ~ "Other"
    )
  ) %>%
  count(status, name = "N drugs") %>%
  mutate(`% of 439` = round(100 * `N drugs` / 439, 1)) %>%
  arrange(desc(`N drugs`))

print(summary_table)