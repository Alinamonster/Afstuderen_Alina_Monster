# =============================================================================
# Reshape data to long format
# =============================================================================
# The master dataset has one row per drug, with separate columns for each
# country's approval and availability status (wide format). For statistical
# testing we need one row per drug-country combination (long format), so that
# country can be used as a variable in the analysis.
# We also filter to approved == TRUE here because availability is only
# meaningful for drugs that are actually approved in that country

df_long_avail <- master %>%
  select(drug_id,
         approved_nl, approved_canada, approved_singapore, approved_brazil, approved_sa,
         available_nl, available_canada, available_singapore, available_brazil, available_sa) %>%
  pivot_longer(
    cols = -drug_id,
    # names_pattern tells pivot_longer how to split column names like
    # "approved_nl" into two parts: the variable type ("approved") and the
    # country ("nl"). The .value placeholder means those become separate columns.
    names_to = c(".value", "country"),
    names_pattern = "(approved|available)_(.+)"
  ) %>%
  filter(approved == TRUE) %>%
  # Add the therapeutic area for each drug so we can stratify tests by disease
  # category. drugs with multiple indications appear multiple times here
  left_join(drug_to_category %>% select(drug_id, category), by = "drug_id") %>%
  filter(!is.na(category))   # Drop rows where no category could be assigned

# Recode the country abbreviations to full names for readability in output
df_long_avail <- df_long_avail %>%
  mutate(country = recode(country,
                          "nl" = "Netherlands",
                          "canada" = "Canada",
                          "singapore" = "Singapore",
                          "brazil" = "Brazil",
                          "sa" = "South Africa"))


# =============================================================================
# Fisher's exact test — one per therapeutic area
# =============================================================================
# For each therapeutic area we build a 2x5 table:
#   rows    = available (TRUE / FALSE)
#   columns = the five countries
# Fisher's exact test then asks: is the distribution of available/unavailable
# drugs across countries more unequal than you'd expect by chance? A significant
# result means the country you're in is associated with whether a drug is
# available in that disease category.

fisher_per_area <- function(area_name, data) {
  area_data <- data %>% filter(category == area_name)
  
  contingency <- table(area_data$available, area_data$country)
  
  # Safety check: if all drugs in this area are either all available or all
  # unavailable, the table only has one row and the test cannot run.
  if (nrow(contingency) < 2 || ncol(contingency) < 2) {
    return(data.frame(
      therapeutic_area = area_name,
      n_drugs = sum(contingency),
      p_value = NA_real_,
      method = "insufficient variation"
    ))
  }
  
  test <- fisher.test(contingency, 
                      simulate.p.value = TRUE, 
                      B = 10000)
  
  data.frame(
    therapeutic_area = area_name,
    n_drugs = sum(contingency),
    p_value = test$p.value,
    method = "Fisher's exact (simulated)"
  )
}


# =============================================================================
# Run the test for all therapeutic areas with sufficient data
# =============================================================================
# We restrict to areas with at least 10 drug-country observations because
# smaller groups give unreliable p-values — even a single drug moving between
# available/unavailable can swing the result dramatically.

areas_to_test <- df_long_avail %>%
  count(category) %>%
  filter(n >= 10) %>%
  pull(category)

set.seed(42)  # Makes simulation reproducible
fisher_results <- bind_rows(lapply(areas_to_test, 
                                   fisher_per_area, 
                                   data = df_long_avail))


# =============================================================================
# Correct for multiple testing using Benjamini-Hochberg (BH)
# =============================================================================
# We ran 19 separate tests (one per therapeutic area). If we used a raw
# alpha of 0.05, we'd expect roughly 1 false positive just by chance.
# BH correction controls the False Discovery Rate: it adjusts p-values so
# that on average no more than 5% of significant findings are false positives.

fisher_results <- fisher_results %>%
  mutate(
    p_adjusted = p.adjust(p_value, method = "BH"),
    significant = p_adjusted < 0.05,
    p_value_fmt = format.pval(p_value, digits = 3),
    p_adj_fmt = format.pval(p_adjusted, digits = 3)
  ) %>%
  arrange(p_adjusted)

print(fisher_results)


# =============================================================================
# LOGISTIC REGRESSION
# =============================================================================
# Models the probability of availability as a
# function of both country and therapeutic area simultaneously.
#
# We keep only categories with at least 10 observations AND variation in the
# outcome

categories_to_keep <- df_long_avail %>%
  group_by(category) %>%
  summarise(
    n = n(),
    n_available = sum(available),
    n_unavailable = sum(!available)
  ) %>%
  filter(n >= 10, n_available > 0, n_unavailable > 0) %>%
  pull(category)

df_model_clean <- df_long_avail %>%
  filter(category %in% categories_to_keep) %>%
  mutate(
    country = factor(country, 
                     levels = c("Netherlands", "Canada", "Singapore", "Brazil", "South Africa")),
    category = factor(category),
    available = as.integer(available)
  )

# Set the reference category for therapeutic area to rare neoplastic disease
# because it is the largest group, giving stable estimates for the intercept
df_model_clean$category <- relevel(df_model_clean$category, ref = "Rare neoplastic disease")


# =============================================================================
# Fit the additive logistic regression model
# =============================================================================
# We assume the country effect is the same size across all therapeutic areas
model_clean <- glm(available ~ country + category, 
                   data = df_model_clean, 
                   family = binomial(link = "logit"))


# =============================================================================
# Estimated marginal means (EMMs) per country
# =============================================================================
# Raw model coefficients are on the log-odds scale and hard to interpret.
# emmeans converts them to predicted probabilities, averaged equally across
# all therapeutic areas — so every disease category gets equal weight
# regardless of how many drugs it contains.

emm_country <- emmeans(model_clean, ~ country, type = "response")
print(emm_country)


# =============================================================================
# Pairwise country comparisons
# =============================================================================
# We compare all 10 country pairs to see which differences are statistically
# significant. The output is in odds ratios (OR): an OR of 3 for
# example means a drug has 3x higher odds of being available in
# country A than in country B, after adjusting for therapeutic area.
#
# Tukey correction adjusts the p-values for the fact that we're making
# 10 comparisons simultaneously — similar in purpose to BH above, but the
# standard method for pairwise comparisons from emmeans.

pairwise_country <- pairs(emm_country, adjust = "tukey")
print(pairwise_country)

pairwise_ci <- confint(pairwise_country)
print(pairwise_ci)

# Combine results and confidence intervals into one clean table
pairs_df <- as.data.frame(pairwise_country)   
ci_df    <- as.data.frame(pairwise_ci)       

pairwise_table <- pairs_df %>%
  left_join(ci_df %>% select(contrast, asymp.LCL, asymp.UCL), by = "contrast") %>%
  mutate(
    OR = round(odds.ratio, 2),
    CI = paste0(round(asymp.LCL, 2), "–", round(asymp.UCL, 2)),
    p_value_fmt = format.pval(p.value, digits = 3),
    significant = p.value < 0.05
  ) %>%
  arrange(p.value) %>%
  select(contrast, OR, CI, p_value_fmt, significant)

print(pairwise_table)


# =============================================================================
# Test whether the country effect varies by therapeutic area
# =============================================================================
# The Likelihood Ratio Test (LRT) compares the two models by checking whether
# the interaction model fits the data significantly better. A significant
# result (p < 0.05) means the country effect is NOT uniform. i.e., the gap
# between countries is larger in some therapeutic areas than others.

model_interaction <- glm(available ~ country * category, 
                         data = df_model_clean, 
                         family = binomial(link = "logit"))

anova_test <- anova(model_clean, model_interaction, test = "LRT")
print(anova_test)