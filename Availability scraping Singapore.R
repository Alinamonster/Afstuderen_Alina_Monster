### Configuration
URL     <- "https://www.moh.gov.sg/managing-expenses/schemes-and-subsidies/list-of-subsidised-drugs/"
OUT_DIR <- "moh_subsidised_drugs"
dir.create(OUT_DIR, showWarnings = FALSE)

### get page 
html <- request(URL) |>
  req_headers("User-Agent" = "Mozilla/5.0") |>
  req_perform() |>
  resp_body_html()

scripts <- html |> html_elements("script") |> html_text()
blob    <- scripts[[which.max(nchar(scripts))]]   # grootste script = de data
message("Found: ", nchar(blob), " signs")

# Escapes (\", \n, \u0026 etc.) have to be omgezet to real tekens
inner <- str_match(blob, 'self\\.__next_f\\.push\\(\\[\\d+,"(.*)"\\]\\)')[,2]

# parse as JSON string
unescaped <- fromJSON(paste0('"', inner, '"'))
message("Unescaped lengte: ", nchar(unescaped))

### Extract rows
# Each row has pattern: {"row":["...","...","...","...","..."],"key":"..."} so we
# use that 
row_pattern <- '\\{"row":(\\[(?:"(?:[^"\\\\]|\\\\.)*",?){5}\\]),"key":'
matches     <- str_match_all(unescaped, row_pattern)[[1]]

message("Aantal rijen gevonden: ", nrow(matches))

### Parse each array as JSON string
rows <- lapply(matches[, 2], function(x) {
  tryCatch(fromJSON(x), error = function(e) rep(NA_character_, 5))
})

drugs <- do.call(rbind, rows) |>
  as.data.frame(stringsAsFactors = FALSE) |>
  as_tibble()

### Give column names
names(drugs) <- c("active_ingredient", "dosage_form", "strength",
                  "subsidy_class", "clinical_indication")

### Clean whitespace
drugs <- drugs |>
  mutate(
    active_ingredient   = trimws(active_ingredient),
    dosage_form         = trimws(dosage_form),
    strength            = trimws(gsub("\\s*\\n\\s*", " ", strength)),
    subsidy_class       = trimws(subsidy_class),
    clinical_indication = trimws(clinical_indication)
  )

message("Totaal: ", nrow(drugs), " geneesmiddelen")

### Save as csv. That way i can edit later
write.csv(drugs, file.path(OUT_DIR, "Singapore_available_scraped.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

### Check if succesfull
head(drugs)
