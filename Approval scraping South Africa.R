### Configuration
# Base URL and output settings
BASE      <- "https://medapps.sahpra.org.za:6006"
PAGE_SIZE <- 500
OUT_DIR   <- "sahpra_medicines"

# Create output directory if it does not exist
dir.create(OUT_DIR, showWarnings = FALSE)

# Authentication values copied from browser session. They might not stay the same forever 
# so when repeating this code, you may have to look this up again and paste new 
# values

# Session cookie
MY_COOKIE <- "_ga=GA1.3.1165790851.1775502279; _ga_HRP4C03DT2=GS2.1.s1775502279$o1$g1$t1775502334$j5$l0$h0; .AspNetCore.Antiforgery.9MJnuRBifPI=CfDJ8PBMnb_IzHFAoKk6eED5uZpnJaSW6yfDAkN3e4NdOyqoN3O_jEtU5FKlIcERuLy4NyrAGd8Lm_YGOGiqtvZQpVH_mDinr-4thQG9AoC4IomzloHWcs5PJFAQq6fDls_Xb8pgw2yCkJT3R-DuNB5WQVU"# Bijvoorbeeld: "_ga=GA1.3.116579...; .AspNetCore.Antiforgery.9MJ=CfDJ8..."

MY_TOKEN <- "CfDJ8PBMnb_IzHFAoKk6eED5uZpnJaSW6yfDAkN3e4NdOyqoN3O_jEtU5FKlIcERuLy4NyrAGd8Lm_YGOGiqtvZQpVH_mDinr-4thQG9AoC4IomzloHWcs5PJFAQq6fDls_Xb8pgw2yCkJT3R-DuNB5WQVU"# Bijvoorbeeld: "CfDJ8PBMnb_IzHFAoKk6eED5uZpn..."


# Construct POST request body
make_body <- function(start, length, token) {
  list(
    "columns[0][data]"          = "applicantName",
    "columns[0][name]"          = "applicantName",
    "columns[0][searchable]"    = "true",
    "columns[0][orderable]"     = "true",
    "columns[0][search][value]" = "",
    "columns[0][search][regex]" = "false",
    "columns[1][data]"          = "productName",
    "columns[1][name]"          = "productName",
    "columns[1][searchable]"    = "true",
    "columns[1][orderable]"     = "true",
    "columns[1][search][value]" = "",
    "columns[1][search][regex]" = "false",
    "columns[2][data]"          = "api",
    "columns[2][name]"          = "api",
    "columns[2][searchable]"    = "true",
    "columns[2][orderable]"     = "true",
    "columns[2][search][value]" = "",
    "columns[2][search][regex]" = "false",
    "columns[3][data]"          = "licence_no",
    "columns[3][name]"          = "licence_no",
    "columns[3][searchable]"    = "true",
    "columns[3][orderable]"     = "true",
    "columns[3][search][value]" = "",
    "columns[3][search][regex]" = "false",
    "columns[4][data]"          = "application_no",
    "columns[4][name]"          = "application_no",
    "columns[4][searchable]"    = "true",
    "columns[4][orderable]"     = "true",
    "columns[4][search][value]" = "",
    "columns[4][search][regex]" = "false",
    "columns[5][data]"          = "reg_date",
    "columns[5][name]"          = "reg_date",
    "columns[5][searchable]"    = "true",
    "columns[5][orderable]"     = "true",
    "columns[5][search][value]" = "",
    "columns[5][search][regex]" = "false",
    "columns[6][data]"          = "status",
    "columns[6][name]"          = "status",
    "columns[6][searchable]"    = "true",
    "columns[6][orderable]"     = "true",
    "columns[6][search][value]" = "",
    "columns[6][search][regex]" = "false",
    "columns[7][data]"          = "secureId",
    "columns[7][name]"          = "",
    "columns[7][searchable]"    = "true",
    "columns[7][orderable]"     = "true",
    "columns[7][search][value]" = "",
    "columns[7][search][regex]" = "false",
    "order[0][column]"          = "0",
    "order[0][dir]"             = "asc",
    "start"                     = as.character(start),
    "length"                    = as.character(length),
    "search[value]"             = "",
    "search[regex]"             = "false",
    "__RequestVerificationToken" = token
  )
}

# Function to get one page of results
fetch_page <- function(start) {
  request(paste0(BASE, "/Home/getData")) |>
    req_method("POST") |>
    # Disable SSL verification if needed
    req_options(ssl_verifypeer = FALSE) |>
    
    # Add request headers
    req_headers(
      "Accept"       = "application/json, text/javascript, */*; q=0.01",
      "Content-Type" = "application/x-www-form-urlencoded; charset=UTF-8",
      "Cookie"       = MY_COOKIE,
      "Origin"       = BASE,
      "Referer"      = BASE,
      "User-Agent"   = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
    ) |>
    
    # Attach POST form data
    req_body_form(!!!make_body(start, PAGE_SIZE, MY_TOKEN)) |>
    
    # Retry failed requests and throttle request rate
    req_retry(max_tries = 5, backoff = ~ 20) |>
    req_throttle(rate = 1) |>
    
    # Prevent automatic stopping on HTTP errors
    req_error(is_error = \(r) FALSE) |>
    
    # Execute request and parse JSON response
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
}

### determine total numebrs of records
first <- fetch_page(start = 0)
# Extract total number of medicines
total <- first$recordsTotal
message("Total: ", total, " drugs")
message("Pages: ", ceiling(total / PAGE_SIZE))


### Download all pages
starts <- seq(0, total - 1, by = PAGE_SIZE)

for (i in seq_along(starts)) {
  s          <- starts[i]
  batch_file <- sprintf("%s/batch_%06d.rds", OUT_DIR, s)
  
  if (file.exists(batch_file)) {
    message(sprintf("[%d/%d] Skipped: start=%d", i, length(starts), s))
    next
  }
  
  message(sprintf("[%d/%d] Retrieving starts=%d...", i, length(starts), s))
  
  result <- tryCatch(
    fetch_page(start = s),
    error = function(e) { message("  Error: ", e$message); NULL }
  )
  
  # Skip empty responses
  if (is.null(result) || is.null(result$data)) {
    message("  Empty response, skip")
    next
  }
  
  flat <- as_tibble(result$data)
  saveRDS(flat, batch_file)
  message(sprintf("  %d rows saved", nrow(flat)))
  
  # Pause between requests to give system enough time for downaloading
  Sys.sleep(1)
}

# Merge all downloaded batches
batch_files   <- list.files(OUT_DIR, pattern = "^batch_.*\\.rds$", full.names = TRUE)
all_medicines <- do.call(rbind, lapply(batch_files, readRDS))
message("Total: ", nrow(all_medicines), " rows")

saveRDS(all_medicines,   file.path(OUT_DIR, "sahpra_all_medicines.rds"))
write.csv(all_medicines, file.path(OUT_DIR, "sahpra_all_medicines.csv"), row.names = FALSE)
