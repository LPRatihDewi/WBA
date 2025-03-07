#######NOTE: This script is prepared for extracting FAB companies' list of operation countries
# as part of FAB company footprint data collection.
#
# What the script do:  1) Extracting content from webpage or pdf documents
#                      2) Create an API connection between OpenAI and R to extract ChatGPT responses
#                      to the prompt of asking the GPT to list country of operation of FAB companies
#
# Please ensure you have created an OpenAI account and generate OpenAI API from the account. The back-end tools used in this script is paid per tokens used, 
# so please make sure you have enough credits for it. For reference, the exercise below cost USD 0.03 
# 
# Created by: Ratih Dewi
# Last modified: 07 March 2025
#
# NEXT STEP: Create a loop for batch processing

#######################################################################################
## Testing 1: Walmart webpage => Good, the webpage structure allow us to directly extract the ISO CODE of their locations, no OpenAI needed 
# Load required libraries
library(rvest)
library(dplyr)
library(stringr)
library(countrycode)
library(tidyverse)

# Define the URL
url <- "https://corporate.walmart.com/about/location-facts"

# Read the webpage
webpage <- read_html(url)

# Extract the <select> element with ID 'location-select'
location_options <- webpage %>%
  html_nodes("#location-select option") %>% # Selects all <option> elements within the <select>
  html_text() # Extracts the text content of each option

# Extract the 'value' attribute of each option
location_values <- webpage %>%
  html_nodes("#location-select option") %>%
  html_attr("value") # Gets the 'value' attributes

# Combine the text and values into a data frame
locations_df <- data.frame(
  name = location_options,
  value = location_values,
  stringsAsFactors = FALSE
)

# Clean the data: remove extra spaces (from "&nbsp;")
locations_df$name <- str_trim(gsub("\\s+", " ", locations_df$name))

# Add ISO-3 country codes
locations_df$iso_code <- countrycode(locations_df$name, origin = "country.name", destination = "iso3c", warn=FALSE)
locations_df_clean <- locations_df %>% 
  filter(!is.na(iso_code),
         !grepl("united-states/", value)) %>% 
  mutate(WBA_ID = "PT_01905",
         company_name = "Walmart",
         source = url) %>% 
  select(!value)

# View the extracted data
view(locations_df_clean)


#######################################################################################
## Testing 2: Nueva Percanova webpage => Good, we need to pinpoint the html_nodes, this give us response that we want in variable with values.

## Step 1: Web Scrapping

# Load required libraries
library(rvest)
library(dplyr)
library(stringr)

# Define the URLs for scraping
urls <- c(
  "https://www.nuevapescanova.com/en/what-we-do-2/we-fish/",
  "https://www.nuevapescanova.com/en/what-we-do-2/we-farm-2/",
  "https://www.nuevapescanova.com/en/what-we-do-2/we-process/",
  "https://www.nuevapescanova.com/en/what-we-do-2/we-sell/"
)

# Function to scrape text from <p> tags
scrape_paragraphs <- function(url) {
  webpage <- read_html(url)
  paragraphs <- webpage %>%
    html_nodes("p") %>%     # Select <p> tags
    html_text() %>%         # Extract text
    str_squish()            # Clean whitespace
  return(paste(paragraphs, collapse = " ")) # Combine all paragraphs into one string
}

# Scrape text from all URLs
scraped_texts <- sapply(urls, scrape_paragraphs)

# Combine all scraped text into one
combined_text <- paste(scraped_texts, collapse = " ")
print(substr(combined_text, 1, 1000)) # Print first 1000 characters for verification


## Step 2: Embedding Implementation
# Load required libraries
library(httr)
library(jsonlite)

# Load the API key from the environment
api_key <- Sys.getenv("OPENAI_API_KEY")

# Verify that the key is loaded
if (nzchar(api_key)) {
  print("API key loaded successfully.")
} else {
  stop("API key not found. Check your .Renviron file.")
}

# Function to generate embeddings
generate_embeddings <- function(text) {
  response <- POST(
    url = "https://api.openai.com/v1/embeddings",
    add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))),
    body = list(
      model = "text-embedding-ada-002",
      input = text
    ),
    encode = "json"
  )
  
  # Parse and inspect the response
  content <- content(response, as = "parsed", simplifyVector = FALSE)
  
  # Handle errors in response
  if (!is.null(content$error)) {
    stop(content$error$message)
  }
  
  # Extract embedding vector
  if (!is.null(content$data[[1]]$embedding)) {
    return(as.numeric(content$data[[1]]$embedding))
  } else {
    stop("Embedding not found in response.")
  }
}


# Split text into smaller chunks (if needed) for embedding, maximum limit is 2000 characters
text_chunks <- strsplit(combined_text, "(?<=.{2000})", perl = TRUE)[[1]]

# Generate embeddings for all text chunks
embeddings <- lapply(text_chunks, generate_embeddings)

# Combine the numeric vectors into a matrix
embedding_vectors <- do.call(rbind, embeddings)


## Step 3: Query and Retrieve

# Define the query (adjust according to your Transformation's needs)
query_text <- "List the countries where the company has production or processing facilities (such as factories, plants, fishing operations, or aquaculture farms), offices (including subsidiaries and branches), or retail locations (such as supermarkets, grocery stores, minimarkets, or restaurants)."

# Generate embedding for the query
query_embedding <- generate_embeddings(query_text)  # Directly returns the numeric vector

# Function to calculate cosine similarity
cosine_similarity <- function(a, b) {
  sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
}

# Compare query embedding with text embeddings (row-wise comparison)
similarities <- apply(embedding_vectors, 1, function(row) cosine_similarity(query_embedding, row))

# Set a similarity threshold
similarity_threshold <- 0.70 # You can adjust this threshold, rule of thumb > 0.8 Highly similar, 0.5 - 0.8 Moderately similar, < 0.5 Weak or no similarity

# Get indices of text chunks with similarity above the threshold
matching_indices <- which(similarities >= similarity_threshold)

# Get the corresponding text chunks
matching_text_chunks <- text_chunks[matching_indices]

# Print the matching text chunks
print(matching_text_chunks)


## Step 4: Use Chat Completions to ask OpenAI complete the task

# Required libraries
library(httr)
library(jsonlite)

# Combine text chunks into a single input
input_text <- paste(matching_text_chunks, collapse = "\n\n")

# Define the prompt (adjust according to your Transformation's needs)
prompt <- paste0(
  "The following text provides information about a company's operations. Countries of operations means where a company has a fishing operation, production facilities, processing facilities, subsidiaries, sales offices, retail stores, or restaurants. Only respond with the list the countries of operations mentioned in ISO-3 format (e.g., USA for United States), and specify next to it whether in the location are production facilities, processing facilities, subsidiaries, sales offices, retail stores, or restaurants (e.g., USA - retail stores",
  input_text,
  sep = "\n\n"
)

# Print the prompt for verification
cat("Prompt:\n", prompt, "\n")

# Call the Chat Completions API
response <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  body = list(
    model = "gpt-4o",  # Specify the model directly
    messages = list(
      list(role = "system", content = "You are a research assistant helping extract countries of operations of companies."),
      list(role = "user", content = prompt)
    ),
    max_tokens = 1000, # you can adjust this value, GPT-4o can handle up to 128,000 tokens (input + output), max number of tokens per request is 16,384
    temperature = 0.1 # 0 - 0.3 is recommended for factual, structured, or predictable outputs.
  ),
  add_headers(
    Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
    `Content-Type` = "application/json"
  ),
  encode = "json"  # Automatically handles JSON encoding
)

# Inspect the raw response
raw_content <- content(response, as = "text", encoding = "UTF-8")
cat("Raw Response:\n", raw_content, "\n")

# Parse the response for analysis
parsed_content <- content(response, as = "parsed", simplifyVector = TRUE)

# Check for errors
if (!is.null(parsed_content$error)) {
  stop(parsed_content$error$message)
}

# Extract the assistant's response
assistant_response <- parsed_content$choices$message$content

# Print the final response
cat(assistant_response)


## Step 5: Process the assistant_response and create a dataframe from the response

library(dplyr)      
library(tidyr)      
library(tibble)     
library(stringr)    
library(purrr)     

# Extract structured results from the OpenAI response
results <- tibble(
  `WBA ID` = "PT_01314",
  Company = "Nueva Pescanova",
  assistant_response = assistant_response
)

final_df <- results %>%
  rowwise() %>%
  mutate(
    extracted_data = list(
      assistant_response %>%
        str_split("\n") %>%
        unlist() %>%
        # Filter only valid lines matching the pattern "- XXX - description"
        keep(~ str_detect(.x, "^\\-\\s[A-Z]{3}\\s\\-\\s")) %>%
        # Extract the country and business operations using regex
        map(~ {
          matches <- str_match(.x, "^\\-\\s([A-Z]{3})\\s\\-\\s(.+)$")
          if (!is.na(matches[1])) {
            tibble(
              country = matches[2],
              business_operations = matches[3]
            )
          } else {
            NULL
          }
        }) %>%
        compact() %>%
        bind_rows()
    )
  ) %>%
  unnest(extracted_data) %>%
  select(`WBA ID`, Company, country, business_operations) %>% 
  mutate(link = paste(unique(urls), collapse = "; "))


# View the resulting dataframe
view(final_df)


#######################################################################################
## Testing 3: Pick N Pay Annual Report 
## Testing note: We need to be mindful the input_text won't exceed tokens limit, we need to add tokens counter on matching_text
## splitting the chunks by 1,000 characters & setting higher similarity threshold can also help.

## Step 1: PDF Scrapping

# Load required libraries
library(pdftools)
library(tidyverse)
library(stringr)

# Define PDF path 
pdf_path <- "C:/Users/lprat/OneDrive - World Benchmarking Alliance/FAB/2024-2026 FAB/Data work/Raw/sources_repo/PT_01405_integrated-annual-report-2024-spread.pdf" # insert your file path
  
# Function to scrape PDF content
extract_pdf_content <- function(pdf_path) {
  # Error handling for file existence
  if (!file.exists(pdf_path)) {
    stop("PDF file does not exist at specified path")
  }
  
  tryCatch({
    # Read PDF and combine all pages
    content <- pdf_text(pdf_path)
    
    # Combine all pages into a single string
    full_text <- paste(content, collapse = " ")
    
    # Clean text (remove extra whitespace, special characters)
    clean_text <- gsub("\\s+", " ", full_text) %>%
      trimws()
    
    return(clean_text)
  }, error = function(e) {
    stop(sprintf("Error reading PDF: %s", e$message))
  })
}

# Extract the content
extracted_text <- extract_pdf_content(pdf_path)

# Print first 500 characters to verify extraction
cat("\nFirst 500 characters of extracted text:\n")
cat(substr(extracted_text, 1, 500))

# Get basic statistics about the extracted text
cat("\n\nText statistics:")
cat("\nTotal characters:", nchar(extracted_text))
cat("\nTotal words:", length(strsplit(extracted_text, " ")[[1]]))


## Step 2: Embedding Implementation
# Load required libraries
library(httr)
library(jsonlite)

# Load the API key from the environment
api_key <- Sys.getenv("OPENAI_API_KEY")

# Verify that the key is loaded
if (nzchar(api_key)) {
  print("API key loaded successfully.")
} else {
  stop("API key not found. Check your .Renviron file.")
}

# Function to generate embeddings
generate_embeddings <- function(text) {
  response <- POST(
    url = "https://api.openai.com/v1/embeddings",
    add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))),
    body = list(
      model = "text-embedding-ada-002",
      input = text
    ),
    encode = "json"
  )
  
  # Parse and inspect the response
  content <- content(response, as = "parsed", simplifyVector = FALSE)
  
  # Handle errors in response
  if (!is.null(content$error)) {
    stop(content$error$message)
  }
  
  # Extract embedding vector
  if (!is.null(content$data[[1]]$embedding)) {
    return(as.numeric(content$data[[1]]$embedding))
  } else {
    stop("Embedding not found in response.")
  }
}


# Split text into smaller chunks (if needed) for embedding
text_chunks_pdf <- strsplit(extracted_text, "(?<=.{1000})", perl = TRUE)[[1]]

# Generate embeddings for all text chunks
embeddings_pdf <- lapply(text_chunks_pdf, generate_embeddings)

# Combine the numeric vectors into a matrix
embedding_vectors_pdf <- do.call(rbind, embeddings_pdf)


## Step 3: Query and Retrieve

# Define the query (adjust according to your Transformation's needs)
query_text <- "List the countries where the company has production or processing facilities (such as factories, plants, fishing operations, or aquaculture farms), offices (including subsidiaries and branches), or retail locations (such as supermarkets, grocery stores, minimarkets, or restaurants)."

# Generate embedding for the query
query_embedding_pdf <- generate_embeddings(query_text)  # Directly returns the numeric vector

# Function to calculate cosine similarity
cosine_similarity <- function(a, b) {
  sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
}

# Compare query embedding with text embeddings (row-wise comparison)
similarities_pdf <- apply(embedding_vectors_pdf, 1, function(row) cosine_similarity(query_embedding_pdf, row))

# Set a similarity threshold (this threshold also helps you stay within tokens limit -especially for PDF file)
similarity_threshold_pdf <- 0.78 # You can adjust this threshold, rule of thumb > 0.8 Highly similar, 0.5 - 0.8 Moderately similar, < 0.5 Weak or no similarity

# Get indices of text chunks with similarity above the threshold
matching_indices_pdf <- which(similarities_pdf >= similarity_threshold_pdf)

# Get the corresponding text chunks
matching_text_chunks <- text_chunks_pdf[matching_indices_pdf]

# Print the matching text chunks
print(matching_text_chunks)


## Step 4: Use Chat Completions to ask OpenAI complete the task

# Required libraries
library(httr)
library(jsonlite)

# Combine text chunks into a single input
input_text <- paste(matching_text_chunks, collapse = "\n\n")

# Define the prompt (adjust according to your Transformation's needs)
prompt <- paste0(
  "The following text provides information about a company's operations. Countries of operations means where a company has a production facilities, processing facilities, subsidiaries, sales offices, retail stores, or restaurants. Only respond with the list the countries of operations mentioned in ISO-3 format (e.g., USA for United States), and specify next to it whether in the location are production facilities, processing facilities, subsidiaries, sales offices, retail stores, or restaurants (e.g., USA - retail stores",
  input_text,
  sep = "\n\n"
)

# Print the prompt for verification
cat("Prompt:\n", prompt, "\n")

# Call the Chat Completions API
response <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  body = list(
    model = "gpt-4o",  # Specify the model directly
    messages = list(
      list(role = "system", content = "You are a research assistant helping extract countries of operations of companies."),
      list(role = "user", content = prompt)
    ),
    max_tokens = 1000, # you can adjust this value, GPT-4o can handle up to 128,000 tokens (input + output), max number of tokens per request is 16,384
    temperature = 0.1 # 0 - 0.3 is recommended for factual, structured, or predictable outputs.
  ),
  add_headers(
    Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
    `Content-Type` = "application/json"
  ),
  encode = "json"  # Automatically handles JSON encoding
)

# Inspect the raw response
raw_content <- content(response, as = "text", encoding = "UTF-8")
cat("Raw Response:\n", raw_content, "\n")

# Parse the response for analysis
parsed_content <- content(response, as = "parsed", simplifyVector = TRUE)

# Check for errors
if (!is.null(parsed_content$error)) {
  stop(parsed_content$error$message)
}

# Extract the assistant's response
assistant_response <- parsed_content$choices$message$content

# Print the final response
cat(assistant_response)


## Step 5: Process the assistant_response and create a dataframe from the response

library(dplyr)      
library(tidyr)      
library(tibble)     
library(stringr)    
library(purrr)     

# Extract structured results from the OpenAI response
results <- tibble(
  `WBA ID` = "PT_01404",
  Company = "Pick N Pay Group",
  assistant_response = assistant_response
)

final_df_pdf <- results %>%
  rowwise() %>%
  mutate(
    extracted_data = list(
      assistant_response %>%
        str_split("\n") %>%
        unlist() %>%
        # Filter only valid lines matching the pattern "- XXX - description"
        keep(~ str_detect(.x, "^\\-\\s[A-Z]{3}\\s\\-\\s")) %>%
        # Extract the country and business operations using regex
        map(~ {
          matches <- str_match(.x, "^\\-\\s([A-Z]{3})\\s\\-\\s(.+)$")
          if (!is.na(matches[1])) {
            tibble(
              country = matches[2],
              business_operations = matches[3]
            )
          } else {
            NULL
          }
        }) %>%
        compact() %>%
        bind_rows()
    )
  ) %>%
  unnest(extracted_data) %>%
  select(`WBA ID`, Company, country, business_operations) %>% 
  mutate(source = "PT_01405_integrated-annual-report-2024-spread.pdf")


# View the resulting dataframe
view(final_df_pdf)

#######################################################################################
## Testing 4: Ajinomoto => the webpage structure allow us to directly extract the ISO CODE of their locations, no OpenAI needed

library(rvest)
library(stringr)
library(dplyr)
library(countrycode)

# Define the URL
url <- "https://www.ajinomoto.com/aboutus/global_network"

# Function to scrape both country and business type accurately
scrape_ajinomoto <- function(url) {
  # Read the webpage
  webpage <- tryCatch(read_html(url), error = function(e) {
    message("❌ Failed to load page: ", e$message)
    return(NULL)
  })
  
  if (is.null(webpage)) return(data.frame(country = character(), business = character())) # Return empty if error
  
  # Extract all company blocks
  company_blocks <- webpage %>%
    html_nodes(".glnw-company")
  
  # Initialize empty dataframe
  data <- data.frame(country = character(), business = character(), stringsAsFactors = FALSE)
  
  # Loop through each company block to extract country and business
  for (block in company_blocks) {
    # Extract country for this block
    country <- block %>%
      html_node(".glnw-company__country") %>%
      html_text(trim = TRUE)
    
    # Extract all business types under this country
    business_types <- block %>%
      html_nodes(".glnw-company__business") %>%
      html_attr("alt")
    
    # Create a dataframe for this block
    block_data <- data.frame(country = rep(country, length(business_types)),
                             business = business_types,
                             stringsAsFactors = FALSE)
    
    # Bind to the main dataframe
    data <- bind_rows(data, block_data)
  }
  
  return(data)
}

# Run the scraper
ajinomoto_data <- scrape_ajinomoto(url)

# View the result
print(ajinomoto_data)

# Add ISO-3 country codes
ajinomoto_data$iso_code <- countrycode(ajinomoto_data$country, origin = "country.name", destination = "iso3c", warn=FALSE)
location_df <- ajinomoto_data %>% 
  filter(!is.na(iso_code)) %>% 
  mutate(WBA_ID = "PT_00057",
         company_name = "Ajinomoto",
         source = url)

location_grouped <- location_df %>%
  group_by(WBA_ID, company_name, source, country, iso_code) %>%
  summarise(business = paste(unique(business), collapse = "; "),
            .groups = 'drop')


