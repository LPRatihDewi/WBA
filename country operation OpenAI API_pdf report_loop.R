#######NOTE: This script is prepared for extracting FAB companies' list of operation countries
# as part of FAB company footprint data collection.
#
# This script processes multiple PDF files in a directory to extract countries of operations
# What the script do:  1) Extracting content from pdf documents
#                      2) Create an API connection between OpenAI and R to extract ChatGPT responses
#                      to the prompt of asking the GPT to list country of operation of FAB companies
#
# Please ensure you have created an OpenAI account and generate OpenAI API from the account. The back-end tools used in this script is paid per tokens used, 
# so please make sure you have enough credits for it. For reference, the exercise below cost USD 0.03 
# 
# Created by: Ratih Dewi
# Last modified: 25 April 2025
#

#######################################################################################

## Batch Processing for Company Operations Extraction
## This script processes multiple PDF files in a directory to extract countries of operations

# Load required libraries
library(pdftools)
library(tidyverse)
library(stringr)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(countrycode)


## SET AIRTABLE API CONNECTION FOR SOURCE LIBRARY

# Load the API keys needed from the environment
api_key <- Sys.getenv("OPENAI_API_KEY")
at_api_key <- Sys.getenv("AT_API_KEY")

# Verify that the key is loaded
if (nzchar(api_key)) {
  print("OPENAI API key loaded successfully.")
} else {
  stop("OPENAI API key not found. Check your .Renviron file.")
}

if (nzchar(at_api_key)) {
  print("AIRTABLE API key loaded successfully.")
} else {
  stop("AIRTABLE API key not found. Check your .Renviron file.")
}

# Airtable info
base_id <- "appu5pLRjmq7PC4V3"
table_id <- "tbluKsP1itMozQmHr"
view_name <- "FAB 2025 Sources - Ratih copy"

# Airtable API endpoint
url <- paste0("https://api.airtable.com/v0/", base_id, "/", table_id)

# Fetch records from the specific view
offset <- NULL
repeat {
  query_params <- list(view = view_name)
  if (!is.null(offset)) query_params$offset <- offset
  
  response <- GET(url,
                  add_headers(Authorization = paste("Bearer", at_api_key)),
                  query = query_params)
  
  content_list <- content(response, as = "parsed", simplifyVector = TRUE)
  
  # Check for pagination
  if (is.null(content_list$offset)) break
  offset <- content_list$offset
}

# Convert all records into a dataframe with all fields
result_df <- data.frame(
  SourceID = content_list$records$fields$`Source ID`,
  Company = sapply(content_list$records$fields$`WBA Company Name`, function(x) {
    if(is.list(x) && length(x) > 0) {
      return(x[[1]])  # Extract first element of the list
    } else if(is.character(x) && length(x) > 0) {
      return(x[1])    # If it's already a character vector
    } else {
      return(NA)      # Handle empty or NULL cases
    }
  }),
  SourceFile = sapply(content_list$records$fields$`Source File`, function(x) if(is.data.frame(x)) x$url[1] else NA),
  SourceFileName = sapply(content_list$records$fields$`Source File`, function(x) if(is.data.frame(x)) x$filename[1] else NA),
  stringsAsFactors = FALSE
)

# Add debug information - print structure of first record's Source File
cat("Structure of first record's Source File field:\n")
print(str(result_df$SourceFile[1]))

# Remove duplicates and older document based on Source ID
filtered_df <- result_df %>%
  mutate(
    base_id = str_replace(SourceID, "-(\\d{4})-\\d+$", ""),
    year = as.numeric(str_extract(SourceID, "-(\\d{4})-") %>% str_replace_all("-", "")),
    version = as.numeric(str_extract(SourceID, "-\\d+$") %>% str_replace("-", "")),
    WBAID = str_sub(base_id, 1, 8)
  ) %>%
  group_by(base_id) %>%
  filter(year == max(year)) %>%
  filter(version == min(version)) %>%
  select(-base_id, -year, -version)

# Set your specific folder path  
current_date <- format(Sys.Date(), "%d%m%Y")
pdf_dir <- file.path("C:/Users/lprat/OneDrive - World Benchmarking Alliance/FAB/2024-2026 FAB/Data work/Raw/Source Library", current_date)
if (!dir.exists(pdf_dir)) {
  dir.create(pdf_dir, recursive = TRUE)
}
dir.create(pdf_dir, showWarnings = FALSE, recursive = TRUE)

# Download PDFs
walk2(filtered_df$SourceFile, filtered_df$SourceID, function(url, source_id) {
  if (!is.na(url)) {
    destfile <- file.path(pdf_dir, paste0(source_id, ".pdf"))
    download.file(url, destfile, mode = "wb")
    cat("Downloaded:", destfile, "\n")
  }
})


# List all PDF files in the directory
pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)

# Create an empty dataframe to store all results
all_results <- tibble()


## WRITE ALL FUNCTIONS NEEDED FOR THIS FLOW

# Function to extract PDF content
extract_pdf_content <- function(pdf_path) {
  # Error handling for file existence
  if (!file.exists(pdf_path)) {
    warning(paste("PDF file does not exist at specified path:", pdf_path))
    return(NULL)
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
    warning(sprintf("Error reading PDF %s: %s", pdf_path, e$message))
    return(NULL)
  })
}

# Function to split text into chunks at sentence boundaries
split_text_at_boundaries <- function(text, target_size = 2000) {
  # Split text into sentences (using periods, question marks, exclamation points as boundaries)
  sentences <- unlist(stringr::str_split(text, stringr::regex("(?<=[\\.\\?\\!])\\s+")))
  
  # Initialize variables
  chunks <- list()
  current_chunk <- ""
  
  for (sentence in sentences) {
    # If adding this sentence would exceed target size and we already have content
    if (nchar(current_chunk) + nchar(sentence) > target_size && nchar(current_chunk) > 0) {
      # If current sentence is very long, we can try splitting at commas
      if (nchar(sentence) > target_size/2) {
        # Split long sentence at commas
        comma_parts <- unlist(stringr::str_split(sentence, stringr::regex("(?<=,)\\s+")))
        
        for (part in comma_parts) {
          # If adding this part would exceed target size and we already have content
          if (nchar(current_chunk) + nchar(part) > target_size && nchar(current_chunk) > 0) {
            # Store current chunk and start a new one
            chunks <- append(chunks, current_chunk)
            current_chunk <- part
          } else {
            # Add this part to the current chunk
            if (nchar(current_chunk) > 0) {
              current_chunk <- paste(current_chunk, part)
            } else {
              current_chunk <- part
            }
          }
        }
      } else {
        # Store current chunk and start a new one with this sentence
        chunks <- append(chunks, current_chunk)
        current_chunk <- sentence
      }
    } else {
      # Add this sentence to the current chunk
      if (nchar(current_chunk) > 0) {
        current_chunk <- paste(current_chunk, sentence)
      } else {
        current_chunk <- sentence
      }
    }
  }
  
  # Add the last chunk if it's not empty
  if (nchar(current_chunk) > 0) {
    chunks <- append(chunks, current_chunk)
  }
  
  return(unlist(chunks))
}

# Function to generate embeddings
generate_embeddings <- function(text) {
  tryCatch({
    response <- POST(
      url = "https://api.openai.com/v1/embeddings",
      add_headers(Authorization = paste("Bearer", api_key)),
      body = list(
        model = "text-embedding-ada-002",
        input = text
      ),
      encode = "json"
    )
    
    # Parse the response
    content <- content(response, as = "parsed", simplifyVector = FALSE)
    
    # Handle errors in response
    if (!is.null(content$error)) {
      warning(content$error$message)
      return(NULL)
    }
    
    # Extract embedding vector
    if (!is.null(content$data[[1]]$embedding)) {
      return(as.numeric(content$data[[1]]$embedding))
    } else {
      warning("Embedding not found in response.")
      return(NULL)
    }
  }, error = function(e) {
    warning(sprintf("Error generating embeddings: %s", e$message))
    return(NULL)
  })
}

# Function to calculate cosine similarity
cosine_similarity <- function(a, b) {
  sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
}

# Function to extract company id from file path
extract_company_name <- function(file_path) {
  # Extract the file name from the path
  file_name <- basename(file_path)
  
  # Extract company identifier (assuming format like "PT_01405.pdf")
  company_id <- str_extract(file_name, "PT_\\d+")
  
  return(list(id = company_id))
}


## COMBINE ALL FUNCTION ABOVE TO CREATE A FUNCTION TO PROCESS A SINGLE PDF FILE

# Function to process a single PDF file
process_pdf <- function(pdf_path) {
  # Extract company information
  company_info <- extract_company_name(pdf_path)
  company_id <- company_info$id
  
  cat("\n\nProcessing:", company_id, "\n")
  cat("File:", pdf_path, "\n")
  
  # Extract the content
  extracted_text <- extract_pdf_content(pdf_path)
  
  if (is.null(extracted_text)) {
    cat("Failed to extract text from", pdf_path, "\n")
    return(NULL)
  }
  
  # Get basic statistics about the extracted text
  char_count <- nchar(extracted_text)
  word_count <- length(strsplit(extracted_text, " ")[[1]])
  
  cat("Text statistics:\n")
  cat("Total characters:", char_count, "\n")
  cat("Total words:", word_count, "\n")
  
  # Split text into smaller chunks at sentence boundaries
  text_chunks <- split_text_at_boundaries(extracted_text, target_size = 1000)
  
  # Check if the text is not too small
  if (length(text_chunks) == 0) {
    cat("Extracted text is too small or empty\n")
    return(NULL)
  }
  
  cat("Split text into", length(text_chunks), "chunks at sentence boundaries\n")
  
  # Generate embeddings for all text chunks
  cat("Generating embeddings for", length(text_chunks), "chunks...\n")
  embeddings <- lapply(text_chunks, generate_embeddings)
  
  # Check if we have valid embeddings
  valid_embeddings <- !sapply(embeddings, is.null)
  if (sum(valid_embeddings) == 0) {
    cat("Failed to generate any valid embeddings\n")
    return(NULL)
  }
  
  # Keep only valid chunks and embeddings
  text_chunks <- text_chunks[valid_embeddings]
  embeddings <- embeddings[valid_embeddings]
  
  # Combine the numeric vectors into a matrix
  embedding_vectors <- do.call(rbind, embeddings)
  
  # Define the query
  query_text <- "List countries where the company has factories, facilities, offices, subsidiaries, branches, restaurants or retail locations."
  
  # Generate embedding for the query
  query_embedding <- generate_embeddings(query_text)
  
  if (is.null(query_embedding)) {
    cat("Failed to generate query embedding\n")
    return(NULL)
  }
  
  # Compare query embedding with text embeddings (row-wise comparison)
  similarities <- apply(embedding_vectors, 1, function(row) cosine_similarity(query_embedding, row))
  
  # Set a similarity threshold
  similarity_threshold <- 0.75
  
  # Get indices of text chunks with similarity above the threshold
  matching_indices <- which(similarities >= similarity_threshold)
  
  if (length(matching_indices) == 0) {
    cat("No matching text chunks found with similarity threshold", similarity_threshold, "\n")
    # Try with a lower threshold if no matches found
    similarity_threshold <- 0.7
    matching_indices <- which(similarities >= similarity_threshold)
    
    if (length(matching_indices) == 0) {
      cat("Still no matching text chunks found with lowered threshold", similarity_threshold, "\n")
      return(NULL)
    }
  }
  
  # Get the corresponding text chunks
  matching_text_chunks <- text_chunks[matching_indices]
  
  cat("Found", length(matching_text_chunks), "matching text chunks\n")
  
  # Combine text chunks into a single input
  input_text <- paste(matching_text_chunks, collapse = "\n\n")
  
  # Check token count (approximate)
  input_tokens <- nchar(input_text) / 4  # rough estimation
  
  if (input_tokens > 15000) {
    cat("Warning: Input text might exceed token limit. Truncating...\n")
    # Take top matching chunks until we're under limit
    sorted_indices <- matching_indices[order(similarities[matching_indices], decreasing = TRUE)]
    
    # Initialize variables
    total_tokens <- 0
    selected_chunks <- c()
    
    # Select chunks until we're close to token limit
    for (idx in sorted_indices) {
      chunk_tokens <- nchar(text_chunks[idx]) / 4
      if (total_tokens + chunk_tokens < 15000) {
        selected_chunks <- c(selected_chunks, text_chunks[idx])
        total_tokens <- total_tokens + chunk_tokens
      } else {
        break
      }
    }
    
    input_text <- paste(selected_chunks, collapse = "\n\n")
    cat("Reduced to approximately", total_tokens, "tokens\n")
  }
  
  # Define the prompt
  prompt <- paste0(
    "The following text provides information about a company's operations. Identify and list all countries where the company operates. A country of operation is defined as any location where the company has active production facilities, processing facilities, subsidiaries, sales offices, retail stores, or restaurants. Only respond with the list of countries mentioned, using ISO 3166-1 alpha-3 codes (e.g., USA for United States). For each country, indicate the relevant type(s) of operation present — using only the specified categories. Format your response as:
USA – retail stores
FRA – production facilities, subsidiaries",
    input_text,
    sep = "\n\n"
  )
  
  # Call the Chat Completions API
  cat("Calling OpenAI API for extraction...\n")
  
  response <- tryCatch({
    POST(
      url = "https://api.openai.com/v1/chat/completions",
      body = list(
        model = "gpt-4o",
        messages = list(
          list(role = "system", content = "You are a research assistant helping extract countries of operations of companies."),
          list(role = "user", content = prompt)
        ),
        max_tokens = 1000,
        temperature = 0.1
      ),
      add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      encode = "json"
    )
  }, error = function(e) {
    cat("Error calling OpenAI API:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(response)) {
    return(NULL)
  }
  
  # Parse the response with error handling
  parsed_content <- tryCatch({
    content(response, as = "parsed", simplifyVector = FALSE)
  }, error = function(e) {
    cat("Error parsing API response:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(parsed_content)) {
    return(NULL)
  }
  
  # Print the structure of the response for debugging
  cat("Response structure:\n")
  str(parsed_content)
  
  # Check for errors
  if (!is.null(parsed_content$error)) {
    cat("API Error:", parsed_content$error$message, "\n")
    return(NULL)
  }
  
  # Extract the assistant's response with safer navigation
  assistant_response <- NA
  tryCatch({
    # Try different possible response structures
    if (!is.null(parsed_content$choices) && length(parsed_content$choices) > 0) {
      if (!is.null(parsed_content$choices[[1]]$message$content)) {
        assistant_response <- parsed_content$choices[[1]]$message$content
      } else if (!is.null(parsed_content$choices[[1]]$text)) {
        assistant_response <- parsed_content$choices[[1]]$text
      }
    }
  }, error = function(e) {
    cat("Error extracting response content:", e$message, "\n")
  })
  
  if (is.na(assistant_response)) {
    cat("Could not extract a valid response from the API\n")
    return(NULL)
  }
  
  cat("Successfully extracted response from API\n")
  
  # Extract structured results from the OpenAI response
  results <- tibble(
    `WBA ID` = company_id,
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
          keep(~ str_detect(.x, "^[A-Z]{3}\\s–\\s")) %>%
          # Extract the country code and business operations using regex
          map(~ {
            matches <- str_match(.x, "^([A-Z]{3})\\s–\\s(.+)$")
            if (!is.na(matches[1])) {
              tibble(
                countryISO = matches[2],
                business_operations = matches[3]
              )
            } else {
              NULL
            }
          }) %>%
          compact() %>%
          bind_rows()
      )
    )
  
  
  # Check if we have extracted data
  if (nrow(final_df) > 0 && any(sapply(final_df$extracted_data, nrow) > 0)) {
    final_df <- final_df %>%
      unnest(extracted_data) %>%
      select(`WBA ID`, countryISO, business_operations) %>% 
      mutate(source = basename(pdf_path))
    
    cat("Successfully extracted", nrow(final_df), "country operations\n")
    return(final_df)
  } else {
    cat("No countries of operation extracted\n")
    return(NULL)
  }
}

## NOW PROCESS PDF IN BATCH

# Process each PDF file
for (pdf_file in pdf_files) {
  # Process the current PDF file
  result <- process_pdf(pdf_file)
  
  # If we got results, add them to the combined results
  if (!is.null(result) && nrow(result) > 0) {
    all_results <- bind_rows(all_results, result)
  }
  
  # Add a small delay to prevent API rate limiting
  Sys.sleep(2)
}

# Add company and country name
all_results <- all_results %>% 
  left_join(filtered_df %>% select(WBAID, Company), by = c(`WBA ID` = "WBAID" )) %>% 
  mutate(Country = countrycode(countryISO, "iso3c", "country.name")) %>% 
  select(`WBA ID`, Company, countryISO, Country, business_operations, source)


# Save the results
output_file <- file.path(pdf_dir, "country_operations_results.csv")
write_csv(all_results, output_file)

cat("\n\nProcessing complete!\n")
cat("Processed", length(pdf_files), "PDF files\n")
cat("Extracted country operations for", length(unique(all_results$`WBA ID`)), "companies\n")
cat("Total country operations extracted:", nrow(all_results), "\n")
cat("Results saved to:", output_file, "\n")

# View the final results
View(all_results)