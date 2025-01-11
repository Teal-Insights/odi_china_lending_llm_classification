# Classification Helper Functions


## classify_single_project()


# 1. Core Classification Function ----
classify_single_project <- function(text, chat, type_spec) {
  result <- purrr::safely(
    ~ chat$extract_data(text, type = type_spec),
    otherwise = NULL,
    quiet = TRUE
  )()
  
  tibble::tibble(
    success = is.null(result$error),
    error_message = if(is.null(result$error)) NA_character_ else as.character(result$error),
    classification = list(result$result)
  )
}

# 2. Process a chunk of data ----
process_chunk <- function(chunk, chat, type_spec) {
  chunk |>
    mutate(
      classification_result = map(
        combined_text,
        ~{
          # Clear all prior turns so we start fresh for *each* record
          chat$set_turns(list())
          
          classify_single_project(.x, chat, type_spec)
        }
      )
    ) |>
    unnest(classification_result) |>
    # For successful classifications, unnest the actual classifications
    mutate(
      primary_class = map_chr(classification, ~.x$classification$primary %||% NA_character_),
      confidence = map_chr(classification, ~.x$classification$confidence %||% NA_character_),
      project_type = map_chr(classification, ~.x$classification$project_type %||% NA_character_),
      justification = map_chr(classification, ~.x$justification %||% NA_character_),
      evidence = map_chr(classification, ~.x$evidence %||% NA_character_)
    ) |>
    select(-classification) # Remove the nested list column
}


## create_chunks()

# 3. Chunk Management ----
create_chunks <- function(data, chunk_size = 100) {
  # Split into chunks without grouping/splitting operation
  n_chunks <- ceiling(nrow(data) / chunk_size)
  chunk_indices <- split(seq_len(nrow(data)), 
                         cut(seq_len(nrow(data)), n_chunks, labels = FALSE))
  
  map(chunk_indices, ~data[.x, ])
}

# 4. Process Chunks ----

process_all_chunks <- function(data, chunk_size = 100, 
                               chat_provider = c("ollama", "openai-gpt-4o","openai-gpt-4o-mini", "claude"),
                               output_dir = "classification_results",
                               run_name = NULL) {
  
  start_time <- Sys.time()
  chat_provider <- match.arg(chat_provider)
  
  # Generate run identifier if not provided
  run_id <- run_name %||% format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Create run-specific directory structure
  run_dir <- here::here(output_dir, chat_provider, run_id)
  chunks_dir <- file.path(run_dir, "chunks")
  logs_dir <- file.path(run_dir, "logs")
  
  dir.create(chunks_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Create and save run metadata
  run_metadata <- list(
    provider = chat_provider,
    run_id = run_id,
    start_time = format(start_time, "%Y-%m-%d %H:%M:%S"),
    chunk_size = chunk_size,
    total_records = nrow(data)
  )
  
  jsonlite::write_json(
    run_metadata,
    file.path(logs_dir, "run_metadata.json"),
    pretty = TRUE
  )
  
  # Initialize chat once
  chat <- switch(chat_provider,
                 "ollama" = chat_ollama(model = "llama3.3", system_prompt = system_prompt_txt),
                 "openai-gpt-4o" = chat_openai(model = "gpt-4o", system_prompt = system_prompt_txt),
                 "openai-gpt-4o-mini" = chat_openai(model = "gpt-4o-mini", system_prompt = system_prompt_txt),
                 "claude" = chat_claude(model = "claude-3-5-sonnet-20241022", system_prompt = system_prompt_txt)
  )
  
  # Create chunks
  chunks <- create_chunks(data, chunk_size)
  
  # Initialize log file with start time
  log_file <- file.path(logs_dir, "processing_log.txt")
  cat(sprintf("Processing started at: %s\n\n", format(start_time)), 
      file = log_file, append = FALSE)
  
  # Process chunks with progress tracking
  progressr::with_progress({
    p <- progressr::progressor(along = chunks)
    
    # Process each chunk using imap
    chunk_results <- imap(chunks, function(chunk, chunk_idx) {
      chunk_start_time <- Sys.time()
      chunk_num <- as.integer(chunk_idx)
      
      # Process chunk
      chunk_results <- process_chunk(chunk, chat, project_analysis_spec)
      
      # Generate filenames
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      filename_base <- sprintf("chunk_%03d_%s", chunk_num, timestamp)
      
      # Split and save results
      successful <- chunk_results |> filter(success)
      failed <- chunk_results |> filter(!success)
      
      # Save as CSV files
      if (nrow(successful) > 0) {
        write_csv(
          successful,
          file.path(chunks_dir, paste0(filename_base, "_success.csv"))
        )
      }
      
      if (nrow(failed) > 0) {
        write_csv(
          failed,
          file.path(chunks_dir, paste0(filename_base, "_failed.csv"))
        )
      }
      
      # Calculate chunk processing time
      chunk_end_time <- Sys.time()
      chunk_duration <- difftime(chunk_end_time, chunk_start_time, units = "mins")
      
      # Log results with timing information
      log_entry <- sprintf(
        "Chunk %d: %d successes, %d failures (Processing time: %.2f minutes)\n", 
        chunk_num, nrow(successful), nrow(failed), as.numeric(chunk_duration)
      )
      cat(log_entry, file = log_file, append = TRUE)
      
      # Update progress
      p(sprintf("Processed chunk %d/%d", chunk_num, length(chunks)))
      
      # Return results for this chunk
      chunk_results
    })
    
    # Combine all results
    final_results <- bind_rows(chunk_results)
    
    # Log completion time and total duration
    end_time <- Sys.time()
    total_duration <- difftime(end_time, start_time, units = "hours")
    
    cat(sprintf("\nProcessing completed at: %s", format(end_time)), 
        file = log_file, append = TRUE)
    cat(sprintf("\nTotal processing time: %.2f hours", as.numeric(total_duration)), 
        file = log_file, append = TRUE)
    
    # Return results
    final_results
  })
}

# 5. Collect Results ----
collect_results <- function(output_dir, chat_provider = NULL, run_id = NULL) {
  # Build path based on provided parameters
  base_path <- here::here(output_dir)
  if (!is.null(chat_provider)) {
    base_path <- file.path(base_path, chat_provider)
    if (!is.null(run_id)) {
      base_path <- file.path(base_path, run_id)
      base_path <- file.path(base_path, "chunks")
    }
  }
  
  # Read all CSV files
  success_files <- list.files(base_path, pattern = "_success.csv$", full.names = TRUE)
  failed_files <- list.files(base_path, pattern = "_failed.csv$", full.names = TRUE)
  
  # Read in files if they exist
  successful <- if (length(success_files) > 0) {
    map_dfr(success_files, read_csv, show_col_types = FALSE)
  } else {
    tibble()  # Empty tibble if no successful results
  }
  
  failed <- if (length(failed_files) > 0) {
    map_dfr(failed_files, read_csv, show_col_types = FALSE)
  } else {
    tibble()  # Empty tibble if no failed results
  }
  
  # Get run metadata if available
  metadata_path <- file.path(dirname(dirname(base_path)), "logs", "run_metadata.json")
  metadata <- if (file.exists(metadata_path)) {
    jsonlite::read_json(metadata_path)
  } else {
    NULL
  }
  
  # Get processing log if available
  log_path <- file.path(dirname(dirname(base_path)), "logs", "processing_log.txt")
  log_content <- if (file.exists(log_path)) {
    readLines(log_path)
  } else {
    NULL
  }
  
  # Return results, including metadata and logs
  list(
    successful = successful,
    failed = failed,
    summary = tibble(
      total = nrow(successful) + nrow(failed),
      successful = nrow(successful),
      failed = nrow(failed),
      success_rate = nrow(successful) / (nrow(successful) + nrow(failed))
    ),
    metadata = metadata,
    log = log_content
  )
}

# 6. Custom Progress Handlers ----
handlers(handler_progress(
  format = "[:bar] :current/:total (:percent) Elapsed: :elapsed ETA: :eta",
  width = 60
))

# 7. {ellmer} JSON Specification ----

# Create specification for the expected JSON structure
project_analysis_spec <- type_object(
  "Project environmental classification",
  classification = type_object(
    primary = type_enum(
      "Primary classification", 
      c("GREEN", "GREY", "BROWN", "NEUTRAL")
    ),
    confidence = type_enum(
      "Confidence level",
      c("HIGH", "MEDIUM", "LOW")
    ),
    project_type = type_enum(
      "Specific type of project",
      c(
        "Solar Power",
        "Wind Power",
        "Hydropower",
        "Nuclear Power",
        "Geothermal Power",
        "Other Low-Carbon Energy",
        "Coal Power",
        "Natural Gas Power",
        "Oil and Gas Infrastructure",
        "Mining (Energy Transition Minerals)",
        "Mining (Other)",
        "Grid Infrastructure",
        "Energy Storage",
        "Energy Efficiency",
        "General Infrastructure",
        "Green Transportation",
        "Green Hydrogen",
        "Social Development",
        "Financial Services",
        "Other Energy (Fossil Fuels)",
        "Other Energy (Low Carbon)",
        "Other"
      )
    )
  ),
  justification = type_string("Justification of the classification"),
  evidence = type_string("Evidence supporting the classification")
)