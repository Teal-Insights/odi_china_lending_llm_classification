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

# 5. Process Chunks [new] ----
process_chunk_safely <- function(chunk, chat, type_spec, monitor) {
  # Check system state before processing
  state <- monitor$check_state()
  
  if (state$is_cooling_down && !state$can_resume) {
    cooldown_remaining <- difftime(
      state$last_thermal_event + minutes(10),
      Sys.time(),
      units = "mins"
    )
    
    message(sprintf(
      "System is cooling down. Waiting %.1f minutes before processing chunk.",
      cooldown_remaining
    ))
    
    # Wait for cooldown
    Sys.sleep(as.numeric(cooldown_remaining, units = "secs"))
  }
  
  # Process chunk with normal logic
  process_chunk(chunk, chat, type_spec)
}

#' Get thermal metrics safely using various macOS commands
#' @return Named list of thermal metrics
get_thermal_metrics <- function() {
  # Try different methods to get thermal data
  
  # Method 1: Try osx-cpu-temp if available
  temp_data <- try(
    system("osx-cpu-temp -f", intern = TRUE, ignore.stderr = TRUE),
    silent = TRUE
  )
  
  if (!inherits(temp_data, "try-error")) {
    # Extract numeric temperature value
    temp_value <- as.numeric(gsub("[^0-9.]", "", temp_data))
    return(list(
      cpu_temp = temp_value,
      gpu_temp = NA_real_,
      power_draw = NA_real_
    ))
  }
  
  # Method 2: Try pmset -g therm
  therm_data <- try(
    system("pmset -g therm", intern = TRUE, ignore.stderr = TRUE),
    silent = TRUE
  )
  
  if (!inherits(therm_data, "try-error")) {
    # Try to extract CPU die temperature if available
    cpu_temp_line <- grep("CPU die temperature", therm_data, value = TRUE)
    if (length(cpu_temp_line) > 0) {
      temp_value <- as.numeric(gsub("[^0-9.]", "", cpu_temp_line))
      return(list(
        cpu_temp = temp_value,
        gpu_temp = NA_real_,
        power_draw = NA_real_
      ))
    }
  }
  
  # Method 3: Try sysctl for basic CPU info
  sysctrl_data <- try(
    system("sysctl machdep.xcpm.cpu_thermal_level", intern = TRUE, ignore.stderr = TRUE),
    silent = TRUE
  )
  
  if (!inherits(sysctrl_data, "try-error")) {
    # Extract thermal level (0-3, where 3 is highest)
    thermal_level <- as.numeric(gsub(".*: ", "", sysctrl_data))
    # Convert thermal level to an approximate temperature (rough estimate)
    temp_value <- switch(thermal_level + 1,
                         45,  # Level 0: Normal
                         65,  # Level 1: Elevated
                         85,  # Level 2: High
                         95)  # Level 3: Very High
    
    return(list(
      cpu_temp = temp_value,
      gpu_temp = NA_real_,
      power_draw = NA_real_
    ))
  }
  
  # If all methods fail, return NA values
  list(
    cpu_temp = NA_real_,
    gpu_temp = NA_real_,
    power_draw = NA_real_
  )
}


process_all_chunks <- function(data,
                               chunk_size = 100,
                               chat_provider = c("ollama", "openai-gpt-4o",
                                 "openai-gpt-4o-mini", "claude"),
                               output_dir = "classification_results",
                               run_name = NULL,
                               enable_monitoring = TRUE,
                               monitoring_config = list(
                                 cooldown_minutes = 10,
                                 thresholds = list(
                                   memory = 90,    # %
                                   cpu = 80,       # %
                                   thermal = 85    # Celsius
                                 ),
                                 interval = 1      # seconds
                               )) {

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

  # Initialize system monitoring if enabled
  monitor <- if (enable_monitoring) {
    process_name <- if (chat_provider == "ollama") "ollama" else NULL

    LLMProcessMonitor$new(
      output_dir = run_dir,
      run_id = run_id,
      process_name = process_name,
      cooldown_minutes = monitoring_config$cooldown_minutes,
      thresholds = monitoring_config$thresholds,
      interval = monitoring_config$interval
    )
  }

  # Create and save run metadata
  run_metadata <- list(
    provider = chat_provider,
    run_id = run_id,
    start_time = format(start_time, "%Y-%m-%d %H:%M:%S"),
    chunk_size = chunk_size,
    total_records = nrow(data),
    monitoring_enabled = enable_monitoring,
    monitoring_config = if (enable_monitoring) monitoring_config else NULL
  )

  jsonlite::write_json(
    run_metadata,
    file.path(logs_dir, "run_metadata.json"),
    pretty = TRUE
  )

  # Initialize chat once
  chat <- switch(chat_provider,
    "ollama" = chat_ollama(model = "llama3.3",
      system_prompt = system_prompt_txt),
    "openai-gpt-4o" = chat_openai(model = "gpt-4o",
      system_prompt = system_prompt_txt),
    "openai-gpt-4o-mini" = chat_openai(model = "gpt-4o-mini",
      system_prompt = system_prompt_txt),
    "claude" = chat_claude(model = "claude-3-5-sonnet-20241022",
      system_prompt = system_prompt_txt)
  )

  # Create chunks
  chunks <- create_chunks(data, chunk_size)

  # Initialize log file with start time
  log_file <- file.path(logs_dir, "processing_log.txt")
  cat(sprintf("Processing started at: %s\n\n", format(start_time)),
    file = log_file, append = FALSE)

  # Start system monitoring if enabled
  if (!is.null(monitor)) monitor$start()

  # Set up monitoring cleanup
  on.exit({
    if (!is.null(monitor)) {
      monitor$stop()

      # Generate monitoring report
      report <- monitor$generate_report()

      # Save monitoring analysis
      if (!is.null(report)) {
        monitoring_summary_file <- file.path(logs_dir, "monitoring_summary.json")
        jsonlite::write_json(
          report$summary,
          monitoring_summary_file,
          pretty = TRUE,
          auto_unbox = TRUE
        )

        # Write cooldown history if any
        if (!is.null(report$cooldown_history)) {
          write_csv(
            report$cooldown_history,
            file.path(logs_dir, "cooldown_history.csv")
          )
        }
      }
    }
  })

  # Process chunks with progress tracking
  results <- progressr::with_progress({
    p <- progressr::progressor(along = chunks)

    # Process each chunk using imap
    chunk_results <- imap(chunks, function(chunk, chunk_idx) {
      chunk_start_time <- Sys.time()
      chunk_num <- as.integer(chunk_idx)

      # Wait for system resources if monitoring enabled
      if (!is.null(monitor)) {
        monitor$wait_until_ready()
      }

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
    bind_rows(chunk_results)
  })

  # Log completion time and total duration
  end_time <- Sys.time()
  total_duration <- difftime(end_time, start_time, units = "hours")

  cat(sprintf("\nProcessing completed at: %s", format(end_time)),
    file = log_file, append = TRUE)
  cat(sprintf("\nTotal processing time: %.2f hours", as.numeric(total_duration)),
    file = log_file, append = TRUE)

  # Return list with results and monitoring info if enabled
  if (!is.null(monitor)) {
    list(
      results = results,
      monitoring = monitor$generate_report()
    )
  } else {
    results
  }
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


# Part 2: System Monitoring ----

#' @title SystemMonitor
#' @description R6 Class for monitoring system resources and health on macOS systems
#'
#' @import R6
#' @import logger
#' @import ps
#' @import tidyverse
#' @import ggplot2

SystemMonitor <- R6::R6Class("SystemMonitor",
  public = list(
    #' @description Create a new system monitor
    #' @param output_dir Directory to store monitoring data
    #' @param run_id Unique identifier for this monitoring session
    #' @param interval Sampling interval in seconds
    #' @param thresholds Named list of threshold values for monitoring
    initialize = function(output_dir,
                          run_id = format(Sys.time(), "%Y%m%d_%H%M%S"),
                          interval = 1,
                          thresholds = list(
                            memory = 90,    # %
                            cpu = 80,       # %
                            thermal = 85    # Celsius
                          )) {
      # Validate inputs
      stopifnot(
        is.character(output_dir), length(output_dir) == 1,
        is.character(run_id), length(run_id) == 1,
        is.numeric(interval), interval > 0
      )

      private$output_dir <- output_dir
      private$run_id <- run_id
      private$interval <- interval
      private$thresholds <- thresholds

      # Set up directories and files
      private$monitor_dir <- file.path(output_dir, "system_metrics", run_id)
      dir.create(private$monitor_dir, recursive = TRUE, showWarnings = FALSE)
      private$metrics_file <- file.path(private$monitor_dir, "metrics.csv")
      private$events_file <- file.path(private$monitor_dir, "events.log")

      # Initialize logger
      logger::log_formatter(logger::formatter_glue)
      logger::log_appender(logger::appender_file(private$events_file))

      logger::log_info("SystemMonitor initialized")
      logger::log_info("Output directory: {private$monitor_dir}")
    },

    #' @description Start monitoring system resources
    start = function() {
      if (private$is_active) {
        logger::log_warn("Monitoring already active")
        return(invisible(self))
      }

      private$is_active <- TRUE
      private$start_time <- Sys.time()
      private$last_metrics <- private$collect_metrics()

      # Write initial metrics with headers
      write_csv(private$last_metrics, private$metrics_file)

      # Start monitoring loop in the background
      private$monitoring_loop()

      logger::log_info("Monitoring started at {format(private$start_time)}")

      invisible(self)
    },

    #' @description Stop monitoring system resources
    stop = function() {
      if (!private$is_active) {
        logger::log_warn("Monitoring not active")
        return(invisible(self))
      }

      private$is_active <- FALSE
      private$stop_time <- Sys.time()

      duration <- difftime(private$stop_time, private$start_time, units = "mins")
      logger::log_info("Monitoring stopped after {round(duration, 1)} minutes")

      invisible(self)
    },

    #' @description Check current system health status
    #' @return List with health status indicators
    check_health = function() {
      metrics <- private$collect_metrics()
      private$last_metrics <- metrics

      # Save metrics to file
      if (private$is_active) {
        write_csv(metrics, private$metrics_file, append = TRUE)
      }

      list(
        memory_pressure = metrics$memory_percent >= private$thresholds$memory,
        cpu_pressure = metrics$cpu_usage >= private$thresholds$cpu,
        thermal_pressure = !is.na(metrics$cpu_temp) &&
          metrics$cpu_temp >= private$thresholds$thermal,
        metrics = metrics,
        timestamp = Sys.time()
      )
    },

    #' @description Get current metrics
    #' @return Tibble with latest metrics
    get_current_metrics = function() {
      private$last_metrics
    },

    #' @description Generate monitoring report with plots
    #' @param plot_width Width of generated plots in inches
    #' @param plot_height Height of generated plots in inches
    generate_report = function(plot_width = 10, plot_height = 6) {
      if (!file.exists(private$metrics_file)) {
        logger::log_error("No metrics data found")
        return(NULL)
      }

      # Read all metrics
      metrics <- read_csv(private$metrics_file, show_col_types = FALSE)

      # Generate summary statistics
      summary_stats <- metrics %>%
        summarise(
          duration_mins = as.numeric(
            difftime(max(timestamp), min(timestamp), units = "mins")
          ),
          avg_memory_used = mean(memory_percent, na.rm = TRUE),
          max_memory_used = max(memory_percent, na.rm = TRUE),
          avg_cpu_usage = mean(cpu_usage, na.rm = TRUE),
          max_cpu_usage = max(cpu_usage, na.rm = TRUE),
          avg_cpu_temp = mean(cpu_temp, na.rm = TRUE),
          max_cpu_temp = max(cpu_temp, na.rm = TRUE)
        )

      # Create visualizations
      memory_plot <- metrics %>%
        ggplot(aes(x = timestamp, y = memory_percent)) +
        geom_line(color = "steelblue") +
        geom_hline(
          yintercept = private$thresholds$memory,
          linetype = "dashed",
          color = "red"
        ) +
        theme_minimal() +
        labs(
          title = "Memory Usage Over Time",
          y = "Memory Usage (%)",
          x = "Time"
        )

      cpu_plot <- metrics %>%
        ggplot(aes(x = timestamp, y = cpu_usage)) +
        geom_line(color = "darkgreen") +
        geom_hline(
          yintercept = private$thresholds$cpu,
          linetype = "dashed",
          color = "red"
        ) +
        theme_minimal() +
        labs(
          title = "CPU Usage Over Time",
          y = "CPU Usage (%)",
          x = "Time"
        )

      thermal_plot <- metrics %>%
        filter(!is.na(cpu_temp)) %>%
        ggplot(aes(x = timestamp)) +
        geom_line(aes(y = cpu_temp), color = "orange") +
        geom_hline(
          yintercept = private$thresholds$thermal,
          linetype = "dashed",
          color = "red"
        ) +
        theme_minimal() +
        labs(
          title = "CPU Temperature Over Time",
          y = "Temperature (°C)",
          x = "Time"
        )

      # Save plots
      ggsave(
        file.path(private$monitor_dir, "memory_usage.png"),
        memory_plot,
        width = plot_width,
        height = plot_height
      )

      ggsave(
        file.path(private$monitor_dir, "cpu_usage.png"),
        cpu_plot,
        width = plot_width,
        height = plot_height
      )

      ggsave(
        file.path(private$monitor_dir, "cpu_temp.png"),
        thermal_plot,
        width = plot_width,
        height = plot_height
      )

      # Return summary and plots
      list(
        summary = summary_stats,
        plots = list(
          memory = memory_plot,
          cpu = cpu_plot,
          thermal = thermal_plot
        ),
        raw_data = metrics
      )
    }
  ),

  private = list(
    # Configuration
    output_dir = NULL,
    run_id = NULL,
    interval = 1,
    thresholds = NULL,

    # State
    is_active = FALSE,
    start_time = NULL,
    stop_time = NULL,
    last_metrics = NULL,

    # File paths
    monitor_dir = NULL,
    metrics_file = NULL,
    events_file = NULL,

    #' @description Background monitoring loop
    monitoring_loop = function() {
      while (private$is_active) {
        tryCatch(
          {
            private$last_metrics <- private$collect_metrics()
            write_csv(private$last_metrics, private$metrics_file, append = TRUE)
          },
          error = function(e) {
            logger::log_error("Error collecting metrics: {conditionMessage(e)}")
          })

        Sys.sleep(private$interval)
      }
    },

    #' @description Collect current system metrics
    collect_metrics = function() {
      # System memory
      mem <- tryCatch(
        ps::ps_system_memory(),
        error = function(e) {
          logger::log_warn("Failed to get memory info: {conditionMessage(e)}")
          list(
            total = NA_real_,
            used = NA_real_,
            available = NA_real_,
            percent = NA_real_
          )
        }
      )
      
      # CPU times and usage
      cpu <- tryCatch({
        times <- unclass(ps::ps_system_cpu_times())  # Convert to named numeric vector
        total_time <- sum(times)
        idle_time <- times["idle"]
        usage <- ((total_time - idle_time) / total_time) * 100
        
        list(
          usage = usage,
          user = times["user"],
          system = times["system"],
          idle = times["idle"]
        )
      }, error = function(e) {
        logger::log_warn("Failed to get CPU info: {conditionMessage(e)}")
        list(
          usage = NA_real_,
          user = NA_real_,
          system = NA_real_,
          idle = NA_real_
        )
      })
      
      # Thermal info
      thermal <- private$get_thermal_metrics()
      
      # Construct metrics tibble
      tibble::tibble(
        timestamp = Sys.time(),
        
        # Memory metrics
        memory_total = mem$total,
        memory_used = mem$used,
        memory_available = mem$available,
        memory_percent = mem$percent,
        
        # CPU metrics  
        cpu_usage = cpu$usage,
        cpu_user = cpu$user,
        cpu_system = cpu$system,
        cpu_idle = cpu$idle,
        
        # Thermal metrics
        cpu_temp = thermal$cpu_temp,
        gpu_temp = thermal$gpu_temp,
        power_draw = thermal$power_draw
      )
    },

    #' @description Get thermal metrics from system
    get_thermal_metrics = function() {
      # Try different methods to get thermal data

      # Method 1: Try osx-cpu-temp if available
      temp_data <- try(
        system("osx-cpu-temp -f", intern = TRUE, ignore.stderr = TRUE),
        silent = TRUE
      )

      if (!inherits(temp_data, "try-error")) {
        # Extract numeric temperature value
        temp_value <- as.numeric(gsub("[^0-9.]", "", temp_data))
        return(list(
          cpu_temp = temp_value,
          gpu_temp = NA_real_,
          power_draw = NA_real_
        ))
      }

      # Method 2: Try pmset -g therm
      therm_data <- try(
        system("pmset -g therm", intern = TRUE, ignore.stderr = TRUE),
        silent = TRUE
      )

      if (!inherits(therm_data, "try-error")) {
        # Try to extract CPU die temperature if available
        cpu_temp_line <- grep("CPU die temperature", therm_data, value = TRUE)
        if (length(cpu_temp_line) > 0) {
          temp_value <- as.numeric(gsub("[^0-9.]", "", cpu_temp_line))
          return(list(
            cpu_temp = temp_value,
            gpu_temp = NA_real_,
            power_draw = NA_real_
          ))
        }
      }

      # Method 3: Try sysctl for basic CPU info
      sysctrl_data <- try(
        system(
          "sysctl machdep.xcpm.cpu_thermal_level",
          intern = TRUE,
          ignore.stderr = TRUE
        ),
        silent = TRUE
      )

      if (!inherits(sysctrl_data, "try-error")) {
        # Extract thermal level (0-3, where 3 is highest)
        thermal_level <- as.numeric(gsub(".*: ", "", sysctrl_data))
        # Convert thermal level to an approximate temperature (rough estimate)
        temp_value <- switch(thermal_level + 1,
          45,  # Level 0: Normal
          65,  # Level 1: Elevated
          85,  # Level 2: High
          95   # Level 3: Very High
        )

        return(list(
          cpu_temp = temp_value,
          gpu_temp = NA_real_,
          power_draw = NA_real_
        ))
      }

      # If all methods fail, return NA values
      list(
        cpu_temp = NA_real_,
        gpu_temp = NA_real_,
        power_draw = NA_real_
      )
    }
  )
)

## LLMProcess ----
#' @title LLMProcessMonitor
#' @description System monitor specialized for LLM processing with thermal protection
#' and process-specific monitoring. Extends SystemMonitor to add functionality for
#' managing long-running LLM processes safely.
#'
#' @import R6
#' @import logger
#' @import ps
#' @import tidyverse

LLMProcessMonitor <- R6::R6Class("LLMProcessMonitor",
  inherit = SystemMonitor,

  public = list(
    #' @description Create a new LLM process monitor
    #' @param output_dir Directory to store monitoring data
    #' @param run_id Unique identifier for this monitoring session
    #' @param process_name Name of the LLM process to monitor (e.g., "ollama")
    #' @param cooldown_minutes Minutes to wait after hitting resource limits
    #' @param thresholds Named list of threshold values for monitoring
    #' @param interval Sampling interval in seconds
    initialize = function(output_dir,
                          run_id = format(Sys.time(), "%Y%m%d_%H%M%S"),
                          process_name = NULL,
                          cooldown_minutes = 10,
                          thresholds = list(
                            memory = 90,    # %
                            cpu = 80,       # %
                            thermal = 85    # Celsius
                          ),
                          interval = 1) {

      super$initialize(
        output_dir = output_dir,
        run_id = run_id,
        interval = interval,
        thresholds = thresholds
      )

      private$process_name <- process_name
      private$cooldown_minutes <- cooldown_minutes
      private$cooldown_history <- tibble::tibble(
        start_time = Sys.time()[0],
        end_time = Sys.time()[0],
        reason = character()
      )

      logger::log_info("LLMProcessMonitor initialized")
      if (!is.null(process_name)) {
        logger::log_info("Monitoring process: {process_name}")
      }
    },

    #' @description Check if it's safe to continue processing
    #' @return Logical indicating if processing can continue
    can_process = function() {
      if (private$is_cooling_down) {
        elapsed <- difftime(Sys.time(), private$cooldown_start,
          units = "mins")

        if (elapsed >= private$cooldown_minutes) {
          private$end_cooldown()
          return(TRUE)
        }
        return(FALSE)
      }

      health <- self$check_health()

      # Check process-specific metrics if monitoring a specific process
      if (!is.null(private$process_name)) {
        proc_metrics <- private$get_process_metrics()
        if (!is.null(proc_metrics)) {
          health$process_pressure <-
            proc_metrics$memory_percent > private$thresholds$memory
        }
      }

      if (health$thermal_pressure ||
        health$memory_pressure ||
        !is.null(health$process_pressure) && health$process_pressure) {

        reason <- paste(
          if (health$thermal_pressure) "thermal",
          if (health$memory_pressure) "memory",
          if (!is.null(health$process_pressure) && health$process_pressure) "process",
          collapse = " and "
        )

        private$start_cooldown(reason)
        return(FALSE)
      }

      TRUE
    },

    #' @description Wait until system is ready to process
    #' @param max_wait Maximum time to wait in minutes
    #' @param check_interval Seconds between checks
    wait_until_ready = function(max_wait = 60, check_interval = 30) {
      start <- Sys.time()

      while (!self$can_process()) {
        if (difftime(Sys.time(), start, units = "mins") > max_wait) {
          stop("Maximum wait time exceeded")
        }

        # Calculate and log remaining cooldown time if applicable
        if (private$is_cooling_down) {
          elapsed <- difftime(Sys.time(), private$cooldown_start, units = "mins")
          remaining <- private$cooldown_minutes - elapsed
          logger::log_info(
            "Cooling down: {round(remaining, 1)} minutes remaining"
          )
        }

        Sys.sleep(check_interval)
      }

      invisible(self)
    },

    #' @description Get history of cooldown periods
    #' @return Tibble with cooldown history
    get_cooldown_history = function() {
      private$cooldown_history
    },

    #' @description Generate extended monitoring report including cooldown periods
    #' @param plot_width Width of generated plots in inches
    #' @param plot_height Height of generated plots in inches
    generate_report = function(plot_width = 10, plot_height = 6) {
      # Get base report from parent class
      report <- super$generate_report(plot_width, plot_height)

      # Add cooldown history
      report$cooldown_history <- private$cooldown_history %>%
        mutate(
          duration = as.numeric(
            difftime(end_time, start_time, units = "mins")
          )
        )

      # Create cooldown visualization if we have any history
      if (nrow(report$cooldown_history) > 0) {
        cooldown_plot <- report$cooldown_history %>%
          ggplot(aes(x = start_time, xend = end_time,
            y = reason, yend = reason)) +
          geom_segment(size = 10, alpha = 0.5) +
          theme_minimal() +
          labs(
            title = "Cooldown Periods",
            x = "Time",
            y = "Reason"
          )

        # Save cooldown plot
        ggsave(
          file.path(private$monitor_dir, "cooldown_periods.png"),
          cooldown_plot,
          width = plot_width,
          height = plot_height
        )

        report$plots$cooldown <- cooldown_plot
      }

      report
    }
  ),

  private = list(
    # Configuration
    process_name = NULL,
    cooldown_minutes = 10,

    # State
    is_cooling_down = FALSE,
    cooldown_start = NULL,
    cooldown_history = NULL,

    #' @description Start a cooldown period
    start_cooldown = function(reason) {
      if (!private$is_cooling_down) {
        private$is_cooling_down <- TRUE
        private$cooldown_start <- Sys.time()

        logger::log_warn(
          "Starting cooldown period due to {reason} pressure"
        )

        # Record start of cooldown
        private$cooldown_history <- private$cooldown_history %>%
          add_row(
            start_time = private$cooldown_start,
            end_time = NA,
            reason = reason
          )
      }
    },

    #' @description End the current cooldown period
    end_cooldown = function() {
      if (private$is_cooling_down) {
        private$is_cooling_down <- FALSE
        end_time <- Sys.time()

        # Update history with end time
        private$cooldown_history <- private$cooldown_history %>%
          mutate(
            end_time = if_else(
              is.na(end_time),
              end_time,
              end_time
            )
          )

        duration <- difftime(
          end_time,
          private$cooldown_start,
          units = "mins"
        )

        logger::log_info(
          "Cooldown period complete after {round(duration, 1)} minutes"
        )
      }
    },

    #' @description Get metrics for the monitored process
    get_process_metrics = function() {
      if (is.null(private$process_name)) {
        return(NULL)
      }

      # Find process by name
      proc <- tryCatch(
        {
          procs <- ps::ps_filter(ps::ps(), name = private$process_name)
          if (length(procs) > 0) procs[[1]] else NULL
        },
        error = function(e) {
          logger::log_warn(
            "Failed to find process {private$process_name}: {conditionMessage(e)}"
          )
          NULL
        })

      if (is.null(proc)) {
        return(NULL)
      }

      # Get process metrics
      tryCatch(
        {
          mem_info <- ps::ps_memory_info(proc)
          cpu_times <- ps::ps_cpu_times(proc)

          list(
            memory_rss = mem_info$rss,
            memory_vms = mem_info$vms,
            memory_percent = mem_info$rss / ps::ps_system_memory()$total * 100,
            cpu_user = cpu_times$user,
            cpu_system = cpu_times$system,
            num_threads = ps::ps_num_threads(proc)
          )
        },
        error = function(e) {
          logger::log_warn(
            "Failed to get process metrics: {conditionMessage(e)}"
          )
          NULL
        })
    }
  )
)