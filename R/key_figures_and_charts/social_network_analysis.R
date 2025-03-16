# ============================================================================
# Social Network Analysis Figures for Paper using REAL data
# ============================================================================

library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(tealodiviz)  # ODI theming
library(here)
library(Cairo)
library(ggrepel)

# Create output directories
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "data"), recursive = TRUE, showWarnings = FALSE)

# Helper functions
save_plot <- function(plot, filename, width = 10, height = 8, dpi = 300) {
  # Save as PNG
  ggsave(
    filename = here("output", "figures", paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
  
  # Try to save as EPS using Cairo
  tryCatch({
    ggsave(
      filename = here("output", "figures", paste0(filename, ".eps")),
      plot = plot,
      width = width,
      height = height,
      device = cairo_ps
    )
    message(paste0("Saved: ", filename, " in PNG and EPS formats"))
  }, error = function(e) {
    # Fall back to PDF if EPS fails
    warning("EPS save failed, falling back to PDF: ", e$message)
    ggsave(
      filename = here("output", "figures", paste0(filename, ".pdf")),
      plot = plot,
      width = width,
      height = height,
      device = cairo_pdf
    )
    message(paste0("Saved: ", filename, " in PNG and PDF formats"))
  })
}

save_data <- function(data, filename) {
  write_csv(
    x = data,
    file = here("output", "data", paste0(filename, ".csv"))
  )
  message(paste0("Saved data: ", filename, ".csv"))
}

# ============================================================================
# Load the actual data and rebuild networks
# ============================================================================

# Load the SNA dataset 
sna_data_path <- here("data", "social_network_analysis", "data_for_social_network_analysis.rds")
if(file.exists(sna_data_path)) {
  sna_data_nested <- readRDS(sna_data_path)
  # Unnest the data to get lender combinations
  data_for_sna <- sna_data_nested %>%
    unnest(unique_lenders) %>%
    unique()
} else {
  stop("SNA data file not found. Please check the path: ", sna_data_path)
}

# Function to build cofinancing network from real data
build_cofinancing_network <- function(data, transaction_class = NULL, top_n = 20, min_collabs = 2) {
  # Optionally filter to a specific transaction class
  if (!is.null(transaction_class)) {
    data <- data %>% filter(transaction_primary_class == transaction_class)
  }
  
  # Create edge list
  edges <- data %>%
    select(transaction_id, standardized_lender_name) %>%
    distinct() %>%
    group_by(transaction_id) %>%
    # Only consider transactions with at least 2 lenders
    filter(n() >= 2) %>%
    summarise(
      pairs = list(combn(standardized_lender_name, 2, simplify = FALSE)),
      .groups = "drop"
    ) %>%
    unnest(pairs) %>%
    mutate(
      from = map_chr(pairs, 1),
      to   = map_chr(pairs, 2)
    ) %>%
    select(from, to) %>%
    # Ensure edge is undirected by sorting lender names
    mutate(
      from = pmin(from, to),
      to   = pmax(from, to)
    ) %>%
    group_by(from, to) %>%
    summarise(weight = n(), .groups = "drop")
  
  # Build node data
  nodes <- data %>%
    distinct(standardized_lender_name, lender_geography, lender_profit_orientation)
  
  # Create tidygraph object (undirected network)
  graph <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
  
  # Compute centrality measures
  graph <- graph %>%
    mutate(
      degree      = centrality_degree(),
      betweenness = centrality_betweenness(),
      closeness   = centrality_closeness()
    )
  
  # Add constraint measure
  graph_igraph <- as.igraph(graph)
  V(graph_igraph)$constraint <- constraint(graph_igraph)
  
  # Add constraint back to tidygraph object
  graph <- graph %>%
    activate(nodes) %>%
    mutate(constraint = V(graph_igraph)$constraint)
  
  # Filter to top_n nodes by degree
  node_data <- graph %>% activate(nodes) %>% as_tibble()
  top_lenders <- node_data %>%
    arrange(desc(degree)) %>%
    slice_head(n = top_n)
  
  graph_filtered <- graph %>%
    activate(nodes) %>%
    filter(standardized_lender_name %in% top_lenders$standardized_lender_name) %>%
    convert(to_subgraph)
  
  # Filter edges by minimum collaborations
  graph_filtered <- graph_filtered %>%
    activate(edges) %>%
    filter(weight >= min_collabs) %>%
    convert(to_subgraph)
  
  return(graph_filtered)
}

# Build the networks from real data
network_overall <- build_cofinancing_network(data_for_sna, top_n = 20, min_collabs = 2)
network_green <- build_cofinancing_network(data_for_sna, transaction_class = "GREEN", top_n = 20, min_collabs = 1)

# ============================================================================
# Figure 1: Green Financing Network (with REAL data)
# ============================================================================

create_green_network_figure <- function(graph = network_green) {
  # Extract data for CSV export
  node_data <- graph %>% 
    activate(nodes) %>%
    as_tibble()
  
  edge_data <- graph %>%
    activate(edges) %>%
    as_tibble()
  
  # Save the data for transparency
  save_data(node_data, "green_network_nodes")
  save_data(edge_data, "green_network_edges")
  
  # Create an improved network visualization addressing the designer's comments
  p_green <- graph %>%
    ggraph(layout = "fr") +
    # Draw edges with enhanced visibility
    geom_edge_link(
      aes(width = weight, alpha = weight),
      color = "gray40", 
      lineend = "round"
    ) +
    # Draw nodes with improved coloring
    geom_node_point(
      aes(
        color = case_when(
          lender_geography == "Chinese" ~ "Chinese",
          lender_profit_orientation == "Policy Driven" ~ "Non-Chinese: Policy Driven",
          lender_profit_orientation == "Profit Maximizing" ~ "Non-Chinese: Profit Maximising",
          TRUE ~ "Other"
        ), 
        shape = lender_profit_orientation,
        size = degree
      ),
      stroke = 0.8
    ) +
    # Add clearer text labels
    geom_node_text(
      aes(label = standardized_lender_name), 
      repel = TRUE, 
      size = 4,
      fontface = "bold",
      bg.color = "white",
      bg.r = 0.15,
      force = 3,  # Stronger repulsion for better separation
      max.overlaps = 20
    ) +
    # Use ODI theme colors with categorical distinctions
    scale_color_manual(values = c(
      "Chinese" = odi_colors$odi_red,
      "Non-Chinese: Policy Driven" = odi_colors$odi_blue,
      "Non-Chinese: Profit Maximising" = odi_colors$odi_yellow,
      "Other" = "gray60"
    )) +
    scale_shape_manual(values = c(
      "Mixed Mandate" = 17,        # triangle
      "Policy Driven" = 18,        # diamond
      "Profit Maximizing" = 16     # circle
    )) +
    scale_edge_width(range = c(0.5, 3)) +
    scale_edge_alpha(range = c(0.4, 0.9)) +
    # Caption only for export version
    labs(
      caption = "Source: Authors' calculations based on AidData GCDF 3.0, 2000-2021",
      color = "Lender (Geography & Profit Orientation)",
      shape = "Profit orientation",
      size = "Degree"
    ) +
    # Clean theme
    theme_void() +
    theme(
      plot.caption = element_text(hjust = 1, size = 10),
      legend.position = "right",
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 8),
      legend.box.spacing = unit(0.5, "cm"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Save the improved plot
  save_plot(p_green, "fig_green_financing_network", width = 14, height = 12)
  
  return(p_green)
}

# ============================================================================
# Figure 2: Most Active Banks in Syndication
# ============================================================================

create_most_active_banks_figure <- function() {
  # Filter for syndicated loans using the correct column
  bank_counts <- data_for_sna %>%
    filter(number_of_lenders == "Syndicated Loan") %>%
    count(standardized_lender_name, lender_geography) %>%
    arrange(desc(n))
  
  # Save full data before filtering to top 20
  save_data(bank_counts, "all_syndicated_banks_data")
  
  # Check Standard Bank's position
  standard_bank_position <- which(grepl("Standard Bank", bank_counts$standardized_lender_name))
  print(paste("Standard Bank position in full ranking:", standard_bank_position))
  
  # Get top 20 for visualization
  top_banks <- bank_counts %>%
    head(20)
  
  # Add region categorization
  bank_data <- top_banks %>%
    mutate(
      region = case_when(
        lender_geography == "Chinese" ~ "Chinese",
        grepl("Sumitomo|Mitsubishi|Mizuho", standardized_lender_name) ~ "Japanese",
        grepl("Citi|JPMorgan|Morgan|America", standardized_lender_name) ~ "US",
        grepl("Standard Bank|Africa", standardized_lender_name) ~ "African",
        TRUE ~ "European"  # Default for other non-Chinese banks
      )
    )
  
  # Also specifically find African banks in the full dataset
  african_banks <- bank_counts %>%
    filter(grepl("Africa|Standard Bank|Ecobank|Nedbank|FirstRand|ABSA", 
                 standardized_lender_name)) %>%
    mutate(region = "African")
  
  print("African banks in syndicated lending:")
  print(african_banks)
  
  # Save the data for the chart
  save_data(bank_data, "most_active_banks_data")
  
  # Create the plot addressing designer's comments
  p_banks <- bank_data %>%
    # Rename the count column for clarity
    rename(transactions = n) %>%
    # Create the plot
    ggplot(
      aes(
        x = transactions,
        y = reorder(standardized_lender_name, transactions),
        fill = region
      )
    ) +
    geom_col() +
    # Use ODI colors for regions
    scale_fill_manual(values = c(
      "Chinese" = odi_colors$odi_red,
      "Japanese" = "#E69F00",
      "European" = odi_colors$odi_blue,
      "US" = "#4D4D4D",  # Dark gray for US
      "African" = odi_colors$odi_yellow
    )) +
    # Format x-axis with initial caps on first word only
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(0, max(bank_data$transactions) * 1.05)
    ) +
    # Add appropriate labels - following designer's comments
    labs(
      x = "Number of transactions",
      y = NULL,
      fill = "Region",
      caption = "Source: Authors' calculations based on AidData GCDF 3.0, 2000-2021"
    ) +
    # Apply ODI theme with enhancements
    theme_odi() +
    theme(
      legend.position = "right",
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(face = "bold", hjust = 1),
      axis.text.x = element_text(size = 9),
      plot.margin = margin(10, 20, 10, 10)
    )
  
  # Save the plot
  save_plot(p_banks, "fig_most_active_banks", width = 12, height = 8)
  
  return(p_banks)
}


green_network_fig <- create_green_network_figure()
print(green_network_fig)

most_active_banks_fig <- create_most_active_banks_figure()
print(most_active_banks_fig)