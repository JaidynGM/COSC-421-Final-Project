# Load required libraries
library(dplyr)
library(tidyr)
library(igraph)
library(ggplot2)
library(tidytext)

setwd("C:/Users/jaidy/COSC421/COSC-421-Final-Project") #change this to whatever directory the csv file is in

# Read and prepare data
spotify_df <- read.csv("spotify-2023.csv", encoding = "latin1")

# Function to clean artist names
clean_artist_names <- function(name) {
  trimws(gsub("[^[:alnum:][:space:]&]", "", name))
}

# Enhanced collaboration analysis
analyze_collaborations <- function(data) {
  # Filter for collaborations and create pairs
  collaborations <- data %>% filter(artist_count > 1)
  
  # Create empty dataframe for artist pairs
  artist_pairs <- data.frame(
    Artist_1 = character(),
    Artist_2 = character(),
    Track_Name = character(),
    Streams = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process each collaboration
  for (i in 1:nrow(collaborations)) {
    artists <- trimws(unlist(strsplit(collaborations$artist.s._name[i], ",")))
    artists <- sapply(artists, clean_artist_names)
    
    if (length(artists) > 1) {
      pairs <- combn(artists, 2, simplify = FALSE)
      
      for (pair in pairs) {
        artist_pairs <- rbind(artist_pairs, data.frame(
          Artist_1 = pair[1],
          Artist_2 = pair[2],
          Track_Name = collaborations$track_name[i],
          Streams = as.numeric(gsub(",", "", collaborations$streams[i]))
        ))
      }
    }
  }
  
  # Aggregate collaboration metrics
  artist_pairs_summary <- artist_pairs %>%
    group_by(Artist_1, Artist_2) %>%
    summarize(
      Collaboration_Count = n(),
      Total_Streams = sum(Streams, na.rm = TRUE),
      Avg_Streams_Per_Collab = mean(Streams, na.rm = TRUE),
      Tracks = paste(Track_Name, collapse = "; ")
    ) %>%
    arrange(desc(Collaboration_Count), desc(Total_Streams))
  
  return(artist_pairs_summary)
}

# Create and analyze network
create_collaboration_network <- function(artist_pairs_summary) {
  # Create graph
  graph <- graph_from_data_frame(
    select(artist_pairs_summary, Artist_1, Artist_2, Collaboration_Count),
    directed = FALSE
  )
  
  # Calculate network metrics
  V(graph)$degree <- degree(graph, mode = "all")
  V(graph)$betweenness <- betweenness(graph, directed = FALSE)
  V(graph)$eigenvector <- eigen_centrality(graph)$vector
  V(graph)$community <- membership(cluster_louvain(graph))
  
  # Calculate edge weight based on collaboration count
  E(graph)$weight <- artist_pairs_summary$Collaboration_Count
  
  return(graph)
}

# Visualization function
plot_collaboration_network <- function(graph, min_degree = 2) {
  # Filter to show only artists with minimum number of collaborations
  subnet <- induced_subgraph(
    graph,
    V(graph)[degree(graph) >= min_degree]
  )
  
  # Create layout
  layout <- layout_with_fr(subnet)
  
  # Plot with communities
  plot(subnet,
       layout = layout,
       vertex.size = sqrt(V(subnet)$degree) * 2,
       vertex.label = V(subnet)$name,
       vertex.label.cex = 0.6,
       vertex.label.dist = 0.8,
       vertex.color = factor(V(subnet)$community),
       edge.width = sqrt(E(subnet)$weight),
       main = "Artist Collaboration Network (Filtered)")
}

# Generate summary statistics
generate_summary_stats <- function(graph, artist_pairs_summary) {
  # Network-level metrics
  network_stats <- list(
    num_artists = vcount(graph),
    num_collaborations = ecount(graph),
    density = graph.density(graph),
    avg_path_length = mean_distance(graph),
    modularity = modularity(cluster_louvain(graph))
  )
  
  # Artist-level metrics
  artist_stats <- data.frame(
    Artist = V(graph)$name,
    Degree = degree(graph),
    Betweenness = betweenness(graph),
    Eigenvector = eigen_centrality(graph)$vector,
    Community = membership(cluster_louvain(graph))
  ) %>%
    left_join(
      artist_pairs_summary %>%
        group_by(Artist_1) %>%
        summarize(Total_Streams = sum(Total_Streams)) %>%
        rename(Artist = Artist_1),
      by = "Artist"
    ) %>%
    arrange(desc(Degree), desc(Betweenness))
  
  return(list(network_stats = network_stats, artist_stats = artist_stats))
}

# Main analysis execution
main_analysis <- function() {
  # Perform analysis
  artist_pairs_summary <- analyze_collaborations(spotify_df)
  graph <- create_collaboration_network(artist_pairs_summary)
  stats <- generate_summary_stats(graph, artist_pairs_summary)
  
  # Create visualizations
  plot_collaboration_network(graph, min_degree = 3)
  
  # Print summary statistics
  cat("\nNetwork Statistics:\n")
  print(stats$network_stats)
  
  cat("\nTop 10 Artists by Degree Centrality:\n")
  print(head(stats$artist_stats, 10))
  
  return(list(
    artist_pairs = artist_pairs_summary,
    graph = graph,
    stats = stats
  ))
}

# Run the analysis
results <- main_analysis()
