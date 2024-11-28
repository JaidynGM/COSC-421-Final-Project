library(dplyr)
library(tidyr)
library(igraph)
library(ggplot2)
library(tidytext)

setwd("C:/Users/jaidy/COSC421/COSC-421-Final-Project")

spotify_df <- read.csv("spotify-2023.csv", encoding = "latin1")

clean_artist_names <- function(name) {
  trimws(gsub("[^[:alnum:][:space:]&]", "", name))
}

analyze_collaborations <- function(data) {

  collaborations <- data %>% filter(artist_count > 1)
  
  artist_pairs <- data.frame(
    Artist_1 = character(),
    Artist_2 = character(),
    Track_Name = character(),
    Streams = numeric(),
    Released_Year = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(collaborations)) {

    artists <- tryCatch({
      artists <- trimws(unlist(strsplit(collaborations$artist.s._name[i], ",")))
      sapply(artists, clean_artist_names)
    }, error = function(e) {
      cat("Error processing artists in row", i, "\n")
      return(NULL)
    })
    
    if (!is.null(artists) && length(artists) > 1) {
      pairs <- combn(artists, 2, simplify = FALSE)

      for (pair in pairs) {
        tryCatch({
          new_row <- data.frame(
            Artist_1 = pair[1],
            Artist_2 = pair[2],
            Track_Name = collaborations$track_name[i],
            Streams = as.numeric(gsub(",", "", collaborations$streams[i])),
            Released_Year = as.numeric(collaborations$released_year[i]),
            stringsAsFactors = FALSE
          )
          artist_pairs <- rbind(artist_pairs, new_row)
        }, error = function(e) {
          cat("Error adding pair in row", i, ":", conditionMessage(e), "\n")
        })
      }
    }
  }
  
  if (nrow(artist_pairs) == 0) {
    stop("No valid artist pairs were created")
  }
  
  artist_pairs_summary <- artist_pairs %>%
    group_by(Artist_1, Artist_2) %>%
    summarize(
      Collaboration_Count = n(),
      Total_Streams = sum(Streams, na.rm = TRUE),
      Avg_Streams_Per_Collab = mean(Streams, na.rm = TRUE),
      First_Collab_Year = min(Released_Year, na.rm = TRUE),
      Latest_Collab_Year = max(Released_Year, na.rm = TRUE),
      Tracks = paste(Track_Name, collapse = "; "),
      .groups = 'drop'
    ) %>%
    arrange(desc(Collaboration_Count), desc(Total_Streams))
  
  return(artist_pairs_summary)
}

create_collaboration_network <- function(artist_pairs_summary) {

  graph <- graph_from_data_frame(
    select(artist_pairs_summary, Artist_1, Artist_2, Collaboration_Count),
    directed = FALSE
  )

  V(graph)$degree <- degree(graph, mode = "all")
  V(graph)$betweenness <- betweenness(graph, directed = FALSE)
  V(graph)$eigenvector <- eigen_centrality(graph)$vector
  V(graph)$community <- membership(cluster_louvain(graph))

  E(graph)$weight <- artist_pairs_summary$Collaboration_Count
  
  return(graph)
}

plot_collaboration_network <- function(graph, min_degree = 2) {

  subnet <- induced_subgraph(
    graph,
    V(graph)[degree(graph) >= min_degree]
  )
  
  layout <- layout_with_fr(subnet)

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

analyze_genres <- function(graph, artist_pairs_summary) {
  communities <- membership(cluster_louvain(graph))
  
  community_analysis <- data.frame(
    Artist = V(graph)$name,
    Community = communities
  ) %>%
    group_by(Community) %>%
    summarize(
      Artist_Count = n(),
      Top_Artists = paste(head(Artist, 3), collapse = ", ")
    )
  
  return(community_analysis)
}

analyze_emerging_artists <- function(graph, artist_pairs_summary) {
  
  emerging_artists <- artist_pairs_summary %>%
    group_by(Artist_1) %>%
    summarize(
      First_Appearance = min(First_Collab_Year),
      Num_Collabs = n(),
      Avg_Streams = mean(Total_Streams)
    ) %>%
    filter(First_Appearance >= max(First_Appearance) - 2) %>%
    arrange(desc(Avg_Streams))
  
  emerging_connections <- artist_pairs_summary %>%
    filter(Artist_1 %in% emerging_artists$Artist_1 |
             Artist_2 %in% emerging_artists$Artist_1) %>%
    arrange(desc(Total_Streams))
  
  return(list(
    emerging_artists = emerging_artists,
    emerging_connections = emerging_connections
  ))
}

generate_summary_stats <- function(graph, artist_pairs_summary) {

  network_stats <- list(
    num_artists = vcount(graph),
    num_collaborations = ecount(graph),
    density = edge_density(graph),
    avg_path_length = mean_distance(graph),
    modularity = modularity(cluster_louvain(graph)),
    transitivity = transitivity(graph)
  )

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
        summarize(
          Total_Streams = sum(Total_Streams),
          Avg_Streams = mean(Total_Streams)
        ) %>%
        rename(Artist = Artist_1),
      by = "Artist"
    ) %>%
    arrange(desc(Degree), desc(Betweenness))
  
  correlations <- cor(
    artist_stats %>% 
      select(Degree, Betweenness, Eigenvector, Total_Streams, Avg_Streams),
    use = "complete.obs"
  )
  
  return(list(
    network_stats = network_stats, 
    artist_stats = artist_stats,
    correlations = correlations
  ))
}

main_analysis <- function() {

  artist_pairs_summary <- analyze_collaborations(spotify_df)
  graph <- create_collaboration_network(artist_pairs_summary)
  stats <- generate_summary_stats(graph, artist_pairs_summary)
  community_analysis <- analyze_genres(graph, artist_pairs_summary)
  emerging_analysis <- analyze_emerging_artists(graph, artist_pairs_summary)
  
  plot_collaboration_network(graph, min_degree = 3)

  cat("\nNetwork Statistics:\n")
  print(stats$network_stats)
  
  cat("\nTop 10 Artists by Degree Centrality:\n")
  print(head(stats$artist_stats, 10))
  
  cat("\nCommunity Analysis:\n")
  print(community_analysis)
  
  cat("\nEmerging Artists Analysis:\n")
  print(head(emerging_analysis$emerging_artists, 10))
  
  cat("\nNetwork Metric Correlations:\n")
  print(stats$correlations)
  
  return(list(
    artist_pairs = artist_pairs_summary,
    graph = graph,
    stats = stats,
    community_analysis = community_analysis,
    emerging_analysis = emerging_analysis
  ))
}

results <- main_analysis()
