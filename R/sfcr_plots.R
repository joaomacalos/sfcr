#' Find cyclical nodes
#'
#' @param m adjacency matrix
#'
#' @importFrom expm `%^%`
#'
.return_loops <- function(m) {
  loops <- numeric()

  for (i in seq_len(nrow(m))) {
    loop_lengths <- diag(m %^% i)
    loops <- c(loops, which(loop_lengths > 0))
  }

  x <- unique(loops)

  return(x)
}


#' Create a \code{tbl_graph} object blocks and cycles information
#'
#' @inheritParams sfcr_baseline
#'
#' @return A \code{tbl_graph}
#'
#' @details This function creates a \code{tbl_graph} with information about
#' the blocks and cycles attached to it. This object can then be used to
#' plot the DAG of the model.
#'
#' @author João Macalós, \email{joaomacalos@@gmail.com}
#'
#' @export
#'
sfcr_dag_blocks <- function(equations) {
  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop("Package \"tidygraph\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package \"igraph\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  k1 <- .eq_as_tb(equations)
  k2 <- .add_time_stamps(k1)
  k3 <- .sfcr_find_adjacency(k2)

  blocks <- .find_blocks(k3)
  blocks <- tibble::enframe(blocks, value = "block")

  loops <- .return_loops(k3)
  loops <- tibble::tibble(name = rownames(k3)[loops], value = T)

  g1 <- igraph::graph_from_adjacency_matrix(t(k3), mode = "directed")
  g2 <- tidygraph::as_tbl_graph(g1)

  g <- suppressMessages(dplyr::left_join(g2, blocks))
  g <- suppressMessages(dplyr::left_join(g, loops)) %>%
    dplyr::mutate(value = dplyr::if_else(is.na(.data$value), F, .data$value))

  return(g)

}

#' Create a \code{tbl_graph} object with cycles information
#'
#' @inheritParams sfcr_baseline
#'
#' @return A \code{tbl_graph}
#'
#' @details This function creates a \code{tbl_graph} with information about
#' the cycles attached to it. This object can then be used to
#' plot the DAG of the model.
#'
#' @author João Macalós, \email{joaomacalos@@gmail.com}
#'
#' @export
#'
sfcr_dag_cycles <- function(equations) {
  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop("Package \"tidygraph\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package \"igraph\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  k1 <- .eq_as_tb(equations)
  k2 <- .add_time_stamps(k1)
  k3 <- .sfcr_find_adjacency(k2)

  loops <- .return_loops(k3)
  loops <- tibble::tibble(name = rownames(k3)[loops], value = T)


  g1 <- igraph::graph_from_adjacency_matrix(t(k3), mode = "directed")
  g2 <- tidygraph::as_tbl_graph(g1)

  g <- suppressMessages(dplyr::left_join(g2, loops)) %>%
    dplyr::mutate(value = dplyr::if_else(is.na(.data$value), F, .data$value))

  return(g)

}

#' Plot the DAG with blocks and cycles information
#'
#' @inheritParams sfcr_baseline
#' @param title Title of the plot.
#' @param size Size of the points.
#'
#' @return A \code{tbl_graph}
#'
#' @details This function creates a \code{tbl_graph} with information about
#' the cycles attached to it. This object can then be used to
#' plot the DAG of the model.
#'
#' @author João Macalós, \email{joaomacalos@@gmail.com}
#'
#'
#' @export
#'
sfcr_dag_blocks_plot <- function(equations, title = NULL, size = 15) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Packages \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("Packages \"ggraph\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("grDevices", quietly = TRUE)) {
    stop("Packages \"grDevices\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("Packages \"RColorBrewer\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  dag_blocks <- sfcr_dag_blocks(equations)

  nb_cols <- dag_blocks %>% tidygraph::activate('nodes') %>% dplyr::pull(.data$block) %>% unique() %>% length()
  mycolors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(nb_cols)


  ggraph::ggraph(dag_blocks, layout = "sugiyama") +
   ggraph::geom_node_point(ggplot2::aes(
     color = forcats::as_factor(.data$block),
     shape = forcats::fct_rev(forcats::as_factor(.data$value))),
     size = size,
     alpha = 0.8) +
    ggraph::geom_edge_link(
    edge_width = 0.5,
    start_cap = ggraph::circle(size/2.5, "mm"),
    end_cap = ggraph::circle(size/2.5, "mm"),
    arrow = ggplot2::arrow(type = "closed",
                 length = ggplot2::unit(3, "mm")),
    alpha = 0.2) +
    ggraph::geom_node_text(ggplot2::aes(label = .data$name)) +
    ggplot2::scale_color_manual(name = "Block", values = mycolors) +
    ggplot2::scale_shape_manual(name = "Cyclical", values = c(16, 15), drop = F) +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
         panel.grid.major = ggplot2::element_blank(),
         panel.grid.minor = ggplot2::element_blank(),
         axis.line = ggplot2::element_blank(),
         panel.background = ggplot2::element_blank(),
         legend.key = ggplot2::element_rect(colour = NA, fill = NA),
         legend.title = ggplot2::element_text(face = "bold")) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 5))) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(shape = c(21, 22), size = 5))) +
    ggplot2::labs(title = title)

}


#' Plot the DAG with cycles information
#'
#' @inheritParams sfcr_dag_blocks_plot
#'
#' @export
#'
sfcr_dag_cycles_plot <- function(equations, title = NULL, size = 15) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Packages \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("Packages \"ggraph\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  dag_loops <- sfcr_dag_cycles(equations)

  ggraph::ggraph(dag_loops, layout = "sugiyama") +
    ggraph::geom_node_point(ggplot2::aes(
      color = forcats::fct_rev(forcats::as_factor(.data$value))),
      size = size,
      alpha = 0.8) +
    ggraph::geom_edge_link(
      edge_width = 0.5,
      start_cap = ggraph::circle(size/2.5, "mm"),
      end_cap = ggraph::circle(size/2.5, "mm"),
      arrow = ggplot2::arrow(
        type = "closed",
        length = ggplot2::unit(3, "mm")),
        alpha = 0.2) +
    ggraph::geom_node_text(ggplot2::aes(label = .data$name)) +
    ggplot2::scale_color_manual(name = "Cyclical", values = c("tomato", "steelblue"), drop = F) +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.line = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          legend.background = ggplot2::element_blank(),
          legend.key = ggplot2::element_rect(colour = NA, fill = NA),
          legend.title = ggplot2::element_text(face = "bold")) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 5))) +
    ggplot2::labs(title = title)
}
