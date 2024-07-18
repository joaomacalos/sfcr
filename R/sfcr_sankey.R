#' Plot Sankey's diagram representation of transactions-flow matrix
#'
#' @param tfm A transactions-flow matrix
#' @param baseline A baseline model
#' @param when When the Sankey's diagram should be evaluated?
#'  * "start": Fifth and fourth periods.
#'  * "end": last two periods of the simulation (stationary state).
#'
#' @author João Macalós
#'
#' @export
#'
sfcr_sankey <- function(tfm, baseline, when = "start") {
  if (!requireNamespace("networkD3", quietly = TRUE)) {
    stop("Packages \"networkD3\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  abortifnot(inherits(baseline, "sfcr_tbl"), "Please supply a valid baseline model.")


  when <- match.arg(when, c("start", "end"))

  bl1 <- attr(baseline, "calls")
  bl2 <- attr(baseline, "external")$lhs
  bl3 <- attr(baseline, "matrix")

  if (when == "start") {
    m <- bl3[4:5, ]
  } else {
    nr <- nrow(bl3)
    m <- bl3[(nr - 1):nr, ]
    rownames(m) <- NULL
  }

  # First column must be named "name"
  nms <- colnames(tfm)
  colnames(tfm) <- c("name", nms[2:length(nms)])


  # Convert entries to valid expressions
  tfm <- tfm %>%
    dplyr::mutate(dplyr::across(-1, ~ stringr::str_replace_all(.x, "d\\((.*?)\\)", "\\(\\1 - \\1\\[-1\\]\\)"))) %>%
    dplyr::mutate(dplyr::across(-1, ~ .add_time2(.x))) %>%
    dplyr::mutate(dplyr::across(-1, ~ gsub(.pvar(bl1$lhs), "m\\[2,'\\1'\\]", .x, perl = T))) %>%
    dplyr::mutate(dplyr::across(-1, ~ gsub(.pvar(bl2), "m\\[2,'\\1'\\]", .x, perl = T))) %>%
    dplyr::mutate(dplyr::across(-1, ~ gsub(.pvarlag(bl1$lhs), "m\\[1,'\\1'\\]", .x, perl = T))) %>%
    dplyr::mutate(dplyr::across(-1, ~ gsub(.pvarlag(bl2), "m\\[1,'\\1'\\]", .x, perl = T))) %>%
    dplyr::mutate(dplyr::across(-1, ~ gsub("___", "", .x)))

  .eval_matrices <- function(x) {
    purrr::map(x, function(y) {
      if (stringr::str_length(y) == 0) {
        NA_real_
      } else {
        eval(str2expression(y))
      }
    })
  }

  # Evaluate values
  tfm <- tfm %>%
    dplyr::mutate(dplyr::across(-1, ~ .eval_matrices(.x)))

  # Reshape
  tfm <- tfm %>%
    tidyr::pivot_longer(cols = -.data$name, names_to = "sector") %>%
    dplyr::mutate(value = as.numeric(.data$value))

  # Positive entries
  tfm_plus <- dplyr::filter(tfm, .data$value > 0)

  # Negative entries
  tfm_minus <- dplyr::filter(tfm, .data$value < 0)

  # Nodes tibble
  nodes <- tibble::tibble(
    name = c(
      unique(tfm$name),
      unique(tfm$sector),
      paste0(unique(tfm$sector), "1")
    ),
    node = 0:(length(.data$name) - 1)
  )

  # Links minus
  links_minus <- tfm_minus %>%
    dplyr::mutate(value = -1 * .data$value) %>%
    dplyr::left_join(nodes, by = c("name")) %>%
    dplyr::left_join(nodes, by = c("sector" = "name")) %>%
    dplyr::rename(source = .data$node.y, target = .data$node.x)

  # Links plus
  links_plus <- tfm_plus %>%
    dplyr::mutate(sector = paste0(.data$sector, 1)) %>%
    dplyr::left_join(nodes, by = c("name")) %>%
    dplyr::left_join(nodes, by = c("sector" = "name")) %>%
    dplyr::rename(source = .data$node.x, target = .data$node.y)

  links <- dplyr::bind_rows(links_minus, links_plus) %>%
    dplyr::select(.data$source, .data$target, .data$value)

  nodes <- nodes %>%
    dplyr::mutate(name = gsub("1", "", .data$name))

  networkD3::sankeyNetwork(
    Links = as.data.frame(links),
    Nodes = as.data.frame(nodes),
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "name",
    units = "dollars",
    fontSize = 14
  )
}
