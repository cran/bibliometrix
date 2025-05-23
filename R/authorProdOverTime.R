utils::globalVariables(c(
  "AU", "n", "TC", "PY", "AU.x", "year", "Author",
  "TCpY", "freq"
))

#' Top-Authors' Productivity over Time
#'
#' It calculates and plots the author production (in terms of number of publications) over the time.
#' @param M is a bibliographic data frame obtained by \code{\link{convert2df}} function.
#' @param k is a integer. It is the number of top authors to analyze and plot. Default is \code{k = 10}.
#' @param graph is logical. If TRUE the function plots the author production over time graph. Default is \code{graph = TRUE}.
#' @return The function \code{authorProdOverTime} returns a list containing two objects:
#' \tabular{lll}{
#' \code{dfAU}  \tab   \tab is a data frame\cr
#' \code{dfpapersAU}\tab    \tab is a data frame\cr
#' \code{graph}   \tab   \tab a ggplot object}
#'
#' @examples
#' data(scientometrics, package = "bibliometrixData")
#' res <- authorProdOverTime(scientometrics, k = 10)
#' print(res$dfAU)
#' plot(res$graph)
#'
#' @seealso \code{\link{biblioAnalysis}} function for bibliometric analysis
#' @seealso \code{\link{summary}} method for class '\code{bibliometrix}'
#'
#' @export
#'
authorProdOverTime <- function(M, k = 10, graph = TRUE) {
  if (!("DI" %in% names(M))) {
    M$DI <- "NA"
  }
  M$TC <- as.numeric(M$TC)
  M$PY <- as.numeric(M$PY)
  M <- M[!is.na(M$PY), ] # remove rows with missing value in PY

  Y <- as.numeric(substr(Sys.time(), 1, 4))
  listAU <- (strsplit(M$AU, ";"))
  nAU <- lengths(listAU)
  df <- data.frame(AU = trimws(unlist(listAU)), SR = rep(M$SR, nAU))
  AU <- df %>%
    group_by(AU) %>%
    count() %>%
    arrange(desc(n)) %>%
    ungroup()
  k <- min(k, nrow(AU))
  AU <- AU %>%
    slice_head(n = k)

  df <- df %>%
    right_join(AU, by = "AU") %>%
    left_join(M, by = "SR") %>%
    select("AU.x", "PY", "TI", "SO", "DI", "TC") %>%
    mutate(TCpY = TC / (Y - PY + 1)) %>%
    group_by(AU.x) %>%
    mutate(n = length(AU.x)) %>%
    ungroup() %>%
    rename(
      Author = AU.x,
      year = PY,
      DOI = DI
    ) %>%
    arrange(desc(n), desc(year)) %>%
    select(-n)

  df2 <- dplyr::group_by(df, Author, year) %>%
    dplyr::summarise(freq = length(year), TC = sum(TC), TCpY = sum(TCpY)) %>%
    as.data.frame()

  df2$Author <- factor(df2$Author, levels = AU$AU[1:k])

  x <- c(0.5, 1.5 * k / 10)
  y <- c(min(df$year), min(df$year) + diff(range(df2$year)) * 0.125)

  data("logo", envir = environment())
  logo <- grid::rasterGrob(logo, interpolate = TRUE)

  g <- ggplot(df2, aes(x = Author, y = year, text = paste("Author: ", Author, "\nYear: ", year, "\nN. of Articles: ", freq, "\nTotal Citations per Year: ", round(TCpY, 2)))) +
    geom_point(aes(alpha = TCpY, size = freq), color = "dodgerblue4") +
    scale_size(range = c(2, 6)) +
    scale_alpha(range = c(0.3, 1)) +
    scale_y_continuous(breaks = seq(min(df2$year), max(df2$year), by = 2)) +
    guides(size = guide_legend(order = 1, "N.Articles"), alpha = guide_legend(order = 2, "TC per Year")) +
    theme(
      legend.position = "right",
      text = element_text(color = "#444444"),
      panel.background = element_rect(fill = "#FFFFFF"),
      plot.title = element_text(size = 24),
      axis.title = element_text(size = 14, color = "#555555"),
      axis.title.y = element_text(vjust = 1, angle = 90) # , face="bold")
      , axis.title.x = element_text(hjust = .95) # ,face="bold")
      , axis.text.x = element_text(face = "bold", angle = 90),
      axis.text.y = element_text(face = "bold")
      # ,axis.line.x = element_line(color="black", size=1)
      , axis.line.x = element_line(color = "grey50", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = .2, color = "grey90")
    ) +
    # coord_fixed(ratio = 2/1) +
    labs(
      title = "Authors' Production over Time",
      x = "Author",
      y = "Year"
    ) +
    geom_line(data = df2, aes(x = Author, y = year, group = Author), size = 1.0, color = "firebrick4", alpha = 0.3) +
    scale_x_discrete(limits = rev(levels(df2$Author))) +
    coord_flip() +
    annotation_custom(logo, xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[2])


  df$DOI <- as.character(df$DOI)
  res <- list(dfAU = df2, dfPapersAU = df, graph = g)
  if (isTRUE(graph)) {
    plot(g)
  }
  return(res)
}
