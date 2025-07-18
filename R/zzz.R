# utils::globalVariables("where")
# utils::globalVariables("any_of")
utils::globalVariables(c("matches", "KW_Merged"))

#' @import stats
#' @import dimensionsR
#' @import ggplot2
#' @import bibliometrixData
#' @import forcats
#' @import ggrepel
#' @import shinycssloaders
#' @import pubmedR
#' @import shiny
#' @import readr
#' @import readxl
#' @import stringi
#' @import tidytext
#' @import openalexR
#' @import ca
#' @import visNetwork
#' @importFrom purrr map2_dfr
#' @importFrom purrr map_dfr
#' @importFrom purrr map_df
#' @importFrom purrr map_chr
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_cols
#' @importFrom dplyr across
#' @importFrom dplyr row_number
#' @importFrom dplyr tibble
#' @importFrom dplyr as_tibble
#' @importFrom dplyr between
#' @importFrom dplyr case_when
#' @importFrom dplyr if_else
#' @importFrom dplyr lead
#' @importFrom dplyr pull
#' @importFrom dplyr arrange
#' @importFrom dplyr do
#' @importFrom dplyr distinct
#' @importFrom dplyr join_by
#' @importFrom dplyr n
#' @importFrom dplyr slice
#' @importFrom dplyr slice_max
#' @importFrom dplyr if_all
#' @importFrom dplyr any_of
#' @importFrom dplyr cummean
#' @importFrom dplyr count
#' @importFrom dplyr desc
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr mutate_if
#' @importFrom dplyr mutate_all
#' @importFrom dplyr ungroup
#' @importFrom dplyr rename
#' @importFrom dplyr rename_with
#' @importFrom dplyr rowwise
#' @importFrom dplyr summarise
#' @importFrom dplyr summarize
#' @importFrom dplyr select
#' @importFrom dplyr anti_join
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr right_join
#' @importFrom dplyr top_n
#' @importFrom dplyr relocate
#' @importFrom dplyr slice_head
#' @importFrom dplyr slice_tail
#' @importFrom dplyr reframe
#' @importFrom plotly add_annotations
#' @importFrom plotly add_lines
#' @importFrom plotly config
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#' @importFrom plotly plot_ly
#' @importFrom plotly subplot
#' @importFrom plotly toRGB
#' @importFrom tibble rownames_to_column
# #' @import shinycssloaders
# #' @import shinythemes
#' @importFrom openxlsx write.xlsx
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr replace_na
#' @importFrom tidyr separate
#' @importFrom tidyr drop_na
#' @importFrom tidyr unite
#' @importFrom tidyr starts_with
#' @importFrom tidyr unnest
#' @importFrom tidyr unite
#' @importFrom graphics abline
#' @importFrom grDevices adjustcolor
#' @importFrom grDevices dev.off
#' @importFrom grDevices pdf
#' @importFrom grDevices chull
#' @importFrom grDevices heat.colors
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom stringdist stringdistmatrix
#' @importFrom rscopus affiliation_retrieval
#' @importFrom rscopus author_df_orig
#' @importFrom rscopus author_search
#' @importFrom rscopus get_complete_author_info
#' @importFrom igraph as_long_data_frame
#' @importFrom igraph get.edgelist
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph as_adjacency_matrix
#' @importFrom igraph graph.adjacency
#' @importFrom igraph degree
#' @importFrom igraph plot.igraph
#' @importFrom igraph delete.vertices
#' @importFrom igraph decompose.graph
#' @importFrom igraph E
#' @importFrom igraph E<-
#' @importFrom igraph V
#' @importFrom igraph V<-
#' @importFrom igraph vcount
#' @importFrom igraph graph_attr
#' @importFrom igraph edge_density
#' @importFrom igraph transitivity
#' @importFrom igraph diameter
#' @importFrom igraph degree_distribution
#' @importFrom igraph centr_degree
#' @importFrom igraph centr_clo
#' @importFrom igraph centr_betw
#' @importFrom igraph centr_eigen
#' @importFrom igraph mean_distance
#' @importFrom igraph closeness
#' @importFrom igraph induced_subgraph
#' @importFrom igraph page.rank
#' @importFrom igraph eigen_centrality
#' @importFrom igraph arpack_defaults
#' @importFrom igraph authority_score
#' @importFrom igraph page_rank
#' @importFrom igraph hub_score
#' @importFrom igraph graph_from_incidence_matrix
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph simplify
#' @importFrom igraph layout.auto
#' @importFrom igraph layout.circle
#' @importFrom igraph layout.sphere
#' @importFrom igraph layout.mds
#' @importFrom igraph layout.kamada.kawai
#' @importFrom igraph layout.fruchterman.reingold
#' @importFrom igraph layout.star
#' @importFrom igraph write.graph
#' @importFrom igraph cluster_walktrap
#' @importFrom igraph cluster_leiden
#' @importFrom igraph cluster_optimal
#' @importFrom igraph cluster_infomap
#' @importFrom igraph cluster_edge_betweenness
#' @importFrom igraph cluster_fast_greedy
#' @importFrom igraph cluster_louvain
#' @importFrom igraph cluster_leading_eigen
#' @importFrom igraph cluster_spinglass
#' @importFrom igraph count_multiple
#' @importFrom igraph membership
#' @importFrom igraph layout.norm
#' @importFrom igraph delete.edges
#' @importFrom igraph betweenness
#' @importFrom Matrix %&%
#' @importFrom Matrix abIseq
#' @importFrom Matrix abIseq1
#' @importFrom Matrix all.equal
#' @importFrom Matrix anyDuplicatedT
#' @importFrom Matrix Arith
#' @importFrom Matrix as.array
#' @importFrom Matrix as.matrix
#' @importFrom Matrix band
#' @importFrom Matrix bandSparse
#' @importFrom Matrix bdiag
#' @importFrom Matrix cbind2
#' @importFrom Matrix chol
#' @importFrom Matrix chol2inv
#' @importFrom Matrix Cholesky
#' @importFrom Matrix coerce
#' @importFrom Matrix colMeans
#' @importFrom Matrix colSums
#' @importFrom Matrix Compare
#' @importFrom Matrix condest
#' @importFrom Matrix cov2cor
#' @importFrom Matrix crossprod
#' @importFrom Matrix det
#' @importFrom Matrix determinant
#' @importFrom Matrix diag
#' @importFrom Matrix diag<-
#' @importFrom Matrix diagN2U
#' @importFrom Matrix Diagonal
#' @importFrom Matrix diagU2N
#' @importFrom Matrix diff
#' @importFrom Matrix drop
#' @importFrom Matrix drop0
#' @importFrom Matrix expand
#' @importFrom Matrix expm
#' @importFrom Matrix fac2sparse
#' @importFrom Matrix forceSymmetric
#' @importFrom Matrix format
#' @importFrom Matrix formatSparseM
#' @importFrom Matrix formatSpMatrix
#' @importFrom Matrix head
#' @importFrom Matrix image
#' @importFrom Matrix invPerm
#' @importFrom Matrix is.null.DN
#' @importFrom Matrix isDiagonal
#' @importFrom Matrix isLDL
#' @importFrom Matrix isSymmetric
#' @importFrom Matrix isTriangular
#' @importFrom Matrix kronecker
#' @importFrom Matrix Logic
#' @importFrom Matrix lu
#' @importFrom Matrix Math
#' @importFrom Matrix Math2
#' @importFrom Matrix Matrix
#' @importFrom Matrix MatrixClass
#' @importFrom Matrix mean
#' @importFrom Matrix nnzero
#' @importFrom Matrix norm
#' @importFrom Matrix onenormest
#' @importFrom Matrix Ops
#' @importFrom Matrix pack
#' @importFrom Matrix print
#' @importFrom Matrix printSpMatrix
#' @importFrom Matrix printSpMatrix2
#' @importFrom Matrix qr
#' @importFrom Matrix qr.coef
#' @importFrom Matrix qr.fitted
#' @importFrom Matrix qr.Q
#' @importFrom Matrix qr.qty
#' @importFrom Matrix qr.qy
#' @importFrom Matrix qr.R
#' @importFrom Matrix qr.resid
#' @importFrom Matrix qrR
#' @importFrom Matrix rankMatrix
#' @importFrom Matrix rbind2
#' @importFrom Matrix rcond
#' @importFrom Matrix readHB
#' @importFrom Matrix readMM
#' @importFrom Matrix rep2abI
#' @importFrom Matrix rowMeans
#' @importFrom Matrix rowSums
#' @importFrom Matrix rsparsematrix
#' @importFrom Matrix show
#' @importFrom Matrix skewpart
#' @importFrom Matrix solve
#' @importFrom Matrix sparse.model.matrix
#' @importFrom Matrix sparseMatrix
#' @importFrom Matrix sparseVector
#' @importFrom Matrix spMatrix
#' @importFrom Matrix summary
#' @importFrom Matrix Summary
#' @importFrom Matrix symmpart
#' @importFrom Matrix t
#' @importFrom Matrix tail
#' @importFrom Matrix tcrossprod
#' @importFrom Matrix tril
#' @importFrom Matrix triu
#' @importFrom Matrix uniqTsparse
#' @importFrom Matrix unpack
#' @importFrom Matrix update
#' @importFrom Matrix updown
#' @importFrom Matrix which
#' @importFrom Matrix writeMM
#' @importFrom stringr str_locate_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom stringr str_split 
#' @importFrom graphics barplot
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics plot
#' @importFrom graphics par
#' @importFrom grDevices colorRampPalette
#' @importFrom grDevices as.raster
#' @importFrom utils capture.output
#' @importFrom utils data
#' @importFrom utils adist
#' @importFrom utils read.csv
#' @importFrom utils write.csv
#' @importFrom SnowballC wordStem
#' @importFrom SnowballC getStemLanguages
# @importFrom rio import
.onAttach <- function(...) {
  packageStartupMessage("Please note that our software is open source and available for use, distributed under the MIT license.\nWhen it is used in a publication, we ask that authors properly cite the following reference:\n\nAria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis,
                        Journal of Informetrics, 11(4), pp 959-975, Elsevier.\n\nFailure to properly cite the software is considered a violation of the license.
                        \nFor information and bug reports:
                        - Take a look at https://www.bibliometrix.org
                        - Send an email to info@bibliometrix.org
                        - Write a post on https://github.com/massimoaria/bibliometrix/issues
                        \nHelp us to keep Bibliometrix and Biblioshiny free to download and use by contributing with a small donation to support our research team (https://bibliometrix.org/donate.html)\n
                        \nTo start with the Biblioshiny app, please digit:
biblioshiny()\n")
}


# ### scale coordinates
# ### Credits to François Briatte. Function is a fork of the package ggnetwork
scale_data <- function(x, scale = diff(range(x))) {
  if (!scale) {
    x <- rep(0.5, length.out = length(x))
  } else {
    x <- scale(x, center = min(x), scale = scale)
  }
  as.vector(x)
}


# ## Plot edges using ggplot2
# ### Credits to François Briatte. Function is a fork of the package ggnetwork
geom_network_edges <- function(
    mapping = NULL,
    data = NULL,
    position = "identity",
    arrow = NULL,
    curvature = 0,
    angle = 90,
    ncp = 5,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    ...) {
  if (!curvature) {
    geom <- ggplot2::GeomSegment
    params <- list(arrow = arrow, na.rm = na.rm, ...)
  } else {
    geom <- ggplot2::GeomCurve
    params <- list(
      arrow = arrow,
      curvature = curvature,
      angle = angle,
      ncp = ncp,
      na.rm = na.rm,
      ...
    )
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatEdges,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

# ### Credits to François Briatte. Function is a fork of the package ggnetwork
geom_network_nodes <- function(
    mapping = NULL,
    data = NULL,
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNodes,
    geom = ggplot2::GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
#
# ### Credits to François Briatte. Function is a fork of the package ggnetwork
StatEdges <- ggplot2::ggproto("StatEdges", ggplot2::Stat,
  compute_layer = function(data, scales, params) {
    unique(subset(data, !(x == xend & y == yend)))
  }
)

### Credits to François Briatte. Function is a fork of the package ggnetwork
StatNodes <- ggplot2::ggproto("StatNodes", ggplot2::Stat,
  compute_layer = function(data, scales, params) {
    if (all(c("xend", "yend") %in% names(data))) {
      unique(subset(data, select = c(-xend, -yend)))
    } else {
      unique(data)
    }
  }
)

### Credits to Patrick Barks. Function is a fork of the package Abbrev
AbbrevTerm <- function(x, check = TRUE) {
  if (check == FALSE) {
    out <- x
  } else {
    # check for invalid x
    if (length(x) > 1) {
      stop("Please provide a single string (length(x) should equal 1)")
    } else if (grepl("[[:space:]]", x)) {
      warning("The provided string contains spaces. This function is only designed to abbreviate a single word.")
    }

    # check whether x is title case
    ch1 <- substr(x, 1, 1)
    is_title <- ch1 == toupper(ch1)

    # check for whole word match
    # TODO: find way to deal with multiple whole word matches (e.g. w/ and w/o diacritics)
    int_whole_match <- stringi::stri_cmp_equiv(x, ltwa_singles$WORD, strength = 1)
    ind_whole_match <- which(int_whole_match == TRUE)

    if (length(ind_whole_match > 0)) {
      # if whole-word match is found, return corresponding abbrev (if no abbrev,
      #  return original x)
      abbrev <- ltwa_singles$ABBREVIATIONS[ind_whole_match]
      word <- ltwa_singles$WORD[ind_whole_match]
      out <- ifelse(is.na(abbrev), word, abbrev)
    } else {
      # else, find matching prefix
      # TODO: give preference to matches with higher strength
      lgl_prefix <- stringi::stri_startswith_coll(x, ltwa_prefix$WORD, strength = 1)
      ind_prefix <- which(lgl_prefix == TRUE)

      # choose abbrev based on number of matching prefixes (0, 1, or 2+)
      if (length(ind_prefix) == 0) {
        # if no matching prefixes, return original x
        out <- x
      } else if (length(ind_prefix) == 1) {
        # if one matching prefixes, return corresponding abbrev
        out <- ltwa_prefix$ABBREVIATIONS[ind_prefix]
        if (is.na(out)) out <- x
      } else {
        # if multiple matching prefixes, choose abbrev from longest match
        ind_prefix <- ind_prefix[which.max(nchar(ltwa_prefix$WORD[ind_prefix]))]
        out <- ltwa_prefix$ABBREVIATIONS[ind_prefix]
      }
    }

    # if x contains only latin chars, remove any diacritics from abbrev
    if (stringi::stri_trans_general(x, "Latin-ASCII") == x) {
      out <- stringi::stri_trans_general(out, "Latin-ASCII")
    }

    # if x is title case, convert out to title case
    if (is_title == TRUE) {
      out <- ToTitleCase(out)
    }
  }

  # return
  return(out)
}


### Credits to Patrick Barks. Function is a fork of the package Abbrev
AbbrevTitle <- function(x) {
  ind <- rank(-nchar(ltwa_phrase$WORD), ties.method = "last")
  ltwa_phrase[ind, ] <- ltwa_phrase

  # check for invalid x
  if (length(x) > 1) {
    stop("Please provide a single string (length(x) should equal 1)")
  }

  # remove parentheses and punctuation, and split string into vector of
  #  individual words/terms
  x <- gsub("[[:space:]]\\(.+\\)|\\,|\\.", "", x)
  xv <- unlist(strsplit(x, " "))

  # if title contains only one word, return that word
  if (length(xv) == 1) {
    out <- ToTitleCase(xv)
  } else {
    # otherwise... check for matching multi-word phrases

    # vector to track which elements of xv already abbreviated
    xv_which_abb <- logical(length(xv))

    # search for multi-word matches (e.g. South Pacific)
    lgl_phrase <- sapply(ltwa_phrase$WORD, function(y) grepl(y, tolower(x)), USE.NAMES = FALSE)
    ind_phrase <- which(lgl_phrase)

    # if any matching multi-word phrases
    if (length(ind_phrase) > 0) {
      for (i in 1:length(ind_phrase)) {
        match_phrase <- unlist(strsplit(ltwa_phrase$WORD[ind_phrase[i]], "[[:space:]]"))
        match_abb <- ltwa_phrase$ABBREVIATIONS[ind_phrase[i]]
        ind_match <- sapply(match_phrase, function(y) grep(y, tolower(xv)), USE.NAMES = FALSE) # should only find sequential matches
        if (length(ind_match[[1]]) > 0) {
          ind <- ind_match[[1]]:(ind_match[[1]] + length(ind_match) - 1)
          xv[ind[1]] <- match_abb
          xv <- c(xv[-ind[-1]])
          xv_which_abb[ind[1]] <- TRUE
          xv_which_abb <- c(xv_which_abb[-ind[-1]])
        }
      }
    }

    # if title fully matches multi-word phrase(s), return
    if (all(xv_which_abb == TRUE)) {
      out <- xv
    } else {
      # otherwise... deal with prepositions, articles, and conjunctions

      # remove prepositions not at beginning of word
      ind_rem_prep <- c(FALSE, mapply(CheckPrep, USE.NAMES = F, x = xv[-1], check = !xv_which_abb[-1]))
      xv <- xv[!ind_rem_prep]
      xv_which_abb <- xv_which_abb[!ind_rem_prep]

      # remove articles and conjunctions
      ind_rem_artcon <- mapply(CheckArtCon, USE.NAMES = F, x = xv, check = !xv_which_abb)
      xv <- xv[!ind_rem_artcon]
      xv_which_abb <- xv_which_abb[!ind_rem_artcon]

      # remove d' and l', if followed by character
      xv <- gsub("^d'(?=[[:alpha:]])|^l'(?=[[:alpha:]])", "", xv, ignore.case = TRUE, perl = TRUE)

      # if title fully matches multi-word phrase(s) (minus articles/conjunctions), return
      if (all(xv_which_abb == TRUE)) {
        out <- xv
      } else {
        # otherwise... check for hyphenated words

        # check for hyphenated words
        ind_dash <- grep("[[:alpha:]]-[[:alpha:]]", xv)
        if (length(ind_dash) > 0) {
          # if hyphens, split hyphenated strings into vectors
          xv <- unlist(strsplit(xv, "-"))

          # update xv_which_abb based on hyphens
          for (i in length(ind_dash):1) {
            xv_which_abb <- append(xv_which_abb, FALSE, ind_dash[i])
          }
        }

        # abbreviate all words in title (excluding multi-word phrases)
        abbrev_full <- mapply(AbbrevTerm, x = xv, check = !xv_which_abb, USE.NAMES = F)

        # add dashes back in, if applicable
        if (length(ind_dash) > 0) {
          for (i in 1:length(ind_dash)) {
            dashed_terms <- abbrev_full[c(ind_dash[i], ind_dash[i] + 1)]
            abbrev_full[ind_dash[i]] <- paste(dashed_terms, collapse = "-")
            abbrev_full <- abbrev_full[-(ind_dash[i] + 1)]
          }
        }

        # collapse title to vector
        out <- paste(abbrev_full, collapse = " ")
      }
    }
  }
  return(out)
}

ToTitleCase <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

CheckPrep <- function(x, check) {
  if (check == TRUE) {
    return(ifelse(tolower(x) %in% df_prep$word, TRUE, FALSE))
  } else {
    return(FALSE)
  }
}

CheckArtCon <- function(x, check) {
  if (check == TRUE) {
    return(ifelse(tolower(x) %in% df_artcon$word, TRUE, FALSE))
  } else {
    return(FALSE)
  }
}

# color palette
colorlist <- function() {
  c(
    "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
    "#B3B3B3", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#B15928", "#8DD3C7", "#BEBADA",
    "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#D9D9D9", "#BC80BD", "#CCEBC5"
  )
}

# Initial to upper case
firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#' Merge DE and ID Fields into a Unified Keywords Column
#'
#' This function creates a new column `KW_Merged` by combining the contents of the `DE` (author keywords) and `ID` (keywords plus) fields 
#' in a bibliographic dataframe. Duplicate keywords within each record are removed, and leading/trailing spaces are trimmed. 
#' The merged keywords are separated by a semicolon (`;`).
#'
#' If the `KW_Merged` column already exists, it will not be overwritten unless `force = TRUE` is specified.
#'
#' @param M A dataframe containing at least the `DE` and/or `ID` columns, typically generated by `convert2df()` from the `bibliometrix` package.
#' @param force Logical. If `TRUE`, an existing `KW_Merged` column will be overwritten. Default is `FALSE`.
#'
#' @return A dataframe with an added (or updated) `KW_Merged` column containing deduplicated and cleaned keyword strings.
#'
#' @examples
#' \dontrun{
#' data(management, package = "bibliometrix")
#' M <- mergeKeywords(management)
#' head(M$KW_Merged)
#' }
#'
#' @export
mergeKeywords <- function(M, force = FALSE){
  if (force) M <- M %>% select(!matches("KW_Merged")) # force to replace old KW_Merged column 
  if (!"KW_Merged" %in% names(M)){
    if (!"DE" %in% names(M)) M$DE <- NA
    if (!"ID" %in% names(M)) M$ID <- NA
    M <- M %>% 
      unite(col = "KW_Merged", DE,ID,sep = "; ", remove = FALSE, na.rm = TRUE) %>% 
      mutate(KW_Merged = ifelse(KW_Merged=="",NA,KW_Merged)) %>% 
      mutate(KW_Merged = map_chr(KW_Merged, ~ {
        .x %>%
          str_split(";") %>%
          unlist() %>%
          str_trim() %>%
          unique() %>%
          paste(collapse = "; ")
      }))
  }
  ### bibliometrix>DB class
  class(M) <- c("bibliometrixDB", "data.frame")
  return(M)
}

# adjust node positions in TM
adjust_positions_oblique <- function(df, xvar = "rcentrality", yvar = "rdensity",
                                     min_dist = 0.5, max_iter = 100, step_factor = 0.5, jitter_strength = 0.1) {
  df_adj <- df

  for (iter in 1:max_iter) {
    moved <- FALSE
    for (i in 1:(nrow(df_adj) - 1)) {
      for (j in (i + 1):nrow(df_adj)) {
        xi <- df_adj[[xvar]][i]
        yi <- df_adj[[yvar]][i]
        xj <- df_adj[[xvar]][j]
        yj <- df_adj[[yvar]][j]

        dx <- xi - xj
        dy <- yi - yj
        dist <- sqrt(dx^2 + dy^2)

        # Se perfettamente sovrapposti, applica jitter obliquo casuale
        if (dist == 0) {
          jitter_angle <- runif(1, 0, 2 * pi)
          offset <- jitter_strength

          df_adj[[xvar]][i] <- xi + cos(jitter_angle) * offset
          df_adj[[yvar]][i] <- yi + sin(jitter_angle) * offset
          df_adj[[xvar]][j] <- xj - cos(jitter_angle) * offset
          df_adj[[yvar]][j] <- yj - sin(jitter_angle) * offset

          moved <- TRUE
        } else if (dist < min_dist) {
          angle <- atan2(dy, dx)
          offset <- (min_dist - dist) * step_factor

          df_adj[[xvar]][i] <- xi + cos(angle) * offset
          df_adj[[yvar]][i] <- yi + sin(angle) * offset
          df_adj[[xvar]][j] <- xj - cos(angle) * offset
          df_adj[[yvar]][j] <- yj - sin(angle) * offset

          moved <- TRUE
        }
      }
    }
    if (!moved) break
  }

  return(df_adj)
}
