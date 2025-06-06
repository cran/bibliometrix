utils::globalVariables("where")

dimensions2df <- function(file, format = "csv") {
  switch(format,
    csv = {
      for (i in 1:length(file)) {
        # D <- rio::import(file[i], quote = '"',dec = ".",skip=1)

        D <- read_csv(file[i], na = character(), quote = '"', trim_ws = FALSE, progress = show_progress(), show_col_types = FALSE) %>%
          mutate(across(where(is.numeric), as.character)) %>%
          mutate(across(where(is.character), function(x) tidyr::replace_na(x, ""))) %>%
          as.data.frame()


        if (i > 1) {
          l <- intersect(l, names(D))
          DATA <- rbind(DATA[l], D[l])
        } else {
          l <- names(D)
          DATA <- D
        }
      }
      # DATA=rio::import(file, quote = '"',dec = ".",skip=1)
    },
    excel = {
      for (i in 1:length(file)) {
        # D1 <- rio::import(file[i], skip=1)
        D <- readxl::read_excel(file[1], skip = 1)
        D <- as.data.frame(D)

        if (i > 1) {
          l <- intersect(l, names(D))
          DATA <- rbind(DATA[l], D[l])
        } else {
          l <- names(D)
          DATA <- D
        }
      }
      # DATA <- rio::import(file, skip = 1)
    }
  )
  # Encoding(DATA) <- "UTF-8"

  bibtag <- NULL
  data("bibtag", envir = environment())
  bibtag <- as.data.frame(bibtag)


  names(DATA) <- trimES(gsub("\\.|-", " ", names(DATA)))

  # i <-
  #   which(names(DATA) == "Authors Affiliations Name of Research organization")
  # if (length(i) > 0) {
  #   names(DATA)[i] = "Authors Affiliations"
  # }
  # i <- which(names(DATA) == "Source title/Anthology title")
  # if (length(i) > 0) {
  #   names(DATA)[i] = "Source title"
  # }

  fields <- names(DATA)

  for (i in 1:length(fields)) {
    # print(i)
    ind <- which(bibtag$DIMENSIONS == fields[i])

    if (length(ind) > 0) {
      fields[i] <- bibtag$TAG[ind]
      # if (length(ind)>1) print(ind)
    }
  }
  names(DATA) <- fields
  ind <- which(names(DATA) == "Source title/Anthology title")
  if (length(ind) == 1) {
    names(DATA)[ind] <- "SO"
  }
  ind <- which(names(DATA) == "Authors Affiliations Name of Research organization")
  if (length(ind) == 1) {
    names(DATA)[ind] <- "C1"
    DATA$AU_UN <- DATA$C1
  }
  ind <- which(names(DATA) == "Authors Affiliations Country of Research organization")
  if (length(ind) == 1) {
    names(DATA)[ind] <- "AU_CO"
  }


  DATA <- postprocessingDim(DATA)

  return(DATA)
}



postprocessingDim <- function(DATA) {
  # DATA <- data.frame(lapply(DATA, toupper), stringsAsFactors = FALSE)

  ## Converting original references in WOS format (AU, PY, SO, VOL, NUM, DOI)
  if ("Cited references" %in% names(DATA)) {
    aaa <- strsplit(DATA$`Cited references`, ";\\[")
    cr <- (unlist(lapply(aaa, function(l) {
      l <- gsub("\\|", "!!!", l)
      l <- strsplit(l, "!!!")

      ## first authors (of cited references)
      au <- sapply(l, `[`, 1)
      au <-
        ifelse(nchar(au) > 2,
          {
            au <- gsub("\\[", "", unlist(lapply(strsplit(au, ";"), function(x) {
              x <- x[1]
            })))
            lastname <- trim(gsub(",.*", "", au))
            firstname <- strsplit(trim(gsub(".*,", "", au)), " ")
            firstname <- lapply(firstname, function(x) {
              if (length(x) > 0) {
                x <- paste(substr(x, 1, 1), sep = "", collapse = "")
              } else {
                x <- ""
              }
              return(x)
            })
            au <- paste(lastname,
              unlist(firstname),
              sep = " "
            )
          },
          "NA"
        )

      ## publication year
      py <- sapply(l, `[`, 4)
      so <- sapply(l, `[`, 3)
      vol <-
        ifelse(nchar(sapply(l, `[`, 5)) > 0, paste("V", sapply(l, `[`, 5),
          sep =
            ""
        ), "")
      num <-
        ifelse(nchar(sapply(l, `[`, 6)) > 0, paste("N", sapply(l, `[`, 6),
          sep =
            ""
        ), "")
      ## doi
      doi <- sapply(l, `[`, 8)
      ref <- paste(au,
        py,
        so,
        vol,
        num,
        doi,
        sep = ", ",
        collapse = ";"
      )
      ref <- gsub("^,*|(?<=,),|,*$", "", ref, perl = T)
    })))

    DATA$CR <- cr
    DATA$CR <- gsub("] ];", "", cr)
  } else {
    DATA$CR <- "NA,0000,NA"
  }

  # Document Type
  if (!("DT" %in% names(DATA))) {
    DATA$DT <- "Article"
  }

  # Authors cleaning and converting in WoS format
  DATA$AF <- DATA$AU

  DATA$AU <- gsub("\\s+", " ", DATA$AU)
  DATA$AU <- gsub("\\(|\\)", "", DATA$AU)

  listAU <- strsplit(DATA$AU, "; ")

  AU <- lapply(listAU, function(l) {
    lastname <- trim(gsub(",.*", "", l))
    firstname <- strsplit(trim(gsub(".*,", "", l)), " ")
    firstname <- lapply(firstname, function(x) {
      if (length(x) > 0) {
        x <- paste(substr(x, 1, 1), sep = "", collapse = "")
      } else {
        x <- ""
      }
      return(x)
    })
    AU <- paste(lastname,
      unlist(firstname),
      sep = " ",
      collapse = ";"
    )
    return(AU)
  })


  DATA$AU <- unlist(AU)

  # keywords
  if (!("DE" %in% names(DATA)) & !("ID" %in% names(DATA))) {
    if ("MeSH terms" %in% names(DATA)) {
      DATA$DE <- DATA$ID <- DATA$`MeSH terms`
    } else {
      DATA$DE <- DATA$ID <- "NA"
    }
  }
  if (("DE" %in% names(DATA)) & !("ID" %in% names(DATA))) {
    DATA$ID <- DATA$DE
  }
  if (!("DE" %in% names(DATA)) & ("ID" %in% names(DATA))) {
    DATA$DE <- DATA$ID
  }

  # Affiliations

  RP <- lapply(regmatches(DATA$RP, gregexpr("(?<=\\().*?(?=\\))", DATA$RP, perl = T)), function(l) {
    if (length(l[1]) == 0) {
      l <- NA
    } else {
      l <- paste(l, collapse = ";")
    }
    return(l)
  })
  DATA$AU1_UN <- unlist(RP)

  i <- which(names(DATA) %in% c("Authors Affiliations"))
  if (length(i) == 1) {
    # names(DATA)[i] <- "AU_UN"
    C1 <- lapply(regmatches(DATA$C1, gregexpr("(?<=\\().*?(?=\\))", DATA$C1, perl = T)), function(l) {
      if (length(l[1]) == 0) {
        l <- NA
      } else {
        l <- paste(l, collapse = ";")
      }
      return(l)
    })
    DATA$AU_UN <- unlist(C1)
    DATA$AU_UN <- gsub("\\(|)", " ", DATA$AU_UN)
  }

  if (("SO" %in% names(DATA)) & ("Anthology title" %in% names(DATA))) {
    ind <- which(is.na(DATA$SO) | DATA$SO == "")
    DATA$SO[ind] <- DATA$`Anthology title`[ind]
    DATA$SO[is.na(DATA$SO) | DATA$SO == ""] <- "NA"
  }

  if (!("SO" %in% names(DATA))) {
    DATA$SO <- "NA"
  }

  ####
  cat("\nCreating ISO Source names...")
  DATA$JI <- DATA$SO
  # DATA$JI <- sapply(DATA$SO, AbbrevTitle, USE.NAMES = FALSE)
  DATA$J9 <- gsub("\\.", "", DATA$JI)
  ####
  AB <- DATA$AB
  TI <- DATA$TI
  DE <- DATA$DE

  DATA <- data.frame(lapply(DATA, toupper), stringsAsFactors = FALSE)

  DATA$AB_raw <- AB
  DATA$TI_raw <- TI
  DATA$DE_raw <- DE

  DATA$PY <- as.numeric(DATA$PY)

  DATA$TC <- as.numeric(DATA$TC)

  DATA$DB <- "DIMENSIONS"

  if (!"AU_CO" %in% names(DATA)) DATA <- metaTagExtraction(DATA, "AU_CO")

  DATA$AU1_CO <- unlist(lapply(strsplit(DATA$AU_CO, ";"), function(l) {
    if (length(l) > 0) {
      l <- l[1]
    } else {
      l <- "NA"
    }
    return(l)
  }))

  DATA <- metaTagExtraction(DATA, "AU1_CO")
  return(DATA)
}
