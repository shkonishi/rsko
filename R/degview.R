#' Mapping of gene and/or compound expression data on KEGG pathway map using pathview.
#' @description Mapping transcriptome and/or metabolome data on KEGG pathway
#' @return the nested list. The result of pathview and data.frame of ids per map.
#' @usage degview(gene_dat, cpd_dat, pathway_id, org, outdir, ...)
#' @param gene_dat named numeric vector, matrix, or data.frame. The 'gene_dat' has
#'      row.names or which is kegg gene id(cge:100770489) or kegg orthology id(K01488).
#'      The organism code is removed automatically(100770489).
#' @param cpd_dat data.frame has row.names which is compoud id.  e.g. C00631
#' @param pathway_id pathway id E.g. c("00010", "04210"). If it is "all", all pathways which
#'      genes and/or compunds were mapped.
#' @param org organism code E.g. "hsa"(human), or "ko"
#' @param outdir the directory of KEGG pathway data file (.xml) and image file (.png).
#'      result of image files (.multi.png) created at current working directory.
#' @param ... other pathwiew options
#' @examples \dontrun{
#' # arguments: gene_dat and cpd_dat
#' cpdat <- rskodat::cpdat
#' kodat <- rskodat::kodat
#' cge100 <- rskodat::cge100
#'
#' # arguments: pathway_id
#' maps1 <- c("00230", "00240")
#'
#' # execute:
#' # both gene and compound
#' suppressPackageStartupMessages(library(pathview))
#' res1 <- degview(gene_dat=kodat, cpd_dat=cpdat, pathway_id=maps1, org="ko",
#'                 outdir="~/pub/tmp")
#' # gene:
#' res2 <- degview(gene_dat=kodat, pathway_id=maps1, org="ko", outdir="~/pub/tmp")
#'
#' # compound:
#' res3 <- degview(cpd_dat=cpdat, pathway_id=maps1, org="ko", outdir="~/pub/tmp")
#'
#' # organism code is "cge":
#' res4 <- degview(cge100, pathway_id = maps1, org = "cge", outdir = "~/pub/tmp")
#'
#' # result:
#' head(res1$id_table); str(res1$gene[1:3]); str(res1$compound[1:3])
#'
#' # result of pathview:
#' head(res1$`00230`$plot.data.gene)
#'
#' # additional
#' tibble::rownames_to_column(res1$gene$`00230`, "gene") %>%
#'    tidyr::gather(., key="key", value="v", -1) %>%
#'    ggplot2::ggplot(., ggplot2::aes(x=key, y=v)) +
#'        ggplot2::geom_bar(stat = "identity") +
#'        ggplot2::facet_wrap(~gene, ncol=3)
#' }
#' @importFrom dplyr %>%
#' @importFrom plyr .
#' @export
#'
degview <- function(gene_dat=NULL, cpd_dat=NULL, pathway_id="all", org="ko", outdir=".", ...) {

  # argument check: 'gene_dat' and/pr 'cpd_dat', which names or rownames is kegg gi or KO number
  if (!is.null(gene_dat)) {
    if (is.vector(gene_dat) & is.null(names(gene_dat))) {
      # 'gene_dat' as named numeric vector
      stop("names of 'gene_dat' must be kegg geneid or nibi-gi ONLY(E.g. 'K15035', 'cge:01977')!")

    } else if (class(gene_dat) == "data.frame" | class(gene_dat) == "matrix") {
      # 'gene_dat' as data.frame which has row.names of ncbi-gi or kegg id.
      if (identical(as.character(1:nrow(gene_dat)), rownames(gene_dat))) {
        stop(paste0("rownames must be kegg id ONLY(E.g. 'K15035', 'cge01977')! \n",
                    "if you use entrez gene id, you shoud modify this script. "))
      }
      # 'gene_dat', if 'org' is 'ko', there is row.names with kegg orthology id.
      if (org == "ko" & unique(substr(rownames(gene_dat), 1, 1)) != "K") {
        stop("if organism code is 'ko', gene id in rownames must be start with 'K'")
      }
      # remove organism code from kegg gene id
      if (org != "ko") {
        rownames(gene_dat) <- sub(paste0(org, ":"), "", rownames(gene_dat))
      }

    } else {
      stop("'gene_dat' is named numeric vector or data.frame which has rownames.")
    }

  } else if (!is.null(cpd_dat)) {
    if (is.vector(cpd_dat) & is.null(names(cpd_dat))) {
      # 'cpd_dat' as named numeric vector
      stop("names of 'cpd_dat' must be compound id ONLY(E.g. 'C00008')! ")

    } else if (class(cpd_dat) == "data.frame" | class(cpd_dat) == "matrix") {
      # 'cpd_dat' as data.frame which rownames must be compound id
      if (identical(as.character(1:nrow(cpd_dat)), rownames(cpd_dat))) {
        stop("row.names must be compound id ONLY(E.g. 'C00008')! ")
      }
    } else {
      stop("'cpd_dat' is named numeric vector or data.frame which has rownames.")
    }
  }

  # argument check: 'outdir'
  if (!file.exists(outdir)) { dir.create(outdir) }

  # get all pathway id mapped differential expressional genes
  map <- NULL; ko <- NULL; cpd <- NULL; ec <- NULL

  if (!is.null(gene_dat) & !is.null(cpd_dat)) {
    # linked pathways by ko
    kopathdat <- system(paste0("wget -q -O - http://rest.kegg.jp/link/pathway/", org), intern = T)
    if (org != "ko") {
      kopath <- data.frame(ko = sub(paste0(org,":"), "", sapply(strsplit(kopathdat, "\t"), "[", 1)),
                           map = sub(paste0("path:",org), "", sapply(strsplit(kopathdat, "\t"), "[", 2)),
                           stringsAsFactors = F)  %>%
        dplyr::filter(ko %in% rownames(gene_dat)) %>%
        dplyr::group_by(map) %>%
        dplyr::summarise(kos = paste0(ko, collapse = ";"))

    } else if (org == "ko") {
      kopath <- data.frame(ko = sub(paste0(org,":"), "", sapply(strsplit(kopathdat, "\t"), "[", 1)),
                           map = sub(paste0("path:",org), "", sapply(strsplit(kopathdat, "\t"), "[", 2)),
                           stringsAsFactors = F) %>%
        .[-grep("map", .$map),] %>%
        dplyr::filter(ko %in% rownames(gene_dat)) %>%
        dplyr::group_by(map) %>%
        dplyr::summarise(kos = paste0(ko, collapse = ";"))
    }

    # linked ko by ec
    eckodat <- system(paste0("wget -q -O - http://rest.kegg.jp/link/ec/", org), intern = T)
    ecko <- data.frame(ko = sub(paste0(org,":"), "", sapply(strsplit(eckodat, "\t"), "[", 1)),
                       ec = sub("ec:", "", sapply(strsplit(eckodat, "\t"), "[", 2)),
                       stringsAsFactors = F) %>%
      dplyr::filter(ko %in% rownames(gene_dat)) %>%
      dplyr::group_by(ko) %>%
      dplyr::summarise(ecs = paste0(ec, collapse = ","))

    # linked pathways by cpd
    cpdpathdat <- system("wget -q -O - http://rest.kegg.jp/link/pathway/cpd", intern = T)
    cpdpath <- data.frame(cpd = sub("cpd:", "", sapply(strsplit(cpdpathdat, "\t"), "[", 1)),
                          map = sub("path:map", "", sapply(strsplit(cpdpathdat, "\t"), "[", 2)),
                          stringsAsFactors = F) %>%
      dplyr::filter(cpd %in% rownames(cpd_dat)) %>%
      dplyr::group_by(map) %>%
      dplyr::summarise(cpds = paste0(cpd, collapse = ";"))

    # list of cpd name
    cpdl <- system("wget -q -O - http://rest.kegg.jp/list/cpd", intern = T)
    cpdlist <- data.frame(cpd = sub("cpd:", "", sapply(strsplit(cpdl, "\t"), "[", 1)),
                          name = sapply(strsplit(sapply(strsplit(cpdl, "\t"), "[", 2), ";"), "[", 1))

    # ko and cpd ids per map
    mapped.ko.id <-
      data.frame(kopath, koecs = unlist(lapply(kopath$kos, function(x) {
            koi <- unlist(strsplit(x, ";"))
            paste(paste(koi, ecko$ecs[match(koi, ecko$ko)], sep = "_"), collapse = ";")
          })), stringsAsFactors = F)

    mapped.cpd.id <-
      data.frame(cpdpath, cpdnm = unlist(lapply(cpdpath$cpds, function(x) {
        cpdi = unlist(strsplit(x, ";"))
        paste(paste(cpdi, cpdlist$name[match(cpdi, cpdlist$cpd)], sep = "_"), collapse = ";")
        })), stringsAsFactors = F)

    mapped.id <- dplyr::full_join(mapped.ko.id, mapped.cpd.id, by = "map")

    ## pathway_id for pathview search
    gm <- c("01100", "01110", "01120", "01130")
    sub.path <- intersect(mapped.ko.id$map, mapped.cpd.id$map) %>% .[!. %in% gm]
    if (all(pathway_id == "all")) {
      pathway_id <- sub.path

    } else if (all(pathway_id != "all") & any(pathway_id %in% sub.path)) {
      pathway_id <- pathway_id[pathway_id %in% sub.path]

    } else if (all(!pathway_id %in% sub.path)) {
      stop(paste("These genes and/or compounds are not mapped on ",
                 paste(pathway_id, collapse = ", ")))
    }

    ## id table
    id_tab <- mapped.id[mapped.id$map %in% pathway_id,]

    ## list of ko data.frame per map
    map.kodats <- vector("list")
    invisible(lapply(seq_along(pathway_id), function(i) {
      map.kodats[[i]] <<- gene_dat[unlist(strsplit(id_tab$kos[i], ";")),] %>%
        `rownames<-`(unlist(strsplit(id_tab$koecs[i], ";")))
    }))
    map.kodats <- stats::setNames(map.kodats, pathway_id)

    ## list of cpd data.frame per map
    map.cpdats <- vector("list")
    invisible(lapply(seq_along(pathway_id), function(i) {
      map.cpdats[[i]] <<- cpd_dat[unlist(strsplit(id_tab$cpds[i], ";")),] %>%
        `rownames<-`(unlist(strsplit(id_tab$cpdnm[i], ";")))
    }))
    map.cpdats <- stats::setNames(map.cpdats, pathway_id)


    ## result list
    res <- list(id_table = id_tab, gene = map.kodats, compound = map.cpdats)


  } else if ( is.null(cpd_dat)) {
    # linked pathways by ko
    kopathdat <- system(paste0("wget -q -O - http://rest.kegg.jp/link/pathway/", org), intern = T)
    if (org != "ko") {
      kopath <- data.frame(ko = sub(paste0(org,":"), "", sapply(strsplit(kopathdat, "\t"), "[", 1)),
                           map = sub(paste0("path:",org), "", sapply(strsplit(kopathdat, "\t"), "[", 2)),
                           stringsAsFactors = F)  %>%
        dplyr::filter(ko %in% rownames(gene_dat)) %>%
        dplyr::group_by(map) %>%
        dplyr::summarise(kos = paste0(ko, collapse = ";"))
    } else if (org == "ko") {
      kopath <- data.frame(ko = sub(paste0(org,":"), "", sapply(strsplit(kopathdat, "\t"), "[", 1)),
                           map = sub(paste0("path:",org), "", sapply(strsplit(kopathdat, "\t"), "[", 2)),
                           stringsAsFactors = F) %>%
        .[-grep("map", .$map),] %>%
        dplyr::filter(ko %in% rownames(gene_dat)) %>%
        dplyr::group_by(map) %>%
        dplyr::summarise(kos = paste0(ko, collapse = ";"))
    }

    # linked ko by ec ko ids per map
    eckodat <- system(paste0("wget -q -O - http://rest.kegg.jp/link/ec/", org), intern = T)
    ecko <- data.frame(ko = sub(paste0(org,":"), "", sapply(strsplit(eckodat, "\t"), "[", 1)),
                       ec = sub("ec:", "", sapply(strsplit(eckodat, "\t"), "[", 2)),
                       stringsAsFactors = F) %>%
      dplyr::filter(ko %in% rownames(gene_dat)) %>%
      dplyr::group_by(ko) %>%
      dplyr::summarise(ecs = paste0(ec, collapse = ","))

    ## ko ids per map
    mapped.id <- kopath[kopath$map %in% pathway_id ,] %>%
      data.frame(., koecs = unlist(lapply(.$kos, function(x){
        koi <- unlist(strsplit(x, ";"))
        paste(paste(koi, ecko$ecs[match(koi, ecko$ko)], sep = "_"), collapse = ";")
      })), stringsAsFactors = F)


    ## ko ids per map
    mapped.ko.id <-
      data.frame(kopath, koecs = unlist(lapply(kopath$kos, function(x){
        koi <- unlist(strsplit(x, ";"))
        paste(paste(koi, ecko$ecs[match(koi, ecko$ko)], sep = "_"), collapse = ";")
      })), stringsAsFactors = F)

    ## pathway_id for pathview search
    sub.path <- mapped.ko.id$map %>% .[!. %in% c("01100", "01110", "01120", "01130")]
    if (all(pathway_id == "all")) {
      pathway_id <- mapped.ko.id$map %>% .[!. %in% c("01100", "01110", "01120", "01130")]

    } else if (all(pathway_id != "all") & any(pathway_id %in% sub.path)) {
      pathway_id <- pathway_id[pathway_id %in% sub.path]

    } else if (all(!pathway_id %in% sub.path)) {
      stop(paste("These genes and/or compounds are not mapped on ",
                 paste(pathway_id, collapse = ", ")))
    }

    ## id table
    id_tab <- mapped.ko.id[mapped.ko.id$map %in% pathway_id,]


    ## list of ko data.frame per map
    map.kodats <- vector("list")
    invisible(lapply(seq_along(pathway_id), function(i) {
      map.kodats[[i]] <<- gene_dat[unlist(strsplit(id_tab$kos[i], ";")),] %>%
        `rownames<-`(unlist(strsplit(id_tab$koecs[i], ";")))
    }))
    map.kodats <- stats::setNames(map.kodats, pathway_id)

    ## result list ----
    res <- list(id_table = id_tab, gene = map.kodats)


  } else if ( is.null(gene_dat) ) {
    # linked pathways by cpd ----
    cpdpathdat <- system("wget -q -O - http://rest.kegg.jp/link/pathway/cpd", intern = T)
    cpdpath <- data.frame(cpd = sub("cpd:", "", sapply(strsplit(cpdpathdat, "\t"), "[", 1)),
                          map = sub("path:map", "", sapply(strsplit(cpdpathdat, "\t"), "[", 2)),
                          stringsAsFactors = F) %>%
      dplyr::filter(cpd %in% rownames(cpd_dat)) %>%
      dplyr::group_by(map) %>%
      dplyr::summarise(cpds = paste0(cpd, collapse = ";"))

    # list cpd and cpd ids per map
    cpdl <- system("wget -q -O - http://rest.kegg.jp/list/cpd", intern = T)
    cpdlist <- data.frame(cpd = sub("cpd:", "", sapply(strsplit(cpdl, "\t"), "[", 1)),
                          name = sapply(strsplit(sapply(strsplit(cpdl, "\t"), "[", 2), ";"), "[", 1))

    ## cpd ids per map
    mapped.cpd.id <-
      data.frame(cpdpath, cpdnm = unlist(lapply(cpdpath$cpds, function(x) {
        cpdi = unlist(strsplit(x, ";"))
        paste(paste(cpdi, cpdlist$name[match(cpdi, cpdlist$cpd)], sep = "_"), collapse = ";")
      })), stringsAsFactors = F)

    ## pathway_id for pathview search ----
    sub.path <- mapped.cpd.id$map %>% .[!. %in% c("01100", "01110", "01120", "01130")]
    if (all(pathway_id == "all")) {
      pathway_id <- sub.path

    } else if (all(pathway_id != "all") & any(pathway_id %in% sub.path)) {
      pathway_id <- pathway_id[pathway_id %in% sub.path]

    } else if (all(!pathway_id %in% sub.path)) {
      stop(paste("These genes and/or compounds are not mapped on ",
                 paste(pathway_id, collapse = ", ")))
    }

    ## id table ----
    id_tab <- mapped.cpd.id[mapped.cpd.id$map %in% pathway_id,]

    ## list of cpd data.frame per map ----
    map.cpdats <- vector("list")
    invisible(lapply(seq_along(pathway_id), function(i) {
      map.cpdats[[i]] <<- cpd_dat[unlist(strsplit(id_tab$cpds[i], ";")),] %>%
        `rownames<-`(unlist(strsplit(id_tab$cpdnm[i], ";")))
    }))
    map.cpdats <- stats::setNames(map.cpdats, pathway_id)


    ## result ----
    res <- list(id_table = id_tab, compound = map.cpdats)
  }


  # pathview ----
  pvdat <- vector("list", length(pathway_id))
  if (!is.null(cpd_dat) & !is.null(gene_dat)) {
    for (i in seq_along(pathway_id)) {
      suppressWarnings(
        pvdat[[i]] <-
          pathview::pathview(gene.data = gene_dat,
                             cpd.data = cpd_dat,
                             pathway.id = pathway_id[i],
                             cpd.idtype = "kegg", # default: kegg
                             gene.idtype = "KEGG", # default:entrez
                             kegg.native = T, # F: pdf output default T
                             map.null = F,
                             low = list(gene = "blue", cpd = "blue"),
                             mid = list(gene = "gray", cpd = "gray"),
                             high = list(gene = "red", cpd = "yellow"),
                             species = org,
                             kegg.dir = outdir, ...)
      )
    }

  } else if (is.null(cpd_dat)) {
    for (i in seq_along(pathway_id)) {
      suppressWarnings(
        pvdat[[i]] <-
          pathview::pathview(gene.data = gene_dat,
                             pathway.id = pathway_id[i],
                             gene.idtype = "KEGG", # default: entrez
                             kegg.native = T, # F: pdf output default T
                             map.null = F,
                             low = list(gene = "blue"),
                             mid = list(gene = "gray"),
                             high = list(gene = "red"),
                             species = org,
                             kegg.dir = outdir, ...)
      )
    }

  } else if (is.null(gene_dat)) {
    for (i in seq_along(pathway_id)) {
      suppressWarnings(
        pvdat[[i]] <-
          pathview::pathview(cpd.data = cpd_dat,
                             pathway.id = pathway_id[i],
                             cpd.idtype = "kegg", # default: kegg
                             kegg.native = T, # F: pdf output default T
                             map.null = F,
                             low = list(cpd = "blue"),
                             mid = list(cpd = "gray"),
                             high = list(cpd = "yellow"),
                             species = org,
                             kegg.dir = outdir, ...)
      )
    }

  }

  # results ----
  pvdat <- stats::setNames(pvdat, pathway_id)
  return(c(res, pvdat))

}
