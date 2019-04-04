#' Fetch KEGG data usig KEGG API
#' @name fetch_kegg
#' @rdname fetch_kegg
#'
#' @title Fetch KEGG data usig KEGG API
#' @description Fetch KEGG data usig KEGG API, for the purpose of finding out association with
#'     transcriptome and metabolome experimental data.
#'
#' @usage fetch_kegg_gene(org, outfmt, out_fst)
#' @usage fetch_kegg_react(outfmt)
#' @usage fetch_kegg_cpd(outfmt)
#'
#' @param org character: KEGG organism id. If "all" , get a list of all organism for confirm a organim id.
#' @param outfmt character:
#'  fetch_kegg_gene(outfmt=c("ntseq", "aaseq", "gene_id", "gene_map","gene_desc", or "pathway"))
#'  fetch_kegg_cpd(outfmt=c("cpd_map", or "cpd_desc")
#'  fetch_kegg_react(outfmt=c("reaction_entry", or "reaction_flat"))
#'  If "ntseq" or "aaseq" is chosen, the nucleotide or amino acid sequence in fasta format is written to "out_fst".
#'  On the other hand, if "gene_id" is selected, an ID correspondence table will be returned.
#'  In case of "fetch_kegg_react", select either "reaction_entry" or "reaction_flat". The "reaction_entry" retruns KEGG REACTION id and discription, which linked to KEGG PATHWAY.
#'  The "reaction_flat" gets all KEGG REACTION entries and returns the converted data frame. It takes too much time.
#'  Using "fetch_kegg_cpd", select an output format from one of these "cpd_map", "cpd_desc".
#' @param out_fst character: out put file path of "fasta" format.
#' @return If type is "gene_id" it will return a data frame and if it is "ntseq" or "aaseq" it will only write fasta to the "out_fst" file.
#'
#' @examples \dontrun{
#' # KEGG GENE
#' ## confirm specific organism id
#' orgs <- fetch_kegg_gene(org = "all")
#'
#' ## create correspondance id table Mycoplasma genitalium G37
#' id_tab <- fetch_kegg_gene(org = "mge", outfmt = "gene_id")
#'
#' ## create links of k-number
#' kid_tab <- fetch_kegg_gene(org = "ko", outfmt = "gene_id")
#'
#' ## link between gene and map.
#' g_map <- fetch_kegg_gene("ko", "gene_map")
#'
#' ## list pathway
#' maps <- fetch_kegg_gene(org ="ko", outfmt = "pathway")
#'
#' ## get ntseq (It takes a long time)
#' fetch_kegg_gene(org="mge", outfmt="ntseq", out_fst="./mge.fna")
#'
#' ## get aaseq
#' fetch_kegg_gene(org="mge", outfmt="aaseq", out_fst="./mge.faa")
#'
#' # KEGG REACTION
#' react <- fetch_kegg_react(outfmt="reaction_entry")
#' react_dat <- fetch_kegg_react(outfmt="reaction_flat")
#'
#' # KEGG COMPOUND
#' cpd_desc <- fetch_kegg_cpd("cpd_desc")
#' cpd_map <- fetch_kegg_cpd("cpd_map")
#'
#' }
#'
#' @rdname fetch_kegg
#' @export
fetch_kegg_gene <- function(org, outfmt, out_fst = NULL){

  # argument check: outfmt ----
  if (org != "all") {
    slct <- c('gene_id', 'gene_map', 'gene_desc','pathway', 'ntseq', 'aaseq')

    if (!outfmt %in% slct) {
      stop( paste(" 'outfmt' must be one of these,",
                  paste(slct, collapse = ", ")))

    }
  }

  if (org == "all") { # get all organism code
    org_dat <- system("wget -q -O - http://rest.kegg.jp/list/organism", intern = T)
    res <- data.frame(do.call(rbind, strsplit(org_dat, "\t")), stringsAsFactors = F)
    return(res)
    stop()

  } else {# organism code
    # get GENE ID and convert to dataframe ----
    com <- paste0("wget -q -O - http://rest.kegg.jp/list/", org)
    GENE <- system(com, intern = T)

    if (org == "ko") {
      GI <- data.frame(GENE = sub("ko:", "", sapply(strsplit(GENE, "\t"), "[", 1)),
                       Description = sapply(strsplit(GENE, "\t"), "[", 2),
                       stringsAsFactors = F)
    } else {
      GI <- data.frame(GENE = sapply(strsplit(GENE, "\t"), "[", 1),
                       Description = sapply(strsplit(GENE, "\t"), "[", 2),
                       stringsAsFactors = F
      )
    }
    gids <- GI$GENE

    if (outfmt == "gene_id") {
      ## commands for KEGG API ----
      com_map <- paste0("wget -q -O - http://rest.kegg.jp/link/pathway/", org) # MAP
      com_ec <- paste0("wget -q -O - http://rest.kegg.jp/link/ec/", org)  # ec
      com_refp <- paste0("wget -q -O - http://rest.kegg.jp/conv/ncbi-proteinid/", org) # Ref.aa
      com_unip <- paste0("wget -q -O - http://rest.kegg.jp/conv/uniprot/", org) # uniprot
      com_ncbi <- paste0("wget -q -O - http://rest.kegg.jp/conv/ncbi-geneid/", org) # ncbi-geneid
      if (org != "ko") {
        com_ko <- paste0("wget -q -O - http://rest.kegg.jp/link/ko/", org) # ortholog id
      } else {
        com_ko <- NULL
      }

      ## system command ----
      genemap <- system(com_map, intern = T)
      ec <- system(com_ec, intern = T)
      suppressWarnings(refp <- system(com_refp, intern = T))
      suppressWarnings(unip <- system(com_unip, intern = T))
      suppressWarnings(ncbi_gene <- system(com_ncbi, intern = T))
      if (!is.null(com_ko)) {
        ko <- system(com_ko, intern = T)
      } else {
        ko <- NA
      }


      ## MAP: merge unique GENE ID following multiple MAP IDs separated ";"----
      KO <- NULL; UPID <- NULL
      if (org != "ko") {
        MAP <- data.frame(
          GENE = sapply(strsplit(genemap, "\t"), "[", 1),
          MAP = sub("path:","",sapply(strsplit(genemap, "\t"), "[", 2)),
          stringsAsFactors = F) %>%
          dplyr::group_by(GENE) %>%
          dplyr::summarise(MAPs = paste(MAP, collapse = ";"))

      } else {
        x <- genemap[!grepl("\tpath:map", genemap)]
        MAP <- data.frame(
          GENE = sub("ko:", "", sapply(strsplit(x, "\t"), "[", 1)),
          MAP = sub("path:","",sapply(strsplit(x, "\t"), "[", 2)), stringsAsFactors = F) %>%
          dplyr::group_by(GENE) %>%
          dplyr::summarise(MAPs = paste(MAP, collapse = ";"))
      }

      ## ec: ----
      ec <- data.frame(
        GENE = sapply(strsplit(ec, "\t"), "[", 1),
        ec = sapply(strsplit(ec, "\t"), "[", 2),
        stringsAsFactors = F) %>%
        dplyr::group_by(GENE) %>%
        dplyr::summarise(ecs = paste(ec, collapse = ";"))


      ## refseq_p: ----
      if (length(refp)) {
        refp <- data.frame(
          GENE = sapply(strsplit(refp, "\t"), "[", 1),
          Ref.aa = sub("ncbi-proteinid:","",sapply(strsplit(refp, "\t"), "[", 2)),
          stringsAsFactors = F)

      } else {
        refp <- data.frame(GENE = gids, Ref.aa = NA, stringsAsFactors = F)

      }

      ## unip: ----
      if (length(unip)) {
        unip <- data.frame(
          GENE = sapply(strsplit(unip, "\t"), "[", 1),
          UPID = sapply(strsplit(unip, "\t"), "[", 2),
          stringsAsFactors = F) %>%
          dplyr::group_by(GENE) %>%
          dplyr::summarise(UPIDs = paste(UPID, collapse = ";"))

      } else {
        unip <- data.frame(GENE = gids, UPIDs = NA, stringsAsFactors = F)
      }

      ## ncbi-gene: ----
      if (length(ncbi_gene)) {
        ncbi <- data.frame(
          GENE = sapply(strsplit(ncbi_gene, "\t"), "[", 1),
          GeneID = sub("ncbi-geneid:","", sapply(strsplit(ncbi_gene, "\t"), "[", 2)),
          stringsAsFactors = F)
      }else{
        ncbi <- data.frame(GENE = gids, GeneID = NA, stringsAsFactors = F)
      }

      ## ko: ----
      if (!is.na(ko)) {
        ko <- data.frame(
          GENE = sapply(strsplit(ko, "\t"), "[", 1),
          KO = sub("ko:","", sapply(strsplit(ko, "\t"), "[", 2)),
          stringsAsFactors = F) %>%
          dplyr::group_by(GENE) %>%
          dplyr::summarise(KOs = paste(KO, collapse = ";"))


      } else {
        ko <- data.frame(GENE = gids, KO = NA, stringsAsFactors = F)
      }

      ## merge id table: ----
      if (org == "ko") {
        id_dat <- Reduce(function(x,y){ merge(x,y, all.x = T, by = "GENE")},
                         list(GI, ec, MAP))
      } else {
        id_dat <- Reduce(function(x,y){ merge(x,y, all.x = T, by = "GENE")},
                         list(GI, ec, ko, MAP, ncbi, refp, unip))
      }

      return(id_dat)

    } else if (outfmt == "gene_map") {
      ## link/pathway ----
      if (org != "ko") {
        com <- paste0("wget -q -O - http://rest.kegg.jp/link/pathway/", org)
        v_linkmap <- system(com, intern = T)
        linkmap <- grep("\tpath:map", v_linkmap, value = T)
        g_map <- data.frame(GENE = sapply(strsplit(v_linkmap, "\t"), "[", 1),
                            MAP = sapply(strsplit(v_linkmap, "\t"), "[", 2),
                            stringsAsFactors = F)
        return(g_map)

      } else {
        com <- paste0("wget -q -O - http://rest.kegg.jp/link/pathway/", org)
        v_linkmap <- system(com, intern = T)
        linkmap <- v_linkmap[!grepl("\tpath:map", v_linkmap)]
        g_map <- data.frame(GENE = sub("ko:", "", sapply(strsplit(linkmap, "\t"), "[", 1)),
                            MAP = sub("path:","", sapply(strsplit(linkmap, "\t"), "[", 2)),
                            stringsAsFactors = F)
        return(g_map)

      }

    } else if (outfmt == "gene_desc") {
      return(GI)
    } else if (outfmt == "pathway") {
      ## list/pathway: ----
      com <- paste0("wget -q -O - http://rest.kegg.jp/list/pathway/", org)
      v_listmap <- system(com, intern = T)
      df_map <- strsplit(v_listmap, "\\t") %>%
      {data.frame(MAP = sub("path:", "", sapply(., "[", 1)),
                  Name = sapply(., "[", 2), stringsAsFactors = F)}
      return(df_map)

    } else if (outfmt == "ntseq") {
      ## chunk per 10 seq ----
      chn_num <- ceiling(length(gids)/10)
      chunk <- function(x,n) split(x, factor(sort(rank(x) %% n)))
      chnk <- chunk(1:length(gids), chn_num)

      ## get ntseq -----
      fna <- vector("character")
      for (i in seq(chnk)) {
        gids_chnk <- paste(gids[chnk[[i]]], collapse = "+")
        com <- paste0("wget -q -O - http://rest.kegg.jp/get/", gids_chnk, "/ntseq")
        fni <- system(com, wait = T, intern = T)
        fna <- c(fna, fni)
      }
      ## writeLines ----
      fst <- file(out_fst, "w")
      writeLines(fna, fst)
      close(fst)

    } else if (outfmt == "aaseq") {
      ## chunk per 10 seq ----
      chn_num <- ceiling(length(gids)/10)
      chunk <- function(x, n) split(x, factor(sort(rank(x) %% n)))
      chnk <- chunk(1:length(gids), chn_num)

      ## get aaseq -----
      faa <- vector("character")
      for (i in seq(chnk)) {
        gids_chnk <- paste(gids[chnk[[i]]], collapse = "+")
        com <- paste0("wget -q -O - http://rest.kegg.jp/get/", gids_chnk, "/aaseq")
        fai <- suppressWarnings(system(com, wait = T, intern = T))
        faa <- c(faa, fai)
      }
      ## writeLines ----
      fst <- file(out_fst, "w")
      writeLines(faa, fst)
      close(fst)

    } else {
      stop("The 'outfmt' is either 'gene_id', 'gene_map', 'gene_desc',
           'pathway', 'ntseq', or 'aaseq'. ")
    }
  }
}



#' @rdname fetch_kegg
#' @export
fetch_kegg_react <- function(outfmt) {
  if (outfmt == "reaction_entry") {

    # get list of all reaction ----
    rn <- system("wget -q -O - http://rest.kegg.jp/list/rn/", intern = T)
    rndat <- data.frame(Rid = sub("rn:", "", sapply(strsplit(rn, "\t"), "[", 1)),
                        Description = sapply(strsplit(rn, "\t"), "[", 2),
                        stringsAsFactors = F)

    # get list of all pathway id  and link reaction ----
    pid <- system("wget -q -O - http://rest.kegg.jp/list/pathway/", intern = T)
    mapid <- sub("path:map", "map", sapply(strsplit(pid, "\t"), "[", 1))
    map_rn <- vector("list", length(mapid))
    for (i in 1:length(mapid)) {
      com <- paste0("wget -q -O - http://rest.kegg.jp/link/rn/", mapid[i])
      map_rn[[i]] <- system(com, intern = T, wait = T)
    }

    # Reaction id linked map ----
    MAPids <- NULL; Rid <- NULL
    linkreact <- data.frame(
      Rid = sapply(strsplit(unlist(map_rn), "path:|\trn:"), "[", 3),
      MAPids = sapply(strsplit(unlist(map_rn), "path:|\trn:"), "[", 2)
    ) %>%
      .[which(!is.na(.$Rid)),] %>%
      dplyr::group_by(Rid) %>%
      dplyr::mutate(MAPids = paste(MAPids, collapse = ";")) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup(Rid)

    # merge reaction entry and linked mapids ----
    react_dat <- merge(rndat, linkreact, by = "Rid", all = T)
    paste("KEGG REACTION entries", nrow(react_dat), "at",date())
    return(react_dat)

  } else if (outfmt == "reaction_flat") {
    # get list of all reaction ----
    rn <- system("wget -q -O - http://rest.kegg.jp/list/rn/", intern = T, wait = T)
    urid <- sub("rn:", "", sapply(strsplit(rn, "\t"), "[", 1)) %>%
      rsko::chunk(., 10, T) %>%
      lapply(., paste, collapse = "+")

    # KEGG REACTION ----
    l_react <- vector("list", length(urid))
    for (i in seq_along(urid)) {
      com <- paste0("wget -q -O - http://rest.kegg.jp/get/", urid[[i]])
      l_react[[i]] <- system(com, intern = T, wait = T)
    }
    print(paste("KEGG REACTION entries", length(l_react), "at", date()))

    l_react <- l_react[sapply(l_react, function(x) !identical(x, character(0)))]

    # split the combined flat file by "///" ----
    # # split vector using an element by splitter
    vsplt <- function(x, splt){
      pos <- grep(splt, x)
      splt_x <- split(x, findInterval(seq_along(x), pos))
      res <- lapply(splt_x, function(x) x[!x %in% splt])
      res[!sapply(res, identical, character(0))]
    }
    react <- vsplt(x = unlist(l_react), splt = "///")


    # all tags ----
    # tags <- unique(unlist(sapply(react, function(x)sapply(strsplit(x, " "), "[",1))))
    # tags <-tags[tags !=""]
    tags <- c("ENTRY", "NAME", "DEFINITION", "EQUATION", "ENZYME", "DBLINKS", "COMMENT",
              "RCLASS", "PATHWAY", "ORTHOLOGY", "MODULE", "REMARK", "REFERENCE" )

    # container ----
    ENTRY <- character(); NAME <- character(); DEFINITION <- character();
    EQUATION <- character(); REMARK <- character(); DBLINKS <- character();
    COMMENT <- character(); RCLASS <- character(); ENZYME <- character();
    PATHWAY <- character(); MODULE <- character();  ORTHOLOGY <- character();
    REFERENCE <- character()

    # Process each record ----
    for (j in 1:length(react)) {
      ent <- react[[j]]

      # Where is each tags at flafile----
      ent_tags <- sapply(strsplit(ent, "[ ]"), "[", 1)
      extag <- ent_tags[ent_tags != ""] # Types of tags each entry has
      extagst <- which(ent_tags != "") # Start line of each tag
      extaglen <- diff(c(extagst, length(ent) + 1)) # The length of the description line of each tag
      extaged <- extagst + extaglen - 1 # End line of each tag

      ## What line the specified tag is in (Determine which tag has more than one line)
      ## Replace NA with tags with no description
      n.tag <- match(tags, extag)
      if (any(is.na(n.tag))) {
        eval(parse(text = paste0(tags[is.na(n.tag)] , "[[j]] <- NA;")))
      }

      # Process each lines corresponding to each tags ----
      for (i in 1:length(extaglen)) {
        lines <- ent[extagst[i]:extaged[i]]
        lines <- sub(paste0(extag[i], "[ ]+"), "", lines, fixed = F)
        lines <- gsub("^[ ]+|[ ]+$","", lines)
        ## Edit by tag
        if (extag[i] == "ENTRY") {
          Val <- unlist(strsplit(lines, split = "[ ]+"))[1]
        } else if (extag[i] == "NAME") {
          Val <- paste(lines, collapse = ";")
        } else if (extag[i] == "DEFINITION") {
          Val <- paste(lines, collapse = ";")
        } else if (extag[i] == "EQUATION") {
          Val <- paste(lines, collapse = ";")
        } else if (extag[i] == "REMARK") {
          Val <- paste(lines, collapse = ";")
        } else if (extag[i] == "DBLINKS") {
          Val <- paste(lines, collapse = ";")
        } else if (extag[i] == "COMMENT") {
          Val <- paste(lines, collapse = ";")
        } else if (extag[i] == "RCLASS") {
          Val <- paste(lines, collapse = ";")
        } else if (extag[i] == "ENZYME") {
          Val <- paste(lines, collapse = ";")
        } else if (extag[i] == "PATHWAY") {
          Val <- paste(lines, collapse = ";")
        } else if (extag[i] == "MODULE") {
          Val <- paste(lines, collapse = ";")
        }else if (extag[i] == "ORTHOLOGY") {
          Val <- paste(lines, collapse = ";")
        }else if (extag[i] == "REFERENCE") {
          Val <- paste(lines, collapse = " ")
        }
        ## Assign to tag list
        eval(parse(text = paste0(extag[i], "[[j]] <- Val")))
      }
    }

    # collect ids of these compounds, enzymes, and maps ----
    Subs <- EQUATION %>%
      strsplit(.,  " <=> ") %>%
      sapply(., "[", 1) %>%
      strsplit(., " ") %>%
      sapply(., function(x) {grep("^C[[:digit:]]{5}|^G[[:digit:]]{5}", x, value = T) %>%
          paste(., collapse = ";" )})
    Pros <- EQUATION %>%
      strsplit(.,  " <=> ") %>%
      sapply(., "[", 2) %>%
      strsplit(., " ") %>%
      sapply(., function(x) {grep("^C[[:digit:]]{5}|^G[[:digit:]]{5}", x, value = T) %>%
          paste(., collapse = ";" )})
    KOids <- strsplit(ORTHOLOGY, ";") %>%
      sapply(., function(x) {
        grep("^K[[:digit:]]{5}", sapply(strsplit(x, " "),"[", 1), value = T) %>%
          paste(., collapse = ";")
      })

    PATHWAYids <- sapply(strsplit(PATHWAY, ";"), function(x)paste(sapply(strsplit(x, " "), "[", 1), collapse = ";"))

    # convert to dataframe
    react_flat <- data.frame(
      cbind(ENTRY, NAME, DEFINITION, EQUATION,
            Subs, Pros, KOids, PATHWAYids,
            REMARK, COMMENT, RCLASS, ENZYME, DBLINKS,
            PATHWAY, MODULE,ORTHOLOGY, REFERENCE),
      stringsAsFactors = F)

    # Return data frame ----
    return(react_flat)
  } else {
    stop("'outfmt' is selected from 'reaction_entry' or 'reaction_flat'")
  }
}

#' @rdname fetch_kegg
#' @export
fetch_kegg_cpd <- function(outfmt){
  if (outfmt == "cpd_map") {
    #cpd_map
    com1 <- "wget -q -O - http://rest.kegg.jp/list/cpd"
    com2 <- "wget -q -O - http://rest.kegg.jp/link/pathway/cpd"
    res1 <- system(com1, intern = T)
    res2 <- system(com2, intern = T)

    cp_d <- data.frame(cmpd = sapply(strsplit(sub("^cpd:", "", res1), "\t"), "[", 1),
                       desc = sapply(strsplit(res1, "\t"), "[", 2),
                       stringsAsFactors = F)

    v <- sub("path:", "", sub("^cpd:", "", res2))
    cp_m <- data.frame(cmpd = sapply(strsplit(v, "\t"), "[", 1),
                       map = sapply(strsplit(v, "\t"), "[", 2),
                       stringsAsFactors = F)

    cpd_map <- dplyr::left_join(cp_m, cp_d, by = "cmpd")
    return(cpd_map)

  } else if (outfmt == "cpd_desc") {
    # cpd_desc
    com1 <- "wget -q -O - http://rest.kegg.jp/list/cpd"
    res1 <- system(com1, intern = T)
    cpd_desc <- data.frame(cmpd = sapply(strsplit(sub("^cpd:", "", res1), "\t"), "[", 1),
                           desc = sapply(strsplit(res1, "\t"), "[", 2),
                           stringsAsFactors = F)
    return(cpd_desc)

  }

}
