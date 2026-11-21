# =============================================================================
# Clause-based syntactic complexity · 節にもとづく統語的複雑さ
# The 14 L2SCA counts and ratios of Lu (2010), computed from a Universal Dependencies
# parse (udpipe, English EWT model) instead of the Stanford parser + Tregex.
# The rules follow the L2SCA Tregex definitions as closely as a dependency parse allows:
#   clause (C)      a predicate with a subject and a finite verb (or an imperative main verb)
#   dependent (DC)  a clause attached as advcl, ccomp, acl(:relcl) or csubj, or coordinated with one
#   T-unit (T)      an independent clause plus everything attached to it; a sentence with no clause
#                   (a fragment) counts as one clause and one T-unit, as in L2SCA
#   complex T (CT)  a T-unit that contains at least one dependent clause
#   VP              clauses plus non-finite verb complements (to + verb, -ing clauses)
#   CP              coordinated nouns, adjectives, adverbs or verb phrases (not coordinated clauses)
#   CN              noun phrases with an adjective, possessive, prepositional phrase, relative clause
#                   or appositive; noun clauses; gerund or infinitive subjects
# udpipe is loaded only when the "Clause-based syntactic complexity" box is ticked. In the browser
# version it is installed from the webR package repository and the model (~16 MB) is downloaded
# from GitHub once per visit. Package names are kept in variables so the Shinylive build does not
# try to bundle udpipe with the rest of the app.
# =============================================================================

UD_PKG <- "udpipe"
UD_MODEL_URL <- "https://raw.githubusercontent.com/jwijffels/udpipe.models.ud.2.5/master/inst/udpipe-ud-2.5-191206/english-ewt-ud-2.5-191206.udpipe"
UD_MODEL_FILE <- "english-ewt-ud-2.5-191206.udpipe"

# Sentence-opening signposts removed for the "MLT without signposts" measure.
# Longer phrases come first so that "first of all" wins over "first".
SIGNPOSTS <- c("first of all", "to begin with", "in conclusion", "to sum it up", "to sum up", "to summarize", "in summary",
               "on the other hand", "in other words", "for this reason", "for these reasons", "as a result", "with all this in mind",
               "most importantly", "in addition", "for example", "for instance", "in my opinion", "in fact",
               "firstly", "first", "secondly", "second", "thirdly", "third", "finally", "lastly", "furthermore", "moreover",
               "additionally", "also", "however", "therefore", "thus", "besides", "consequently", "then", "so", "and", "but")

in_webr <- function() identical(R.version$os, "emscripten")

# Load udpipe and the English model once per session. Returns the model or stops with a message.
ud_env <- new.env()
ud_model <- function() {
  if (!is.null(ud_env$model)) return(ud_env$model)
  pkg <- UD_PKG
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (in_webr()) {
      w <- "webr"
      inst <- tryCatch(get("install", envir = asNamespace(w)), error = function(e) NULL)
      if (!is.null(inst)) try(inst(pkg), silent = TRUE)
    }
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(if (in_webr()) "The udpipe package could not be loaded in this browser." else
             "Install the udpipe package first: install.packages(\"udpipe\")")
  }
  dir <- if (in_webr()) tempdir() else tools::R_user_dir("seeingresearchdesign", "cache")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, UD_MODEL_FILE)
  if (!file.exists(path) || file.info(path)$size < 1e6) {
    ok <- tryCatch(utils::download.file(UD_MODEL_URL, path, mode = "wb", quiet = TRUE) == 0, error = function(e) FALSE)
    if (!ok || !file.exists(path)) stop("The English parsing model could not be downloaded from GitHub.")
  }
  ud_env$model <- getExportedValue(pkg, "udpipe_load_model")(path)
  ud_env$model
}

# Parse texts; returns a data frame with doc_id, sentence_id, token_id, token, lemma, upos, xpos, feats, head_token_id, dep_rel
ud_parse <- function(texts) {
  m <- ud_model()
  ann <- getExportedValue(UD_PKG, "udpipe_annotate")(m, x = gsub("\\s+", " ", trimws(texts)), doc_id = as.character(seq_along(texts)),
                                                     parser = "default", tagger = "default")
  d <- as.data.frame(ann, stringsAsFactors = FALSE)
  d <- d[!grepl("-", d$token_id), , drop = FALSE]                     # drop multiword-token ranges
  d$token_id <- as.integer(d$token_id); d$head_token_id <- as.integer(d$head_token_id)
  d$feats[is.na(d$feats)] <- ""; d$xpos[is.na(d$xpos)] <- ""
  d
}

SCA_FIN <- c("VBD", "VBZ", "VBP", "MD")
SCA_SUBJ <- c("nsubj", "nsubj:pass", "csubj", "csubj:pass", "expl")
SCA_DC <- c("advcl", "ccomp", "acl", "acl:relcl", "csubj", "csubj:pass")

# Counts for one sentence (s = rows of one sentence from ud_parse)
sca_sentence <- function(s) {
  n <- nrow(s); id <- s$token_id; hd <- s$head_token_id; dep <- s$dep_rel
  pos_of <- function(i) match(i, id)
  kids <- lapply(id, function(i) which(hd == i))
  is_word <- s$upos != "PUNCT" & grepl("[A-Za-z0-9]", s$token)
  fin_self <- s$xpos %in% SCA_FIN | grepl("VerbForm=Fin", s$feats, fixed = TRUE)
  finite <- vapply(seq_len(n), function(k) fin_self[k] && !(dep[k] %in% c("aux", "aux:pass", "cop")) ||
                     any(dep[kids[[k]]] %in% c("aux", "aux:pass", "cop") & fin_self[kids[[k]]]), TRUE)
  has_subj <- vapply(seq_len(n), function(k) any(dep[kids[[k]]] %in% SCA_SUBJ), TRUE)
  imper <- dep == "root" & s$xpos == "VB" & !has_subj
  clause <- (has_subj & finite & !(dep %in% c("aux", "aux:pass", "cop"))) | imper
  # dependent clauses: attached as a DC, or a conjunct of one
  dc <- logical(n)
  for (k in which(clause)) {
    j <- k
    while (dep[j] == "conj" && !is.na(pos_of(hd[j]))) j <- pos_of(hd[j])
    dc[k] <- dep[k] %in% SCA_DC || (j != k && clause[j] && dep[j] %in% SCA_DC)
  }
  ind <- clause & !dc
  anc <- function(k) { out <- integer(0); j <- k; while (hd[j] != 0 && !is.na(pos_of(hd[j]))) { j <- pos_of(hd[j]); out <- c(out, j) }; out }
  # complex T-units: the nearest independent clause above each dependent clause
  owners <- unlist(lapply(which(dc), function(k) { a <- anc(k); a <- a[ind[a]]; if (length(a)) a[1] else NA_integer_ }))
  C <- sum(clause); DC <- sum(dc); Tn <- sum(ind)
  CT <- length(unique(owners[!is.na(owners)]))
  if (C == 0) { C <- 1; Tn <- 1 }                                    # fragment
  else if (Tn == 0) { Tn <- 1; CT <- max(CT, 1) }                     # only dependent clauses (e.g. "Because it rains.")
  verbish <- s$upos %in% c("VERB", "AUX") & !(dep %in% c("aux", "aux:pass", "cop"))
  VP <- sum(clause) + sum(verbish & !clause & dep %in% c("xcomp", "advcl", "csubj", "ccomp"))
  # coordinate phrases: in UD the cc word hangs on the second conjunct
  CP <- 0
  for (k in which(dep == "cc")) {
    cj <- pos_of(hd[k]); if (is.na(cj) || dep[cj] != "conj") next
    if (clause[cj] && has_subj[cj]) next                               # coordinated clauses are T-units, not phrases
    first <- pos_of(hd[cj]); if (is.na(first)) next
    if (s$upos[first] %in% c("NOUN", "PROPN", "PRON", "ADJ", "ADV", "VERB", "AUX", "NUM")) CP <- CP + 1
  }
  # complex nominals
  noun_mod <- vapply(seq_len(n), function(k) s$upos[k] %in% c("NOUN", "PROPN") && dep[k] != "compound" &&
                       any(dep[kids[[k]]] %in% c("amod", "nmod:poss", "nmod", "acl", "acl:relcl", "appos")), TRUE)
  CN <- sum(noun_mod) + sum(clause & dep %in% c("ccomp", "csubj", "csubj:pass")) +
        sum(!clause & dep == "csubj" & s$xpos %in% c("VBG", "VB"))
  # words left after a sentence-opening signpost (and its comma) are removed
  words <- tolower(s$token[is_word])
  W <- length(words); drop <- 0
  joined <- paste(words, collapse = " ")
  for (p in SIGNPOSTS) if (startsWith(joined, p) && (nchar(joined) == nchar(p) || substr(joined, nchar(p) + 1, nchar(p) + 1) == " ")) {
    drop <- length(strsplit(p, " ")[[1]]); break
  }
  c(W = W, S = 1, VP = VP, C = C, T = Tn, DC = DC, CT = CT, CP = CP, CN = CN, DM = drop)
}

# One row of counts and ratios per text
syntax_measures <- function(texts) {
  empty <- function() c(n_tunits = NA, n_clauses = NA, mlt = NA, mlc = NA, mls_parsed = NA, c_s = NA, c_t = NA, t_s = NA,
                        dc_c = NA, dc_t = NA, ct_t = NA, cp_t = NA, cp_c = NA, cn_t = NA, cn_c = NA, vp_t = NA,
                        signpost_words = NA, mlt_no_signposts = NA)
  ok <- nzchar(trimws(texts)) & !is.na(texts)
  out <- matrix(NA_real_, length(texts), length(empty()), dimnames = list(NULL, names(empty())))
  if (!any(ok)) return(as.data.frame(out))
  d <- ud_parse(texts[ok])
  dv <- function(a, b) if (b > 0) a / b else NA_real_
  rows <- lapply(split(d, factor(d$doc_id, levels = as.character(seq_len(sum(ok))))), function(doc) {
    if (!nrow(doc)) return(empty())
    cnt <- Reduce(`+`, lapply(split(doc, doc$sentence_id), function(s) {
      if (!any(s$upos != "PUNCT" & grepl("[A-Za-z0-9]", s$token))) return(c(W = 0, S = 0, VP = 0, C = 0, T = 0, DC = 0, CT = 0, CP = 0, CN = 0, DM = 0))
      sca_sentence(s)
    }))
    with(as.list(cnt), c(n_tunits = T, n_clauses = C, mlt = dv(W, T), mlc = dv(W, C), mls_parsed = dv(W, S), c_s = dv(C, S),
                         c_t = dv(C, T), t_s = dv(T, S), dc_c = dv(DC, C), dc_t = dv(DC, T), ct_t = dv(CT, T),
                         cp_t = dv(CP, T), cp_c = dv(CP, C), cn_t = dv(CN, T), cn_c = dv(CN, C), vp_t = dv(VP, T),
                         signpost_words = DM, mlt_no_signposts = dv(W - DM, T)))
  })
  out[ok, ] <- do.call(rbind, rows)
  as.data.frame(out)
}
