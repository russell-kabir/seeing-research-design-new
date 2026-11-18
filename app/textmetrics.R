# =============================================================================
# Text metrics for the Writing development and AUT tabs (base R only, so they
# run in webR / Shinylive). No UI here: writing.R and aut.R call these.
#
#   * CSV reading that copes with Excel's "CSV UTF-8" BOM and Shift-JIS files
#   * English tokens and sentences
#   * Lexical diversity: TTR, Guiraud's R, MATTR, MTLD, HD-D
#   * Sentence-level complexity proxies (no parser needed)
#   * Articles, including a/an mismatch flags
#   * Causal connectives after Halliday & Hasan (1976), with KWIC lines
#   * Krippendorff's alpha (nominal, ordinal, interval)
# =============================================================================

# ---- CSV --------------------------------------------------------------------
# Returns list(df, note). Every column is read as text.
read_text_csv <- function(path) {
  raw <- readBin(path, "raw", file.info(path)$size)
  if (!length(raw)) stop("The file is empty.")
  note <- NULL
  if (length(raw) >= 3 && identical(raw[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) raw <- raw[-(1:3)]
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  if (!validUTF8(txt)) {
    conv <- tryCatch(iconv(txt, "CP932", "UTF-8"), error = function(e) NA_character_)
    if (is.na(conv) || !validUTF8(conv))
      stop("This file isn't UTF-8. In Excel, use File → Save As → \"CSV UTF-8 (comma delimited)\".")
    txt <- conv
    note <- "Converted from Shift-JIS. Next time, save as \"CSV UTF-8\" in Excel."
  }
  txt <- gsub("\r\n?", "\n", txt)
  df <- read.csv(text = txt, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character",
                 na.strings = character(0), encoding = "UTF-8", strip.white = FALSE)
  names(df) <- trimws(names(df))
  df <- df[, nzchar(names(df)), drop = FALSE]
  keep <- apply(df, 1, function(r) any(nzchar(trimws(r))))
  list(df = df[keep, , drop = FALSE], note = note)
}

# Guess which column holds what, from common names.
guess_col <- function(cols, candidates, fallback = NULL) {
  lc <- tolower(gsub("[^A-Za-z0-9]", "", cols))
  for (c in candidates) { hit <- which(lc == c); if (length(hit)) return(cols[hit[1]]) }
  fallback
}

# ---- Tokens and sentences ---------------------------------------------------
norm_text <- function(x) {
  x <- gsub("[‘’ʼ`´]", "'", x)
  x <- gsub("[“”]", "\"", x)
  x <- gsub("[–—]", " - ", x)
  x <- gsub(" ", " ", x)
  x
}

WORD_RX <- "[A-Za-z0-9]+(?:['][A-Za-z]+)*(?:-[A-Za-z0-9]+)*"

# lower-case word tokens (letters, digits, internal apostrophes and hyphens)
tokens <- function(x) {
  m <- regmatches(x, gregexpr(WORD_RX, x, perl = TRUE))[[1]]
  tolower(m)
}

ABBREV <- c("mr", "mrs", "ms", "dr", "prof", "st", "vs", "etc", "e.g", "i.e", "no", "jr", "sr", "fig", "approx")

# split into sentences on . ! ? (and full-width equivalents), keeping abbreviations together
sentences <- function(x) {
  x <- gsub("\\s+", " ", trimws(x))
  if (!nzchar(x)) return(character(0))
  pieces <- strsplit(x, "(?<=[.!?。！？])[\"')\\]]*\\s+(?=[\"'(\\[]?[A-Z0-9])", perl = TRUE)[[1]]
  out <- character(0)
  for (p in pieces) {
    if (length(out)) {
      prev <- out[length(out)]
      last <- tolower(sub(".*?([A-Za-z.]+)\\.$", "\\1", prev, perl = TRUE))
      if (grepl("\\.$", prev) && last %in% ABBREV) { out[length(out)] <- paste(prev, p); next }
    }
    out <- c(out, p)
  }
  out[grepl("[A-Za-z0-9]", out)]
}

# ---- Lexical diversity ------------------------------------------------------
ld_ttr <- function(tok) if (length(tok)) length(unique(tok)) / length(tok) else NA_real_
ld_guiraud <- function(tok) if (length(tok)) length(unique(tok)) / sqrt(length(tok)) else NA_real_

# Moving-average TTR (Covington & McFall, 2010). NA when the text is shorter than the window.
ld_mattr <- function(tok, window = 50) {
  n <- length(tok)
  if (n < window) return(NA_real_)
  mean(vapply(seq_len(n - window + 1), function(i) length(unique(tok[i:(i + window - 1)])) / window, 0))
}

# MTLD (McCarthy & Jarvis, 2010): mean of the forward and backward passes, TTR threshold .72.
# A factor ends when the running TTR falls to .72 or below. TAALED / the Python
# lexical_diversity package instead end a factor only below .72 and after >= 10 words
# (strict = TRUE, min_len = 10); both settings are offered in the tab.
mtld_pass <- function(tok, thr = 0.72, strict = FALSE, min_len = 0) {
  factors <- 0; seen <- character(0); cnt <- 0; ttr <- 1
  for (w in tok) {
    cnt <- cnt + 1
    if (!(w %in% seen)) seen <- c(seen, w)
    ttr <- length(seen) / cnt
    done <- if (strict) ttr < thr else ttr <= thr
    if (done && cnt >= min_len) { factors <- factors + 1; seen <- character(0); cnt <- 0; ttr <- 1 }
  }
  if (cnt > 0) factors <- factors + (1 - ttr) / (1 - thr)
  if (factors > 0) length(tok) / factors else NA_real_
}
ld_mtld <- function(tok, thr = 0.72, taaled = FALSE) {
  if (length(tok) < 2) return(NA_real_)
  st <- isTRUE(taaled); ml <- if (st) 10 else 0
  (mtld_pass(tok, thr, st, ml) + mtld_pass(rev(tok), thr, st, ml)) / 2
}

# HD-D (McCarthy & Jarvis, 2007): for each word type, the chance it appears in a random
# 42-word sample drawn without replacement, summed and divided by 42 (a 0–1, TTR-like scale).
# Multiply by 42 for the "D"-style sum some tools report.
ld_hdd <- function(tok, sample = 42) {
  n <- length(tok)
  if (n < sample) return(NA_real_)
  f <- table(tok)
  sum(1 - dhyper(0, f, n - f, sample)) / sample
}

# ---- Sentence-level complexity proxies ----------------------------------------
# Without a parser we count surface markers. They are crude, but they are computed the
# same way for every text, so learner (written) and edited versions stay comparable.
SUBORD <- c("because", "although", "though", "whereas", "while", "if", "unless", "when", "whenever",
            "since", "until", "before", "after", "once", "which", "who", "whom", "whose", "where", "whether")
COORD <- c("and", "but", "or", "yet", "nor")

# ---- Articles ---------------------------------------------------------------
AN_EXCEPT_RX <- "^(one|once|uni[^n]|unit|use|usu|uti|ufo|ura|ure|uro|eu|ewe|u-)"
A_EXCEPT_RX  <- "^(hour|honest|honou?r|heir|herb)"
# TRUE where the article doesn't fit the next word ("a apple", "an book"). Spelling-based,
# so it is a flag for a human to check, not a verdict.
aan_bad <- function(art, nxt) {
  vowel <- grepl("^[aeiou]", nxt)
  (art == "a"  & ((vowel & !grepl(AN_EXCEPT_RX, nxt)) | grepl(A_EXCEPT_RX, nxt))) |
  (art == "an" & ((!vowel & !grepl(A_EXCEPT_RX, nxt) & !grepl("^[0-9]", nxt)) | (vowel & grepl(AN_EXCEPT_RX, nxt))))
}
article_issues <- function(tok) {
  idx <- which(tok %in% c("a", "an") & seq_along(tok) < length(tok))
  if (!length(idx)) return(integer(0))
  idx[aan_bad(tok[idx], tok[idx + 1])]
}
# Character positions of flagged a/an + next word in an (already normalized) text
find_aan <- function(text) {
  m <- gregexpr("(?<![A-Za-z0-9'])(a|an)\\s+([A-Za-z0-9][A-Za-z0-9'-]*)", text, perl = TRUE, ignore.case = TRUE)[[1]]
  if (m[1] == -1) return(data.frame(start = integer(0), end = integer(0)))
  st <- attr(m, "capture.start"); ln <- attr(m, "capture.length")
  art <- tolower(substring(text, st[, 1], st[, 1] + ln[, 1] - 1))
  nxt <- tolower(substring(text, st[, 2], st[, 2] + ln[, 2] - 1))
  bad <- aan_bad(art, nxt)
  data.frame(start = as.integer(m)[bad], end = (as.integer(m) + attr(m, "match.length") - 1)[bad])
}

# ---- Causal connectives (Halliday & Hasan, 1976, Table of conjunctive relations: causal) ----
# key = column in the results; pattern = regex on lower-cased text (word boundaries added).
# rule: NULL, or a special disambiguation rule applied below.
DEM <- "(?:this|that)"
CAUSAL <- list(
  list(key = "gen_simple",   en = "General, simple", ja = "一般・単純", col = "External/internal", ex = "so, then, hence, therefore",
       pats = c("hence", "therefore", "so", "then")),
  list(key = "gen_emphatic", en = "General, emphatic", ja = "一般・強調", col = "External/internal", ex = "consequently, because of this",
       pats = c("consequently", paste0("because of ", DEM))),
  list(key = "spec_reason",  en = "Specific: reason", ja = "特定：理由", col = "External/internal", ex = "for this reason, on account of this",
       pats = c(paste0("for ", DEM, " reason"), paste0("on account of ", DEM))),
  list(key = "spec_result",  en = "Specific: result", ja = "特定：結果", col = "External/internal", ex = "as a result, in consequence",
       pats = c("as a result", "in consequence")),
  list(key = "spec_purpose", en = "Specific: purpose", ja = "特定：目的", col = "External/internal", ex = "for this purpose, with this in mind",
       pats = c(paste0("for ", DEM, " purpose"), paste0("with ", DEM, " in mind"), "so that")),
  list(key = "rev_simple",   en = "Reversed causal, simple", ja = "逆因果・単純", col = "Internal", ex = "for, because",
       pats = c("because", "for")),
  list(key = "int_reason",   en = "Internal causal: reason", ja = "内的因果：理由", col = "Internal", ex = "it follows, on this basis",
       pats = c("it follows", paste0("on ", DEM, " basis"))),
  list(key = "int_result",   en = "Internal causal: result", ja = "内的因果：結果", col = "Internal", ex = "arising out of this",
       pats = c(paste0("arising (?:out of|from) ", DEM))),
  list(key = "int_purpose",  en = "Internal causal: purpose", ja = "内的因果：目的", col = "Internal", ex = "to this end",
       pats = c(paste0("to ", DEM, " end"))),
  list(key = "cond_simple",  en = "Conditional, simple", ja = "条件・単純", col = "Conditional", ex = "then (after if)",
       pats = character(0)),
  list(key = "cond_emphatic", en = "Conditional, emphatic", ja = "条件・強調", col = "Conditional", ex = "in that case, in such an event, that being so",
       pats = c(paste0("in ", DEM, " case"), "in such (?:an|a) (?:event|case)", "that being so")),
  list(key = "cond_general", en = "Conditional, generalized", ja = "条件・一般化", col = "Conditional", ex = "under the circumstances",
       pats = c("under (?:the|these|those) circumstances")),
  list(key = "cond_reversed", en = "Conditional, reversed polarity", ja = "条件・逆極性", col = "Conditional", ex = "otherwise, under other circumstances",
       pats = c("under other circumstances", "otherwise")),
  list(key = "resp_direct",  en = "Respective, direct", ja = "観点・直接", col = "Respective", ex = "in this respect, in this regard, with reference to this",
       pats = c(paste0("in ", DEM, " (?:respect|regard)"), paste0("with (?:reference|regard|respect) to ", DEM))),
  list(key = "resp_reversed", en = "Respective, reversed polarity", ja = "観点・逆極性", col = "Respective", ex = "in other respects, aside from this",
       pats = c("in other respects", paste0("aside from ", DEM), paste0("apart from ", DEM)))
)
names(CAUSAL) <- vapply(CAUSAL, `[[`, "", "key")

# Not in Halliday & Hasan's table, but common in learner and academic writing. Counted separately.
CAUSAL_EXT <- list(key = "extended", en = "Extended (not in H&H)", ja = "拡張（H&H の表にない）", col = "Extended",
                   ex = "thus, that's why, due to this, accordingly, as a consequence",
                   pats = c("thus", "accordingly", "as a consequence", paste0("(?:that|this) is why"), paste0("(?:that|this)'s why"),
                            paste0("due to ", DEM), paste0("owing to ", DEM), paste0("thanks to ", DEM), paste0("as a result of ", DEM)))

SUBJ_START <- c("i", "we", "you", "they", "he", "she", "it", "there", "this", "that", "these", "those", "the", "a", "an",
                "my", "our", "your", "their", "his", "her", "its", "people", "students", "many", "most", "some", "everyone",
                "nobody", "someone", "if", "when", "japanese", "children", "teachers", "parents")
SO_NOT <- c("much", "many", "far", "long", "few", "little", "called", "to", "on", "as", "-")

# Find causal connectives in one text (run norm_text() on it first, so positions match).
# Returns a data.frame of hits with character positions.
find_causal <- function(text, extended = TRUE) {
  low <- tolower(text)
  claimed <- rep(FALSE, nchar(low))
  specs <- c(CAUSAL, if (extended) list(extended = CAUSAL_EXT))
  pats <- do.call(rbind, lapply(specs, function(s) if (length(s$pats))
    data.frame(key = s$key, pat = s$pats, stringsAsFactors = FALSE)))
  pats$nw <- lengths(strsplit(gsub("\\(\\?:[^)]*\\)", "x", pats$pat), " "))
  pats <- pats[order(-pats$nw, -nchar(pats$pat)), ]
  hits <- list()
  for (i in seq_len(nrow(pats))) {
    rx <- paste0("(?<![A-Za-z'])", pats$pat[i], "(?![A-Za-z'])")
    m <- gregexpr(rx, low, perl = TRUE)[[1]]
    if (m[1] == -1) next
    for (j in seq_along(m)) {
      s <- m[j]; e <- s + attr(m, "match.length")[j] - 1
      if (any(claimed[s:e])) next
      claimed[s:e] <- TRUE
      hits[[length(hits) + 1]] <- data.frame(key = pats$key[i], start = s, end = e, stringsAsFactors = FALSE)
    }
  }
  if (!length(hits)) return(data.frame(key = character(0), start = integer(0), end = integer(0), match = character(0),
                                       review = logical(0), why = character(0), stringsAsFactors = FALSE))
  h <- do.call(rbind, hits)
  h <- h[order(h$start), ]
  h$match <- substring(text, h$start, h$end)
  h$review <- FALSE; h$why <- ""
  drop <- rep(FALSE, nrow(h))
  for (i in seq_len(nrow(h))) {
    w <- tolower(h$match[i])
    before <- substr(low, max(1, h$start[i] - 60), h$start[i] - 1)
    after <- substr(low, h$end[i] + 1, min(nchar(low), h$end[i] + 40))
    prev_w <- tail(regmatches(before, gregexpr("[a-z']+|[,.;:!?]", before, perl = TRUE))[[1]], 1)
    next_w <- head(regmatches(after, gregexpr("[a-z0-9']+|[,.;:!?-]", after, perl = TRUE))[[1]], 1)
    prev_w <- if (length(prev_w)) prev_w else ""; next_w <- if (length(next_w)) next_w else ""
    sent_start <- !nzchar(trimws(before)) || grepl("[.!?]\\s*[\"')]*\\s*$", before)
    sent_before <- sub(".*[.!?]", "", before)
    if (w == "so") {
      if (next_w %in% SO_NOT || prev_w %in% c("do", "does", "did", "done", "doing", "think", "hope", "believe", "or", "even", "and") && !(prev_w == "and" && next_w %in% SUBJ_START) ||
          grepl("^\\s*,?\\s*on\\b", after)) { drop[i] <- TRUE; next }
      if (!(sent_start || prev_w %in% c(",", ";", "and") || next_w %in% c(SUBJ_START, ","))) {
        h$review[i] <- TRUE; h$why[i] <- "so: maybe an intensifier (so good)"
      }
    } else if (w == "then") {
      if (grepl("\\bif\\b", sent_before)) { h$key[i] <- "cond_simple"; next }
      if (prev_w %in% c("and", "since", "until", "by", "from", "just", "back", "now")) { drop[i] <- TRUE; next }
      h$review[i] <- TRUE; h$why[i] <- "then: causal, conditional or time?"
    } else if (w == "for") {
      if (!(prev_w %in% c(",", ";") && next_w %in% SUBJ_START) || next_w %in% c("example", "instance")) { drop[i] <- TRUE; next }
      h$review[i] <- TRUE; h$why[i] <- "for: conjunction or preposition?"
    } else if (w == "otherwise") {
      if (!sent_start) { h$review[i] <- TRUE; h$why[i] <- "otherwise: conditional or respective?" }
    } else if (grepl("^in (this|that) case$", w)) {
      h$review[i] <- TRUE; h$why[i] <- "in this/that case: condition or 'in this instance'?"
    } else if (w %in% c("as a result", "because") && next_w == "of") {
      h$review[i] <- TRUE; h$why[i] <- paste0(w, " of + noun: a prepositional phrase, not a linking adverb")
    } else if (w == "so that") {
      h$review[i] <- TRUE; h$why[i] <- "so that: structural purpose clause"
    }
  }
  h[!drop, , drop = FALSE]
}

kwic_line <- function(text, start, end, width = 45) {
  t <- gsub("\\s+", " ", text)
  list(left = substr(t, max(1, start - width), start - 1), hit = substr(t, start, end),
       right = substr(t, end + 1, min(nchar(t), end + width)))
}

# ---- All writing measures for one text -----------------------------------------
writing_measures <- function(text, extended = TRUE, taaled = FALSE) {
  text <- norm_text(text %||% "")
  tok <- tokens(text)
  words <- tok[grepl("[a-z]", tok)]
  n <- length(tok); nw <- length(words)
  sents <- sentences(text); ns <- max(1, length(sents))
  per100 <- function(k) if (n) 100 * k / n else NA_real_
  ch <- find_causal(text, extended)
  cnt <- vapply(c(names(CAUSAL), "extended"), function(k) sum(ch$key == k), 0)
  hh <- sum(cnt[names(CAUSAL)])
  the <- sum(tok == "the"); aan <- sum(tok %in% c("a", "an"))
  it <- 0
  if (n > 1) it <- sum(tok[-n] == "i" & tok[-1] == "think")
  c(list(n_words = n, n_types = length(unique(words)), n_sentences = length(sents),
         mls = if (length(sents)) n / ns else NA_real_,
         sub_per_sent = sum(tok %in% SUBORD) / ns, coord_per_sent = sum(tok %in% COORD) / ns,
         ttr = ld_ttr(words), guiraud = ld_guiraud(words), mattr50 = ld_mattr(words, 50),
         mtld = ld_mtld(words, taaled = taaled), hdd = ld_hdd(words),
         art_the = the, art_a_an = aan, art_per100 = per100(the + aan),
         art_def_share = if (the + aan) the / (the + aan) else NA_real_,
         art_aan_flags = length(article_issues(tok)),
         i_think = it, i_think_per100 = per100(it),
         causal_hh = hh, causal_hh_per100 = per100(hh), causal_hh_per_sent = hh / ns,
         causal_ext = cnt[["extended"]], causal_review = sum(ch$review)),
    as.list(setNames(cnt[names(CAUSAL)], paste0("c_", names(CAUSAL)))))
}

# ---- Krippendorff's alpha ------------------------------------------------------
# m: units x coders matrix (NA = missing). level: "nominal", "ordinal", "interval".
kripp_alpha <- function(m, level = c("nominal", "ordinal", "interval")) {
  level <- match.arg(level)
  m <- as.matrix(m)
  keep <- rowSums(!is.na(m)) >= 2
  m <- m[keep, , drop = FALSE]
  if (!nrow(m)) return(NA_real_)
  vals <- sort(unique(as.vector(m[!is.na(m)])))
  if (length(vals) < 2) return(if (length(vals) == 1) 1 else NA_real_)
  K <- length(vals)
  o <- matrix(0, K, K)
  for (u in seq_len(nrow(m))) {
    x <- match(m[u, !is.na(m[u, ])], vals); mu <- length(x)
    for (a in seq_len(mu)) for (b in seq_len(mu)) if (a != b) o[x[a], x[b]] <- o[x[a], x[b]] + 1 / (mu - 1)
  }
  nc <- rowSums(o); n <- sum(nc)
  d2 <- matrix(0, K, K)
  for (c in seq_len(K)) for (k in seq_len(K)) {
    d2[c, k] <- switch(level,
      nominal = as.numeric(c != k),
      interval = (as.numeric(vals[c]) - as.numeric(vals[k]))^2,
      ordinal = if (c == k) 0 else { lo <- min(c, k); hi <- max(c, k); (sum(nc[lo:hi]) - (nc[c] + nc[k]) / 2)^2 })
  }
  De <- sum(outer(nc, nc) * d2)
  if (De == 0) return(1)
  1 - (n - 1) * sum(o * d2) / De
}
