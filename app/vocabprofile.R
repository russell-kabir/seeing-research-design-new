# =============================================================================
# Vocabulary profiling (base R; runs in webR). Used by the Writing tab.
#
# Lists (in data/wordlists/):
#   ngsl_tsl.csv  NGSL 1.01 (bands 1–500, 501–1000, 1001–2000, 2001–2801, supplementary)
#                 + TOEIC Service List 1.1. Browne, Culligan & Phillips; CC BY-SA 4.0.
#   cefrj.csv     CEFR-J Wordlist 1.5 (A1–B2; Tono Laboratory, TUFS; free with citation)
#                 + Octanove Vocabulary Profile C1/C2 1.0 (CC BY-SA 4.0).
#   lemmas.csv    inflected form → headword, from Měchura's English lemmatization list (ODbL),
#                 kept only where the headword is on one of the lists.
# A user can also upload any list as a CSV with columns headword, level.
# =============================================================================

VP_LEMMA <- local({
  d <- read.csv("data/wordlists/lemmas.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
  # pronoun forms the lemmatizer leaves alone (NGSL counts my, me, mine… under I)
  pr <- list(i = c("me", "my", "mine", "myself"), you = c("your", "yours", "yourself", "yourselves"),
             he = c("him", "his", "himself"), she = c("her", "hers", "herself"), it = c("its", "itself"),
             we = c("us", "our", "ours", "ourselves"), they = c("them", "their", "theirs", "themselves"))
  d <- rbind(d, data.frame(form = unlist(pr), lemma = rep(names(pr), lengths(pr)), stringsAsFactors = FALSE))
  split(d$lemma, d$form)
})

vp_read_list <- function(path, order) {
  d <- read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8", colClasses = "character")
  list(level = setNames(d$band, d$headword), order = order)
}

VP_LISTS <- list(
  ngsl = c(vp_read_list("data/wordlists/ngsl_tsl.csv", c("1", "2", "3", "4", "sup", "tsl")),
           list(en = "NGSL 1.01 + TOEIC Service List 1.1", ja = "NGSL 1.01 ＋ TOEIC サービスリスト 1.1",
                labels = c(`1` = "NGSL 1–500", `2` = "NGSL 501–1000", `3` = "NGSL 1001–2000", `4` = "NGSL 2001–2801",
                           sup = "NGSL supplementary", tsl = "TSL (TOEIC)"),
                cols = c(`1` = "#1f6f5c", `2` = "#3f9a7f", `3` = "#d9a521", `4` = "#d9762b", sup = "#7a8b99", tsl = "#7d5ba6"),
                prefix = "ngsl")),
  cefrj = c(vp_read_list("data/wordlists/cefrj.csv", c("A1", "A2", "B1", "B2", "C1", "C2")),
            list(en = "CEFR-J Wordlist 1.5 + Octanove C1/C2", ja = "CEFR-J 語彙表 1.5 ＋ Octanove C1/C2",
                 labels = c(A1 = "A1", A2 = "A2", B1 = "B1", B2 = "B2", C1 = "C1", C2 = "C2"),
                 cols = c(A1 = "#1f6f5c", A2 = "#3f9a7f", B1 = "#d9a521", B2 = "#d9762b", C1 = "#c0473a", C2 = "#7d5ba6"),
                 prefix = "cefr"))
)
VP_OFF_COL <- "#9aa3ab"

# Build a list from an uploaded CSV (headword + level columns; levels keep the order they first appear,
# or numeric order if they are numbers)
vp_custom_list <- function(df) {
  cols <- names(df); lc <- tolower(cols)
  h <- cols[match(TRUE, lc %in% c("headword", "word", "lemma", "item"), nomatch = 1)]
  l <- cols[match(TRUE, lc %in% c("level", "band", "cefr", "grade", "rank_band", "group"), nomatch = min(2, length(cols)))]
  hw <- tolower(trimws(df[[h]])); lv <- trimws(df[[l]])
  keep <- nzchar(hw) & nzchar(lv) & !grepl(" ", hw)
  hw <- hw[keep]; lv <- lv[keep]
  ord <- unique(lv)
  if (all(!is.na(suppressWarnings(as.numeric(ord))))) ord <- ord[order(as.numeric(ord))]
  # a headword listed twice keeps its easiest (earliest) level
  rk <- match(lv, ord); o <- order(hw, rk); hw <- hw[o]; lv <- lv[o]; first <- !duplicated(hw)
  pal <- grDevices::colorRampPalette(c("#1f6f5c", "#d9a521", "#c0473a", "#7d5ba6"))(length(ord))
  list(level = setNames(lv[first], hw[first]), order = ord, en = "Your list", ja = "アップロードしたリスト",
       labels = setNames(ord, ord), cols = setNames(pal, ord), prefix = "custom")
}

CONTRACT <- c("n't" = "not", "'re" = "be", "'m" = "be", "'ve" = "have", "'ll" = "will", "'d" = "would", "'s" = "")
NT_IRREG <- c("can't" = "can", "won't" = "will", "shan't" = "shall", "ain't" = "be")

# Tokens with positions, ready for profiling. One row per profiled word part.
vp_tokens <- function(text) {
  m <- gregexpr(WORD_RX, text, perl = TRUE)[[1]]
  if (m[1] == -1) return(data.frame(start = integer(0), end = integer(0), form = character(0), cap = logical(0), initial = logical(0)))
  st <- as.integer(m); en <- as.integer(st + attr(m, "match.length") - 1L)
  raw <- substring(text, st, en)
  before <- substring(text, pmax(1, st - 4), st - 1)
  initial <- st == 1 | grepl("[.!?:\"「]\\s*$", before) | grepl("^\\s*$", substring(text, 1, st - 1))
  out <- list()
  for (i in seq_along(raw)) {
    w <- raw[i]; lw <- tolower(w); parts <- character(0)
    if (lw %in% names(NT_IRREG)) parts <- c(NT_IRREG[[lw]], "not")
    else if (lw == "cannot") parts <- c("can", "not")
    else {
      hit <- FALSE
      for (cl in names(CONTRACT)) if (endsWith(lw, cl) && nchar(lw) > nchar(cl)) {
        parts <- c(substr(lw, 1, nchar(lw) - nchar(cl)), CONTRACT[[cl]]); hit <- TRUE; break
      }
      if (!hit) parts <- strsplit(lw, "-", fixed = TRUE)[[1]]
    }
    parts <- parts[nzchar(parts)]
    for (p in parts) out[[length(out) + 1]] <- list(start = st[i], end = en[i], form = p,
                                                   cap = grepl("^[A-Z]", w) && w != "I", initial = initial[i])
  }
  data.frame(start = vapply(out, `[[`, 0L, "start"), end = vapply(out, `[[`, 0L, "end"),
             form = vapply(out, `[[`, "", "form"), cap = vapply(out, `[[`, TRUE, "cap"),
             initial = vapply(out, `[[`, TRUE, "initial"), stringsAsFactors = FALSE)
}

# Look up one form: the form itself if it is a headword, else its lemma(s); when a form could
# belong to two headwords (leaves → leaf / leave) the easier level wins.
vp_lookup <- function(form, L) {
  lev <- L$level; cand <- c(if (!is.na(lev[form])) form, intersect(VP_LEMMA[[form]] %||% character(0), names(lev)))
  if (!length(cand)) return(c(NA_character_, NA_character_))
  r <- match(lev[cand], L$order)
  best <- cand[r == min(r)][1]
  c(best, lev[[best]])
}

# Word-level profile for one text. Numbers are skipped; capitalised words that are not
# sentence-initial and not on the list are treated as proper nouns and skipped.
vp_profile_words <- function(text, L) {
  tk <- vp_tokens(text)
  if (!nrow(tk)) return(cbind(tk, headword = character(0), level = character(0), status = character(0)))
  hw <- character(nrow(tk)); lv <- character(nrow(tk)); status <- character(nrow(tk))
  for (i in seq_len(nrow(tk))) {
    f <- tk$form[i]
    if (grepl("^[0-9]", f)) { status[i] <- "number"; hw[i] <- NA; lv[i] <- NA; next }
    r <- vp_lookup(f, L); hw[i] <- r[1]; lv[i] <- r[2]
    status[i] <- if (!is.na(lv[i])) "on" else if (tk$cap[i] && !tk$initial[i]) "proper" else "off"
  }
  tk$headword <- hw; tk$level <- lv; tk$status <- status
  tk
}

# Paragraph-level summary: % of profiled tokens per level, off-list %, and types per level
vp_summary <- function(wp, L) {
  use <- wp[wp$status %in% c("on", "off"), , drop = FALSE]
  n <- nrow(use)
  pct <- vapply(L$order, function(l) if (n) 100 * sum(use$level == l, na.rm = TRUE) / n else NA_real_, 0)
  off <- if (n) 100 * sum(use$status == "off") / n else NA_real_
  out <- c(setNames(pct, paste0(L$prefix, "_", L$order)), setNames(off, paste0(L$prefix, "_off")))
  out[paste0(L$prefix, "_tokens")] <- n
  out[paste0(L$prefix, "_types_off")] <- length(unique(use$form[use$status == "off"]))
  if (L$prefix == "cefr") {
    on <- use[use$status == "on", , drop = FALSE]
    num <- match(on$level, L$order)
    out["cefr_mean"] <- if (nrow(on)) mean(num) else NA_real_
    out["cefr_b1plus"] <- if (n) 100 * sum(num >= 3) / n else NA_real_
  }
  if (L$prefix == "ngsl") {
    out["ngsl_cov"] <- if (n) 100 * sum(use$level %in% c("1", "2", "3", "4", "sup"), na.rm = TRUE) / n else NA_real_
    out["ngsl_tsl_cov"] <- if (n) 100 * sum(use$status == "on") / n else NA_real_
  }
  out
}
