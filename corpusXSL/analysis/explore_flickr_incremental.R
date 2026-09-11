# Exploratory: what does the Flickr8k data look like as input to an
# incremental cross-situational learner? Content-word filtering, learnable
# vocabulary size (words whose eliminative candidate set narrows to a
# singleton given ALL their captions), co-occurrence density.

setwd("/Users/gkacherg/Documents/GitHub/active-zipf/8k-flickr")
lex <- read.delim("8k-flickr_lexicon_fixed.csv", header = FALSE, col.names = c("word", "count"), quote = "")
sent <- read.delim("8k-flickr_sentences_fixed.csv", header = FALSE, col.names = c("photo", "words"),
                    quote = "", colClasses = c("integer", "character"))

# Hand-curated English function-word stoplist (articles, prepositions,
# pronouns, auxiliaries, conjunctions, quantifiers, copulas) -- these
# co-occur with nearly everything and are neither plausible "referents" nor
# the kind of word this line of work is about (cf. the paper's CHILDES
# analysis, which is about object/meaning words).
STOP <- c("a","an","the","and","or","but","of","in","on","at","to","for","with","by","from",
          "as","is","are","was","were","be","been","being","am","this","that","these","those",
          "it","its","he","she","they","them","his","her","their","him","i","you","we","me","my",
          "your","our","us","who","what","which","there","here","then","than","so","up","down",
          "out","off","over","under","into","onto","near","next","behind","front","back","side",
          "while","during","after","before","above","below","between","through","around","along",
          "some","any","all","both","each","few","more","most","other","another","no","not","only",
          "very","too","also","just","about","across","against","toward","towards","upon","per",
          "s","t","re","ve","ll","d","m","o","one","two","three","four","five","six","several",
          "group","its","'s")

lex$content <- !(lex$word %in% STOP) & nchar(lex$word) > 1 & lex$count > 1
content_words <- lex$word[lex$content]
cat("total lexicon:", nrow(lex), " | count>1:", sum(lex$count > 1),
    " | content words (count>1, not stop):", length(content_words), "\n")

wi <- setNames(seq_along(content_words), content_words)

# per-caption content-word index vectors
caption_words <- lapply(strsplit(sent$words, " ", fixed = TRUE), function(w) unname(wi[w[w %in% content_words]]))
caps_by_photo <- split(caption_words, sent$photo)
caps_by_photo <- caps_by_photo[order(as.integer(names(caps_by_photo)))]
M <- length(caps_by_photo)
cat("photos:", M, " | mean content words/caption:",
    round(mean(lengths(caption_words)), 2), "\n")

# For each content word, the full eliminative candidate set given ALL its
# captions = intersection of the word-sets of every caption it appears in.
# Word is "learnable" iff that intersection is exactly {itself}.
N <- length(content_words)
final_cand_size <- integer(N)
appears_in_caps <- integer(N)
# accumulate: for each word, intersect across all captions containing it
cand <- vector("list", N)
for (ci in seq_along(caption_words)) {
  cw <- caption_words[[ci]]
  if (length(cw) < 1) next
  for (w in cw) {
    appears_in_caps[w] <- appears_in_caps[w] + 1L
    if (is.null(cand[[w]])) cand[[w]] <- cw
    else cand[[w]] <- intersect(cand[[w]], cw)
  }
}
final_cand_size <- sapply(cand, function(x) if (is.null(x)) NA_integer_ else length(x))
learnable <- which(final_cand_size == 1)
cat("\nwords appearing in >=1 caption:", sum(!is.na(final_cand_size)), "\n")
cat("LEARNABLE (final candidate set == {self}):", length(learnable),
    sprintf(" (%.1f%% of content words)\n", 100 * length(learnable) / N))
cat("final candidate-set size distribution (all words):\n")
print(quantile(final_cand_size, c(0, .25, .5, .75, .9, .99, 1), na.rm = TRUE))

# among learnable words, how many captions do they appear in? (governs how
# much opportunity a passive learner has to encounter them)
cat("\ncaptions-per-word, learnable words:\n")
print(quantile(appears_in_caps[learnable], c(0, .1, .25, .5, .75, .9, 1)))
cat("learnable words appearing in only 1-2 captions (near-unlearnable in practice):",
    sum(appears_in_caps[learnable] <= 2), "\n")

saveRDS(list(caps_by_photo = caps_by_photo, content_words = content_words,
             learnable = learnable, appears_in_caps = appears_in_caps,
             final_cand_size = final_cand_size),
        "/Users/gkacherg/Documents/GitHub/active-zipf/corpusXSL/analysis/flickr_incremental_data.rds")
cat("\nsaved flickr_incremental_data.rds\n")
