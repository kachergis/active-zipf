# Zipf exponent of the CHILDES word-frequency distribution used in the main
# paper (hapax legomena excluded, 27,673 words), separately for function and
# content words (Reviewer 1: the two classes may follow different slopes).
#
# Fits p_r = r^-a / H_{W,a} over frequency ranks r = 1..W within each word
# class, the parametrization used throughout the paper, by maximum likelihood
# on the token counts. A log-log least-squares slope over ranks 10..W is given
# for comparison. Also reports the fast-mapping penalty W^(a-1) H_{W,a}
# relative to a uniform distribution of the same size (main paper, Eq. 6).

suppressMessages(library(dplyr))
d <- read.csv("../childes_english_word_freq_cleaned_noHapaxes.csv", stringsAsFactors = FALSE)

# Closed-class English function words: determiners, pronouns, auxiliaries and
# modals (with contracted forms), prepositions, conjunctions, wh-words,
# negation, and common particles. Interjections and discourse markers are
# listed separately, so the content-word fit can be checked with and without them.
FUNCTION <- c(
  "a","an","the","this","that","these","those","some","any","no","every","each","all","both","either","neither",
  "much","many","more","most","few","less","least","other","another","such","what","which","whose",
  "i","me","my","mine","myself","you","your","yours","yourself","yourselves","he","him","his","himself",
  "she","her","hers","herself","it","its","itself","we","us","our","ours","ourselves","they","them",
  "their","theirs","themselves","one","ones","someone","somebody","something","anyone","anybody","anything",
  "everyone","everybody","everything","nobody","nothing","none",
  "be","am","is","are","was","were","been","being","do","does","did","doing","done","have","has","had","having",
  "can","could","will","would","shall","should","may","might","must","gonna","wanna","gotta","hafta",
  "i'm","you're","he's","she's","it's","we're","they're","that's","there's","here's","what's","where's",
  "who's","let's","i'll","you'll","we'll","it'll","i've","you've","we've","i'd","you'd",
  "don't","doesn't","didn't","can't","won't","isn't","aren't","wasn't","weren't","couldn't","wouldn't",
  "shouldn't","haven't","hasn't",
  "of","in","on","at","to","for","with","by","from","about","into","onto","over","under","up","down",
  "out","off","through","around","after","before","behind","between","near","like","than","without",
  "inside","outside","upon","across","along","against","toward","towards",
  "and","or","but","so","because","if","when","while","until","though","although","then","as",
  "who","whom","where","why","how","whether",
  "not","n't","there","here","now","just","too","very","also","only","again","still","even","yet","ever",
  "s","'s","t","re","ve","ll","d","m","na","ta")
INTERJ <- c("oh","yeah","yes","no","okay","ok","uh","um","uhhuh","uhoh","hmm","mhm","mm","ah","wow","hey",
            "hi","bye","please","thank","thanks","huh","yep","nope","ooh","whoops","oops","alright","right",
            "well","boom","yay","shh","ow")

d <- d %>% filter(word_count > 0) %>%
  mutate(class = ifelse(word %in% FUNCTION, "function",
                        ifelse(word %in% INTERJ, "interjection", "content")))

fit_zipf <- function(counts) {
  n <- sort(counts, decreasing = TRUE); W <- length(n); r <- seq_len(W); lr <- log(r)
  nll <- function(a) a * sum(n * lr) + sum(n) * log(sum(r^-a))
  a_mle <- optimize(nll, c(0.1, 3))$minimum
  keep <- r >= 10
  a_ols <- -unname(coef(lm(log(n[keep] / sum(n)) ~ lr[keep]))[2])
  H <- sum(r^-a_mle)
  c(W = W, tokens = sum(n), a_mle = a_mle, a_ols_rank10plus = a_ols,
    penalty_vs_uniform = W^(a_mle - 1) * H)
}

res <- rbind(
  all = fit_zipf(d$word_count),
  function_words = fit_zipf(d$word_count[d$class == "function"]),
  content_words = fit_zipf(d$word_count[d$class == "content"]),
  content_and_interjections = fit_zipf(d$word_count[d$class != "function"]))
print(round(res, 3))
write.csv(as.data.frame(res) %>% mutate(set = rownames(res), .before = 1),
          "childes_exponent_fits.csv", row.names = FALSE)
cat("saved childes_exponent_fits.csv\n")
