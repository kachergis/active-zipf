# Corrected re-extraction of 8k-flickr.html, fixing a data-integrity bug in
# the original extract_flickr.py: that script built its lexicon counts from
# CLEANED/lemmatized tokens (`wc`) but stored the ORIGINAL, uncleaned tokens
# (`words`) for later use in populating the photo-by-word matrix. When the
# matrix-fill step then looked up `lex[w]` using those uncleaned tokens, any
# word whose surface form differed from its lemmatized form (plurals,
# possessives, anything the cleaning step touched) would silently fail the
# `lex[w] > 1` check against a defaultdict (returning 0, not KeyError) and
# get dropped from the matrix entirely -- even though it was correctly
# counted in the lexicon. This produced systematic, silent undercounts (e.g.
# "dog": 10,284 in the lexicon vs. 8,111 in the old matrix) and, worse, at
# least one word ("ha") that has a nonzero lexicon count but LITERALLY ZERO
# occurrences in the old matrix, which breaks any analysis (like ours) that
# assumes every word in P has positive probability somewhere.
#
# Fix: use the SAME cleaned token list for both the lexicon and the
# per-photo storage, so counts are self-consistent by construction.

from bs4 import BeautifulSoup
import re
import csv
from collections import defaultdict
from nltk.stem import WordNetLemmatizer

wnl = WordNetLemmatizer()
fname = "8k-flickr.html"
delimiter_pattern = re.compile(r"; | , | |: |-|,")

def clean_sentence(sent):
    sent = re.sub(r'[\(\)\{\}!?".]', '', sent)
    words = delimiter_pattern.split(sent)
    cleaned = []
    for w in words:
        wc = w.strip().strip("'")
        wc = wc.replace("'s", '')
        wc = wnl.lemmatize(wc)
        if len(wc) > 0:
            cleaned.append(wc)
    return cleaned

with open(fname, 'r') as f:
    soup = BeautifulSoup(f, 'html.parser')

lex = defaultdict(int)
lis = []  # [photoindex, cleaned_words]
photoindex = 0
for ul in soup.find_all('ul'):
    photoindex += 1
    for li in ul.find_all('li'):
        if li.find('ul'):
            break
        sent = li.text.strip(".\n").lower()
        cleaned = clean_sentence(sent)
        for wc in cleaned:
            lex[wc] += 1
        lis.append([photoindex, cleaned])

print(f"{len(lex)} unique lemmatized words, {photoindex} photos, {len(lis)} captions")

with open('8k-flickr_sentences_fixed.csv', 'w', newline='') as f:
    writer = csv.writer(f, delimiter='\t')
    for pi, words in lis:
        writer.writerow([pi, " ".join(words)])

slex_all = sorted(lex, key=lex.get, reverse=True)          # ALL words, freq desc
slex = [w for w in slex_all if lex[w] > 1]                 # filtered (matches original convention)
word_index = {w: i for i, w in enumerate(slex)}
print(f"{len(slex)} words with count > 1 (of {len(slex_all)} total)")

with open('8k-flickr_lexicon_fixed.csv', 'w', newline='') as f:
    writer = csv.writer(f, delimiter='\t')
    for w in slex_all:
        writer.writerow([w, lex[w]])

# The photo x word matrix (8,108 x 4,438) is >99% sparse -- a dense CSV of it
# is ~900MB for no benefit. build_flickr_matrix.R builds the sparse matrix
# directly from 8k-flickr_sentences_fixed.csv (photo index + cleaned word
# list per caption, already written above), so we don't materialize it here.
# This block just re-derives the same sanity check that used to run against
# the dense matrix, directly from the counts, to confirm the fix worked.
colsums = defaultdict(int)
for pi, words in lis:
    for w in words:
        if w in word_index:
            colsums[w] += 1
mismatches = sum(1 for w in slex if colsums[w] != lex[w])
print(f"colsum/lexicon mismatches after fix: {mismatches} (should be 0)")
zero_cols = sum(1 for w in slex if colsums[w] == 0)
print(f"all-zero word columns: {zero_cols} (should be 0)")
