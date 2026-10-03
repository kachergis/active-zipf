#m = re.compile(r'^<li><.*>(.*)</a></li>$')

from bs4 import BeautifulSoup
import re
import csv
from collections import defaultdict

fname = "8k-flickr.html"
file = open(fname, 'r')
soup = BeautifulSoup(file, 'html.parser')

delimiter_pattern = re.compile(r"; | , | |: |-|,")

# just need to find ul, increment index counter, and  
# store index with the next 5 li's

photoindex = 0
uls = soup.find_all('ul')

lex = defaultdict(int)

def add_to_lexicon(sent):
	# return list of indices corresponding to  
	# unique words in the sentence
	sent = re.sub('[\(\)\{\}!?".]', '', sent)
	words = delimiter_pattern.split(sent) #sent.split()
	for w in words:
		wc = w.strip()
		wc = wc.strip("'")
		if len(wc)>0:
			lex[wc] += 1
	return words


# go through the sentences and build the lexicon
lis = []
for ul in uls:
	photoindex += 1
	for li in ul.find_all('li'):
		if li.find('ul'):
			break
		sent = li.text.encode("utf-8").strip(".\n").lower()
		words = add_to_lexicon(sent)
		lis.append([photoindex,words])
with open('8k-flickr_sentences.csv','wb') as lexfile:
	writer = csv.writer(lexfile, delimiter='\t')
	for li in lis:
		writer.writerow([li[0], " ".join(li[1])])
	#writer.writerows(lis)


slex = sorted(lex, key=lex.get, reverse=True) # sort by frequency
with open('8k-flickr_lexicon.csv','wb') as lexfile:
	writer = csv.writer(lexfile, delimiter='\t')
	for w in slex:
		writer.writerow([w,lex[w]])

#for li in lis:
#	print li
print(len(lex)) # 8725 unique words, 5207 appear >1 time

import numpy
# each row will be the counts of unique words 
# for the sentences describing one picture
m = numpy.zeros(shape=(photoindex+1, 5207))

#print lis[0]
for s in lis:
	for w in s[1]:
		#print w, lex[w]
		if lex[w]>1: # ignore the 3500 single items
			i = slex.index(w)
			m[s[0]][i] += 1

numpy.savetxt("8k-flickr_photo_by_wordfreq.csv", m, delimiter=",")
