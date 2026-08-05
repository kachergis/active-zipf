from __future__ import division
from pylab import *
from numpy.random import choice
from scipy.optimize import bisect

SUN = load('sundb/sun_full.npz')
X = SUN['X']

# Normalization for  0 <= X[i,j] <= 1
# Each X[i,j] is a coin for the j th word-object type in the i th scene type
X = X / X.sum(axis=1, keepdims=1)
(L, N) = X.shape

# Expected number of word-object pairs for coefficent `a`
def f_expected(a):
	return mean(clip(a * X, 0, 1).sum(axis=1))

# The maximum determined by the dataset
def f_max():
	return mean((X > 0).sum(axis=1))

N_WORDS = 7
N_OBJECTS = 3
N_PAIRS = N
REQUIRED = max(N_WORDS, N_OBJECTS)

fmax = f_max()
print 'max', fmax
if REQUIRED > fmax:
	raise Exception('Dataset cannot establish for #required %d (max %f)' % (REQUIRED, fmax))

# Find the coefficent for words
a = bisect(lambda a: N_WORDS - 1 - f_expected(a), 0, fmax + 1)
print 'a', a, f_expected(a)

# Find the coefficent for objects
b = bisect(lambda a: N_OBJECTS - 1 - f_expected(a), 0, fmax + 1)
print 'b', b, f_expected(b)

words = {}
objects = {}
NSAMPLES = 1000
for t in xrange(NSAMPLES):
	# Choose a scene type
	i = randint(L)
	p = X[i]

	I = find(p > 0)
	q = p[I]
	q = q / sum(q)
	j = choice(len(q), p=q)
	
	# Determine # of words
	c = random(len(q))
	c[j] = 0
	w = I[a * q > c]

	# Determine # of objects
	c = random(len(q))
	c[j] = 0
	o = I[b * q > c]

	# Decide word-object types; min(N_WORDS, N_OBJECTS) are the same
	if len(w) == len(o):
		o = w
	if len(w) > len(o):
		p[I[j]] = 0
		o = choice(w, size=len(o) - 1, p=p[w] / sum(p[w]), replace=False)
		o = r_[I[j], o]
	if len(w) < len(o):
		p[I[j]] = 0
		w = choice(o, size=len(w) - 1, p=p[o] / sum(p[o]), replace=False)
		w = r_[I[j], w]
	
	words[t] = w
	objects[t] = o
	
	if len(intersect1d(w, o)) == 0:
		print len(w), len(o)
	assert len(w) >= 1
	assert len(o) >= 1

print 'N_WORDS', N_WORDS
print 'mean(#words)', mean(map(len, words.values()))
print 'N_OBJECTS', N_OBJECTS
print 'mean(#objects)', mean(map(len, objects.values()))
h = bincount(map(len, words.values()))
plot(h, 'o--')
print 'len(word)', h
h = bincount(map(len, objects.values()))
plot(h, 'o--')
print 'len(objects)', h
show()

