ph_wf = read.csv("8k-flickr_photo_by_wordfreq.csv", header=F)
# could add colnames (words)

save(ph_wf, file="8k-flickr_photo_by_wordfreq.RData")