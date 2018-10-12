

require(Rcpp)
require(stringr)
require(tm)
require(f.utils)
require(microbenchmark)
sourceCpp('~/Dropbox (2.0)/Work/Software/memnet/src/string_split.cpp')
sourceCpp('~/Dropbox (2.0)/Work/Software/memnet/src/beagle.cpp')
sourceCpp('~/Dropbox (2.0)/Work/Software/memnet/src/word_count.cpp')
source('~/Dropbox (2.0)/Work/Software/memnet/R/text.R')
source('~/Dropbox (2.0)/Work/Software/memnet/R/BEAGLE.R')

options(stringsAsFactors = F)

# get bnc text
d = readRDS('~/Dropbox (2.0)/Work/Projects/Side projects/25 DD&NL/1 Data/BNC/BNCProcessed.RDS')

d_sel = subset(d, nchar(d$text) > 200 & nchar(d$text) < 202)

# concat text
text = paste0(d_sel$text, collapse = ' ')

# tolower
text = str_to_lower(text)

# count words
counts = count_tokens(text)
counts = counts[!counts$token %in% tm::stopwords('en'),]
counts = counts[order(counts$count, decreasing = T),]

# remove stops
add_stops = c(counts[nchar(counts > 2),], c('erm','one','two','three','didn'))
counts = counts[!counts$token %in% c(tm::stopwords('en'),add_stops), ]

# set targets
targets = counts[1:1000, 'token']

# run beagle
rep = beagle_c(text, targets, return = 'all', add_stops = add_stops)

# get cosines
cosines = cosine_mat(rep[[1]])
rownames(cosines) = targets
colnames(cosines) = targets

cos = cosines['job', ]
cos[order(cos,decreasing = T)][1:10]

summary(c(cosines))

hist(cos)



car1 = rep$representation[targets == 'car']
week1 = rep$representation[targets == 'week']


# limit text
sub_text = paste0(d$text[1:10100], collapse = ' ')

# run beagle
rep = beagle_c(sub_text, targets, return = 'all')

car2 = rep$representation[targets == 'car']
week2 = rep$representation[targets == 'week']

cosine(car1,car2)
cosine(week1,week2)

cosine(week1,car1)
cosine(week1,week2)

# get cosines
cosines = cosine_mat(rep[[1]])
rownames(cosines) = targets
colnames(cosines) = targets

cos = cosines['car', ]
cos[order(cos,decreasing = T)][1:10]

cosines['car', 'week']

cosines


rep$indices[[4]]
ind_to_char(rep$indices[[4]], rep)


tmp  = normalize(runif(1000,-1,1))
tmp2 = normalize(runif(1000,-1,1))
tmp3 = normalize(runif(1000,-1,1))

for(i in 1:10) {tmp = tmp + tmp2}
print(cosine(tmp,tmp2))
for(i in 1:100) {tmp = tmp + normalize(runif(1000,-1,1))}
print(cosine(tmp,tmp2))


tmp  = normalize(runif(100000,-1,1))
tmp2 = normalize(runif(100000,-1,1))
tmp3 = normalize(runif(100000,-1,1))

for(i in 1:10) {tmp = chunk_norm(tmp,tmp2)}
print(cosine(tmp,tmp2))
for(i in 1:10) {tmp = chunk_norm(tmp,normalize(runif(100000,-1,1)))}
print(cosine(tmp,tmp2))










tmp = normalize(runif(1000,-1,1))
tmp2 = normalize(runif(1000,-1,1))
tmp3 = normalize(runif(1000,-1,1))

for(i in 1:10) {tmp = tmp + tmp2}; print(cosine(tmp,tmp2))
for(i in 1:100) {tmp = tmp + tmp3};print(cosine(tmp,tmp2))

tmp = normalize(runif(1000,-1,1))
tmp2 = normalize(runif(1000,-1,1))
tmp3 = normalize(runif(1000,-1,1))

for(i in 1:10) {tmp = chunk(tmp,tmp2)};print(cosine(tmp,tmp2))
for(i in 1:100) {tmp = chunk(tmp,normalize(runif(1000,-1,1)))};print(cosine(tmp,tmp2))


tmp = normalize(runif(1000,-1,1))
tmp2 = normalize(runif(1000,-1,1))
tmp3 = normalize(runif(1000,-1,1))

for(i in 1:10) {tmp = chunk(tmp,tmp2)}; print(cosine(tmp,tmp2))
for(i in 1:100) {tmp = chunk(tmp,tmp3)};print(cosine(tmp,tmp2))


tmp = normalize(runif(1000,-1,1))
tmp2 = normalize(runif(1000,-1,1))

plot(chunk(chunk_norm(tmp,tmp2),tmp3),normalize(tmp + tmp2 + tmp3))

plot(chunk(chunk_norm(tmp,tmp2),tmp3), chunk(chunk_norm(tmp,tmp3),tmp2))


table(chunk(chunk_norm(tmp,tmp2),tmp3)-normalize(tmp + tmp2 + tmp3))


tmp = chunk(tmp, runif(1000,-1,1))

table(str_detect(d$text[1:10100],'week'),
  str_detect(d$text[1:10100],'car'))


basis = rep[[2]]
map = data.frame('word'=basis[[1]][[1]],'index'=basis[[1]][[2]])



cosines = cosine_mat(rep[[1]])
rownames(cosines) = targets
colnames(cosines) = targets

cos = cosines['car', ]
cos[order(cos,decreasing = T)][1:10]





cosine_vec = upper_mat(cosines)

e1071::skewness(cosine_vec,na.rm = T)


hist(cosine_vec)







hist(cosines)

count_tokens(text)

microbenchmark(token_tokenizer(text),Boost_tokenizer(text))

mean(token_tokenizer(text)[[1]] == Boost_tokenizer(text))

# sent split
sents = sent_tokenizer(text)

# # get tokens
# tokens = unlist(token_tokenizer(sents))
# token_tab = table(tokens)
# token_tab = token_tab[order(token_tab, decreasing = T)]



proc.time()[3] - t







max(basis[[1]][[2]])


sents[[4]]
basis[[5]]
basis2[[5]]

basis[[1]][[1]][basis[[1]][[2]] %in% basis[[5]]]

x = basis2[[1]][[1]][basis2[[1]][[2]] %in% basis2[[5]]]
x
x[!x %in% tm::stopwords('en')]
basis[[1]][[1]][basis[[1]][[2]] %in% basis[[5]]]

rownames(b) = targets
colnames(b) = targets

e = b['school',]
e[order(e,decreasing = T)][1:20]




table(unlist(indices))

table(unlist(sapply(num_sents, function(x) if(2 %in% x) x)))


two_semts = sapply(num_sents, function(x) if(2 %in% x) x)
two_semts = two_semts[length(two_semts)>0]
table(unlist(two_semts))[1:20]
a[[2]][1:20,]




t = proc.time()[3]
res = beagle_basis(sents,lims,targets)
proc.time()[3] - t

t = proc.time()[3]
res = beagle_basis2(sents,lims,targets)
proc.time()[3] - t

sum(lengths(res[[1]][[3]])>0)



head(res[[1]][[1]])
head(res[[1]][[2]])
length(res[[1]][[3]])

object.size(res)

res = beagle_basis(sents,lims,targets)

make_word(2000)





t = proc.time()[3]
res = beagle_basis(sents,lims,targets)
proc.time()[3] - t

t = proc.time()[3]
a = token_tokenizer(sents)
proc.time()[3] - t

