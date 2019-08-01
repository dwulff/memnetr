# # get java lib
# dyn.load('/Applications/IBM/SPSS/Statistics/23/jre/lib/server/libjvm.dylib')
#
# # load packages
# require(Rcpp)
# require(NLP)
# require(openNLP)
# require(openNLPmodels.en)
# require(stringi)
#
#
# # tree split function
# tree_split = function(sents){
#
#   # keep track
#   print('New job')
#
#   # define annotators
#   sent_token_annotator = Maxent_Sent_Token_Annotator()
#   word_token_annotator = Maxent_Word_Token_Annotator()
#   parse_annotator = Parse_Annotator()
#
#   # load tree_extract
#   sourceCpp('/Users/dwulff/Desktop/tree_extract.cpp')
#
#   # keep track
#   i = 0
#
#   sentences = list()
#   for(sentence in sents){
#
#     # keep track
#     i = i + 1
#     #print(i)
#
#     # check if sentence has letters
#     if(grepl('[[:alpha:]]',sentence)){
#
#       # annotate
#       annotated = annotate(sentence, list(sent_token_annotator, word_token_annotator))
#
#       # parse
#       parsed = suppressMessages(unlist(parse_annotator(sentence, annotated)$feature))
#
#       # get sentehces
#       sentences[[i]] = tree_extract(parsed,'S')
#
#       # collect garbage
#       #gc()
#
#     } else {
#
#       # get sentehces
#       sentences[[i]] = NULL
#
#     }
#   }
#
#   return(sentences)
# }
#
