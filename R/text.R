
#' expand abbreviations
#'
#' @export


expand_abbreviations = function(text){

  # handle abbreviations
  corrections = matrix(
    c("i'm", "i am",
      "n't", " not",
      "'d", " would",
      "'ll", " will",
      "'ve", " have",
      "let's","let us",
      "'re"," are",
      "'s",""), ncol = 2, byrow = T)

  # replace
  for(i in 1:nrow(corrections)) {
    text = stringr::str_replace_all(text, corrections[i,1], corrections[i,2])
    }

  # return
  return(text)
  }


#' Sentence tokenizer
#'
#' @export

sent_tokenizer = function(text) {

  # sentence pattren
  pattern = '[^[:space:]][^[.!?;]]*[.!?;]|[^[:space:]][^[.!?;]]*$'

  # extract sentences
  return(stringr::str_extract_all(text, pattern, simplify = F)[[1]])

  }


#' Token tokenizers
#'
#' @export

token_tokenizer = function(text) {

  # token pattern
  pattern = "[:digit:]+[:punct:][:digit:]+|[:alnum:]+[-]{1}[:alnum:]+[-]*[:alnum:]*|[:alnum:]+[']{1}[:alnum:]+|[:alnum:]+"

  # limits
  lims = str_locate_all(text, pattern)

  # split
  split_string_list(text, lims)

  }


#' Count tokens
#'
#' @export

count_tokens = function(text, order = T){

  # tokenize
  words = tm::Boost_tokenizer(text)

  # get tab
  tab = as.data.frame(cnt_tokens(words))

  # name
  names(tab) = c('token', 'count')

  # transform counts
  tab$count = as.numeric(.subset2(tab, 'count'))

  # order
  if(order == TRUE) tab = tab[order(.subset2(tab, 'count'), decreasing = T), ]

  # remove rownames
  rownames(tab) = NULL

  # return
  return(tab)
  }



#' Detect specific words
#'
#' @export
find_words = function(text, words, count = TRUE){

  # determine terms
  terms = paste0(paste0('[[^[:alnum:]]|^]',words,'[[^[:alnum:]]|$]'),
                 collapse='|')

  # search and return
  if(count == FALSE) return(stringr::str_detect(text, terms))
  if(count == TRUE) return(stringr::str_count(text, terms))
}


#' Pos tag
#'
#' @export

setup_annotators = function(){

  dyn.load('/Applications/IBM/SPSS/Statistics/23/jre/lib/server/libjvm.dylib')

  # load packages
  if(!require(NLP)) require(NLP)
  if(!require(openNLP)) require(openNLP)
  if(!require(openNLPmodels.en)) require(openNLPmodels.en)

  ## Need sentence and word token annotations.
  if(!exists("sent_token_annotator")) .GlobalEnv$sent_token_annotator = Maxent_Sent_Token_Annotator()
  if(!exists("word_token_annotator")) .GlobalEnv$word_token_annotator = Maxent_Word_Token_Annotator()
  if(!exists("pos_tag_annotator")) .GlobalEnv$pos_tag_annotator = Maxent_POS_Tag_Annotator()

  }

#' Pos tag
#'
#' @export

get_tag_sent = function(sentence){

  # setup annotators
  setup_annotators()

  # annotate
  annotation = annotate(sentence,list(.GlobalEnv$sent_token_annotator,
                             .GlobalEnv$word_token_annotator))

  # pos tag
  pos_annotation = annotate(sentence, .GlobalEnv$pos_tag_annotator, annotation)

  # extract tags
  pos_tags = unlist(pos_annotation$features[pos_annotation$type == 'word'])

  return(pos_tags)

  }

#' Detect past
#'
#' @export

detect_past = function(sentences, parallel = TRUE){

  # detect function
  .GlobalEnv$detect = function(sent){

    # annotate
    pos_tags = get_tag_sent(sent)

    # test
    return(any(c('VBD','VBN') %in% pos_tags))

    }

  if(length(sentences) == 1){

    return(detect(sentences))

  } else {

    # detect many fun
    .GlobalEnv$detect_many = function(sents){

      # create container
      out = logical(length(sents))

      # iterate
      len_sent = length(sents)
      for(i in 1:len_sent) out[i] = .GlobalEnv$detect(sents[i])

      # return
      return(out)
      }

    if(parallel == FALSE){

      return(.GlobalEnv$detect_many(sentences))

      } else {

      # jobs
      jobs = split(sentences, ceiling((1:length(sentences)) / (length(sentences)/parallel::detectCores())))

      # run on cluster
      cl = parallel::makeCluster(parallel::detectCores())
      parallel::clusterExport(cl, c('detect','get_tag_sent','setup_annotators'))
      msg = suppressMessages(parallel::clusterEvalQ(cl, setup_annotators()))
      out = parallel::clusterApplyLB(cl, x = jobs, fun = .GlobalEnv$detect_many)
      parallel::stopCluster(cl)

      # return
      return(unlist(out))
      }

    }

  }




#' Detect future
#'
#' @export

detect_future = function(sentences){

  # future words
  words = c('will','going to','gonna')

  # future terms
  future_terms = paste0(c(paste0('[^[:alnum:]]',words,'[^[:alnum:]]'),
                          paste0('^',words,'[^[:alnum:]]'),
                          paste0('[^[:alnum:]]',words,'$'),
                          paste0('^',words,'$'),
                          "[[:alnum:]]+'ll",
                          "[[:alnum:]]+'ll$"),
                        collapse='|')

  # detect
  return(str_detect(sentences, future_terms))

  }

#' Get sentiment
#'
#' @export

get_sentiment = function(sentences, which = 'afinn', fun = 'mean'){

  # sents
  if(which == 'afinn'){
    out = numeric(length(sentences))
    } else{
    out = character(length(sentences))
    }

  # loop
  n_sent = length(sentences)
  for(i in 1:n_sent){

    # get sentence
    sentence = sentences[i]

    # tokenize
    tokens = tm::Boost_tokenizer(sentence)

    # get lex
    if(which == 'nrc') lex = tidytext::get_sentiments('nrc')
    if(which == 'afinn') lex = tidytext::get_sentiments('afinn')
    if(which == 'bing') lex = tidytext::get_sentiments('bing')
    if(which == 'loughran') lex = tidytext::get_sentiments('loughran')

    # get sentiment
    if(which == 'afinn'){
      sent = f.utils::replace_cn(tokens, lex[[1]], lex[[2]])
      } else {
      sent = f.utils::replace_cc(tokens, lex[[1]], lex[[2]])
      }

    # aggregate
    if(which == 'afinn'){
      out[i] = eval(call(fun,sent,na.rm=T))
      } else {
      tab = table(sent)
      tab = tab[names(tab) != 'NA']
      if(length(tab) > 0) out[i] = names(tab)[which.max(tab)] else out[i] = NA
      }
    }

  # detect
  return(out)

  }


