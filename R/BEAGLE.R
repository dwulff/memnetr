#' Run beagle context
#'
#' @export

beagle_c = function(text, targets, precision = 1000, stopwords = 'en', to_lower = TRUE, return = 'representation_only', add_stops = NULL){

  # Note: Consider transforming utftoint and back before sending it to cpp
  # Note: use boost tokenizer -> much faster
  # Note: Implement stemmer

  # tolower
  if(to_lower) text = str_to_lower(text)

  # sent split
  sents = sent_tokenizer(text)

  # stops
  if(stopwords == 'en') stops = tm::stopwords('en')

  # add stops
  if(!is.null(add_stops)) stops = c(stops, add_stops)

  # check targets against stops
  if(any(targets %in% stops)) {
    ind = which(targets %in% stops)
    msg = paste(paste(targets[ind],ind,sep=':'),collapse='; ')
    message(paste0('Stopword in targets:\n', msg))
    }

  # remove stopwords contained in targets
  stops = stops[!stops %in% targets]

  # determine limits
  pattern = "[:digit:]+[:punct:][:digit:]+|[:alnum:]+[-]{1}[:alnum:]+[-]*[:alnum:]*|[:alnum:]+[']{1}[:alnum:]+|[:alnum:]+"
  lims = stringr::str_locate_all(sents, pattern)

  # get beagle basis
  basis = beagle_basis_stops(sents, lims, targets, stops)

  # identify sentences with indices
  indices = basis[[1]][[3]]
  sel = lengths(indices) > 0

  # reduce sents & indices
  num_sents = basis[c(F, sel)]
  indices = indices[sel]

  # run beagle
  representation = beagle_con(num_sents, indices, length(targets), precision)

  #return basis
  if(return != 'representation_only') {

    # get dictionary
    dict = data.frame('token' = basis[[1]][[1]], 'index' = basis[[1]][[2]])

    # includes
    incl = basis[[1]][[3]]
    pos = basis[[1]][[4]]

    # get sents as inds
    inds = basis[-1]


    # out object
    out = list('representation' = representation,
               'sentences' = sents,
               'indices' = inds,
               'dictionary' = dict,
               'includes' = incl,
               'positions' = pos)

    # set class
    class(out) = 'beagle'

    # return
    return(out);
    }

  # return
  return(representation)

  }

#' Transform indices to character
#'
#' @export
ind_to_char = function(inds, object){

  # test class
  if(class(object) != 'beagle') stop('object not of class beagle')

  # get dict
  dict = object$dictionary

  # transform fun
  ind_to_char_ = function(ind) paste0(f.utils::replace_nc(ind, dict$index, dict$token), collapse = ' ')

  # return
  if(is.list(inds)){
    return(sapply(inds, ind_to_char_))
    } else {
    return(ind_to_char_(inds))
    }
  }


