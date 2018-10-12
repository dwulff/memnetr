
# Install the XML package
library('XML')

# Get the entire list of files
get_paragraph = function(turn){

  # create paragraph
  paragraph = ''

  # iterate over sentences
  n_sentences = length(turn)
  for(s in 1:n_sentences){

    # extract elements
    sent = turn[[s]]
    sent = sent[!names(sent) %in% c('.attrs','pause','unclear',
                                    'shift','vocal','event','align',
                                    'gap','trunc','comment')]

    if(length(sent) == 0) break

    # create container
    sentence = character(length(sent))

    # iterate over words
    n_words = length(sent)
    for(w in 1:n_words){
      word = sent[[w]]
      sentence[w] = ifelse('w' %in% names(word),word$w$text,word$text)
    }

    # add sentence to paragraph
    paragraph = paste(paragraph,paste(sentence, collapse = ''))

  }

  # return
  return (paragraph)
}

get_description = function(xml_file){

  # get person descriptions
  descr = xpathApply(xml_file,"//particDesc")
  if(length(descr) == 0) return(NULL)
  descr = xmlChildren(descr[[1]])

  # description container
  description = matrix(nrow = length(descr), ncol = 7)

  # extract person descriptions
  n_descr = length(descr)
  for(d in 1:n_descr){

    des = descr[[d]]
    id = xpathApply(des,'@xml:id')[[1]]
    ageGr = xpathApply(des,'@ageGroup')[[1]]
    role = xpathApply(des,'@role')[[1]]
    sex = xpathApply(des,'@sex')[[1]]

    nams = names(des)
    age  = ifelse('age' %in% nams,xmlValue(des['age'][[1]]),NA)
    name = ifelse('persName' %in% nams,xmlValue(des['persName'][[1]]),NA)
    occu = ifelse('occupation' %in% nams,xmlValue(des['occupation'][[1]]),NA)

    # bind results
    description[d,] = c(id,name,role,age,ageGr,sex,occu)
  }

  # return
  return(description)
}

get_conversation = function(speakers,description,conversation_id,verbose = FALSE){

  # create container
  results = list()

  # counter
  counter = 0

  # iterate over speakers
  n_speakers = length(speakers)
  if(n_speakers == 0) return(NULL)
  for(p in 1:n_speakers){

    # feedback
    if(verbose == TRUE) cat(File,' - ',conversation_id,' - ',p,'\n')

    # extract person
    turn = speakers[[p]] # Take just one person

    # if person is null then go to next
    if(is.null(turn)) next

    # convert person to list
    turn = xmlToList(turn) # Convert this person to a list

    # extract id
    id = as.character(turn$.attrs) # Take the id name of the person who speaks

    # limit to sentences
    turn = turn[names(turn) == 's']
    if(length(turn) == 0) next

    # get description
    if(!is.null(description)){
      if(id %in% description[,1]){
        descr = description[description[,1] == id,-1]
      } else {
        descr = rep(NA,6)
      }
    } else {
      descr = rep(NA,6)
    }

    # get paragraph
    paragraph = get_paragraph(turn)

    # increment counter
    counter = counter + 1

    # store results
    results[[counter]] = c(File,conversation_id,id,descr,paragraph)
  }

  # bind different paragraph to data.frame
  conversation = data.frame(do.call(rbind,results), stringsAsFactors = F)
  names(conversation) = c('file','conversation','speaker','name','role','age',
                          'age_group','sex','occupation','text')

  # adding the listeners
  conversation$listeners = paste(unique(conversation$speaker),collapse='_')
  conversation = conversation[,c(1:9,11,10)]

  # return
  return(conversation)
}



#' BNC reader
#'
#' Reads BNC XML file
#'
#' @param filename
#'
#' @export
bnc_reader = function(filename, verbose = FALSE){

  # load xml
  xml_file = xmlParse(file = filename)

  # get type
  type = xmlGetAttr(xpathApply(xml_file,"//stext")[[1]],'type')

  # get description matrix
  description = get_description(xml_file)

  # check for divs
  n_divs = length(xpathApply(xml_file,'//div'))

  if(n_divs == 0){
    speakers = xpathApply(xml_file,"//u")
    conversation = get_conversation(speakers,description,1,verbose)
  } else {
    conversations = list()
    for(d in 1:n_divs){
      speakers = xpathApply(xml_file,paste0('//div[',d,']/u'))
      conversations[[d]] = get_conversation(speakers,description,d,verbose)
    }
    conversation = data.frame(do.call(rbind,conversations), stringsAsFactors = F)
  }

  # add type
  conversation$type = type

  # output
  return(conversation)

}




