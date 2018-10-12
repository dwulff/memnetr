nzz_reader = function(Files, keys = "", verbose = FALSE){

  # container
  texts = list()

  # loop through files
  for(i in 1:length(Files)){

    # get Filename
    File = Files[i]

    # verbose
    if(verbose) cat('Processing ', File, '\n')

    # read file
    text = read_texts(File)

    # search for keys
    if(keys[1] != ""){

      # define pattern
      pattern = paste0(stringr::str_to_lower(keys),
                       collapse = '|')

      # create lower text
      text_lower = stringr::str_to_lower(text)

      # check if keys in text
      if(find_words(text_lower, pattern, FALSE)){

        # extract content
        text_split = stringr::str_split(text, pattern = '@ENDOFLINE@')[[1]]
        text_split = stringr::str_split(text_split, pattern = '\t')
        text_table = do.call(rbind, text_split[lengths(text_split) == 4])

        # extract key matching entries
        text_elems = stringr::str_to_lower(text_table[, 4])
        sel = find_words(text_elems, animals, FALSE)
        text_table = text_table[sel, ]
        texts[[i]] = text_table

        } else {

        # store empty
        texts[[i]] = NULL
        }

    } else {

      # extract content
      text_split = stringr::str_split(text, pattern = '@ENDOFLINE@')[[1]]
      text_split = stringr::str_split(text_split, pattern = '\t')
      text_table = do.call(rbind, text_split[lengths(text_split) == 4])
      ts[[i]] = text_table
      }
    }

  text = do.call(rbind, texts)
  }
