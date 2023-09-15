import string

def indexes(iterable, obj):
    result = []
    for index, elem in enumerate(iterable):
        if elem == obj:
            result.append(index)
    return result
  

def match_with_gaps(my_word, other_word, secret_word):
    '''
    my_word: string with _ characters, current guess of secret word
    other_word: string, regular English word
    returns: boolean, True if all the actual letters of my_word match the 
        corresponding letters of other_word, or the letter is the special symbol
        _ , and my_word and other_word are of the same length;
        False otherwise: 
    '''
    split_my = list(my_word)
    split_other = list(other_word)
    match = bool
    
    if len(secret_word) == len(split_other):
      my_index = []
      other_index = []
    
      for element in split_my:
        '''
        Take the indexe of the caracteres, and compare them with the letters 
        of other word
        '''
        if element in (string.ascii_lowercase + string.ascii_uppercase):
          my_index = indexes(split_my, element)
          other_index = indexes(split_other, element)
          if my_index == other_index:
            match = True
          else:
            match = False
      
      return match
    
    else:
      match = False
      return match
  