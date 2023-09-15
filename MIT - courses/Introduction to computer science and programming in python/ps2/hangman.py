import random
import string
import os
import add_functions
import game_features

WORDLIST_FILENAME = "ps2\words.txt"



def load_words():
    """
    Returns a list of valid words. Words are strings of lowercase letters.
    
    Depending on the size of the word list, this function may
    take a while to finish.
    """
    print("Loading word list from file...")
    # inFile: file
    inFile = open(WORDLIST_FILENAME, 'r')
    # line: string
    line = inFile.readline()
    # wordlist: list of strings
    wordlist = line.split()
    print("  ", len(wordlist), "words loaded.")
    return wordlist



def choose_word(wordlist):
    """
    wordlist (list): list of words (strings)
    
    Returns a word from wordlist at random
    """
    return random.choice(wordlist)

# end of helper code

# -----------------------------------

# Load the list of words into the variable wordlist
# so that it can be accessed from anywhere in the program
wordlist = load_words()


def is_word_guessed(secret_word, letters_guessed):
    '''
    secret_word: string, the word the user is guessing; assumes all letters are
      lowercase
    letters_guessed: list (of letters), which letters have been guessed so far;
      assumes that all letters are lowercase
    returns: boolean, True if all the letters of secret_word are in letters_guessed;
      False otherwise
    '''
    correct = 0
    
    for letter in secret_word:
      if letter in letters_guessed:
        correct += 1
    
    return correct == len(secret_word)



def get_guessed_word(secret_word : str, letters_guessed : list):
    '''
    secret_word: string, the word the user is guessing
    letters_guessed: list (of letters), which letters have been guessed so far
    returns: string, comprised of letters, underscores (_), and spaces that represents
      which letters in secret_word have been guessed so far.
    '''
    split_secret = list(secret_word)
    fill_word = ['_ ']*len(secret_word)
    
    for letter in split_secret:
      if letter in letters_guessed:
        ind_letters = indexes(split_secret, letter)
    
        for element in ind_letters:
          fill_word[element] = letter
    
    answer = ''.join(fill_word)
    return answer


def get_available_letters(letters_guessed : list):
    '''
    letters_guessed: list (of letters), which letters have been guessed so far
    returns: string (of letters), comprised of letters that represents which letters have not
      yet been guessed.
    '''
    not_guessed = ''
    for letter in string.ascii_lowercase:
      if letter not in letters_guessed:
        not_guessed += letter

    
    return not_guessed



def hangman(secret_word):
    '''
    secret_word: string, the secret word to guess.
    
    Starts up an interactive game of Hangman.
    
    * At the start of the game, let the user know how many 
      letters the secret_word contains and how many guesses s/he starts with.
      
    * The user should start with 6 guesses

    * Before each round, you should display to the user how many guesses
      s/he has left and the letters that the user has not yet guessed.
    
    * Ask the user to supply one guess per round. Remember to make
      sure that the user puts in a letter!
    
    * The user should receive feedback immediately after each guess 
      about whether their guess appears in the computer's word.

    * After each guess, you should display to the user the 
      partially guessed word so far.
    
    Follows the other limitations detailed in the problem write-up.
    '''
    
    n_guesses = 6
    n_warnings = 3
    guesses = []
    unique_letters = 0
    game_win = False
    
    print("Welcome to the game Hangman!" 
          + "\n ------------------------------"
          + "\n I'm thinking in a word with", len(secret_word), "letters")
    
    
    while n_guesses > 0:
    
      avaliable_guesses = get_available_letters(guesses)
  
      print("\n You have", n_guesses, "guesses remaining \n")
      print("Avaliable letters:", avaliable_guesses)
      
      letter = input("Guess a letter:" )
      
      print('------------------------------')
      
      if letter in (string.ascii_lowercase + string.ascii_uppercase) and len(letter) == 1 and letter not in guesses:
        guesses.append(str.lower(letter))
        
        if is_word_guessed(secret_word, guesses) == True:
          for letter in secret_word:
            if secret_word.count(letter) == 1:
              unique_letters += 1
          total_score = n_guesses*unique_letters     
          print("Congretulations, you find the word! :) \n"
              + "The Secret word was ", secret_word,
              "\n Your total score is:", total_score)
          game_win = True
          break
        
        if letter in secret_word:
          print("Good guess:", get_guessed_word(secret_word, guesses))
        
        else:
          print("Oops! That latter is not in my word:", get_guessed_word(secret_word, guesses))
          if letter in ['a', 'e', 'i', 'o', 'u']:
            n_guesses -= 2
          else:
            n_guesses -= 1
        
      else:
        n_warnings -= 1
        print("Ooops this isn't a valid letter, you have", n_warnings, "warnings left")
        
      if n_warnings == 0:
        n_guesses -= 1
        n_warnings = 3
    
    if game_win == False:
      print("--------------------------------- \n" + "Sorry, you lose!  \n" + " Your secret word was ", secret_word)
    
    print("\n Wanna play again?")
    
    restart = []
    while (len(restart) == 0):
      restart = input()
      if (restart == "yes"):
        secret_word = choose_word(wordlist)
        hangman(secret_word)
      
      elif (restart == "no"):
        restart = []
        break
    
      else:
        print("Please answer with 'yes' or 'no' :D")
        restart = []


# -----------------------------------

def indexes(iterable, obj):
    result = []
    for index, elem in enumerate(iterable):
        if elem == obj:
            result.append(index)
    return result

def match_with_gaps(my_word, other_word):
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
    
    if len(split_my) == len(split_other):
      match_index = 0
    
      for element in split_my:
        '''
        Take the indexe of the caracteres, and compare them with the letters 
        of other word
        '''
        match_index = indexes(split_my, element)
        for index in match_index:
          if split_my[index] == split_other[index]:
            return True
    
    else:
      return False
      


def show_possible_matches(my_word):
    '''
    my_word: string with _ characters, current guess of secret word
    returns: nothing, but should print out every word in wordlist that matches my_word
             Keep in mind that in hangman when a letter is guessed, all the positions
             at which that letter occurs in the secret word are revealed.
             Therefore, the hidden letter(_ ) cannot be one of the letters in the word
             that has already been revealed.

    '''
    hint_words = []
    inFile = open("ps2\words.txt", 'r')
    # line: string
    line = inFile.readline()
    # wordlist: list of strings
    wordlist = line.split()
    
    for word in wordlist:
      if match_with_gaps(word, my_word) == True:
        hint_words.append(word)
    
    return hint_words


def hangman_with_hints(secret_word):
    '''
    secret_word: string, the secret word to guess.
    
    Starts up an interactive game of Hangman.
    
    * At the start of the game, let the user know how many 
      letters the secret_word contains and how many guesses s/he starts with.
      
    * The user should start with 6 guesses
    
    * Before each round, you should display to the user how many guesses
      s/he has left and the letters that the user has not yet guessed.
    
    * Ask the user to supply one guess per round. Make sure to check that the user guesses a letter
      
    * The user should receive feedback immediately after each guess 
      about whether their guess appears in the computer's word.

    * After each guess, you should display to the user the 
      partially guessed word so far.
      
    * If the guess is the symbol *, print out all words in wordlist that
      matches the current guessed word. 
    
    Follows the other limitations detailed in the problem write-up.
    '''
    
    n_guesses = 50
    n_warnings = 3
    guesses = []
    unique_letters = 0
    
    game_state = ''
    game_life = ''
    
    '''
    game options: 
    - menu: ok
    - help command:  
    - hint command: 
    - reset command: 

    '''
    game_win = False
    
    
    print("Welcome to the game Hangman!" + "\n Press enter to instructions!")
    input()
    
    print("- Digit * to get a hint \n"
          + "- Digit 'quit' to quit the game \n"
          + "- Digit 'help' to see the commands again \n"
          + "- Digit 'reset' to restart the game \n \n"
          + "- Press enter to start the game!")
    input()
    
    game_state = 'guessing'
    game_life = 'running'
    
    os.system('cls')
    
    while game_life == 'running':
      
      while game_state == 'guessing':
    
        avaliable_guesses = get_available_letters(guesses)
  
        print("\n You have", n_guesses, "guesses remaining \n")
        print("Avaliable letters:", avaliable_guesses)

        letter = input("Guess a letter:" )
        
        print('------------------------------')

        if letter in (string.ascii_lowercase + string.ascii_uppercase) and len(letter) == 1 and letter not in guesses:
          guesses.append(str.lower(letter))

          if is_word_guessed(secret_word, guesses) == True:
            for letter in secret_word:
              if secret_word.count(letter) == 1:
                unique_letters += 1
            total_score = n_guesses*unique_letters     
            print("Congretulations, you find the word! :) \n"
                + "The Secret word was ", secret_word,
                "\n Your total score is:", total_score)
            game_win = True
            break
          
          if letter in secret_word:
            print("Good guess:", get_guessed_word(secret_word, guesses))

          else:
            print("Oops! That latter is not in my word:", get_guessed_word(secret_word, guesses))
            if letter in ['a', 'e', 'i', 'o', 'u']:
              n_guesses -= 2
            else:
              n_guesses -= 1
              
        elif letter is '*':
          game_state = 'help'
          
          print("- Digit * to get a hint \n"
          + "- Digit 'quit' to quit the game \n"
          + "- Digit 'reset' to restart the game \n \n"
          + "- Press enter to continue the game!")
          

        else:
          n_warnings -= 1
          print("Ooops this isn't a valid letter, you have", n_warnings, "warnings left")

        if n_warnings == 0:
          n_guesses -= 1
          n_warnings = 3

      if game_win == False:
        print("--------------------------------- \n" + "Sorry, you lose!  \n" + " Your secret word was ", secret_word)

      print("\n Wanna play again?")


# When you've completed your hangman_with_hint function, comment the two similar
# lines above that were used to run the hangman function, and then uncomment
# these two lines and run this file to test!
# Hint: You might want to pick your own secret_word while you're testing.


if __name__ == "__main__":
    # pass

    # To test part 2, comment out the pass line above and
    # uncomment the following two lines.
    
    secret_word = choose_word(wordlist)
    hangman(secret_word)

###############
    
    # To test part 3 re-comment out the above lines and 
    # uncomment the following two lines. 
    
    #secret_word = choose_word(wordlist)
    #hangman_with_hints(secret_word)
