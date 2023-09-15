import hangman
import random
import add_functions
import string
import os


class GameState():

    def __init__(self, path = 'ps2\words.txt'):
        self.secret_word = ''
        self.n_warnings = 3
        self.n_guesses = 6
        self.word_path = path
        self.wordlist = []
        self.my_word = []
        self.letters_guessed = []
        self.not_guessed = []
        self.hint_words = []
        self.letters_guessed = []
        self.game_win = bool
        
        
    def load_words(self):
        """
        Returns a list of valid words. Words are strings of lowercase letters.
        Depending on the size of the word list, this function may
        take a while to finish.
        """
        print("Loading word list from file...")
        # inFile: file
        inFile = open(self.word_path, 'r')
        # line: string
        line = inFile.readline()
        # wordlist: list of strings
        self.wordlist = line.split()
        print("  ", len(self.wordlist), "words loaded.")
        return self.wordlist
    
    def choose_word(self):
        """
        wordlist (list): list of words (strings)
        Returns a word from wordlist at random
        """
        self.secret_word = random.choice(self.wordlist)
        return self.secret_word


    def is_word_guessed(self):
        '''
        secret_word: string, the word the user is guessing; assumes all letters are
          lowercase
        letters_guessed: list (of letters), which letters have been guessed so far;
          assumes that all letters are lowercase
        returns: boolean, True if all the letters of secret_word are in letters_guessed;
          False otherwise
        '''
        correct = 0

        for letter in self.secret_word:
          if letter in self.letters_guessed:
            correct += 1
    
        return correct == len(self.secret_word)
    
    def get_guessed_word(self):
        '''
        secret_word: string, the word the user is guessing
        letters_guessed: list (of letters), which letters have been guessed so far
        returns: string, comprised of letters, underscores (_), and spaces that represents
          which letters in secret_word have been guessed so far.
        '''
        split_secret = list(self.secret_word)
        fill_word = [' _ ']*len(self.secret_word)

        for letter in split_secret:
          if letter in self.letters_guessed:
            ind_letters = add_functions.indexes(split_secret, letter)

            for element in ind_letters:
              fill_word[element] = letter

        self.my_word = ''.join(fill_word)
        return 
    
    def get_available_letters(self):
        '''
        letters_guessed: list (of letters), which letters have been guessed so far
        returns: string (of letters), comprised of letters that represents which letters have not
          yet been guessed.
        '''
        not_guessed = []
        for letter in string.ascii_lowercase:
          if letter not in self.letters_guessed:
            not_guessed += letter
            self.not_guessed = not_guessed
            
        return self.not_guessed
    
    def show_possible_matches(self):
        '''
        my_word: string with _ characters, current guess of secret word
        returns: nothing, but should print out every word in wordlist that matches my_word
                 Keep in mind that in hangman when a letter is guessed, all the positions
                 at which that letter occurs in the secret word are revealed.
                 Therefore, the hidden letter(_ ) cannot be one of the letters in the word
                 that has already been revealed.
        '''
      
        for word in self.wordlist:
          if add_functions.match_with_gaps(self.my_word, word, self.secret_word):
            self.hint_words.append(word)
        
    def match_with_gaps(self, other_word):
        '''
        my_word: string with _ characters, current guess of secret word
        other_word: string, regular English word
        returns: boolean, True if all the actual letters of my_word match the 
            corresponding letters of other_word, or the letter is the special symbol
            _ , and my_word and other_word are of the same length;
            False otherwise: 
        '''
        split_my = list(self.my_word)
        split_other = list(other_word)

        if len(split_my) == len(split_other):
          match_index = 0

          for element in split_my:
            '''
            Take the indexe of the caracteres, and compare them with the letters 
            of other word
            '''
            match_index = add_functions.indexes(split_my, element)
            for index in match_index:
              if split_my[index] == split_other[index]:
                return True

        else:
            return False


    def player_guess(self):
        if self.n_guesses == 6:
          print("I'm thinking in a word with", len(self.secret_word), "letters. \n")
        print("You have", self.n_guesses, "guesses remaining \n")
        print("Avaliable letters:", self.get_available_letters())
        letter = input("Guess a letter:" )
        
        print('------------------------------')
        
        if letter in (string.ascii_lowercase + string.ascii_uppercase) and len(letter) == 1 and letter not in self.letters_guessed:
            self.letters_guessed.append(str.lower(letter))

            if self.is_word_guessed() == True:
              for letter in self.secret_word:
                if self.secret_word.count(letter) == 1:
                  unique_letters += 1
              total_score = self.n_guesses*unique_letters     
              print("Congretulations, you find the word! :) \n"
                  + "The Secret word was ", self.secret_word,
                  "\n Your total score is:", total_score)
              self.game_win = True
            
            if letter in self.secret_word:
                self.get_guessed_word()
                print("Good guess:", self.my_word)
            
            else:
                self.get_guessed_word()
                print("Oops! That latter is not in my word:", self.my_word)
                if letter in ['a', 'e', 'i', 'o', 'u']:
                    self.n_guesses -= 2
                else:
                    self.n_guesses -= 1

        else:
          if letter not in ['*', "reset", "help", "quit"] or letter in self.letters_guessed:
            self.n_warnings -= 1
            print("Ooops this isn't a valid letter, you have", self.n_warnings, "warnings left")

        if letter is '*':
             self.show_possible_matches()
             print("The hints are:", self.hint_words)
             input()
            
        if letter is 'reset':
            reset_game()
        
        if letter is 'help':
            game_helper()
            input()
        
        if letter is 'quit':
            print("quero sairrrrr")
            
        if self.n_warnings == 0:
            self.n_guesses -= 1
            self.n_warnings = 3

        if self.game_win == False:
          print("--------------------------------- \n" + "Sorry, you lose!  \n" + " Your secret word was ", self.secret_word)
                  
        return 
      
def game_menu():
  os.system('cls')
  print("Quick tutorial:")
  print("- Digit * to get a hint \n"
  + "- Digit 'quit' to quit the game \n"
  + "- Digit 'help' to see the commands again \n"
  + "- Digit 'reset' to restart the game \n \n"
  + "- Press enter to start the game!"+
  "------------------------------ \n")
      
def game_helper():
  os.system('cls')
  print("- Digit * to get a hint \n"
  + "- Digit 'quit' to quit the game \n"
  + "- Digit 'reset' to restart the game \n \n"
  + "- Press enter to continue the game! \n"+
  "------------------------------ \n")
  input()

def reset_game():
  os.system('cls')
  print("You have reseted the game \n"
  + "Press enter to start again")
        
        
    
def game_init():
  os.system('cls')
  print("Welcome to the game Hangman! \n" 
  + "------------------------------ \n"
  + "Your objetive is to correct answer the word that I'm thinking! \n \n" 
  + "Please enjoy :v --> By Caio Wingeter \n"
  + "Press enter to continue")

