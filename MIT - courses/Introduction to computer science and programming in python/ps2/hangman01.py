import random
import string
import os
import add_functions
import game_features

def Start_Game():
    game = game_features.GameState()
    '''
    Load the data from the txt file
    '''
    game.load_words()
    '''
    Choose a word in the self.list
    '''
    game.choose_word()
    '''
    Game start and menu
    '''
    game_features.game_init()
    input()
    game_features.game_menu()
    input()

    
    '''
    Start Actual gameplay 
    '''
    while game.n_guesses > 0:
        game.player_guess()
    
            

Start_Game()