import random

class FairRoullete():
    def __init__(self):
        self.pockets = []
        for i in range(1, 37):
            self.pockets.append(i)
        self.ball = None
        self.pocketsOdds =  len(self.pockets) - 1
    
    def spin(self):
        self.ball = random.choice(self.pockets)
        
    def betPocket(self, pocket, amt):
        if str(pocket) == str(self.ball):
            return amt*self.pocketsOdds
        else: 
            return -amt
    
    def __str__(self) -> str:
        return 'Fair Roulette'
    
    
def playRoulette(game : FairRoullete, numSpins, pocket, bet, toPrint = True):
    totPocket = 0
    for i in range(numSpins):
        game.spin()
        totPocket += game.betPocket(pocket, bet)
        print(game.ball)
    if toPrint:
        print(numSpins, 'spins of ', game)
        print('Expected return betting', pocket, '=', \
            str(100*totPocket/numSpins) + '%\n')
        return (totPocket/numSpins)
    
game = FairRoullete()
playRoulette(game, 10, 5, 2)