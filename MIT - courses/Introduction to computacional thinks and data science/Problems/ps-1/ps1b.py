###########################
# 6.0002 Problem Set 1b: Space Change
# Name:
# Collaborators:
# Time:
# Author: charz, cdenise

#================================
# Part B: Golden Eggs
#================================

# Problem 1
def dp_make_weight(egg_weights : tuple, target_weight, memo = {}):
    """
    Find number of eggs to bring back, using the smallest number of eggs. Assumes there is
    an infinite supply of eggs of each weight, and there is always a egg of value 1.
    
    Parameters:
    egg_weights - tuple of integers, available egg weights sorted from smallest to largest value (1 = d1 < d2 < ... < dk)
    target_weight - int, amount of weight we want to find eggs to fit
    memo - dictionary, OPTIONAL parameter for memoization (you may not need to use this parameter depending on your implementation)
    
    Returns: int, smallest number of eggs needed to make target weight
    """
    
    if memo == {}:
        memo['number_eggs'] = []
        memo['weight_acumulated'] = 0
        memo['temp_target'] = 0
        
        memo['index'] = len(egg_weights) - 1
        current_weight = egg_weights[memo['index']]
        n_eggs = target_weight//current_weight
        memo['number_eggs'].append(n_eggs)
        memo['weight_acumulated'] = current_weight*n_eggs
        memo['temp_target'] = target_weight - current_weight*n_eggs

    
    else:
        memo['index'] -= 1
        current_weight = egg_weights[memo['index']]
        #print(memo['temp_target'] , current_weight)
        n_eggs = memo['temp_target']//current_weight

        memo['weight_acumulated'] += current_weight*n_eggs
        memo['number_eggs'].append(n_eggs)
        memo['temp_target'] -= current_weight*n_eggs
    
    
    while memo['index'] != 0:
        dp_make_weight(egg_weights, target_weight, memo)
        
        
    return sum(memo['number_eggs'])
    
    

# EXAMPLE TESTING CODE, feel free to add more if you'd like
if __name__ == '__main__':
    egg_weights = (1, 5, 10, 25)
    n = 99
    print("Egg weights = (1, 5, 10, 25)")
    print("n = 99")
    print("Expected ouput: 9 (3 * 25 + 2 * 10 + 4 * 1 = 99)")
    print("Actual output:", dp_make_weight(egg_weights, n))
    print()
    