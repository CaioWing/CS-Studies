###########################
# 6.0002 Problem Set 1a: Space Cows 
# Name:
# Collaborators:
# Time:

from ps1_partition import get_partitions
import time

#================================
# Part A: Transporting Space Cows
#================================

# Problem 1
def load_cows(filename):
    """
    Read the contents of the given file.  Assumes the file contents contain
    data in the form of comma-separated cow name, weight pairs, and return a
    dictionary containing cow names as keys and corresponding weights as values.

    Parameters:
    filename - the name of the data file as a string

    Returns:
    a dictionary of cow name (string), weight (int) pairs
    """
    dataList = {}
    with open(filename, 'r') as file:

        for line in file:
            dataList[line.split(',')[0]] = line.split(',')[1]
            #print('Key:', line.split(',')[0], 'Weight:', line.split(',')[1])
    return dataList


# Problem 2
def greedy_cow_transport(cows,limit=10):
    """
    Uses a greedy heuristic to determine an allocation of cows that attempts to
    minimize the number of spaceship trips needed to transport all the cows. The
    returned allocation of cows may or may not be optimal.
    The greedy heuristic should follow the following method:

    1. As long as the current trip can fit another cow, add the largest cow that will fit
        to the trip
    2. Once the trip is full, begin a new trip to transport the remaining cows

    Does not mutate the given dictionary of cows.

    Parameters:
    cows - a dictionary of name (string), weight (int) pairs
    limit - weight limit of the spaceship (an int)
    
    Returns:
    A list of lists, with each inner list containing the names of cows
    transported on a particular trip and the overall list containing all the
    trips
    """
    particupar_trip = []
    total_trips = []
    values = []
    names = []
    t_weights = 0
    
    for name in cows:
        values.append(int(cows[name]))
        names.append(name)
    
    while values != [0]*len(values):
        greater = max(values)
        ind = values.index(greater)
        t_weights += greater
        
        if t_weights < limit:
            particupar_trip.append(names[ind])
            values[ind] = 0
        
        elif t_weights > limit and len(particupar_trip) == 1:
            total_trips.append(names[ind])
            particupar_trip = []
            t_weights = 0
            
        else:
            total_trips.append(particupar_trip)
            values[ind] = greater
            particupar_trip = []
            t_weights = 0
        
    if particupar_trip != []:
        total_trips.append(particupar_trip)

    return total_trips

# Problem 3
def brute_force_cow_transport(cows : dict, limit=10):
    """
    Finds the allocation of cows that minimizes the number of spaceship trips
    via brute force.  The brute force algorithm should follow the following method:

    1. Enumerate all possible ways that the cows can be divided into separate trips 
        Use the given get_partitions function in ps1_partition.py to help you!
    2. Select the allocation that minimizes the number of trips without making any trip
        that does not obey the weight limitation
            
    Does not mutate the given dictionary of cows.

    Parameters:
    cows - a dictionary of name (string), weight (int) pairs
    limit - weight limit of the spaceship (an int)
    
    Returns:
    A list of lists, with each inner list containing the names of cows
    transported on a particular trip and the overall list containing all the
    trips
    """
    list_cows = []
    valid_choises = []
    
    
    for partition in get_partitions(cows.keys()):
        list_cows.append(partition)
    
    for travel in range(len(list_cows)):
        control = 0
        partial_choises = []
        n_groups = len(list_cows[travel])
        
        for number in range(n_groups):
            weight_value = 0
            
            for cow in list_cows[travel][number]:
                weight_value += int(cows[cow])

            if weight_value < limit:
                control += 1
                
            if control == len(list_cows[travel]):
                partial_choises.append(list_cows[travel])
        
        if partial_choises != []:
            valid_choises.append(partial_choises)
        
            
    return valid_choises


# Problem 4
def compare_cow_transport_algorithms():
    """
    Using the data from ps1_cow_data.txt and the specified weight limit, run your
    greedy_cow_transport and brute_force_cow_transport functions here. Use the
    default weight limits of 10 for both greedy_cow_transport and
    brute_force_cow_transport.
    
    Print out the number of trips returned by each method, and how long each
    method takes to run in seconds.

    Returns:
    Does not return anything.
    """
    cow_dict = load_cows('Introduction to computacional thinks and data science\Problems\ps-1\ps1_cow_data.txt')
    start = time.time()
    greedy_cow_transport(cow_dict,limit=10)
    end = time.time()
    print('Greedy algoritm take: ', start - end, 'seconds')
    print('The trip was:', greedy_cow_transport(cow_dict,limit=10))
    
    start = time.time()
    brute_force_cow_transport(cow_dict,limit=10)
    end = time.time()
    print('Brute algoritm take: ', end - start, 'seconds')

# cow_dict = load_cows('Introduction to computacional thinks and data science\Problems\ps-1\ps1_cow_data.txt')
# print(greedy_cow_transport(cow_dict,limit=10))
# print(len(brute_force_cow_transport(cow_dict,limit=10)))

# list_cows = []
# for partition in get_partitions(cow_dict.keys()):
#     list_cows.append(partition)
    
# print(list_cows[5])
# print(list_cows[5][0])
# for element in list_cows[5][0]:
#     print(element)

#print(cow_dict[list_cows[5][0][0]])

compare_cow_transport_algorithms()