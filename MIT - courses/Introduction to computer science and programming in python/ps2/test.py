total_cost = 1000000
annual_salary = 120000
#portion_saved = 0.1
months = 36
raising_tax = 0.07
r = 0.04
#current_savings = 0

#for i in range(1, 5):
#        current_savings += annual_salary*portion_saved
#        current_savings = current_savings*(1 + 0.04/12)
#        months += 1
#        print(current_savings)



        
portion_saved_min = 0
portion_saved_max = 1
current_savings = 0
current_saved_min = 0
current_saved_max = months*annual_salary/12*(1 + raising_tax)**months  

while current_savings < total_cost*0.25*0.97 or current_savings > total_cost*0.25*1.03:
    current_savings = 0
    portion_saved = (portion_saved_min + portion_saved_max)/2

    for i in range(months):
        if i%6 == 0:
            current_savings += annual_salary/12*portion_saved*(1 + raising_tax)
            current_savings *= (1 + r/12)
        else:
            current_savings += annual_salary/12*portion_saved
            current_savings *= (1 + r/12)
                
    if current_saved_min < current_savings < total_cost*0.25:
            portion_saved_min = portion_saved
                    
    elif current_saved_max > current_savings > total_cost*0.25:
            portion_saved_max = portion_saved         
               
a = ['a', 'b', 'c', 'd', 'e', 'f']
b = 'olhameu'
for element in b:
    if element in a:
        print(True)
        

a = ['t', 'e', 's', 't', 'e', 'f']
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

is_word_guessed('teste', a)

a = ['a']
print(a*6)


class rabbit():
  tag = 1
  def __init__(self, age):
    self.age = age
    self.tag += 1
    
  def get_age(self):
    return self.age
  
  def __str__(self) -> str:
      return "I'm a rabbit (" + str(self.tag) + ") with "+ str(self.age) + " years"
    
r1 = rabbit(20)
r2 = rabbit(20)
print(r2)


def indexes(iterable, obj):
    result = []
    for index, elem in enumerate(iterable):
        if elem == obj:
            result.append(index)
    return result
            
secret_word = 'ababa'

split_secret = list(secret_word)
print(split_secret)
fill_word = ['_ ']*len(split_secret)

letters_guessed = ['a', 'c', 'd']
ind = indexes(split_secret, 'a')

fill_word.insert(ind, 'a') 
print(fill_word)


def indexes(iterable, obj):
    result = []
    for index, elem in enumerate(iterable):
        if elem == obj:
            result.append(index)
    return result

for letter in split_secret:
  if letter in letters_guessed:
    ind_letters = indexes(split_secret, letter)
    
    for element in ind_letters:
      fill_word[element] = letter
          
print(fill_word)
#print(fill_word)

534%10


