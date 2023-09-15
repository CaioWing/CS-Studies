
class Test1():

    def __init__(self, annual_salary, portion_saved, total_cost, current_savings = 0):
        self.r = 0.04
        self.annual_salary = annual_salary
        self.portion_saved = portion_saved
        self.total_cost = total_cost
        self.current_savings = current_savings

    def savings(self):
        months = 0
        while current_savings < self.total_cost*0.25:
            current_savings += self.annual_salary/12*self.portion_saved
            current_savings *= (1 + self.r/12)
            months += 1
            
        self.current_savings = current_savings
        return months
    
    def savings_with_rasing(self, raising_tax, current_savings = 0):
        months = 0
        while current_savings < self.total_cost*0.25:
            if months%6 == 0:
                current_savings += self.annual_salary/12*self.portion_saved*(1 + raising_tax)
                current_savings *= (1 + self.r/12)
                months += 1
            else:
                current_savings += self.annual_salary/12*self.portion_saved
                current_savings *= (1 + self.r/12)
                months += 1
        
        self.current_savings = current_savings
        return months
    
    def best_saving_rate(self, raising_tax, months, current_savings = 0):
        
        portion_saved_min = 0
        portion_saved_max = 1

        current_saved_min = 0
        current_saved_max = months*self.annual_salary/12*(1 + raising_tax)**months  

        while current_savings < self.total_cost*0.25*0.99 or current_savings > self.total_cost*0.25*1.01:
            current_savings = 0
            portion_saved = (portion_saved_min + portion_saved_max)/2

            for i in range(months):
                if i%6 == 0:
                    current_savings += self.annual_salary/12*portion_saved*(1 + raising_tax)
                    current_savings *= (1 + self.r/12)
                else:
                    current_savings += self.annual_salary/12*portion_saved
                    current_savings *= (1 + self.r/12)
                
            if current_saved_min < current_savings < self.total_cost*0.25:
                    portion_saved_min = portion_saved
                    
            elif current_saved_max > current_savings > self.total_cost*0.25:
                    portion_saved_max = portion_saved
                
            print(portion_saved)
                
        return portion_saved

#annual_salary = input("What's your annual salary:")
#portion_saved = input("What's the portion of your salary to save:")
#total_cost = input("What's the total cost of your dream home:")

#months = savings(float(annual_salary), float(portion_saved), float(total_cost))

#print("Will in total take about ", months, " months to save the total money.")

total_cost = 1000000
annual_salary = 150000
portion_saved = 0.1

test = Test1(annual_salary, portion_saved, total_cost)
test.best_saving_rate(0.07, 36)
    