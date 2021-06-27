
print("This program converts your amount into coins")
amount = int(input("Enter the amount: "))

coins_list = [1,5,10,25]
coins_list.sort(reverse = True)

if coins_list[-1] != 1:
    coins_list.append(1)

def make_change(coins_list, amount):
    change = []
    
    for coin in coins_list:
        
        while amount:
            if coin <= amount:
                change.append(coin)
                amount -= coin
            else:
                break
    return change


print("Your amount",amount," is equal",make_change(coins_list, amount), "in coins")