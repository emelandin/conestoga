# Check odd or even number

num = int(input("Enter a number: "))

check_num = num%2
if check_num == 0:
    print(num, "it is an even number.")
elif check_num == 1:
    print(num, "it is an odd number.")
else:
    print("Invalid input")