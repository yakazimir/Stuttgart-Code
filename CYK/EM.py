import random
from random import choice 


coins = ['coin1', 'coin2'] 

outCome = ['H', 'T']
count = {'coin1':
             {'H':0, 'T':0}, 
         'coin2':
             {'H':0, 'T':0}}

i = 0 
while i < 5:
    choice = random.choice(coins) 
    j = 0
    while j < 10:
        side = random.choice(outCome) 
        count[choice][side] += 1
        j += 1
    i += 1

print count


for coin in coins: 
    heads = count[coin]['H']
    tails = count[coin]['T']
    try: 
        print 'MLH Heads: '+coin+' '+str(
            float(heads)/float(heads+tails))
    except: 
        print "MLH HEADS: "+coin+' 0'



