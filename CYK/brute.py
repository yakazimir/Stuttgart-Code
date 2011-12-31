import itertools 
from itertools import combinations
from sets import Set

flips = { 
    1:['H','T','T','T','H','H','T','H','T','H'], 
    2:['H','H','H','H','T','H','H','H','H','H'], 
    3:['H','T','H','H','H','H','H','T','H','H'], 
    4:['H','T','H','T','T','T','H','H','T','T'], 
    5:['T','H','H','H','T','H','H','H','T','H'],
    }


count = {
    'A':{'H':0,'T':0}, 
    'B':{'H':0,'T':0}}


target = 5
values = ['A', 'B'] 
initialProb = {'A':0.60, 'B':0.50}
limit = len(flips.keys())
combCount = 0

def MLH(count): 
    global values 
    for item in values: 
        heads = count[item]['H'] 
        tails = count[item]['T']
        print 'MLH Heads: '+\
            item+' '+str(
            float(heads)/float(heads+tails))


def reset(count): 
    count = {
        'A':{'H':0,'T':0}, 
        'B':{'H':0,'T':0}}
    return count

def sides(coin, list): 
    global count 
    for item in list: 
        count[coin][item] +=1
    return count


def compl(A, B): 
    global combCount
    global count
    i = 1 
    while i <= limit: 
        if i in A: 
            print "A: "+str(flips[i])
            sides('A',flips[i])
        else: 
            print "B: "+str(flips[i])
            sides('B',flips[i])
        i +=1
    print count
    MLH(count)
    print '\n\n'
    combCount += 1
    count = reset(count)
    


def comb(k,m): 
    runs = Set(flips.keys())
    combin = combinations(runs, k)
    for item in combin: 
        A = Set(item)
        B = runs - A
        compl(A,B)


def main(): 
    k = 1
    while k <= target: 
        m = 1
        while m <= target: 
            sum = k+m 
            if sum == target:
                comb(k,m)
            else: 
                pass 
            m+=1
        k+=1

main()
print combCount

#print flips
    #print str(k)+\
    #    ' '+str(m)


#val1 = initialProb[values[0]] 
#val2 = initialProb[values[1]]




# k = 1
# while k <= target: 
#     m = 1
#     while m <= target: 
#         sum = k+m 
#         if sum == target: 
#             val1 = initialProb[values[0]] 
#             val2 = initialProb[values[1]]
#             print (k*10) * val1
#             print (m*10) * val2
#         m += 1
#     k += 1
#print flips

# i = 1
# count = 0
# while i <= 10: 
#     j = 1 
#     while j <= 10: 
#         z = 1
#         while z <= 10:
#             sum = i+j+z
#             if sum == 18: 
#                 count +=1
#                 #print str(i)+\
#                 #    ' '+str(j)+' '+str(z)
#             else: 
#                 pass
#             z += 1 
#         j+=1 
#     i+=1 


#print count
