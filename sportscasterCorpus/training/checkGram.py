import re 
left = re.compile('(.+) \-\-\> (.+)\s*')

grammar = open('grammarWProbs.txt') 

i = 0 
currentR = "" 
prob = 0 
sum = 0


 
for line in grammar:
    leftRule = re.search(left,line)
    if leftRule.groups()[0] != currentR: 
        if sum < .99: 
            print line
        elif sum > 1.00000000001: 
            print "!!!!!"
        sum = 0 
        sum += float((leftRule.groups()[1].split(" "))[-1])
        currentR = leftRule.groups()[0]
    else: 
        sum += float((leftRule.groups()[1].split(" "))[-1])
        print currentR

    i +=1 


print i


