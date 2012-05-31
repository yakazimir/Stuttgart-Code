import re 
from sets import Set

file = open('sentencesSports.txt') 
sentences = [i.split() for i in open('sentencesSports.txt')] 
contexts = Set(); objects = {}

#X = Set()
to = 0
for l in sentences:

    print ("START",' '.join(l),"END")

    for i in range(len(l)): 
        
        ### FOR PRODUCING 'SYNTACTIC CONCEPTS' 
        ### IN SENSE OF CLARK
        

        m = l[i]
        if l[:i] == [] : le = "END"
        else: le = ' '.join(l[:i]) 

        if l[i+1:] == [] : r = "START" 
        else: r = ' '.join(l[i+1:])

        #print le,; print m,; print r

        print (le,m,r)


        ###FOR PRODUCING WORD DISTRIBUTIONS 

        # ##getting full word length 
        # if i-1 == -1: le = "\\b" ; m = l[i]
        # else: le = l[i-1]; m = l[i]

        # try: r = l[i+1] 
        # except IndexError: r = "\\e"  

        # c = (le,r); #print c,; print m
        
        # f = re.sub(', ',',',str(c))

        # if m in objects.keys(): 
        #     objects[m].add(f) 
        # else: objects[m] = Set([f])


        # #contexts.append(str(c))
        # contexts.add(f)


####PRINTING LIST FOR FC PROGRAM
print "######FUTBOL CONTEXT FILE###"
print "####ATTRIBUTES"
print "attributes:",
for item in contexts: print re.sub(', ',',',item),
print
print "################"
print "objects:", 
for item in objects.keys(): print re.sub(', ',',',item), 
print 
print "################"
print "################"

for (i,j) in objects.items() : 
    print i+"--"+" ".join(j)


######################
###FOR TESTING ACCURACY
######################
#print contexts
#print objects
# for (key,value) in objects.items(): 
#     for item in value: 
#         if not item in contexts: 
#             print item
#print len(contexts)
#print p
