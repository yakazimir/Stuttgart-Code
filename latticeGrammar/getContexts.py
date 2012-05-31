import re 
from sets import Set

file = open('sentences.txt') 
sentences = [i.split() for i in open('sentences.txt')] 
contexts = Set(); objects = {}

#X = Set()

for l in sentences:


    for i in range(len(l)): 
        
        ##getting full word length 


        if i-1 == -1: le = "\\b" ; m = l[i]
        else: le = l[i-1]; m = l[i]

        try: r = l[i+1] 
        except IndexError: r = "\\e"  

        c = (le,r); #print c,; print m
        
        f = re.sub(', ',',',str(c))

        if m in objects.keys(): 
            objects[m].add(f) 
        else: objects[m] = Set([f])


        #contexts.append(str(c))
        contexts.add(f)

#p = 0
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

#print contexts
#print objects

# for (key,value) in objects.items(): 
    
#     for item in value: 

#         if not item in contexts: 

#             print item



print len(contexts)
#print p
