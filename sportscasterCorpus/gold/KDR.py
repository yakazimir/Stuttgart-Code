import re 
nlTag = re.compile('<nl lang="en"> (.+) </nl>')
mrlTag = re.compile('<mrl lang="soccer"> (.+) </mrl>')
predicate = re.compile('([a-zA-Z]+) \(')

LLPair = [] 
posPairs = {} 


def lMap(item1, item2, dict): 
    if item2 in dict:
        dict[item2].append(
            item1)
    else:
        dict[item2] = [item1]

files = ['2001final-gold', '2002final-gold', 
         '2003final-gold', '2004final-gold']
predicates = {} 
#contexts = {}
contexts = []

# def addContext(context): 
#     global contexts 
#     if context in contexts: 
#         contexts[context] += 1 
#     else: 
#         contexts[context] = 1

#     return [i for i in contexts.keys()].index(context)
def addContext(context): 
    global contexts 
    if context in contexts: 
        pass  
    else: 
        contexts.append(context)

    return contexts.index(context)

words = {}
wordCount = {}
def contextCal(string):
    global words
    seq = string.split(' ')
    for i in range(len(seq)):
        #print seq[i].lower()
        if i == 0: 
            #print '\t'+'\\b'
            L = 'B'
        else: 
            #print '\t'+seq[i-1].lower()
            L = seq[i-1].lower()
        try: 
            #print '\t'+seq[i+1].lower()
            R = seq[i+1].lower()
        except: 
            R = 'E'
            #print '\t'+'\\b'        
        #print (L,R)
            
        value = addContext((L,R))
        #print (L,R)
        #print value
        wordCount[seq[i].lower()] = wordCount.setdefault(
            seq[i].lower(),0) + 1
        #words[seq[i].lower()] = words
        if seq[i].lower() in words.keys():
            #words[seq[i].lower()][value] += 1
            try: 
                words[seq[i].lower()][value] += 1
            except: 
                words[seq[i].lower()][value] = 1
        else:
            words[seq[i].lower()] = {value:1}

        L = ''
        R = ''
        i+=1
i = 0 
for entry in files: 
    file = open(entry) 
    for line in file:
        i += 1
        nl = re.search(nlTag, line)
        mrl = re.search(mrlTag, line)
        if nl:
            LLPair.append(
                nl.groups()[0])
        elif mrl: 
            LLPair.append(
                mrl.groups()[0])
            lMap(
                re.sub('[0-9]', "", LLPair[0]), 
                re.sub('[0-9]', "", LLPair[1]),
                posPairs
                )
            #conextCalc(LLPair[1])
            contextCal(re.sub('[0-9]|\.|\'''s', "",LLPair[0]))
            LLPair = [] 


# for item in posPairs: 
#    print item
#    for item2 in posPairs[item]: 
#        print '\t'+item2


#print len([i for i in contexts.keys()])
#print len(contexts)
for context in contexts.keys(): 
   print context

def viewDist(dict):
    #print title + ':'
    print sorted(dict.iteritems(), 
           key=lambda (k,v):(v,k), 
           reverse=True)

#print len(words.keys())

print words
#print contexts



print '\n\n'
viewDist(wordCount)
