import re 
from sets import Set

##FILES 
file1 = open("../original-MRLGrammar.txt")
trainingFiles = ['2001final-train', 
                 '2002final-train', 
                 '2003final-train', 
                 '2004final-train']
##REGEXs
listItems = ['\*n\:S -> \(\{ (\w+) \}\)', 
             '\*n:S -> \(\{ (\w+) \( \*n:(\w+) \) \}\)', 
             '\*n:S -> \(\{ (\w+) \( \*n:(\w+) \, \*n:(\w+) \) \}\)'] 
player = re.compile('\*n\:Player -> \(\{ (\w+[0-9]+) \}\)')  
playMode = re.compile('\*n\:Playmode -> \(\{ ((\w+\_?\w*)+) \}\)')
relation = re.compile("|".join(listItems))
exampleStart = re.compile('\<example id\=\"([0-9]+)\"')
NLtext = re.compile('<nl>\s(.+[^\??|\.?])\s<\/nl>') 
semID = re.compile('<semid>\s(.+)\s<\/semid>')
exampleEnd = re.compile('\<\/example\>') 
semIDVal = re.compile('<sem id=\"([0-9]+)\">\s(.+)\s<\/sem>')
semEnd = re.compile('<\/sems>')

##LISTS
players = [] 
playModes = [] 
sentenceNodes = {}  

##ORIGINAL GRAMMAR ATTRIBUTES
def ruleType(s):
    predicate = s[0]
    try: 
        arg1 = s[1]
        try : 
            arg2 = s[2]
            sentenceNodes[predicate]=(arg1,arg2)
        except:
            sentenceNodes[predicate]=(arg1) 
    except: 
        sentenceNodes[predicate] = 0 

for line in file1: 
    pL = re.search(player, line); pM = re.search(playMode,line) 
    relationR = re.search(relation,line)
    if pL: 
        players.append(pL.groups()[0])
    elif pM: 
        playModes.append(pM)
    elif relationR:
        items = [i for i in filter(
                (lambda x: x !=None),
                list(relationR.groups()))] 
        ruleType(items)
    
    file1.closed

#WORDS FROM TRAINING DATA 
    
trainingSentences = {} 
contexts = []
contextIndex = {}
words = {} 

def getContexts(senNumPair,vals): 
    for num,value in senNumPair.items(): 
        contextVal = Set([vals[path] for path in value[1]])
        if contextVal in contexts: 
           #print "c"+str(contexts.index(contextVal))+" "+ value[0].lower()
           contextIndex[contexts.index(contextVal)] =\
                contextIndex.setdefault(contexts.index(contextVal),0) + 1  
        else:
            contexts.append(contextVal)
            #print "c"+str(contexts.index(contextVal))+" "+ value[0].lower()
            contextIndex[contexts.index(contextVal)] =\
                contextIndex.setdefault(contexts.index(contextVal),0) + 1       

i = 1
for game in trainingFiles:
    content = open(game) 
    gameSemVals = {};senNum = {} 
    for line in content: 
        text = re.search(NLtext,line);semM = re.search(semID,line)
        semE = re.search(semEnd,line);semVal = re.search(semIDVal,line)
        if text:
            sentence = text.groups()[0] 
            for word in text.groups()[0].split(" "): 
                words[word] =\
                    words.setdefault(word,0) + 1 
                # words[word.lower()] =\
#                     words.setdefault(word.lower(),0) + 1 
        elif semM: 
            senNum[i] = [sentence,semM.groups()[0].split(" ")]
            i+=1
        elif semVal: 
            gameSemVals[semVal.groups()[0]] = semVal.groups()[1]
        elif semE: 
            getContexts(senNum,gameSemVals)
        else: 
            pass 
    content.closed


predicateContext = {}

#items = [i for i in filter(
#                (lambda x: x !=None),
#                list(relationR.groups()))] 


for item in contexts:
   for item2 in item:
       if not re.search('^\w+$', item2): 
           revised =  tuple([i for i in filter(
                       (lambda x: re.match(r'(\w+[0-9]*)',x)), item2.split(" "))])
           if revised in predicateContext: 
               predicateContext[revised].append(contexts.index(item)) 
           else: 
               predicateContext[revised] = [contexts.index(item)]
       else:
           if item2 in predicateContext: 
               predicateContext[item2].append(contexts.index(item))
           else: 
               predicateContext[item2] = [contexts.index(item)]

#Printing GRAMMAR

j = 1

print "Root -> S{null}"
for item in sentenceNodes: print "Root --> S{"+item+"}"; j+=1

##PRINTING PRODUCTION W\ CONTEXTS

for item in sentenceNodes: 
    try: 
        arity =  len(sentenceNodes[item])
        if arity > 2:
            if sentenceNodes[item] == "Player": 
                for player in players: 
                    tuple = (item, player) 
                    try: 
                        for num in predicateContext[tuple]: 
                            j += 2
                            print "S{"+item+"("+player+")} --> c"+str(num)+" Phrase{"+player+"} "+"Phrase{"+item+"}"
                            print "S{"+item+"("+player+")} --> c"+str(num)+" Phrase{"+item+"} "+"Phrase{"+player+"}"
                    except: 
                        pass 
            elif sentenceNodes[item] == "Playmode": 
                for play in playModes: 
                    tuple = ("playmode", play.groups()[0])
                    try: 
                        for num in predicateContext[tuple]: 
                            j += 2
                            print "S{"+item+"("+play.groups()[0]+\
                                ")} --> c"+str(num)+" Phrase{"+play.groups()[0]+"} "+"Phrase{"+item+"}"
                            print "S{"+item+"("+play.groups()[0]+\
                                ")} --> c"+str(num)+" Phrase{"+item+"} "+"Phrase{"+play.groups()[0]+"}"
                    except: 
                        pass  
            else: 
                print "error" 
        else:
            for player1 in players: 
                for player2 in players: 
                    tuple = (item,player1, player2) 
                    try: 
                        for num in predicateContext[tuple]: 
                            j += 6 
                            print "S{"+item+"("+player1+","+player2+")} --> c"+str(num)+\
                                " Phrase{"+item+"} "+"Phrase{"+player1+"} "+"Phrase{"+player2+"}"
                            print "S{"+item+"("+player1+","+player2+")} --> c"+str(num)+\
                                " Phrase{"+item+"} "+"Phrase{"+player2+"} "+"Phrase{"+player1+"}"
                            print "S{"+item+"("+player1+","+player2+")} --> c"+str(num)+\
                                " Phrase{"+player1+"} "+"Phrase{"+item+"} "+"Phrase{"+player2+"}"
                            print "S{"+item+"("+player1+","+player2+")} --> c"+str(num)+\
                                " Phrase{"+player2+"} "+"Phrase{"+item+"} "+"Phrase{"+player1+"}"
                            print "S{"+item+"("+player1+","+player2+")} --> c"+str(num)+\
                                " Phrase{"+player2+"} "+"Phrase{"+player1+"} "+"Phrase{"+item+"}"
                            print "S{"+item+"("+player1+","+player2+")} --> c"+str(num)+\
                                " Phrase{"+player1+"} "+"Phrase{"+player2+"} "+"Phrase{"+item+"}"                         
                    except: 
                        pass 
    except:
        for num in predicateContext[item]: 
            j += 1
            print "S{"+item+"} --> c"+str(num)+" Phrase{"+item+"}"



##NULL S: VERY IMPORTANT
h = 0 
while h < 954: 
    print "S{null} --> c"+str(h)+" Phrase{null}"
    h += 1
    j += 1

print "Phrase{null} --> Word{null}"; j += 1 
print "Phrase{null} --> Phrase{null} Word{null}"; j += 1 

for item in players:
    j += 3
    print "Phrase{"+item+"} --> "+"Word{"+item+"}"
    print "Phrase{"+item+"} --> PhX{"+item+"} "+"Word{"+item+"}"
    print "Phrase{"+item+"} --> PhX{"+item+"} "+"Word{none}"

for item in playModes: 
    print "Phrase{"+item.groups()[0]+"} --> "+"Word{"+item.groups()[0]+"}"
    print "Phrase{"+item.groups()[0]+"} --> PhX{"+item.groups()[0]+"} "+"Word{"+item.groups()[0]+"}"
    print "Phrase{"+item.groups()[0]+"} --> PhX{"+item.groups()[0]+"} "+"Word{null}"
    j+= 3

for item in players:
    j += 4
    print "PhX{"+item+"} --> "+"Word{"+item+"}"
    print "PhX{"+item+"} --> PhX{"+item+"} "+"Word{"+item+"}"
    print "PhX{"+item+"} --> PhX{"+item+"} "+"Word{null}"
    print "PhX{"+item+"} --> Word{null}"

for item in playModes: 
    print "PhX{"+item.groups()[0]+"} --> "+"Word{"+item.groups()[0]+"}"
    print "PhX{"+item.groups()[0]+"} --> Word{null}"
    print "PhX{"+item.groups()[0]+"} --> PhX{"+item.groups()[0]+"} "+"Word{"+item.groups()[0]+"}"
    print "PhX{"+item.groups()[0]+"} --> PhX{"+item.groups()[0]+"} "+"Word{null}"
    j+= 4

for item in players:
    j += 3
    print "Ph{"+item+"} --> PhX{"+item+"} "+"Word{"+item+"}"
    print "Ph{"+item+"} --> PhX{"+item+"} "+"Word{null}"
    print "Ph{"+item+"} --> "+"Word{null}"
    

for item in playModes: 
    print "Ph{"+item.groups()[0]+"} --> PhX{"+item.groups()[0]+"} "+"Word{"+item.groups()[0]+"}"
    print "Ph{"+item.groups()[0]+"} --> PhX{"+item.groups()[0]+"} "+"Word{null}"
    print "Ph{"+item.groups()[0]+"} --> "+"Word{null}"
    j+= 3


for word in words.keys(): 
    j += 1
    print "Word{null} --> "+word

for item in players: 
    for word in words.keys(): 
        j += 1
        print "Word{"+item+"} --> "+word

for item in playModes: 
    for word in words.keys(): 
        j+=1 
        print "Word{"+item.groups()[0]+"} --> "+word


for item in contexts: 
    for context in item:
        print "c"+str(contexts.index(item))+" --> "+re.sub(" ","",context)
        j += 1

for item in contexts: 
    print "c"+str(contexts.index(item))+" --> S{null}"
    j += 1


print j 




#for item in predicateContext: 
#    print item 
#    for num in predicateContext[item]:
#        print "\t"+str(num)       
#print predicateContext.keys()










#print "\t"
#print len(contexts)
#print len(contextIndex.keys())

###print contexts 
#for item in contexts: 
#    print item

#print len(contexts.keys())




        #contexts[tuple(contextVal)] =\
        #    contexts.setdefault(tuple(contextVal),0) + 1

#print str(num)+": ",
        #print "\t"+value[0]


        # for item in contexts: 
#             print "\t"+str(vals[path])
        # for path in value[1]: 
#             print "\t"+str(vals[path])



#print "\t"+sentence 
            #print "\t"+str(semM.groups()[0].split(" "))   
#print i 


#print semVal.groups()[0]
            #print semVal.groups()[1]

# print i 
# for word in words: 
#     print word

# print words
# print len(words.keys())
#print text.groups()[0].split(" ")
