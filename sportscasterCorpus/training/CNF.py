import re 
from sets import Set
from itertools import permutations

##FILES ##TRAINING FILES ARE TRAINING DATA EXT.
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
    else: 
        pass 
    file1.closed

#WORDS FROM TRAINING DATA 
    
###################################
# CONTEXT AND WORDS FROM TRAINING #
###################################

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
                ##LOWER CASE OR UPPER 
#                 words[word] =\
#                     words.setdefault(word,0) + 1 
                words[word.lower()] =\
                    words.setdefault(word.lower(),0) + 1 
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


###RELATIONS WITH CONTEXTS
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

###################################
###################################
###################################


###PRINTING GRAMMAR 

##########################
# COMPILING S NODE LISTS #
##########################

startVar = "S{"
eb = "}"; sp = "("; ep =")"
contextR = ")} --> c" 
startS = "S --> Root "
j = 0

starts = []
startLeft = {} 

def withContext(loc,tuple, predicate): 
    global startLeft 
    if isinstance(tuple,str): 
        st = "S{"+tuple+"}"
    else: 
        try: 
            st = "S{"+tuple[0]+"("+tuple[1]+","+tuple[2]+")}"
        except: 
            st = "S{"+tuple[0]+"("+tuple[1]+")}"
    f = []
    for num in loc: 
        if len(tuple) > 4: 
            f.append((tuple,num))
        else:
            g = [] 
            for arr in permutations(list(tuple),len(tuple)): 
                g.append(arr) 
            f.append((g,num))
        
    startLeft[st] = f 

for item in sentenceNodes:
    if isinstance(sentenceNodes[item], tuple): 
        for player1 in players: 
            for player2 in players: 
                pair = (item, player1, player2) 
                if pair in predicateContext.keys():
                    predicate = item+sp+player1+","+player2+ep
                    starts.append(predicate)
                    withContext(predicateContext[pair], pair, predicate)
                else: 
                    pass  
    elif isinstance(sentenceNodes[item], str): 
        if sentenceNodes[item] == "Player": 
            for player in players: 
                pair = (item, player) 
                if pair in predicateContext.keys():
                    predicate = item+sp+player+ep
                    starts.append(predicate) 
                    withContext(predicateContext[pair],pair, predicate)
                else: 
                    pass 
        elif sentenceNodes[item] == "Playmode": 
            for play in playModes: 
                pair = ("playmode", play.groups()[0]) 
                if pair in predicateContext.keys(): 
                    predicate = item+sp+play.groups()[0]+ep
                    starts.append(predicate) 
                    withContext(predicateContext[pair], pair,predicate)
                else: 
                    pass 
        else: 
            print "error" 
    else: 
        starts.append(item)
        withContext(predicateContext[item],item,item) 

##########################
##########################
##########################

####################
# printing S NODES #
####################
j += len(starts)
startUniform = 1.0/(float(len(starts)+1.0))

for sVal in starts: 
     print startS+"S{"+sVal+"} "+str(startUniform)


print "S --> Root S{null} "+str(startUniform);  j+= 1
print "Root --> s: "+str(1.0); j+=1

for lS in startLeft: 
    if re.search('\{\w+\}',lS): 
        j += len(startLeft[lS])
        unif = 1.0/float(len(startLeft[lS]))
        for c in startLeft[lS]: 
            print lS+" --> c"+str(c[1])+" Phrase{"+c[0]+"} "+str(unif)
    else:  
        unif = 0 
        l = len(startLeft[lS])
        r =  len(startLeft[lS][0][0])
        unif = 1.0/float(float(l)*float(r))
        for right in startLeft[lS]:
            j += len(right[0])
            context = right[1]
            for rules in  right[0]: 
                try: 
                    print lS+" --> c"+str(context)+\
                        " Phrase{"+rules[0]+"} "+"Phrase{"+rules[1]+"} "+\
                        "Phrase{"+rules[2]+"} "+str(unif)
                except: 
                    print lS+" --> c"+str(context)+\
                        " Phrase{"+rules[0]+"} "+"Phrase{"+rules[1]+"} "+str(unif)



##NULL S: VERY IMPORTANT
h = 0 
unifNull = float(float(1.0)/float(954.0))
while h < 954: 
    print "S{null} --> c"+str(h)+" Phrase{null} "+str(unifNull)
    h += 1
    j += 1


####################
####################
####################

#################
# PHRASE NODES  #
#################

print "Phrase{null} --> Word{null} "+ str(1.0); j += 1 
print "Phrase{null} --> Phrase{null} Word{null} "+ str(1.0); j += 1 

for item in players:
    j += 3
    unifPhr = float(float(1.0)/float(3.0))
    print "Phrase{"+item+"} --> "+"Word{"+item+\
        "} "+str(unifPhr)
    print "Phrase{"+item+"} --> PhX{"+item+"} "+\
        "Word{"+item+"} "+str(unifPhr)
    print "Phrase{"+item+"} --> PhX{"+item+"} "+\
        "Word{null} "+str(unifPhr)

for item in playModes:
    unifPhr = float(float(1.0)/float(3.0))
    print "Phrase{"+item.groups()[0]+"} --> "+\
        "Word{"+item.groups()[0]+"} "+str(unifPhr)
    print "Phrase{"+item.groups()[0]+"} --> PhX{"+item.groups()[0]+"} "+\
        "Word{"+item.groups()[0]+"} "+str(unifPhr)
    print "Phrase{"+item.groups()[0]+"} --> PhX{"+\
        item.groups()[0]+"} "+"Word{null} "+str(unifPhr)
    j+= 3

for item in players:
    j += 4
    unifPhx = float(float(1.0)/float(4.0))
    print "PhX{"+item+"} --> "+"Word{"+item+"} "+str(unifPhx)
    print "PhX{"+item+"} --> PhX{"+item+"} "+\
        "Word{"+item+"} "+str(unifPhx)
    print "PhX{"+item+"} --> PhX{"+item+\
        "} "+"Word{null} "+str(unifPhx)
    print "PhX{"+item+"} --> Word{null} "+\
        str(unifPhx)

for item in playModes: 
    unifPhx = float(float(1.0)/float(4.0))
    print "PhX{"+item.groups()[0]+"} --> "+\
        "Word{"+item.groups()[0]+"} "+str(unifPhx)
    print "PhX{"+item.groups()[0]+\
        "} --> Word{null} "+str(unifPhx)
    print "PhX{"+item.groups()[0]+"} --> PhX{"+\
        item.groups()[0]+"} "+"Word{"+item.groups()[0]+"} "+str(unifPhx)
    print "PhX{"+item.groups()[0]+"} --> PhX{"+\
        item.groups()[0]+"} "+"Word{null} "+str(unifPhx)
    j+= 4

for item in players:
    j += 3
    unifPhr = float(float(1.0)/float(3.0))
    print "Ph{"+item+"} --> PhX{"+item+"} "+\
        "Word{"+item+"} "+str(unifPhr)
    print "Ph{"+item+"} --> PhX{"+item+"} "+\
        "Word{null} "+str(unifPhr)
    print "Ph{"+item+"} --> "+"Word{null} "+str(unifPhr)
    

for item in playModes: 
    unifPhr = float(float(1.0)/float(3.0))
    print "Ph{"+item.groups()[0]+"} --> PhX{"+\
        item.groups()[0]+"} "+"Word{"+item.groups()[0]+"} "+str(unifPhr)
    print "Ph{"+item.groups()[0]+"} --> PhX{"+\
        item.groups()[0]+"} "+"Word{null} "+str(unifPhr)
    print "Ph{"+item.groups()[0]+"} --> "+\
        "Word{null} "+str(unifPhr)
    j+= 3


#################
#################
#################


#####################
# PRINTING CONCEPTS #
#####################

unifW = float(1.0/954.0)

for word in words.keys(): 
    j += 1
    print "Word{null} --> "+word+" "+\
        str(unifW)

for item in players: 
    for word in words.keys(): 
        j += 1
        print "Word{"+item+"} --> "+\
            word+" "+str(unifW)

for item in playModes: 
    for word in words.keys(): 
        j+=1 
        print "Word{"+item.groups()[0]+\
            "} --> "+word+" "+str(unifW)

#####################
#####################
#####################

################
# Context maps #
################

for item in contexts: 
    print "c"+str(contexts.index(item))+\
        " --> "+str(contexts.index(item))+" "+str(1.0)
    j += 1

################
################

#FINAL RULE COUNT
print j 
