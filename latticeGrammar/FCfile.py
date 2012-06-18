import re 
from sets import Set

#########################
# FOR PRODUCING FC FILE #
# JUNE 2012             #
#########################

class inputFiles(object): 
    
    def __init__(self,s,l): 
        #CONTEXTS AND COUNTS
        self.contexts = {}
        ##WORDS AND CONTEXTS
        self.words = {}
        ##CONTEXT INDEX -- VALUES
        self.logicIndex = {}
        ##CONTEXTS OF WORDS IN TRAINING
        self.wordContexts = {}
        ###COUNTS OF WORDS
        self.wordCounts = {}
        self.sentence = self.__privateP(s)
        self.logic = self.__privateP(l)
        self.numOfContexts = len(self.contexts.keys())
        self.numOfWords = len(self.words.keys())
        self.logicConcepts = list(Set(sum(self.logicIndex.values(),[])))
        
    def sortContexts(self): 
        return sorted([(v,k) for (k,v) in self.contexts.items()], reverse=True)

    def sortWords(self): 
        return sorted([(v,k) for (k,v) in self.wordCounts.items()], reverse=True)
        
    def __privateP(self, fi):
        f = open(fi)
        x = {self.__privateC(i) for i in [j.split() for j in f]} 
        f.closed; return x

    def __privateC(self, x):   
        z = x[1:]; logic = False 
        if re.search(r'\)$',' '.join(x[1])): logic = True  
        
        if logic == False:            
            for i in range(len(z)):
                con = str((' '.join(z[:i]),' '.join(z[i+1:]))) 
                self.contexts[con] = self.contexts.setdefault(con,0)+1
                self.words.setdefault(z[i],[con]).append(con)
                self.wordCounts[z[i]] = self.wordCounts.setdefault(z[i],0)+1
                self.wordContexts.setdefault(z[i],[x[0]]).append(x[0])
        else:          
            lO = Set(re.sub('\(|\)|\,',' ',' '.join(x[1:])).split())
            self.logicIndex[x[0]] = list(lO)

        return (' '.join(z), x[0])

    # def printToFile(self,name,n=None):
    #     f = open(name+".FC", "w")
    #     f.write(file1.makeFile((150,400)))
    #     f.closed

    def makeFile(self, n=None):
        attrP = "############\n##ATTRIBUTES\n############"
        objP = "############\n##OBJECTS\n############"
        objAttrP = "############\n##OBJ/ATTRS\n############"
           
        def on(x,y):
            a = Set(self.words[x]).intersection(Set(y))
            if a != Set() and len(list(a))>1 : 
                #print list(a)
                return (x,list(a)+self.wordConcepts(x))
            else: return (None,None)

        if n == None:            
            print attrP
            wO = self.contexts.keys()+self.logicConcepts+["null"]
            print "attributes:"+";".join(wO)
            print objP
            print "objects:"+";".join(self.words.keys())
            print objAttrP
            for (i,j) in self.words.items(): 
                wO2 = j+self.wordConcepts(i)+["null"]
                print i+"--"+";".join(wO2)
        else:
            #try:
            topNCon = [i[1] for i in self.sortContexts()[:n[1]] if i[0] > 3]
            topNWords = [i[1] for i in self.sortWords()[:n[0]] if i[0] > 2]
            relConSet = list(Set(sum([self.wordConcepts(i) for i in topNWords],[])))
            relContexts = list(Set(sum([self.words[i] for i in topNWords],[])).intersection(Set(topNCon))) 
            filt = dict((key,value) for (key,value) in {on(i,relContexts+relConSet)[0]:on(i,relContexts+relConSet)[1] for 
                                                        i in topNWords}.iteritems() if key != None)
            print attrP
            wO = list(Set(sum([i for i in filt.values()],[])))+relConSet+["null"]
            #wO = list(Set(sum(filt.values())))+relConSet+["null"]
            print "attributes:"+";".join(wO)
            print objP
            
            print "objects:"+";".join(filt)   
            print objAttrP
            for (w,j) in filt.iteritems():
                print w+"--"+";".join(j)
      
            #except: print "input must be tuple (#Words,#Contexts)"         
            
           
    def wordConcepts(self, n=None):
        if n == None: 
            for word in self.words.keys(): 
                cStuff = Set(sum([self.logicIndex[i] for i in self.wordContexts[word]],[]))
                print word+": ",; print list(cStuff)
        else: 
            try: 
                cStuff = Set(sum([self.logicIndex[i] for i in self.wordContexts[n]],[]))
                return list(cStuff)
            except: return "word not found"
                

file1 = inputFiles('sSentences.txt', 'contexts.txt')

file1.makeFile((150,500))



  # filt = filter(None,{on(i,relContexts)[0]:0 for i in topNWords})
            #filt = filter(None,{on(i,relContexts)[0]:on(i,relContexts) for i in topNWords})

            #filt = dict((key,value) for 
            #            key, value in a.iteritems() if key != None)


#print "attributes:"+";".join(self.contexts.keys())
    # print topNCon
            # print "##########"
            # print topNWords
   #print relConSet
                #print relContexts
        #print w
                #wO2 = self.words[w]+self.wordConcepts(w)+["null"]
                    #a = Set(wO2).intersection(Set(relContexts))
                    #if a == Set(): 
                    #    print "yes"
                    # for item in wO2: 
                    #     if item not in relConSet+relConSet: 
                    #         print "\tyes" 
                    #     else: 
                    #         print "\tno"
                    #print w+"--"+";".join(wO2) 

            #$print filt
            #print len(filt) 
            
