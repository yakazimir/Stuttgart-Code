import re 
from sets import Set

#########################
# FOR PRODUCING FC FILE #
# JUNE 2012             #
#########################

class inputFiles(object): 
    
    def __init__(self,s,l): 
        self.contexts = {}
        self.words = {}
        self.logicIndex = {}
        self.wordContexts = {}
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


    def makeFile(self, n=None):
        attrP = "############\n##ATTRIBUTES\n############"
        objP = "############\n##OBJECTS\n############"
        objAttrP = "############\n##OBJ/ATTRS\n############"
           
        if n == None:            
            print attrP
            wO = self.contexts.keys()+self.logicConcepts
            #print "attributes:"+";".join(self.contexts.keys())
            print "attributes:"+";".join(wO)
            print objP
            print "objects:"+";".join(self.words.keys())
            print objAttrP
            for (i,j) in self.words.items(): 
                wO2 = j+self.wordConcepts(i)
                print i+"--"+";".join(wO2)
        else: 
            pass 
         

    def wordConcepts(self, n=None):
        if n == None: 
            for word in self.words.keys(): 
                cStuff = Set(sum([self.logicIndex[i] 
                                  for i in self.wordContexts[word]],[]))
                print word+": ",; print list(cStuff)
        else: 
            try: 
                cStuff = Set(sum([self.logicIndex[i] 
                                  for i in self.wordContexts[n]],[]))
                return list(cStuff)
            except: return "word not found"
                

file1 = inputFiles('sSentences.txt', 'contexts.txt')



