import re 
from sets import Set 

class inputFiles(object): 
    def __init__(self, s, windowSize=None): 
        self.wordCounts = {} 
        self.wordContexts = {} 
        self.words = {}
        self.contexts = {}
        self.sentences = self.__privateP(s,windowSize)  

    def sortWords(self): 
        return sorted([(v,k) for (k,v) in self.wordCounts.items()], reverse=True)

    def sortContexts(self): 
        return sorted([(v,k) for (k,v) in self.contexts.items()], reverse=True)
        
    def __privateP(self,fi,w): 
        f = open(fi)
        x = {self.__privateC(i,w) for i in [j.split() for j in f]} 
        f.closed 
        return x 

    def __checkEmpty(self, l):
        l = list(Set(l)) 
        if len(l) > 1 and '__' in l: 
            return [i for i in l if i != '__'] 
        else: return l

    def __privateC(self, x, w=None): 
        jk = {' '.join(x[start:end]):(start,end) for start in\
                  range(len(x)) for end in range(start+1,len(x)+1)}
        for (subpat,ind) in jk.items(): 
            s = ind[0]; f = ind[1]
            con = [' '.join(x[:s])+'__'+' '.join(x[f:])]
            try: 
                con += [' '.join(x[s-i:s])+'__'+' '.join(x[f:f+i]) for i in range(1,w)] 
            except: pass
            self.wordCounts[subpat] = self.wordCounts.setdefault(subpat,0)+1
            for c in self.__checkEmpty(con): 
                self.contexts[c] = self.contexts.setdefault(c,0)+1
                try : self.words[subpat].append(c)  
                except: self.words[subpat] = [c]

        return ' '.join(x)
        
    def printFile(self,n=None): 
        for (i,j) in self.words.items(): 
            print i+"--"+";".join(Set(j))

t = inputFiles("futbolData/input/sentences.txt",3)
t.printFile()

print len(t.words)
print len(t.contexts)
print str(len(t.words)+len(t.contexts))

