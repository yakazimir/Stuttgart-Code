import re
from sets import Set
from random import choice

class fileIn(object): 
    def __init__(self,x): 
        self.attributeVals = {}
        self.attributes = []
        self.objectVals = {}
        self.objects = []
        self.graph = self.__computeGraph(x)    
    def __computeGraph(self,x):
        def vo(v): 
            if re.search('attributes', v):
                a = re.sub('\n','',v).split(':')[1]
                self.attributes = a.split()
            elif re.search('objects',v): 
                o = re.sub('\n','',v).split(':')[1] 
                self.objects = o.split()
            elif re.search('\w+\-\-',v):
                ov = re.sub('\n','',v).split('--')
                self.objectVals[ov[0]] = ov[1].split()
                for item in ov[1]:
                    if item != ' ':
                        if not item in self.attributeVals: 
                            self.attributeVals[item] = [ov[0]] 
                        else: 
                            self.attributeVals[item].append(ov[0])                
            else : pass 
        f = open(x)
        for line in f: vo(line)
        f.closed   
    def _conSet(self,n, y): 
        if y == "objs": 
            setList = [set(self.objectVals[i]) for i in n]
            u = set.intersection(*setList)
            return u   
        elif y == "attrs": 
            setList = [set(self.attributeVals[i]) for i in n]
            u = set.intersection(*setList)
            return u 
        else: pass 
            
    def _findB(self): 
        S = [list(j) for j in Set([tuple(self.attributeVals[i]) 
                                   for i in self.attributeVals])]
        Q = S 
        print Q 
        #while Q != []: 
        SX = choice(Q); del Q[Q.index(SX)] 
        print Q


#def main(): 
fi = fileIn('exampleObjects.txt')


