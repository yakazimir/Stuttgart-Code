import re
from sets import Set
from random import choice
import copy


conv = {"a":0,"b":1,"c":2,"d":3,"e":4}

class fileIn(object):
    """this is a new class"""
    def __init__(self,x): 
        """this is a test to see if it works"""
        self.attributeVals = {}
        self.attributes = []
        self.objectVals = {}
        self.objects = []
        self.graph = self.__computeGraph(x)    
    def __computeGraph(self,x):
        """this is a test to see if it all works"""
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
        """this is a test to see if it works"""
        try: 
            if y == "objs": 
                setList = [set(self.objectVals[i]) for i in n]
                u = set.intersection(*setList)
                return list(u)   
            elif y == "attrs": 
                setList = [set(self.attributeVals[i]) for i in n]
                u = set.intersection(*setList)
                return list(u) 
            else: pass 
        except TypeError : pass 
            
    def _findB(self): 
        S = [list(j) for j in Set([tuple(self.attributeVals[i]) 
                                   for i in self.attributeVals])]
        Q = copy.deepcopy(S)
        while Q != []:
            ##DEQUEUE
            SX = choice(Q); del Q[Q.index(SX)]; G = self._conSet(SX,"objs")
            if G != None: 
                nSet = [i for i in self.attributes if i not in G]
                for y in nSet:
                    SNEW = list(Set([i for i in self.attributeVals[y]]).intersection(Set(SX))) 
                    if SNEW not in S: 
                        S.append(SNEW)
                        Q.append(SNEW)
                    else: pass 
            else: pass 
        print len([list(i) for i in Set([tuple(i) for i in S])])
        print [list(i) for i in Set([tuple(i) for i in S])]
            


   #while Q != []: 
        #print G
        #print nSet
        #print self.attributeVals[nSet[0]]


#def main(): 
fi = fileIn('exampleObjects.txt')


