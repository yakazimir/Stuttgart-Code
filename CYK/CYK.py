from sets import Set
import time
from padnums import pprint_table 
import sys
import itertools
out = sys.stdout 
import re

#GRAMMAR 
# grammar1 = { 
#     'S':('NP', 'VP'), 
#     'NP':('DET', 'N'),
#     'DET':['the', 'a', 'that'], 
#     'N':['dog', 'cat', 'animal', 'wolf', 'spider'], 
#     'VP':('V', 'NP'), 
#     'V':['loves', 'hates', 'fought']  
# }

grammar1 = { 
    'S':('N', 'V'), 
    'V':[('V', 'N'),('V','NPP'), 
         ['eats','loves','hates']],
    'N':[('N', 'P'),
         ['she','he','pizza', 'pasta', 'anchovies', 'meat', 'bread', 'cheese']],  
    'P':('PP', 'N'), 
    'PP':['without', 'with','on'],
    ##redefined from above
    'NPP':('N', 'P'),
    
}

#sentence = 'she loves pizza with anchovies on bread without cheese'
sentence = 'she loves pizza without meat'

sentence2 = 'that dog loves the spider'
print 'input: '+ sentence

class Parser(object):
    def __init__(self, Grammar, Input): 
        self.Grammar = Grammar
        self.Input = Input.position 
        self.table = {}
      
    def _accCheck(self, table): 
        if ((0, len(self.Input)), ['S']) in table.iteritems(): 
            print "accepted"
            count = [str(i+1) for i in range(len(self.Input))]
            count.insert(0, "")
            table = [count,]
            for i in range(len(self.Input)):
                table.append([str(i)])
                for j in range(len(self.Input)): 
                    try: 
                        value = self.table[(i,j+1)] 
                        if value: 
                            table[i+1].append(
                                re.sub('\[|\]|\'| ', 
                                       '', str(value)+'|'))
                    except: 
                        table[i+1].append("  |")
            pprint_table(out, table)
        else: 
           print "table not complete"
           #print self.table
            
    def _testList(self, k, z, j): 
        val1 = self.table[(k,z)]
        val2 = self.table[(z,j)]    
        values = [ 
            i for i in itertools.product(
                val1, val2)
            ]
        return values

    def _inParse(self, i, j): 
        k = j - 2
        while (k > -1):
            z = k + 1
            while (z < j):
                try: 
                    pair = self._testList(k, z, j)
                    for item in pair: 
                        if item in self.Grammar.rightSide:
                            self._writeTable(item, k, j)
                        else: 
                            pass 
                except: 
                    pass 
                z += 1 
            k -= 1

    def parse(self): 
        for i in range(len(self.Input)):
            if self.Input[i] in self.Grammar.rightSide:
                i = self.Input.index(self.Input[i]) 
                j = i + 1 
                self._writeTable(self.Input[i], i, j)
                self._inParse(i,j)
            else:  
                return "not recognized" 
                break 
             
        value = self._accCheck(self.table)
        del self 
        return value
 

    def _writeTable(self, Value, i, j): 
        if (i,j) in self.table: 
            self.table[(i, j)].append(
                self.Grammar.terminalMap[Value])
        else: 
              self.table[
                    (i, j)
                    ] = self.Grammar.terminalMap[Value] 
          
        print self.table
        
class Grammar(object):
    def __init__(self, grammar):
        self.terminalMap = self._tEnum(grammar)
        self.rightSide = Set(
            key for key in self.terminalMap.iterkeys()
            ) 
    
    def _Add(self, dict, key, value): 
        if not value in dict: 
            dict[value] = [key]
        else: 
            dict[value].append(key) 
            
    def _tEnum(self, value): 
        terminalMap = {} 
        for (key,value) in value.items():
            if isinstance(value, list): 
                for entry in value:
                    if isinstance(entry, tuple):
                        self._Add(terminalMap, key, entry)
                        #terminalMap[entry] = key
                    elif isinstance(entry, list):
                        for item2 in entry: 
                            self._Add(terminalMap, key, item2) 
                    else: 
                        self._Add(terminalMap, key, entry) 
            else: 
                self._Add(terminalMap, key, value)
                #terminalMap[value] = [key] 
        return terminalMap 

class Input(object): 
    def __init__(self, language): 
        self.position = language.split(' ')
        self.posMap = self._Pos(self.position)
    
    def _Pos(self, position): 
        pass 

    
Grammar = Grammar(grammar1)
Input = Input(sentence)

#print Grammar.rightSide
#print Grammar.terminalMap

print Grammar.terminalMap
print Grammar.rightSide


Parser = Parser(Grammar, Input) 
start = time.clock() 
Parser.parse()
elapsed = (time.clock() - start) 
print 'CPU Time: '+str(elapsed) 



