from sets import Set
import time
import padnums
from padnums import pprint_table 
import sys
out = sys.stdout 

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
    'V':[('V', 'N'),
         ['eats', 'loves', 'hates']],
    'N':[('N', 'P'),
         ['she','he','pizza', 'pasta', 'anchovies', 'meat', 'bread', 'cheese']],  
    'P':('PP', 'N'), 
    'PP':['without', 'with','on'],
    
}

sentence = 'she eats meat on bread with pasta without cheese'
#sentence = 'she eats pizza without anchovies'

sentence2 = 'that dog loves the spider'
print 'input: '+ sentence

class Parser(object):
    def __init__(self, Grammar, Input): 
        self.Grammar = Grammar
        self.Input = Input.position 
        self.table = {}
      
    def _accCheck(self, table): 
        if ((0, len(self.Input)), 'S') in table.iteritems(): 
            print "accepted"
            count = [str(i+1) for i in range(len(self.Input))]
            count.insert(0, "")
            table = [count, 
                     ]
            for i in range(len(self.Input)):
                table.append([str(i)])
                for j in range(len(self.Input)): 
                    try: 
                        value = self.table[(i,j+1)] 
                        if value: 
                            table[i+1].append(value+'|')
                    except: 
                        table[i+1].append("' '|")
            pprint_table(out, table)
        else: 
            print "not recognized"
            
    def _inParse(self, i, j): 
        k = j - 2
        while (k > -1):
            z = k + 1
            while (z < j):
                try: 
                    pair = (
                        self.table[(k,z)], 
                        self.table[(z,j)]
                        )
                    if pair in self.Grammar.rightSide:
                        self._writeTable(pair, k, j)
                except: 
                    pass 
                z += 1 
            k -= 1

    def _parse(self): 
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
        return value
 

    def _writeTable(self, Value, i, j): 
        self.table[
            i, j
            ] = self.Grammar.terminalMap[Value]
        
class Grammar(object):
    def __init__(self, grammar):
        self.terminalMap = self._tEnum(grammar)
        self.rightSide = Set(
            key for key in self.terminalMap.iterkeys()
            ) 

    def _tEnum(self, value): 
        terminalMap = {} 
        for (key,value) in value.items():
            if isinstance(value, list): 
                for entry in value:
                    if isinstance(entry, tuple):
                        terminalMap[entry] = key 
                    elif isinstance(entry, list):
                        for item2 in entry: 
                            terminalMap[item2] = key
                    else: 
                        terminalMap[entry] = key
            else: 
                terminalMap[value] = key 
        return terminalMap 

class Input(object): 
    def __init__(self, language): 
        self.position = language.split(' ')
        self.posMap = self._Pos(self.position)
    
    def _Pos(self, position): 
        pass 

    
Grammar = Grammar(grammar1)
Input = Input(sentence)

#print Grammar.terminalMap
#print Grammar.rightSide


Parser = Parser(Grammar, Input) 
start = time.clock() 
Parser._parse()
elapsed = (time.clock() - start) 
print 'CPU Time: '+str(elapsed) 



