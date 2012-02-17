import re 
left = re.compile('(.+) \-\-\> (.+)\s*')

grammar = open('CNFGRAM.txt')
currentR = "" 

for line in grammar:
    leftRule = re.search(left,line)
    listF = leftRule.groups()[1].split(" ")
    if leftRule.groups()[0] != currentR:
        print "\t\t)"
        currentR = leftRule.groups()[0]
        if len(leftRule.groups()[1].split(" ")) == 3: 
            print "\""+leftRule.groups()[0]+"\""+" -> "+\
                "List(R"+re.sub("\'","\"",
                                str(((listF[0],listF[1]),float(listF[2]))))+","
        elif len(leftRule.groups()[1].split(" ")) == 2:
            print "\""+leftRule.groups()[0]+"\""+" -> "+\
                "List(L"+re.sub("\'","\"",
                                str(((listF[0],float(listF[1])))))+"," 
        else: 
            print "error" 
    else:
        if len(leftRule.groups()[1].split(" ")) == 3: 
            print "\t\t"+"R"+re.sub("\'","\"",
                                    str(((listF[0],listF[1]),float(listF[2]))))+","
        elif len(leftRule.groups()[1].split(" ")) == 2:
            print "\t\t"+"L"+re.sub("\'","\"",
                                    str(((listF[0],float(listF[1])))))+"," 


        #print len(leftRule.groups[1])
        #print "\t"+str(leftRule.groups(1))
        #pass 
