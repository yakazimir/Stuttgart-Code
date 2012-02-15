from numpy import array 
from scipy.cluster.vq import vq, kmeans, kmeans2, whiten 
import matplotlib.pyplot as plt 
import pylab  
import random
pylab.close() 

#randomList = []
#randomPairs = []*10000
randomPairs = []

i = 0
while i < 10000:
    rand1 = random.random()
    rand2 = random.random()
    randomPairs.append([rand1, rand2])
    i += 1


features = array(randomPairs)
# features = array([[1, 2], [14, 3], [9,6], [6, 4],
#                   [1, 3],
#                   [2, 4],
#                   [5, 6], 
#                   [10, 12], 
#                   [6, 9], 
#                   [7, 10], 
#                   [9, 13],
#                   [10, 13], 
#                   [10, 14], 
#                   [10, 9],
#                   [3, 10], 
#                   [12, 14], 
#                   [4, 9], 
#                   [14, 5], 
#                   [12, 3], 
#                   [3, 9],
#                   [13, 16], 
#                   [20, 16], 
#                   [5, 1], 
#                   [22, 17], 
#                   [17, 22], 
#                   [23, 4], 
#                   [5, 17], 
#                   [20, 10], 
#                   [23, 4], 
#                   [12, 20], 
#                   [12, 24], 
#                   [19, 7], 
#                   [18, 18], 
#                   [17,16],
#                   [23, 23], 
#                   [30, 10],
#                   [30, 5]])

whitened = whiten(features) 
res, idx = kmeans2(whitened, 3)

#print random
colors = ([([0,0,0],[1,0,0],[0,0,1])[i] for i in idx])


pylab.scatter(features[:,0], features[:,1], s = 6, c=colors)
pylab.show()




#print colors

#print value

#pylab.scatter([400, 450, 800, 1000], [5.5, 6.5, 3.1, 9.7])

#pylab.show()


#print res
#pylab.scatter( 

#print value











# #from numpy import array 
# #from scipy

# import numpy
# #import matplotlib
# #matplotlib.use('Agg')
# import matplotlib.pyplot as plt
# from scipy.cluster.vq import *
# import pylab
# pylab.close()

# # generate some random xy points and
# # give them some striation so there will be "real" groups.
# xy = numpy.random.rand(30,2)

# xy[3:8,1] -= .9
# xy[22:28,1] += .9

# # make some z vlues
# z = numpy.sin(xy[:,1]-0.2*xy[:,1])


# # whiten them
# z = whiten(z)

# # let scipy do its magic (k==3 groups)
# res, idx = kmeans2(numpy.array(zip(xy[:,0],xy[:,1],z)),3)


# # convert groups to rbg 3-tuples.
# colors = ([([0,0,0],[1,0,0],[0,0,1])[i] for i in idx])

# # show sizes and colors. each color belongs in diff cluster.
# pylab.scatter(xy[:,0],xy[:,1],s=20*z+9, c=colors)

# plt.savefig("path.png")
# plt.show()
# #pylab.savefig('clust.png')
# #pylab.show('clust.png')
