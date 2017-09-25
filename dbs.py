import matplotlib.pyplot as plt
import csv
from numpy.random import rand
from numpy import square, sqrt, genfromtxt


def regionQuery(P, eps, D):	
	neighbourPts = []
	for Dpoint in D:
		if sqrt(square(P - Dpoint))<int(eps):
			neighbourPts.append(Dpoint)

	return neighbourPts

def DBSCAN(D, eps, MinPts):
	noise = []
	visited = []
	C = []
	c_n = -1
	for point in D:
		if point not in visited:
			visited.append(point) #marking point as visited
			#print visited
			neighbourPts = regionQuery(point, eps, D)
			# print (neighbourPts)
			if len(neighbourPts) < MinPts:
				noise.append(point)
				#print "noise: ", point
			else:
				#C.append([])
				#print "clPoint: ", point
				c_n+=1
				x = []
				expandCluster(point, neighbourPts, C, c_n, eps, MinPts, D, visited, x)

	print("CLUSTER: ")
	for x in C:
		print(x)
	print("NOISE: ")
	print(noise)

	#print("no. of clusters: ", len(C))
	#print("length of noise:", len(noise))

	'''for cluster in C:
		col =[rand(1),rand(1),rand(1)]		
		print cluster		
		plt.scatter([i[1] for i in cluster],[i[2] for i in cluster],color=col)
	plt.show() 	'''

		

def expandCluster(P, neighbourPts, C, c_n, eps, MinPts, D, visited, x):

	x.append(P)
	for point in neighbourPts:
		if point not in visited:
			visited.append(point)
			#print "expandVisited: ", visited
			neighbourPts_2 = regionQuery(point, eps, D)
			if len(neighbourPts_2) >= MinPts:
				neighbourPts += neighbourPts_2
		if point not in (i for i in x):
			x.append(point)
			#C[c_n].append(point)
	C.append(x)



eps = input("enter eps")

l = []
#l = [1, 2, 60, 65, 63, 68, 62, 61, 69, 99]
with open('cs1.csv', 'rU') as csvfile:
    readCSV = csv.reader(csvfile, delimiter=',')
    for row in readCSV:
        l.append(int(row[0]))

# print(l)

import timeit
start_time = timeit.default_timer()

DBSCAN(l,eps,10)

elapsed = timeit.default_timer() - start_time
# print("time", elapsed)

exit()