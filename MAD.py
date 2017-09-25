import numpy as np
import csv
import timeit

start_time = timeit.default_timer()

l = []
with open('memory.csv', 'rU') as csvfile:
    readCSV = csv.reader(csvfile, delimiter=',')
    l.append(92)
    for row in readCSV:
        l.append(int(row[1]))
# print("ABSJ:", l)
print(l)
print("\n\n")
mean = round(np.mean(l, axis=0), 2)
print("Mean: ", mean)
median = round(np.median(l, axis=0), 2)
print("Median: ", median)
sd = round(np.std(l, axis=0), 2)
print("SD: ", sd)

l1 = []
for x in l:
	l1.append(abs(float("%.2f" % (x-median))))

MAD = round(np.median(l1, axis=0), 2)
print("MAD: ", MAD)

ls = []
for x in l1:
	ls.append(abs(float("%.2f" % (x/MAD))))
#print(ls)

out = []
for x in ls:
	if(x > 1):
		out.append(l[ls.index(x)])
print("\n\nOutliers: ")
print(out)

elapsed = timeit.default_timer() - start_time
print("time: ", elapsed)

exit()