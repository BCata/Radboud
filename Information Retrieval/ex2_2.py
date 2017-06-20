import sys
import operator
import math

with open(sys.argv[1]) as f:
    lines = f.readlines()

queryURL = {}
queryDict = {}
top10Queries = []
uniqueURLs = set()

def processQuery(query):
	if query in queryDict:
		queryDict[query] = queryDict[query]+1
	else:
		queryDict[query] = 1

def processClicked(clicked):
	if clicked in queryURL:
		queryURL[clicked] = queryURL[clicked]+1
	else:
		queryURL[clicked] = 1

for line in lines:
	lineList = line.split("\t")
	query = lineList[6]
	url = lineList[9]
	eventType = lineList[1]
	if eventType == 'q':
		processQuery(query)
	if eventType == 'c':
		processClicked((query,url))
		uniqueURLs.add(url)

sortedQueries = sorted(queryDict.items(), key = operator.itemgetter(1), reverse = True)
for i in range(0,10):
	top10Queries.append(sortedQueries[i][0])

URLlist = list(uniqueURLs)

queryVectorDict = {}

for query in top10Queries:
	temp = []
	for url in URLlist:
		if (query,url) in queryURL:
			temp.append(queryURL[(query,url)])
		else:
			temp.append(0)
	queryVectorDict[query] = temp

def calculateCos(v1, v2):
	l = len(v1)
	prod = 0
	v1Len = 0
	v2Len = 0
	for i in range(0,l):
		prod += v1[i]*v2[i]
		v1Len += v1[i]*v1[i]
		v2Len += v2[i]*v2[i]
	v1Len = math.sqrt(v1Len)
	v2Len = math.sqrt(v2Len)
	return prod/(v1Len*v2Len)

cosineSim = {}

for q1 in top10Queries:
	for q2 in top10Queries:
		cosineSim[(q1,q2)] = calculateCos(queryVectorDict[q1], queryVectorDict[q2])

for key, value in cosineSim.iteritems():
	print str(key) + " -> " + str(value)
