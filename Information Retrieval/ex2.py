import sys
import operator

with open(sys.argv[1]) as f:
    lines = f.readlines()

queryDict = {}
clickedDict = {}

def processQuery(query):
	if query in queryDict:
		queryDict[query] = queryDict[query]+1
	else:
		queryDict[query] = 1

def processClicked(clicked):
	if clicked in clickedDict:
		clickedDict[clicked] = clickedDict[clicked]+1
	else:
		clickedDict[clicked] = 1

for line in lines:
	lineList = line.split("\t")
	query = lineList[6]
	clicked = lineList[9]
	eventType = lineList[1]
	if eventType == 'q':
		processQuery(query)
	if eventType == 'c':
		processClicked(clicked)

unique = len(queryDict)

print "Number of unique queries: " + str(unique)

sortedQueries = sorted(queryDict.items(), key = operator.itemgetter(1), reverse = True)
print "The top-10 most frequent queries"
for i in range(0,10):
	print str(i+1) + ") " + str(sortedQueries[i])

sortedURLs = sorted(clickedDict.items(), key = operator.itemgetter(1), reverse = True)
print "The top-10 most clicked URLs"
for i in range(0,10):
	print str(i+1) + ") " + str(sortedURLs[i])






