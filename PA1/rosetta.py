# PYTHON: Reverse-sort the lines from standard input 
import sys                # bring in a standard library 
lines = sys.__stdin__.readlines() # read every line from stdin into an array! 

def reverse_compare(a,b): # defining a new function with two arguments
  return cmp(b,a)         # swap the standard comparison

  # the : loosely means "we're going to enter a new level of control-flow
  # and I'll be tabbing over to show you that" 
  
endval = lines[::2]
startval = lines[1::2]
setOfLines = []
for a in lines:
	if a not in setOfLines:
		setOfLines.append(a)
		
noIncomingEdges = []

for l in startval:
	temp = 0
	for k in endval:
		if l == k:
			temp = 1
	if temp == 0:
		noIncomingEdges.append(l) ,		
		
		
if len(noIncomingEdges) == 0:
	print "cycle"
else:
	temp = set(noIncomingEdges)
	setofNoInEdges = list(temp)
	setofNoInEdges.sort(cmp = reverse_compare)
	
	finalLines = []
	tableOfValues = []
	
	for i in setOfLines:
		for j in setOfLines:
			tableOfValues.append(0) ,
	
	#create edges via boolean table
	for i in range(len(startval)):
		rowIndex = setOfLines.index(startval[i])
		colIndex = setOfLines.index(endval[i])
		tableOfValues[rowIndex* len(setOfLines) + colIndex] = 1
	
	
	while (len(setofNoInEdges) != 0):
		node = setofNoInEdges.pop()
		finalLines.append(node)
		rowOfNode = setOfLines.index(node) * len(setOfLines)
		outCheck = 0
		outVals = []
		
		#check for # of vertices without other incoming edges
		for j in range(len(setOfLines)):
			if tableOfValues[rowOfNode + j] == 1:
				checkInEdgesZ = 0
				for k in range(len(setOfLines)):
					colofEndV = tableOfValues[j + k*len(setOfLines)]
					if colofEndV == 1 and setOfLines[k] != node:
						checkInEdgesZ = 1
				if checkInEdgesZ == 0:
					outCheck += 1
				
		for j in range(len(setOfLines)):
			if tableOfValues[rowOfNode + j] == 1:
				
				#get rid of edge
				tableOfValues[rowOfNode + j] = 0
				
				#check for other incoming edges to said vertex
				checkInEdges = 0
				for k in range(len(setOfLines)):
					columnofEndV = tableOfValues[j + k*len(setOfLines)]
					if columnofEndV == 1:
						checkInEdges = 1
				
				if checkInEdges == 0:
					outVals.append(setOfLines[j])
					outCheck -= 1
					
				#if no other nodes have edges to said vertex go to vertex 
				if checkInEdges == 0 and outCheck == 0:	
					outVals.sort()
					for i in range(len(outVals)):
						tmpLine = outVals.pop()
						setofNoInEdges.append(tmpLine)
	
	checkEdges = 0
	for a in tableOfValues:
		if a == 1:
			checkEdges = 1
	if checkEdges == 1:
		print "cycle"
	else:
		for a in finalLines:
			print a ,
	


""" 
lines.sort(cmp = reverse_compare) # sort them using our comparison
                        # this is a destructive, in-place sort 
for one_line in lines:  # iterate over every element of that now-sorted array
  print one_line ,      # the ending comma means "don't print another newline"
"""
