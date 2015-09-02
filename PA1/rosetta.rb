# RUBY: Reverse-sort the lines from standard input
lines = [ ]     # a list variable to hold all the lines we'll read in
working = true  # are there still more lines to read in? 
while working
  line = gets           # read a line from standard input 
  if line == nil        # nil is "nothing, it didn't work" 
    working = false     # we're done reading stuff
  else
    lines[lines.length] = line # append 'line' to the end of 'lines
  end # end of 'if'
end # end of 'while' 

setOfLines = [ ]
startVal = [ ]
endVal = [ ]

#get start values and end values
lines.each_with_index do |thing, index|
	if index % 2 == 0
		endVal << thing
	else
		startVal << thing
	end
end

#get set of lines
lines.each do |thing|
	if !setOfLines.include?(thing)
		setOfLines << thing
	end
end

#get set of strings with no incoming edges
noIncomingEdges = [ ]
startVal.each do |a|
	temp = false
	endVal.each do |b|
		if a == b
			temp = true
		end
	end
	if !temp
		noIncomingEdges << a
	end
end

if noIncomingEdges.empty?
	puts "cycle"
else
	tmp = [ ]
	noIncomingEdges.each do |a|
		if !tmp.include?(a)
			tmp << a
		end
	end
	#reverse sort the initial list of no incoming edges
	sorted = tmp.sort do |a,b|
		b <=> a
	end
	
	finalLines = [ ]
	tableOfValues = [ ]
	totalDim = setOfLines.length * setOfLines.length - 1
	#create boolean-ish table of values
	for i in 0..totalDim
		tableOfValues.push(0)
	end
	for i in 0..startVal.length - 1
		rowIndex = setOfLines.index(startVal[i])
		colIndex = setOfLines.index(endVal[i])
		startNodeIndex = rowIndex*setOfLines.length
		tableOfValues[startNodeIndex + colIndex] = 1
	end
	
	while sorted.length != 0 do
		node = sorted.pop()
		finalLines.push(node)
		rowOfNode = setOfLines.index(node) * setOfLines.length
		outCheck = 0
		outVals = [ ]
		
		for j in 0..setOfLines.length - 1
			if tableOfValues[rowOfNode + j] == 1
				checkInEdgesZ = 0
				for k in 0..setOfLines.length - 1
					colofEndV = tableOfValues[j + k*setOfLines.length]
					if colofEndV == 1 && setOfLines[k] != node
						checkInEdgesZ = 1
					end
				end
				if checkInEdgesZ == 0
					outCheck += 1
				end
			end
		end
		
		for j in 0..setOfLines.length - 1
			if tableOfValues[rowOfNode + j] == 1
				#remove edge
				tableOfValues[rowOfNode + j] = 0
				
				#make sure there are no other nodes that go into same destination
				checkInEdges = 0
				for k in 0..setOfLines.length - 1
					columnofEndV = tableOfValues[j + k*setOfLines.length]
					if columnofEndV == 1
						checkInEdges = 1
					end
				end
				
				#push in one of the values that will be sorted
				if checkInEdges == 0
					outVals.push(setOfLines[j])
					outCheck -= 1
				end
					
				#if no other nodes have edges to said vertex go to vertex 
				if checkInEdges == 0 && outCheck == 0	
					tmpOutVals = outVals.sort
					for i in 0..outVals.length - 1
						tmpLine = tmpOutVals.pop()
						sorted.push(tmpLine)
					end
				end
			end
		end
	end
	
	checkEdges = 0
	tableOfValues.each do |a|
		if a == 1
			checkEdges = 1
		end
	end
	if checkEdges == 1
		print "cycle"
	else
		finalLines.each do |a|
			puts a
		end
	end
end