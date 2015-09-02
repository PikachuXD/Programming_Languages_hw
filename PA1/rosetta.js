// JAVASCRIPT: Reserve-Sort the lines from standard input

// We'll use the NodeJS ReadLine library to simplify line-by-line
// reading from the stdin input stream. 

var readline = require('readline');

var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false  
  // terminal=false prevents ReadLine from echoing our input back to us
});

var lines = new Array(); // we'll store all of the lines here
var startVal = new Array();
var endVal = new Array();
var setOfLines = new Array();
var noIncomingEdges = new Array();

rl.on('line', function(this_line) {
  // Every time there is a line available on stdin, this function will be
  // called. 
	lines.push(this_line); // add this line to what we've seen so far
}); 

rl.on('close', function() {
  // When we're finally out of input on stdin, this function will be
  // called.
  // We'll sort the lines in reverse order. We could also sort normally and
  // then call lines.reverse(), but we wanted to demonstrate passing a
  // custom comparison function to sort(). 
  /*lines.sort(function(a,b) {
    return (b > a);
  }); 
	*/
	for (var i = 0; i < lines.length; i++) {
		if (i % 2 == 0) {
			endVal.push(lines[i]);
		} else {
			startVal.push(lines[i]);
		}
		var check = setOfLines.indexOf(lines[i]);
		if (check == -1) {
			setOfLines.push(lines[i]);
		}
	}
	
	for (var i = 0; i < startVal.length; i++) {
		var temp = 0;
		for (var j = 0; j < startVal.length; j++) {
			if (startVal[i] == endVal[j]) {
				temp = 1;
			}
		}
		if (temp == 0) {
			noIncomingEdges.push(startVal[i]);
		}
	}
	
	if (noIncomingEdges.length == 0) {
		process.stdout.write('cycle');
	} else {
		
		var tmp = new Array();
		for (var i = 0; i < noIncomingEdges.length;i++) {
			var chk = tmp.indexOf(noIncomingEdges[i]);
			if (chk == -1) {
				tmp.push(noIncomingEdges[i]);
			}
		}
		tmp.sort();
		tmp.reverse();
		var tableOfValues = new Array();
		var finalLines = new Array();
		var totalDim = setOfLines.length * setOfLines.length;
		
		for (var i = 0; i < totalDim; i++) {
			tableOfValues.push(0);
		}
		
		for (var i = 0; i < startVal.length;i++) {
			var rowIndex = setOfLines.indexOf(startVal[i]);
			var colIndex = setOfLines.indexOf(endVal[i]);
			tableOfValues[rowIndex * setOfLines.length + colIndex] = 1;
		}
		
		while (tmp.length != 0) {
			var node = tmp.pop();
			finalLines.push(node);
			var rowOfNode = setOfLines.indexOf(node) * setOfLines.length;
			var outCheck = 0;
			var outVals = new Array();
			
			for (var j = 0; j < setOfLines.length; j++) {
				if (tableOfValues[rowOfNode + j] == 1) {
					var checkInEdgesZ = 0;
					for (var k = 0; k < setOfLines.length; k++) {
						var colofEndV = tableOfValues[j + k*setOfLines.length];
						if (colofEndV == 1 && setOfLines[k] != node) {
							checkInEdgesZ = 1;
						}
					}
					if (checkInEdgesZ == 0) {
						outCheck += 1;
					}
				}
			}
			
			for (var j = 0; j < setOfLines.length; j++) {
				if (tableOfValues[rowOfNode + j] == 1) {
					//remove edge
					tableOfValues[rowOfNode + j] = 0;
					
					//make sure no other nodes go into same vertex
					var checkInEdges = 0;
					for (var k = 0; k < setOfLines.length; k++) {
						var colofEndV = tableOfValues[j + k*setOfLines.length];
						if (colofEndV == 1) {
							checkInEdges = 1;
						}
					}
					//push in one value that will be sorted
					if (checkInEdges == 0) {
						outVals.push(setOfLines[j]);
						outCheck -= 1;
					}
					
					if (checkInEdges == 0 && outCheck == 0) {
						outVals.sort(function(a,b) {
							return (a > b);
						});	
						while (outVals.length > 0) {
							var tmpLine = outVals.pop();
							tmp.push(tmpLine);
						}
					}
				}
			}
		}
		var checkEdges = 0;
		for (var i = 0; i < tableOfValues.length; i++) {
			if (tableOfValues[i] == 1) {
				checkEdges = 1;
			}
			
		}
		
		if (checkEdges == 1) {
			process.stdout.write('cycle');
		} else {
			for (var i = 0; i < finalLines.length; i++) {
				process.stdout.write(finalLines[i] + '\n');
			}
		}
		
	}
	
}); 
