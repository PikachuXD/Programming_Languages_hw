/* C: Reverse-sort the lines from standard input */
#include <stdio.h>      /* bring in some standard library stuff */
#include <stdlib.h>
#include <string.h>

struct string_list_cell {
  char * head;
  struct string_list_cell * tail;
} ;

int reverse_comparison(const void *a, const void *b);
int comparison(const void *a, const void *b);
int find(char **arr, int n, char* target);

int main() {
  struct string_list_cell * lines = NULL;

  int line_count = 0; /* we'll track the number of lines we read */

  while (1) {
    char line_buffer[80];

    fgets(line_buffer, sizeof(line_buffer), stdin);

    if (feof(stdin)) break;

    {
      struct string_list_cell * new_cell =
        malloc(sizeof(*new_cell));

      new_cell -> head = strdup(line_buffer);

      new_cell -> tail = lines;
      lines = new_cell;

      line_count++;
    }
  }
    char * * array;
    char * * setOfVals;
    char * * noIncomingEdges;
    char * * interNoIncEdges;
    char * * finalLines;
    int * tableOfValues;

    int totalLineCAccum, i, j, k, noIncEdgeSize, tableSize, SoVAccum = 0;
	array = malloc(line_count * sizeof(*array));

    while (lines != NULL) {
      char* tmp = lines->head;
	  if (find(array, totalLineCAccum, tmp) == -1) {
		k++;
	  }
	  array[totalLineCAccum] = tmp;

      totalLineCAccum++;
      lines = lines->tail;
    }

	interNoIncEdges = malloc(k * sizeof(*interNoIncEdges));
	setOfVals = malloc(k * sizeof(*setOfVals));
	finalLines = malloc(k * sizeof(*finalLines));

	for (i = 0; i < line_count; i++) {
		if (find(setOfVals, SoVAccum, array[i]) == -1) {
			setOfVals[SoVAccum] = array[i];
			SoVAccum++;
		}
	}
	/*
	for (i = 0; i < SoVAccum; i++) {
		printf("%s", setOfVals[i]);
	}
	*/
	tableSize = k * k;
	tableOfValues = (int*) malloc(sizeof(int) * tableSize);

	for (i = 0; i < tableSize; i++) {
		tableOfValues[i] = 0;
	}

	for (i = 0; i < line_count - 1; i+=2) {
		char* endVal = array[i+1];
		char* startVal = array[i];
		int rowIndex = find(setOfVals, SoVAccum, startVal);
		int colIndex = find(setOfVals, SoVAccum, endVal);
		tableOfValues[rowIndex*SoVAccum + colIndex] = 1;
		/*printf("%s", startVal);*/
		/*printf("%d%s%d%s%d", rowIndex, startVal, colIndex, endVal, tableOfValues[rowIndex* SoVAccum + colIndex]);*/
	}

	noIncEdgeSize ^= noIncEdgeSize;
	for (i = 0; i < SoVAccum; i++) {
		int chkIncEdges = 0;
		for (j = 0; j < SoVAccum; j++) {
			int colBuffer = j * SoVAccum;
			if (tableOfValues[colBuffer + i] == 1) {
				chkIncEdges = 1;
			}
		}
		if (chkIncEdges == 0) {
			noIncEdgeSize += 1;
		}
	}

	free(lines);
	free(array);
	if (noIncEdgeSize == 0) {
		printf("cycle");
	} else {
		noIncomingEdges = malloc(noIncEdgeSize * sizeof(*noIncomingEdges));
		int noIncEdgeAccum = 0;
		noIncEdgeAccum ^= noIncEdgeAccum;
		for (i = 0; i < SoVAccum; i++) {
			int chkIncEdges = 0;
			for (j = 0; j < SoVAccum; j++) {
				int colBuffer = j * SoVAccum;
				if (tableOfValues[colBuffer + i] == 1) {
					chkIncEdges = 1;
				}
			}

			if (chkIncEdges == 0) {
				noIncomingEdges[noIncEdgeAccum] = setOfVals[i];
				noIncEdgeAccum++;
			}
		}

		qsort(noIncomingEdges, noIncEdgeSize, sizeof(noIncomingEdges[0]), reverse_comparison);

		for (i = 0; i < noIncEdgeSize; i++) {
			interNoIncEdges[i] = noIncomingEdges[i];
		}
		/*finalLines = topSort(setOfVals, interNoIncEdges, tableOfValues, noIncEdgeSize, SoVAccum);*/

		int flAcc = 0;
		while (noIncEdgeSize > 0) {
			char* node = interNoIncEdges[noIncEdgeSize-1];
			finalLines[flAcc] = node;
			flAcc++;
			noIncEdgeSize--;

			int rowOfNode = find(setOfVals, SoVAccum, node) * SoVAccum;
			int outCheck, outSize = 0;
			char** outVals;

			outCheck ^= outCheck;
			outSize ^= outSize;
			for (i = 0; i < SoVAccum; i++) {
				if (tableOfValues[rowOfNode + i] != 0) {
					int chkInEdgesZ = 0;
					for (j = 0; j < SoVAccum; j++) {
						int colOfEndV = tableOfValues[i + j*SoVAccum];
					if (colOfEndV == 1 && strcmp(node, setOfVals[j]) != 0) {
						chkInEdgesZ = 1;
					}
				}
				if (chkInEdgesZ == 0) {
					outSize ++;
				}
			}
		}

		outVals = malloc(outCheck * sizeof(*outVals));

		for (i = 0; i < SoVAccum; i++) {
			if (tableOfValues[rowOfNode + i] == 1) {
				/*
					remove edge
				*/
				tableOfValues[rowOfNode + i] = 0;

				/*
					make sure no other nodes go into same vertex
				*/
				int chkInEdges = 0;
				for (j = 0; j < SoVAccum; j++) {
					int colOfEndV = tableOfValues[i + j*SoVAccum];
					if (colOfEndV == 1) {
						chkInEdges = 1;
					}
				}

				if (chkInEdges == 0) {
					outVals[outCheck] = setOfVals[i];
					outCheck++;
				}

				if (chkInEdges == 0 && outCheck == outSize) {
					qsort(outVals, outSize, sizeof(outVals[0]), comparison);
					while (outSize > 0) {
						char* tmpLine = outVals[outSize - 1];
						outSize--;
						noIncEdgeSize++;
						interNoIncEdges[noIncEdgeSize-1] = tmpLine;
						/*printf("%s", tmpLine);*/
					}
				}
			}
		}
		}

		int cycCheck = 0;
		for (i = 0; i < tableSize; i++) {
			if (tableOfValues[i] == 1) cycCheck = 1;
		}
		if (cycCheck == 1) {
			printf("cycle");
		} else {
			for (i = 0; i < SoVAccum; i++) {
				printf("%s\n", finalLines[i]);
			}
		}
	}


	return 0;
}

/* Now it's time to define our comparison function */
int reverse_comparison(const void *a_ptr, const void *b_ptr) {
  /* The contract for C-style standard library sorting is that your
   * comparison function takes pointers to elements rather than elements
   * themselves. So we'll just dereference everything once: */
  char * a = * (char * *)a_ptr;
  char * b = * (char * *)b_ptr;
  /* Now we want to compare a and b. */
  return strcmp(b,a);
  /* The C standard library comes with a string comparison function that
   * does just what we need. We'll reverse the order of the arguments,
   * though. */
}

int comparison(const void *a_ptr, const void *b_ptr) {
  char * a = * (char * *)a_ptr;
  char * b = * (char * *)b_ptr;
  /* Now we want to compare a and b. */
  return strcmp(a, b);
}

int find(char **arr, int n, char* target) {
	int asdf = 0;
	if (n == 0) {
		return -1;
	}
	for (asdf = 0; asdf < n; asdf++) {
		if (strcmp(arr[asdf], target) == 0) return asdf;
	}
	return -1;
}
