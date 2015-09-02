def readStdin():
	toReturn = []
	try:
		while True:
			a = int(raw_input().strip())
			toReturn = [a]+toReturn
	finally: return toReturn

def getResult(a):
	b = 0
	for i in a:
		b += i
	return b / len(a)
	
def main():
	a = readStdin()
	print str(getResult(a))
main()
