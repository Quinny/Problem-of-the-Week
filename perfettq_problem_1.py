def partitions(s, k): # generate all the different k paritions of s
  if not k:
    yield [s]
    return
  for i in range(len(s) + 1):
    for tail in partitions(s[i:], k - 1):
      yield [s[:i]] + tail

def occurances(string, char): # get a list of all occurances of a given character
	l = []
	for i in range (len(string)):
		if string[i] == char:
			l.append(i)
	return l

def matches(parts,pattern):
	equals = [] # 2d list of all indexes which should be equal
	for c in pattern:
		tmp = occurances(pattern,c)
		if tmp not in equals:
			equals.append(tmp)
	for p in parts:
		found = True
		distinct = []
		for e in equals:
			m = check(p, e)
			if m[0] and m[0] not in distinct:
				distinct.append(m[0])
			else:
				found = False
				break
			found = found and m[1]
		if found:
			return True
	return False

def check(part, indexes): # checks if a given partition indexes are all equal
	if len(indexes) == 1:
		return [part[indexes[0]],True]
	first = part[indexes[0]]
	for i in range (1,len(indexes)):
		if part[indexes[i]] != first:
			return ["",False]
	return [first,True]

pattern = input()
string = input()
parts = filter(lambda x: '' not in x, list(partitions(string,len(pattern) - 1))) # filter out all partitions that contain the empty string
if(matches(parts, pattern)):
	print "1"
else:
	print "0"