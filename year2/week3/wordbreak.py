def can_make_sentence_trie(sentence, trie):
    if not sentence:
        return True

    return any(
            can_make_sentence_trie(sentence[len(p):], dictionary)
            for p in trie.prefixes_of(sentence))

class Trie:
    def insert(self, seq):
        current = self.root
        for i in seq:
            if i not in current:
                current[i] = {}
            current = current[i]
        current["__end__"] = None

    def __init__(self, seq):
        self.root = {}
        for i in seq: self.insert(i)

    def prefixes_of(self, seq):
        current = self.root
        prefix = ""
        for i in seq:
            if i not in current:
                break
            prefix += i
            current = current[i]
            if "__end__" in current:
                yield prefix

n = int(input())
dictionary = Trie(input().strip() for i in range(n))
m = int(input())

for _ in range(m):
    print(int(can_make_sentence_trie(input().strip(), dictionary)))
