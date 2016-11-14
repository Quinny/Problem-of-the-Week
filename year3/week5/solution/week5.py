class TrieNode:
    def __init__(self):
        self.children = {}
        self.end_of_word = False

class Trie:
    def __init__(self):
        self.root = TrieNode()

    def insert(self, seq):
        def r(node, i):
            if i not in node.children:
                node.children[i] = TrieNode()
            return node.children[i]
        leaf = reduce(r, seq, self.root)
        leaf.end_of_word = True

    def prefix_any(self, seq):
        current = self.root
        for i in seq:
            if current.end_of_word:
                return True
            if i not in current.children:
                return False
            current = current.children[i]
        return current.end_of_word

def main():
    n = int(raw_input())
    prefixes = (map(int, raw_input().split(".")) for _ in xrange(n))
    t = Trie()
    map(lambda prefix: t.insert(prefix), prefixes)

    n = int(raw_input())
    ips = (map(int, raw_input().split(".")) for _ in xrange(n))
    verdicts = (t.prefix_any(ip) for ip in ips)
    for v in verdicts:
        if v:
            print "banned"
        else:
            print "valid"

main()
