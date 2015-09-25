import java.util.*;

class TrieNode {
    boolean wordEnd = false;
    HashMap<Character, TrieNode> children = new HashMap<Character, TrieNode>();
};

class Trie {
    TrieNode root = new TrieNode();

    void insert(String s) {
        TrieNode cur = root;
        for (char c: s.toCharArray()) {
            if (!cur.children.containsKey(c))
                cur.children.put(c, new TrieNode());
            cur = cur.children.get(c);
        }
        cur.wordEnd = true;
    }

    ArrayList<String> prefixMatches(String s) {
        TrieNode cur = root;
        ArrayList<String> ret = new ArrayList<String>();
        String build = "";

        for (char c: s.toCharArray()) {
            if (cur.wordEnd)
                ret.add(build);
            if (!cur.children.containsKey(c))
                break;
            build += c;
            cur = cur.children.get(c);
        }
        if (s.equals(build) && cur.wordEnd)
            ret.add(s);
        return ret;
    }
};


public class Week3 {

    public static boolean check(Trie t, String s) {
        if (s.length() == 0)
            return true;
        ArrayList<String> p = t.prefixMatches(s);
        for (String i: p) {
            if (check(t, s.substring(i.length())))
                return true;
        }
        return false;
    }

    public static void main(String[] args) {
        Trie t = new Trie();

        Scanner stdin = new Scanner(System.in);
        int m = stdin.nextInt();
        stdin.nextLine();
        for (int i = 0; i < m; ++i) {
            String word = stdin.nextLine();
            t.insert(word);
        }
        int n = stdin.nextInt();
        stdin.nextLine();
        for (int i = 0; i < n; ++i) {
            String sentence = stdin.nextLine();
            boolean valid = check(t, sentence);
            if (valid)
                System.out.println("1");
            else
                System.out.println("0");
        }
    }
}
