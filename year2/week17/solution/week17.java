import java.util.*;
import java.util.function.*;

/*
 * Java 8 style huffman encoding.  Notice that there is not a single
 * null check in the entire program
 */

public class week17 {
    static class HuffmanNode<T> {
        T data;
        int frequency;
        // Use optional to avoid having to null check
        Optional<HuffmanNode> left, right;

        HuffmanNode(T d, int f) {
            data = d;
            frequency = f;
            left = Optional.empty();
            right = Optional.empty();
        }

        HuffmanNode(HuffmanNode l, HuffmanNode r) {
            frequency = l.frequency + r.frequency;
            left = Optional.of(l);
            right = Optional.of(r);
        }

        // Traverse and call f on each leaf node, passing the data
        // and the code
        void traverse(BiConsumer<T, String> f) {
            traverse(f, "");
        }

        void traverse(BiConsumer<T, String> f, String code) {
            if (!left.isPresent() && !right.isPresent())
                f.accept(data, code);
            // if the node is present, traverse it
            left.ifPresent(node -> node.traverse(f, code + "0"));
            right.ifPresent(node -> node.traverse(f, code + "1"));
        }
    }

    // Build a huffman tree given a histogram
    public static <T> HuffmanNode<T> buildTree(HashMap<T, Integer> histogram) {
        PriorityQueue<HuffmanNode<T>> pq =
            new PriorityQueue<>((x, y) -> x.frequency - y.frequency);

        // Insert all values from the histogram into the priority queue
        histogram.forEach((k, v) -> pq.add(new HuffmanNode<T>(k, v)));
        while (pq.size() > 1) {
            HuffmanNode<T> n1 = pq.poll();
            HuffmanNode<T> n2 = pq.poll();

            HuffmanNode<T> parent = new HuffmanNode<>(n1 , n2);
            pq.add(parent);
        }
        return pq.peek();
    }

    public static void main(String args[]) {
        HashMap<String, Integer> histogram = new HashMap<>();
        Scanner stdin = new Scanner(System.in);

        while(stdin.hasNext())
            histogram.merge(stdin.next(), 1, (x, y) -> x + y);
        HuffmanNode<String> root = buildTree(histogram);
        root.traverse((data, code) -> System.out.println(data + " " + code));
    }
}
