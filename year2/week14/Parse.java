import java.util.*;

public class Parse {
    public static Optional<Integer> integer(String s) {
        try {
            int x = Integer.parseInt(s);
            return Optional.of(x);
        }
        catch (Exception e) {
            return Optional.empty();
        }
    }
}
