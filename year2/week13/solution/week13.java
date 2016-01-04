import java.util.*;
import java.util.function.*;
import java.util.concurrent.*;

public class week13 {
    public static void main(String args[]) {
        ActionBuffer ab = new ActionBuffer(5);
        for (int i = 0; i < 10; ++i) {
            Image im = new Image(i + "");
            im.load()
                .thenAccept((Image x) -> ab.defer(() -> System.out.println(x.path)));
        }
    }
}

interface Action extends Function<Void, Void> {
    default Void apply(Void v) {
        apply();
        return null;
    }
    void apply();
}

class ActionBuffer {
    ArrayDeque<Action> defered = new ArrayDeque<Action>();
    int waiting;

    public ActionBuffer(int n) {
        waiting = n;
    }

    public synchronized void defer(Action a) {
        defered.add(a);
        if (defered.size() == waiting)
            flush();
    }

    public synchronized void flush() {
        while (!defered.isEmpty())
            defered.remove().apply();
    }
}

class Image {
    public String path;

    public Image(String path) {
        this.path = path;
    }

    CompletableFuture<Image> load() {
        return CompletableFuture.supplyAsync(() -> {
            try {
                Thread.sleep(new Random().nextInt(4000));
            } catch (Exception e) {}
            return this;
        });
    }
}
