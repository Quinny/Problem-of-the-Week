import java.util.*;
import java.net.*;
import java.io.*;

public class GuessGameClient implements Runnable {
    Socket socket;
    BufferedReader in;
    BufferedWriter out;
    int secret;
    int guesses = 0;

    public GuessGameClient(Socket s) {
        socket = s;
        try {
            in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
        } catch (Exception e) { /* nah */ }
        this.secret = 1 + (int)(Math.random() * 1000);
    }
    
    public void run() {
        try { handle(); } catch (Exception e){}
    }

    public void handle() throws Exception {
        String req;
        while ((req = in.readLine()) != null) {
            Optional<Integer> check = Parse.integer(req);
            if (check.isPresent()) {
                int v = check.get();
                ++guesses;
                if (guesses == 11) {
                    say("too many");
                    break;
                }
                if (v == secret) {
                    say("good " + guesses);
                    break;
                }
                if (v < secret)
                    say("low");
                if (v > secret)
                    say("high");
            }
            else {
                break;
            }
        }
        socket.close();
    }

    private void say(String s) throws Exception {
        out.write(s + "\n");
        out.flush();
    }
}
