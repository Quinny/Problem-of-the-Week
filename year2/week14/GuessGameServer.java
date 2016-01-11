import java.util.*;
import java.net.*;

public class GuessGameServer {
    public GuessGameServer() {
    }

    public void serve() throws Exception {
        ServerSocket server = new ServerSocket(9000);
        while (true) {
            Socket client = server.accept();
            new Thread(new GuessGameClient(client)).start();
        }
    }

    public static void main(String args[]) throws Exception {
        GuessGameServer s = new GuessGameServer();
        s.serve();
    }
}
