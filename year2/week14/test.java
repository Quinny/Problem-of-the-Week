import java.util.*;
import java.net.*;
import java.io.*;

public class test {
    static BufferedWriter out;
    static BufferedReader in;
    public static void main(String args[]) throws Exception {
        Socket s = new Socket("api.quinnftw.com", 9000);
        out = new BufferedWriter(new OutputStreamWriter(s.getOutputStream()));
        in = new BufferedReader(new InputStreamReader(s.getInputStream()));
    
        int hi = 1000;
        int low = 1;

        while (true) {
            int mid = (hi + low) / 2;
            guess(mid);
            String ans = check();
            if (ans.equals("low"))
                low = mid + 1;
            else if (ans.equals("high"))
                hi = mid;
            else {
                System.out.println(ans);
                break;
            }
        }
        s.close();
    }

    public static void guess(int n) throws Exception {
        out.write(n + "\n");
        out.flush();
    }

    public static String check() throws Exception {
        return in.readLine();
    }
}
