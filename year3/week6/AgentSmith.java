

import java.util.*;

/*
 * Ali Ayoub [24.01.2017]
 * POTW 2016 week 6 created by Quinn Perfetto
 * http://potw.quinnftw.com/problem/2016/6/
 *
 * Agent Smith
 * Determine which program is Agent Smith
 */

public class AgentSmith {
  public static void main(String[] args) {
    Scanner in = new Scanner(System.in);
    int n = in.nextInt();
    in.nextLine();

    HashSet<Integer> programs = new HashSet<Integer>();
    while (in.hasNextInt()) {
      int next = in.nextInt();
      if (programs.contains(next)) {
        System.out.println(next);
        return;
      }
      programs.add(next);
    }

    in.close();
  }
}
