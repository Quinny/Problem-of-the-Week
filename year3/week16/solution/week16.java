import java.util.*;

class week16 {
  public static ArrayList<HashMap<String, Integer>> initializeDistrictCounts(int n_districts) {
    ArrayList<HashMap<String, Integer>> districtCounts = new ArrayList<>();
    for (int i = 0; i < n_districts; i++) {
      districtCounts.add(new HashMap<String, Integer>());
    }
    return districtCounts;
  }

  public static void main(String[] args) {
    Scanner kb = new Scanner(System.in);
    int nDistricts = kb.nextInt();
    int nVotes = kb.nextInt();
    ArrayList<HashMap<String, Integer>> districtCounts = initializeDistrictCounts(nDistricts);

    for (int i = 0; i < nVotes; ++i) {
      String candidate = kb.next();
      int district = kb.nextInt();
      districtCounts.get(district).merge(candidate, 1, (x, y) -> x + y);
    }

    ArrayList<String> winners = new ArrayList<>();
    for (HashMap<String, Integer> counts : districtCounts) {
      winners.add(
          counts.entrySet().stream().max((e1, e2) -> e1.getValue() - e2.getValue()).get().getKey());
    }
  }
}
