import java.util.*;

/*
 * POTW exercise - http://potw.quinnftw.com/
 * Coolness Perceptron Classifier - very simple single-layered perceptron training for linearly
 * separable data
 * Used to learn the weights applied to two parameters for determination of coolness
 * Parameters: 1 - similarity to Quinn from quinnftw.com
 *         2 - time spent in Java Lab in UWindsor
 */

public class CoolNerdPerceptronClassifier {
  // parseInput()
  private int[][][] trainingData;
  private int[][][] appliedData;
  private int trainingDataSize;
  private int appliedDataSize;

  // learnFromData()
  private double[] weights;
  private double bias;
  private double learningRate;
  private int threshold;

  public void parseInput() {
    Scanner sc = new Scanner(System.in);

    /*
     * [i][0][0] = row = single student data = {{,}, {}}
     * [0][i][0] = sub-cell per row = {{,}, {output}}
     * [0][0][i] = element in sub-cell of row = parameter = {{similarity, java lab}, {}}
     *
     */

    trainingDataSize = sc.nextInt();
    trainingData = new int[trainingDataSize][2][2];
    int parsedLines = 0;

    while (parsedLines < trainingDataSize) {
      trainingData[parsedLines][0][0] = sc.nextInt();
      trainingData[parsedLines][0][1] = sc.nextInt();
      trainingData[parsedLines][1][0] = sc.nextInt();
      parsedLines++;
    }

    appliedDataSize = sc.nextInt();
    appliedData = new int[appliedDataSize][2][2];
    parsedLines = 0;

    while (parsedLines < appliedDataSize) {
      appliedData[parsedLines][0][0] = sc.nextInt();
      appliedData[parsedLines][0][1] = sc.nextInt();
      appliedData[parsedLines][1][0] = 0;
      parsedLines++;
    }

    sc.close();
  }

  public void learnFromData() {
    // initialize weights and bias as random numbers between -1 and 1
    weights = new double[2];
    weights[0] = Math.random() * 2 - 1;
    weights[1] = Math.random() * 2 - 1;
    bias = Math.random() * 2 - 1;

    threshold = 0; // threshold for coolness
    learningRate = 0.1;

    double weightedSum;
    double output;
    double error;
    // start learning
    while (true) {
      int totalError = 0;

      for (int i = 0; i < trainingDataSize; i++) {
        weightedSum = 0;

        // calculate the weightedSum for the current row parameters
        for (int ii = 0; ii < 2; ii++) {
          weightedSum += trainingData[i][0][ii] * weights[ii];
        }

        // check whether the output passes the threshold for coolness
        output = (weightedSum + bias) > threshold ? 1 : -1;

        // error = expected output - true output
        error = trainingData[i][1][0] - output;
        totalError += error;

        // determine new weights
        for (int ii = 0; ii < 2; ii++) {
          weights[ii] = weights[ii] + (trainingData[i][0][ii] * error * learningRate);
        }

        // determine new bias
        bias = bias + (1 * error * learningRate);
      }

      if (totalError == 0) {
        break;
      }
    }
  }

  public void determineCoolness() {
    double output = 0;
    int result;

    for (int i = 0; i < appliedDataSize; i++) {
      for (int ii = 0; ii < 2; ii++) {
        output += (appliedData[i][0][ii] * weights[ii]);
      }
      output += bias;
      result = output > 0 ? 1 : -1;
      appliedData[i][1][0] = result;
      output = 0;
    }
  }

  public String toString() {
    String output = "";

    for (int i = 0; i < appliedDataSize; i++) {
      if (appliedData[i][1][0] > threshold) {
        output += "Cool\n";
      } else {
        output += "Nerd\n";
      }
    }

    return output;
  }

  public static void main(String[] args) {
    CoolNerdPerceptronClassifier c1 = new CoolNerdPerceptronClassifier();

    c1.parseInput();
    c1.learnFromData();
    c1.determineCoolness();
    System.out.println(c1.toString());
  }
}
