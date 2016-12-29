#include <stdio.h>

// A _very_ simple perceptron binary classifier implemented in C.
//
// The goal of this perceptron is to "learn" a linear function which separates
// the training data provided. Note that not all datasets are linearly
// separable so this may not always be possible, but for the sake of simplicity
// this code will assume the data given is linearly separable.
//
// This perceptron classifies data by creating a linear peicewise function of
// the form:
//
//     1  if (x * x_weight) + (y * y_weight) + bias > 0,
//    -1  otherwise
//
// The weights can be thought of as the level of "importance" of the
// corresponding variable, and the bias as some threshold which must be exceeded
// to be considered a "1".
//
// The weights and bias originally begin as arbitrary values (generally random
// between [-1, 1], but 0 in this example) and are slowly tweaked using the
// perceptron delta formula until the line produced separates the data.
//
// It may help to view this gif representing the process:
// https://static1.squarespace.com/static/51d342a0e4b0290bcc56387d/t/51df1a33e4b063b4f56781fd/1373575738428/two_class.gif
//
// The perceptron delta forumla is as follows:
// new_weight = old_weight + (input * error * learning_rate)
//
// where,
//    old weight is the current variables weight,
//
//    input is the value of the variable provided,
//
//    error is the difference between the expected output and the actual output
//    of the perceptron,
//
//    and learning rate is some constant which dictates how much to tweak the
//    weights.  It is important that this learning rate is carefully selected as
//    setting it too high will result in large variance in the line, and too low
//    will barely move the line at all.

// The struct to represent the simple perceptron.
typedef struct {
  double x_weight;
  double y_weight;
  double bias;
  double learning_rate;
} SimplePerceptron;

// Initialize the values in the perceptron.  0 is used for the weights and bias
// here, but generally random values in [-1, 1] are used
void init_sp(SimplePerceptron* self, double lr) {
  self->x_weight = 0;
  self->y_weight = 0;
  self->bias = 0;
  self->learning_rate = lr;
}

// Apply the peicewise perceptron function to the input variables and return the
// output.
int apply_sp(const SimplePerceptron* self, int x, int y) {
  double v = (self->x_weight * x) + (self->y_weight * y) + self->bias;
  return v > 0 ? 1 : -1;
}

// Given the input variables and the true output, adjust the weights and the
// bias according to the perceptron delta formula.
int learn_sp(SimplePerceptron* self, int x, int y, int true_output) {
  int predicted_output = apply_sp(self, x, y);
  int error = true_output - predicted_output;

  self->x_weight = self->x_weight + (x * error * self->learning_rate);
  self->y_weight = self->y_weight + (y * error * self->learning_rate);
  // Bias can be considered a weight with a constant input value of 1.
  self->bias = self->bias + (1 * error * self->learning_rate);

  return error;
}

// Train the perceptron on the given dataset until there is no error, i.e. the
// function generated perfectly separates the data.
void train_until_convergence_sp(SimplePerceptron* self, const int* x,
                                const int* y, const int* output,
                                const int n_examples) {
  while (1) {
    int total_error = 0;
    for (int i = 0; i < n_examples; ++i) {
      total_error += learn_sp(self, x[i], y[i], output[i]);
    }

    if (total_error == 0) break;
  }
}

int main() {
  // Train the perceptron to learn the bitwise OR function, without explicitly
  // providing the rules.
  int or_X[] = {1, 1, -1, -1};
  int or_Y[] = {1, -1, 1, -1};
  int or_A[] = {1, 1, 1, -1};  // 1 | 1 = 1, -1 | 1 = 1, -1 | -1 = -1.

  SimplePerceptron perceptron;
  init_sp(&perceptron, 0.01);
  train_until_convergence_sp(&perceptron, or_X, or_Y, or_A, 4);

  for (int i = 0; i < 4; ++i) {
    printf("%2d | %2d = %2d\n", or_X[i], or_Y[i],
           apply_sp(&perceptron, or_X[i], or_Y[i]));
  }
}
