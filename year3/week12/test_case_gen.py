from matplotlib import pyplot as plt
import random

def get_x((x, y)):
    return x

def get_y((x, y)):
    return y

def plot_datasets(one, two, test):
    plt.xlabel("Time Spent in the Java Lab")
    plt.ylabel("Similarity to Quinn")

    nerd     = plt.scatter(map(get_x, one), map(get_y, one), color='red')
    not_nerd = plt.scatter(map(get_x, two), map(get_y, two), color='blue')
    testing  = plt.scatter(map(get_x, test), map(get_y, test), color='green')

    plt.legend((not_nerd, nerd, testing), ("Cool", "Nerd", "First Year"),
            scatterpoints = 1, loc = "upper left")

    plt.show()

def generate_linearly_separable_classes((x_min, x_max), (y_min, y_max), n):
    class_one_x = random.randint(x_min, x_max)
    class_one_y = random.randint(y_min, y_max)

    d1 = [(random.randint(x_min, class_one_x), random.randint(y_min, class_one_y))
            for i in range(n)]

    d2 = [(random.randint(class_one_x, x_max), random.randint(class_one_y, y_max))
            for i in range(n)]

    return d1, d2

def add_label(xs, label):
    return map(lambda (x, y): [x, y, label], xs)


def main():
    xs, ys = generate_linearly_separable_classes((0, 100), (0, 100), 20)

    c1_x_min = min(map(get_x, xs))
    c1_x_max = max(map(get_x, xs))
    c1_y_min = min(map(get_y, xs))
    c1_y_max = max(map(get_y, xs))
    c1_test_data = [(random.randint(c1_x_min, c1_x_max),
        random.randint(c1_y_min, c1_y_max)) for i in range(5)]

    c2_x_min = min(map(get_x, ys))
    c2_x_max = max(map(get_x, ys))
    c2_y_min = min(map(get_y, ys))
    c2_y_max = max(map(get_y, ys))
    c2_test_data = [(random.randint(c2_x_min, c2_x_max),
        random.randint(c2_y_min, c2_y_max)) for i in range(5)]

    total_test_data = c1_test_data + c2_test_data
    random.shuffle(total_test_data)

    ds = add_label(xs, 1) + add_label(ys, -1)
    random.shuffle(ds)

    print len(ds)
    for e in ds:
        print " ".join(map(str, e))

    print len(total_test_data)
    for e in total_test_data:
        print " ".join(map(str, e))

    plot_datasets(xs, ys, total_test_data)

main()
