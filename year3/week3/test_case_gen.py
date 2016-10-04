import random

raw_words = map(lambda x: x.strip().lower(), open("/usr/share/dict/words").readlines())
words = [random.choice(raw_words) for i in range(10)]

def generate_random_document():
    n_words = random.randint(10, 100)
    return " ".join([random.choice(words) for _ in range(n_words)])

def main():
    n_documents = random.randint(100, 1000)
    print n_documents
    for i in range(n_documents):
        print generate_random_document()

    print random.choice(words)

main()

