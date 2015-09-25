def is_valid(sentence, dictionary):
    if not sentence:
        return True

    for i in range(len(sentence)):
        if sentence[:i + 1] in dictionary:
            check = is_valid(sentence[i + 1:], dictionary)
            if check:
                return True
    return False

def main():
    n = int(raw_input().strip())
    dictionary = { raw_input().strip() for i in range(n) }

    m = int(raw_input().strip())
    print "\n".join(map(str, [int(is_valid(raw_input().strip(), dictionary)) for i in range(m)]))

if __name__ == "__main__":
    main()
