from faker import Factory
fake = Factory.create()

def upper_first(s):
    return s[0].upper() + s[1:]

def eventify(s):
    n = map(upper_first, s.split())
    return ''.join(n)

for i in range(500):
    print eventify(fake.bs())
