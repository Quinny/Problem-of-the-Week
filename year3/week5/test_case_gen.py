import random

def random_ip():
    return '.'.join(str(random.randint(0, 255)) for i in range(4))

def random_ip_prefix(ip):
    end = random.randint(1, 4)
    return ".".join(ip.split(".")[0:end])

def main():
    n_banned_prefixes = random.randint(100, 1000)
    print n_banned_prefixes
    for i in range(n_banned_prefixes):
        print random_ip_prefix(random_ip())

    n_ips = random.randint(100, 100000)
    print n_ips
    for i in range(n_ips):
        print random_ip()

main()
