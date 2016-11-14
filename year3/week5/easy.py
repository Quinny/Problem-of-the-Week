def is_banned(rules, ip):
    return any(ip == r or ip.startswith(r + ".") for r in rules)

def main():
    n_rules = int(raw_input())
    rules = [raw_input() for i in range(n_rules)]

    n_ips = int(raw_input())
    for i in range(n_ips):
        print "valid" if not is_banned(rules, raw_input()) else "banned"

main()
