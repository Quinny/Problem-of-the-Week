import sqlite3
from flask import Flask, request

app        = Flask(__name__)
login_page = open("login.html").read()

def db_connection():
    conn = sqlite3.connect('db.db')
    cur  = conn.cursor()
    return conn, cur

def close_db(conn, curr):
    curr.close()
    conn.close()

@app.route("/login")
def login():
    return login_page

@app.route("/auth", methods = ["POST"])
def auth():
    conn, curr = db_connection()
    try:
        username = request.form['username']
        password = request.form['password']
    except:
        username = ""
        password = ""
    query = "SELECT * from users WHERE username='{}' AND password='{}'"\
        .format(username, password)
    curr.execute(query)
    ret = str(curr.fetchall())
    close_db(conn, curr)
    return ret

if __name__ == "__main__":
    app.run(host="0.0.0.0")
