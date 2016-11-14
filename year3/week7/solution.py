import socket

request = "GET /problem/s3cret HTTP/1.1\r\n" +\
          "Host: www.potw.quinnftw.com\r\n\r\n"

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(("potw.quinnftw.com", 80))
s.send(request)
print s.recv(10000)
