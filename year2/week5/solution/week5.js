var http = require("http");

function getJSON(url, callback) {
    http.get(url, function(res){
        var body = '';

        res.on('data', function(chunk){
            body += chunk;
        });

        res.on('end', function(){
            callback(JSON.parse(body).data);
        });
    });
}

function userCompare(a, b) {
    return b.solved - a.solved;
}

function printChar(n, c) {
    for (var i = 0; i < n; ++i)
        process.stdout.write(c);
}

function drawGraph(users) {
    sorted = users.concat().sort(userCompare);
    users = sorted.slice(0, 10);
    var level = sorted[0].solved;
    while (level > 0) {
        for (var i in users) {
            if (users[i].solved >= level)
                printChar(sorted[i].student_id.length, '.');
            else
                printChar(sorted[i].student_id.length, ' ');
            process.stdout.write(" ");
        }
        console.log();
        level--;
    }
    for (var i in users)
        process.stdout.write(users[i].student_id + " ");
    console.log()
}

getJSON("http://potw.quinnftw.com/api/solvers", drawGraph);
