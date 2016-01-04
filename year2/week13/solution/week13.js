function actionBuffer(n) {
    var self    = this;
    var waiting = n;
    const reset = n;
    var defered = [];

    this.defer = function(f) {
        --waiting;
        defered.push(f);
        if (waiting == 0) {
            self.flush();
            waiting = reset;
        }
    }

    this.flush = function() {
        while (defered.length != 0) {
            defered.shift()();
        }
    }
}

function bind(f, args) {
    return function() {
        f.apply(null, args);
    };
}

q = new actionBuffer(5);
for (var i = 0; i < 5; ++i) {
    console.log("queueing ", i);
    q.defer(bind(console.log, [i]));
}
