from Queue     import Queue
from threading import Thread


'''

A simple worker pool function which allows one to submit
work to be delegated across a fixed number of threads

'''
class WorkerPool:
    # Worker thread function which pulls of the work and does it
    def work(self):
        while True:
            w = self.work_queue.get()
            w()
            if self.done:
                return None

    # Spawn n_worker worker threads
    def __init__(self, n_workers):
        self.work_queue = Queue()
        self.workers = [Thread(target=self.work) for _ in range(n_workers)]
        map(lambda x: x.start(), self.workers)
        self.done = False

    # submit a job to the queue
    def submit(self, w):
        if not self.done:
            self.work_queue.put(w)

    # join all currently running worker threads
    def join(self):
        map(lambda x: x.join(), self.workers)

    def run_async(self, f):
        def wrap(*args):
            self.submit(lambda: f(*args))
        return wrap


