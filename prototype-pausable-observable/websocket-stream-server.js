const Rx = require(`rxjs`);
const { 
  combineLatest, 
  flatMap, 
  map,
  merge, 
  scan, 
} = require(`rxjs/operators`);
const WebSocket = require(`ws`);

const server = new WebSocket.Server({ port: 8080, });

const head = (xs) => xs[0];
const add = (x) => (y) => x + y;
const increment = add(1);

// a stream of sockets
const socket$ = Rx.fromEvent(server, `connection`).pipe(map(head));

// the number of opened connections since the server was started
const connectionCount$ = socket$.pipe(scan(increment, 0));

connectionCount$.subscribe({
  next: (count) => {
    console.log(`connection count ${count}`);
  },
});

// a stream of socket close events
const close$ = socket$.pipe(
  flatMap((socket) => Rx.fromEvent(socket, `close`)),
);

const zero$ = Rx.of(0);

// the number of closed connections since the server was started
const closeCount$ = Rx.merge(
  zero$,
  close$.pipe(scan(increment, 0)) 
);

closeCount$.subscribe({
  next: (count) => {
    console.log(`close count ${count}`);
  },
});

const subtract = (x, y) => x - y;

// the current number of open sockets
const activeCount$ = Rx.combineLatest(
  [ connectionCount$, closeCount$, ], 
  subtract
);

activeCount$.subscribe({
  next: (count) => {
    console.log(`active count: ${count}`);
  },
});

// stop polling when there are no open connections
const isPaused = (count) => 1 > count;

const isPaused$ = activeCount$.pipe(map(isPaused));

isPaused$.subscribe({
  next: (isPaused) => {
    console.log(`is paused ${isPaused}`);
  },
});
