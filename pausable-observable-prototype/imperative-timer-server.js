const Rx = require(`rxjs`);
const { multicast, } = require(`rxjs/operators`);
const WebSocket = require(`ws`);

const server = new WebSocket.Server({ port: 3001, });

const createTicks = () => Rx.timer(0, 1000).pipe(multicast(new Rx.Subject()));

let ticks = null;
let subscription = null;
let sessionStarts = 0;
let sessionEnds = 0;

server.on(`connection`, (socket) => {
  sessionStarts = sessionStarts + 1;
  console.log(`session starts ${sessionStarts}`);

  if (sessionStarts - sessionEnds === 1) {
    console.log(`starting timer`);
    ticks = createTicks();
    subscription = ticks.connect();
  }

  ticks.subscribe({
    next: (tick) => {
      console.log(`tick: ${tick}`);
    }
  });

  socket.on(`close`, () => {
    sessionEnds = sessionEnds + 1;
    console.log(`session ends: ${sessionEnds}`);

    if (sessionStarts - sessionEnds < 1) {
      console.log(`stopping timer`)
      subscription.unsubscribe();
      timer = null;
      subscription = null;
    }
  });
});
