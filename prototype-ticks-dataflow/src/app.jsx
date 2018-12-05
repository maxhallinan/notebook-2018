import Rx from 'rxjs/Rx'

const TimeStamped = (props) => {
  const date = new Date(props.timestamp);
  const hours = date.getHours();
  const minutes = date.getMinutes();
  const seconds = date.getSeconds();
  const milliseconds = date.getMilliseconds();
  const timestamp = `${hours}:${minutes}:${seconds}:${milliseconds}`;

  return (
    <li>
      <span>[{timestamp}] </span>
      <code>
        {props.content}
      </code>
    </li>
  );
}

const toTimeStamped = ([ timestamp, content ]) => (
  <TimeStamped
    content={content}
    timestamp={timestamp}
  />
);

const toMaxFiveList = (xs) => {
  const target = []; 

  for (const index in xs) {
    if (target.length > 5) {
      break;
    }
    target.push(xs[index]);
  }

  return target;
};

const toView = (state) => (
  <div>
    <button className="connect-btn">
      Connect
    </button>
    <button 
      disabled={state.connectionCount <= state.disconnectionCount}
      className="disconnect-btn">
        Disconnect
    </button>
    <ol>
      <li>
        connection$:
        <ul>
          {toMaxFiveList(state.connectionCollection).map(toTimeStamped)}
          {state.connectionCollection.length > 5 && <span>... {state.connectionCollection.length - 5}</span>}
        </ul>
      </li>
      <li>connectionCount$: {state.connectionCount}</li>
      <li>
        socket$:
        <ul>
          {toMaxFiveList(state.socketCollection).map(toTimeStamped)}
          {state.socketCollection.length > 5 && <span>... {state.socketCollection.length - 5}</span>}
        </ul>
      </li>
      <li>
        close$:
        <ul>
          {toMaxFiveList(state.closeCollection).map(toTimeStamped)}
          {state.closeCollection.length > 5 && <span>... {state.closeCollection.length - 5}</span>}
        </ul>
      </li>
      <li>closeCount$: <code>{state.closeCount}</code></li>
      <li>currentCount$: <code>{state.currentCount}</code></li>
      <li>pause$: <code>{JSON.stringify(state.pause)}</code></li>
      <li>tick$: <code>{state.tick}</code></li>
    </ol>
  </div>
);

const toState = ([ 
  connectionCount, 
  disconnectionCount, 
  connectionEvent,
  connectionCollection,
  socket,
  socketCollection,
  closeEvent,
  closeCollection,
  closeCount,
  currentCount,
  pause,
  tick,
]) => ({
  connectionCount,
  disconnectionCount,
  connectionCollection,
  connectionEvent,
  socket,
  socketCollection,
  closeEvent,
  closeCollection,
  closeCount,
  currentCount,
  pause,
  tick,
});

const toFakeConnectionEvent = () => 
  `[ WebSocket, http.IncomingMessage ]`;

const toFakeWebSocket = () => `WebSocket`;

const toFakeCloseEvent = () => `[ code, reason ]`;

const toFirstOutList = (xs, x) => [ [ Date.now(), x ], ...xs ];

export function App (sources) {
  const connectionClick$ = 
    sources.DOM.select('.connect-btn').events('click');
  const disconnectClick$ =
    sources.DOM.select('.disconnect-btn').events('click');
  const add = (x1) => (x2) => x1 + x2;
  const addOne = add(1);
  const connectionCount$ = 
    Rx.Observable.of(0)
      .concat(connectionClick$.scan(addOne, 0));
  const disconnectionCount$ = 
    Rx.Observable.of(0)
      .concat(disconnectClick$.scan(addOne, 0));
  
  /* start fake data */
  const connection$ = connectionClick$.map(toFakeConnectionEvent);
  const connectionCollection$ = 
    connection$.scan(toFirstOutList, []);
  const socket$ = connectionClick$.map(toFakeWebSocket);
  const socketCollection$ =
    socket$.scan(toFirstOutList, []);
  const close$ = disconnectClick$.map(toFakeCloseEvent);
  const closeCollection$ =
      close$.scan(toFirstOutList, []);
  const closeCount$ = 
    Rx.Observable.of(0)
      .concat(disconnectClick$.scan(addOne, 0));
  const subtract = (x1, x2) => x1 - x2;
  const currentCount$ = Rx.Observable.combineLatest(
    [ connectionCount$, closeCount$ ],
    subtract
  );
  const isPaused = (currentCount) => currentCount < 1;
  const pause$ = currentCount$.map(isPaused);
  const tick$ = pause$.switchMap(
    (isPaused) => isPaused ? Rx.Observable.never() : Rx.Observable.timer(0, 1000),
  );
  /* end fake data */

  const stateSource$ = Rx.Observable.combineLatest(
    connectionCount$,
    disconnectionCount$,
    connection$.startWith(''),
    connectionCollection$.startWith([]),
    socket$.startWith(''),
    socketCollection$.startWith([]),
    close$.startWith(''),
    closeCollection$.startWith([]),
    closeCount$.startWith(''),
    currentCount$.startWith(''),
    pause$.startWith(''),
    tick$.startWith(''),
  );
  const state$ = stateSource$.map(toState);
  const vtree$ = state$.map(toView);
  const sinks = {
    DOM: vtree$
  }
  return sinks
}
