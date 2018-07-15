import Rx from 'rxjs/Rx'

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
  </div>
);


const toState = ([ 
  connectionCount, 
  disconnectionCount, 
]) => ({
  connectionCount,
  disconnectionCount,
});

export function App (sources) {
  const connectionClicks$ = 
    sources.DOM.select('.connect-btn').events('click');
  const disconnectClick$ =
    sources.DOM.select('.disconnect-btn').events('click');
  const add = (x1) => (x2) => x1 + x2;
  const addOne = add(1);
  const connectionCount$ = 
    Rx.Observable.of(0)
      .concat(connectionClicks$.scan(addOne, 0));
  const disconnectionCount$ = 
    Rx.Observable.of(0)
      .concat(disconnectClick$.scan(addOne, 0));
  const stateSource$ = Rx.Observable.combineLatest(
    connectionCount$,
    disconnectionCount$,
  );
  const state$ = stateSource$.map(toState);
  const vtree$ = state$.map(toView);
  const sinks = {
    DOM: vtree$
  }
  return sinks
}
