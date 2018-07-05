import xs from 'xstream';
import { run } from '@cycle/run';
import { makeDOMDriver, h1 } from '@cycle/dom';

// expects a collection of source streams as input
// source streams are returned by drivers
// should return a collection of sink streams
function main(sources) {
  const sinks = {
    // DOM is a stream of virtual DOM
    DOM: xs.periodic(1000).map(n => h1(`${n} seconds elapsed`)),
  };

  return sinks
}

const drivers = {
  DOM: makeDOMDriver('#root'),
};

// dispose cleans up resources
const dispose = run(main, drivers);
