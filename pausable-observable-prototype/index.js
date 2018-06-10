const Rx = require('rxjs');
const { scan, switchMap, tap, } = require('rxjs/operators');

// create a timer that ticks every second
const createTimer = () => Rx.timer(0, 1000);

// create a mutable stream
// mutability is useful here to control the paused state from the outer context
const pauser = new Rx.Subject();

const pausableTimer = pauser.pipe(
  // toggle the `isPaused` state
  scan((isPaused) => !isPaused, false),
  // if the timer is paused, return an observer that never produces a value
  switchMap(isPaused => isPaused ? Rx.NEVER : createTimer())
);

// observe timer
pausableTimer.subscribe({
  next: () => {
    console.log(`tick`);
  },
});

// stop/start timer every 3 seconds
setInterval(() => {
  pauser.next();
}, 3000);
