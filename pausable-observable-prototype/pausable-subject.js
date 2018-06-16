const Rx = require(`rxjs`);
const {
  multicast,
  scan,
  switchMap,
} = require(`rxjs/operators`);

const pauser = new Rx.Subject();

const createTicks = () => Rx.timer(0, 1000);

const pausableTicks = pauser.pipe(
  scan((isPaused) => !isPaused, false),
  switchMap(isPaused => isPaused ? Rx.NEVER : createTicks()),
  multicast(new Rx.Subject())
);
pausableTicks.connect();

pausableTicks.subscribe({
  next: (x) => {
    console.log(`1 ${x}`);
  },
});

pausableTicks.subscribe({
  next: (x) => {
    console.log(`2 ${x}`);
  },
});

let isPaused = false;
setInterval(() => {
  isPaused = !isPaused;
  pauser.next(isPaused);
}, 3000)
