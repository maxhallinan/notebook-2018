// eslint-disable-next-line no-console
const log = (...args) => console.log(...args);
// const ws = new WebSocket(`ws://localhost:3000`);
// const ws = new WebSocket(`ws://ec2-54-165-15-25.compute-1.amazonaws.com`);
const ws = new WebSocket(`ws://ec2-hello-world-603057692.us-east-1.elb.amazonaws.com/hello-world`);
ws.onopen = () => log(`Opening connection`);
ws.onerror = (err) => log(`Error: ${err.message}`);
ws.onmessage = (msg) => log(`Message received:`, msg.data);
