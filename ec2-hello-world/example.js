// eslint-disable-next-line no-console
const log = (...args) => console.log(...args);
const ws = new WebSocket(`ws://localhost:3000`);
ws.onopen = () => log(`Opening connection`);
ws.onerror = (err) => log(`Error: ${err.message}`);
ws.onmessage = (msg) => log(`Message received:`, JSON.parse(msg.data));
