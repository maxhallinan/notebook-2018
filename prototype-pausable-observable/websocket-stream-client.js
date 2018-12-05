const log = (...args) => console.log(...args);
const ws = new WebSocket(`ws://localhost:8080`);
ws.onopen = () => log(`Opening connection`);
ws.onerror = (err) => log(`Error: ${err.message}`);
ws.onmessage = (msg) => log(`Message received:`, JSON.parse(msg.data));
