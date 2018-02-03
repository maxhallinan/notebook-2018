// tcp echo server client.
// Takes input from standard in, streams to echo server, sends data
// from echo server to standard out.
const net = require('net');

const client = net.createConnection({ port: 5000, });

process.stdin.pipe(client).pipe(process.stdout);
