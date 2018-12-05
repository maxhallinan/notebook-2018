require('lookup-multicast-dns/global');
const net = require('net');
const jsonStream = require('duplex-json-stream');

const nick = process.argv[2];
const host = `${process.argv[3]}.local`;

const client = jsonStream(net.createConnection(5000, host));

process.stdin.on('data', function (data) {
  client.write({
    msg: data.toString(),
    nick,
  })
});

client.pipe(process.stdout);
