const express = require(`express`);

const app = express();

app.get(`/`, (req, res) => {
  res.send(`hello world!`);
});

const port = Number(process.env.HELLO_WORLD_PORT) || 3000;
app.listen(port, () => {
  console.log(`server running on port ${port}`);
});
