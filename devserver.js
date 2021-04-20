const express = require("express");
const path = require("path");
const app = express();

app.get("/manage", (req, res) => {
  res.sendFile(path.join(__dirname, "src", "frontend", "assets", "index.html"));
});
app.get("/authorize", (req, res) => {
  res.sendFile(path.join(__dirname, "src", "frontend", "assets", "index.html"));
});

app.listen("8081");
