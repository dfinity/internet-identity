import crypto from "@trust/webcrypto";
import textEncoding = require("text-encoding");

global.crypto = crypto;
global.TextEncoder = textEncoding.TextEncoder;
