#!/usr/bin/env node
require('child_process').execFileSync(`${__dirname}/run`, { stdio: 'inherit' });
