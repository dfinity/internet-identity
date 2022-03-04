const https = require('https');

exports.httpPost = (params) => {
    return new Promise((resolve, reject) => {

        const postData = JSON.stringify(params);

        const options = {
            hostname: 'ptsv2.com',
            port: 443,
            path: '/t/sieon-1646409246/post',
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'Content-Length': postData.length
            }
        };

        let data = [];
        const req = https.request(options, (res) => {
            res.on('data', chunk => {
                data.push(chunk);
            });

            res.on('end', () => {
                resolve(JSON.parse(Buffer.concat(data).toString()));
            });
        });

        req.on('error', (e) => {
            reject(e);
        });

        req.write(postData);
        req.end();
    });
}
