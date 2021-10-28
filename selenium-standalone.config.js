module.exports = {
  drivers: {
    chrome: {
      // This version needs to match the chrome version on GitHub Actions
      version: '94.0.4606.41',
      arch: process.arch,
      baseURL: 'https://chromedriver.storage.googleapis.com'
    },
  },
}
