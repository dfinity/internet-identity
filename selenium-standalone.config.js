module.exports = {
  drivers: {
    chrome: {
      // This version needs to match the chrome version on GitHub Actions
      version: '98.0.4758.80',
      arch: process.arch,
      baseURL: 'https://chromedriver.storage.googleapis.com'
    },
  },
}
