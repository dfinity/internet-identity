module.exports = {
  drivers: {
    chrome: {
      // This version needs to match the chrome version on GitHub Actions
      version: "102.0.5005.61",
      arch: process.arch,
      baseURL: 'https://chromedriver.storage.googleapis.com'
    },
  },
}
