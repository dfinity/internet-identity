const wdio = require('webdriverio');

const opts = {
    path: '/wd/hub',
    port: 4723,
    capabilities: {
        platformName: 'Android',
        platformVersion: '11.0',
        deviceName: 'Android Emulator',
        automationName: 'UIAutomator2',
        browserName: 'Chrome',
        newCommandTimeout: 180
    }
};

const identificationOpts = {
    path: '/wd/hub',
    port: 4723,
    capabilities: {
        platformName: "Android",
        platformVersion: "11.0",
        deviceName: "Android Emulator",
        automationName: "UiAutomator2",
        newCommandTimeout: 180
    }
};

async function main() {
    // setup fingerprint
    const settingsClient = await wdio.remote(identificationOpts);

    await settingsClient.$('//android.widget.TextView[@text="Fingerprint"]').click();
    await settingsClient.executeScript('mobile: shell', [{
        command: 'input',
        args: ['text', 1234]
    }]);
    await settingsClient.executeScript('mobile: shell', [{
        command: 'input',
        args: ['keyevent', 66]
    }]);
    await settingsClient.$('//android.widget.Button[@text="NEXT"]').click();
    await settingsClient.pause(5000);
    await settingsClient.fingerPrint(111);
    await settingsClient.pause(5000);
    await settingsClient.fingerPrint(111);
    await settingsClient.pause(5000);
    await settingsClient.fingerPrint(111);
    await settingsClient.pause(5000);
    await settingsClient.$('//android.widget.Button[@text="DONE"]').click();
    await settingsClient.deleteSession();

    // test registration
    const browser = await wdio.remote(opts);
    await browser.url("https://identity.ic0.app/");
    await browser.pause(5000);
    await browser.saveScreenshot('screenshots/android/1-open.png');
    await browser.$('#registerButton').click();
    await browser.$('#registerAlias').setValue('e2e test device 1');
    await browser.saveScreenshot('screenshots/android/2-register.png');
    await browser.$('button[type="submit"]').click();

    const identificationClient = await wdio.remote(identificationOpts);
    await identificationClient.pause(2000);
    await identificationClient.$("android.widget.Button").click();

    await identificationClient.pause(3000);
   //await identificationClient.saveScreenshot('screenshots/android/4-ident-get-started.png');

    await identificationClient.$("//android.widget.TextView[@text='Use your screen lock']").click();
    await identificationClient.pause(3000);
    //await identificationClient.saveScreenshot('screenshots/android/5-ident-finger.png');

    await identificationClient.fingerPrint(111);

    await browser.pause(5000);
    await browser.saveScreenshot('screenshots/android/6-ident-success.png');

    await browser.$('#confirmRegisterButton').click();
    await browser.saveScreenshot('screenshots/android/7-confirm-register.png');

    await identificationClient.fingerPrint(111);
    await browser.pause(8000);
    await browser.saveScreenshot('screenshots/android/8-registered.png');

    // const identityAnchor = await client.$('.highlightBox').getText();
    // assert.greaterThan(identityAnchor.length, 4)

    await browser.$('#displayUserContinue').click();
    await browser.$('#displayWarningPrimary').click();
    await browser.$('#skipRecovery').click();

    await browser.pause(3000);
    await browser.saveScreenshot('screenshots/android/9-done.png');
    // expect(client.$('h1')).toHaveTextContent('Anchor Management');

    await browser.deleteSession();
}

main();