export const dialogContents = (userId: bigint) => {
  return `<button type="button" aria-label="close dialog" id="closeDialog">
  <svg id="icon_close" data-name="icon/close" xmlns="http://www.w3.org/2000/svg" width="14" height="14" viewBox="0 0 14 14">
    <rect id="Rectangle_91" data-name="Rectangle 91" width="14" height="14" fill="none"></rect>
    <path id="Line" d="M0,0,9,9" transform="translate(2.5 2.5)" fill="none" stroke="gray" stroke-miterlimit="10" stroke-width="3"></path>
    <path id="Line-2" data-name="Line" d="M9,0,0,9" transform="translate(2.5 2.5)" fill="none" stroke="gray" stroke-miterlimit="10" stroke-width="3"></path>
  </svg>
</button>
<section>
  <h2>Existing User</h2>
  <p>We remember your user #</p>
  <fieldset>
    <label for="registerUserNumber">User #</label>
    <input type="text" id="registerUserNumber" name="registerUserNumber" style="background-image: url(&quot;data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAAAXNSR0IArs4c6QAAAmJJREFUWAntV7uKIkEUvbYGM4KID3wEIgjKRLLpKGLgFwiCfslGhkb7IbLgAzE1GhMxWxRRBEEwmEgDERWfW6fXuttq60a2wU6B1qlzb9U5fatsKROJVigUArvd7oeAyePx6Af3qGYymT7F2h8Wi+V7Pp+fmE7iv4Sw81GieusKIzNh4puCJzdaHIagCW1F4KSeQ4O4pPLoPb/3INBGBZ7avgz8fxWIxWIUCoX43Blegbe3NwoGg88zwMoncFUB8Yokj8dDdrv9MpfHVquV/H4/iVcpc1qgKAp5vV6y2WxaWhefreB0OimXy6kGkD0YDKhSqdB2u+XJqVSK4vE4QWS5XKrx0WjEcZ/PR9lslhwOh8p1Oh2q1Wp0OBw4RwvOKpBOp1kcSdivZPLvmxrjRCKhiiOOSmQyGXp5ecFQbRhLcRDRaJTe39//BHW+2cDr6ysFAoGrlEgkwpwWS1I7z+VykdvtliHuw+Ew40vABvb7Pf6hLuMk/rGY02ImBZC8dqv04lpOYjaw2WzUPZcB2WMPZet2u1cmZ7MZTSYTNWU+n9N4PJbp3GvXYPIE2ADG9Xqder2e+kTr9ZqazSa1222eA6FqtUoQwqHCuFgscgWQWC6XaTgcEiqKQ9poNOiegbNfwWq1olKppB6yW6cWVcDHbDarIuzuBBaLhWrqVvwy/6wCMnhLXMbR4wnvtX/F5VxdAzJoRH+2BUYItlotmk6nLGW4gX6/z+IAT9+CLwPPr8DprnZ2MIwaQBsV+DBKUEfnQ8EtFRdFneBDKWhCW8EVGbdUQfxESR6qKhaHBrSgCe3fbLTpPlS70M0AAAAASUVORK5CYII=&quot;); background-repeat: no-repeat; background-attachment: scroll; background-size: 16px 18px; background-position: 98% 50%;">
  </fieldset>
  <p>
    If you have authenticated using this device before, you can attempt
    to re-authenticate by clicking Reconnect. Otherwise, click Link to
    set this device up using an already authenticated browser
  </p>
  <div class="spacer"></div>
  <div class="row">
    <button id="toggleReconnect" aria-controls="reconnectSection">
      Reconnect
    </button>
    <button id="toggleAddDevice" aria-controls="addDeviceLinkSection">
      Link
    </button>
  </div>
</section>
<section class="hidden" id="reconnectSection" aria-expanded="false">
  <p>
    Click this link to reauthenticate this device.
  </p>
  <button type="submit" id="reconnectTrigger">Reconnect</button>
</section>
<section class="hidden" id="addDeviceLinkSection" aria-expanded="false">
  <p>
    Copy the URL below and open it in your already authenticated
    browser. This page will automatically update when you have
    succeeded.
  </p>
  <input type="text" id="addDeviceLink" readonly="">
</section>`;
};
