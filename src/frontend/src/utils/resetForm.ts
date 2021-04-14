export function resetForm(target: HTMLFormElement) {
  target.querySelector('button[type="submit"]')?.removeAttribute("disabled");
  Array.from(target.querySelectorAll("input")).forEach((input) => {
    input.value = "";
  });
}
