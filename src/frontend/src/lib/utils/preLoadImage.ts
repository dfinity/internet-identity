// A wrapper around HTMLImageElement, to trigger loading an image before before it's in the DOM.
export class PreLoadImage {
  // The image element created in order for the browser to load the image.
  private img: HTMLImageElement;

  constructor(imageSrc: string) {
    const img = new Image();
    img.src = imageSrc; // Setting the src kicks off the fetching
    this.img = img;
  }

  // Return the same source as the one passed to the constructor.
  // By now, the image has been loaded by the browser.
  getSrc(): string {
    return this.img.src;
  }
}
