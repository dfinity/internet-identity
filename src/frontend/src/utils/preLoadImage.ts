// A wrapper around HTMLImageElement, to trigger a load of an image before it is needed.
export class PreLoadImage {
  // The image element created in order for the browser to load the image.
  // Only the src attribute is used.
  public img: HTMLImageElement;

  constructor(imageSrc: string) {
    const img = new Image();
    img.src = imageSrc; // Setting the src kicks off the fetching
    this.img = img;
  }
}
