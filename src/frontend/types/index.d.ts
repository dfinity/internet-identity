declare module "*.png" {
  const value: any;
  export = value;
}

declare module globalThis {
  mockActor: any;
}
