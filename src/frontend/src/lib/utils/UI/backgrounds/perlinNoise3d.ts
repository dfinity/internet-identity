/**
 * 3D Perlin Noise implementation in TypeScript.
 * Adapted from P5.js and other sources.
 */
export class PerlinNoise3D {
  private static readonly PERLIN_YWRAPB = 4;
  private static readonly PERLIN_YWRAP = 1 << PerlinNoise3D.PERLIN_YWRAPB;
  private static readonly PERLIN_ZWRAPB = 8;
  private static readonly PERLIN_ZWRAP = 1 << PerlinNoise3D.PERLIN_ZWRAPB;
  private static readonly PERLIN_SIZE = 4095;

  private static readonly SINCOS_PRECISION = 0.5;
  private static readonly SINCOS_LENGTH = Math.floor(
    360 / PerlinNoise3D.SINCOS_PRECISION,
  );
  private static readonly DEG_TO_RAD = Math.PI / 180.0;
  private static readonly sinLUT: number[] = [];
  private static readonly cosLUT: number[] = [];
  private static readonly perlin_PI: number = PerlinNoise3D.SINCOS_LENGTH >> 1;

  private perlin: number[] | null = null;
  public perlin_octaves: number = 4;
  public perlin_amp_falloff: number = 0.5;

  constructor() {
    // Initialize lookup tables if not already done
    if (PerlinNoise3D.sinLUT.length === 0) {
      for (let i = 0; i < PerlinNoise3D.SINCOS_LENGTH; i++) {
        PerlinNoise3D.sinLUT[i] = Math.sin(
          i * PerlinNoise3D.DEG_TO_RAD * PerlinNoise3D.SINCOS_PRECISION,
        );
        PerlinNoise3D.cosLUT[i] = Math.cos(
          i * PerlinNoise3D.DEG_TO_RAD * PerlinNoise3D.SINCOS_PRECISION,
        );
      }
    }
  }

  /**
   * Seeds the Perlin noise generator.
   * @param seed The seed value.
   */
  public noiseSeed(seed?: number): void {
    // Linear Congruential Generator
    const m = 4294967296;
    const a = 1664525;
    const c = 1013904223;
    let z = (seed == null ? Math.random() * m : seed) >>> 0;

    this.perlin = new Array(PerlinNoise3D.PERLIN_SIZE + 1);
    for (let i = 0; i < PerlinNoise3D.PERLIN_SIZE + 1; i++) {
      z = (a * z + c) % m;
      this.perlin[i] = z / m;
    }
  }

  /**
   * Returns Perlin noise value at coordinates (x, y, z).
   * @param x X coordinate
   * @param y Y coordinate (default 0)
   * @param z Z coordinate (default 0)
   */
  public get(x: number, y: number = 0, z: number = 0): number {
    if (!this.perlin) {
      // Lazy initialize with random values
      this.perlin = new Array(PerlinNoise3D.PERLIN_SIZE + 1);
      for (let i = 0; i < PerlinNoise3D.PERLIN_SIZE + 1; i++) {
        this.perlin[i] = Math.random();
      }
    }

    x = Math.abs(x);
    y = Math.abs(y);
    z = Math.abs(z);

    let xi = Math.floor(x);
    let yi = Math.floor(y);
    let zi = Math.floor(z);
    let xf = x - xi;
    let yf = y - yi;
    let zf = z - zi;

    let r = 0;
    let ampl = 0.5;

    const noise_fsc = (i: number): number => {
      // using cosine lookup table
      return (
        0.5 *
        (1.0 -
          PerlinNoise3D.cosLUT[
            Math.floor(i * PerlinNoise3D.perlin_PI) %
              PerlinNoise3D.SINCOS_LENGTH
          ])
      );
    };

    for (let o = 0; o < this.perlin_octaves; o++) {
      let of =
        xi +
        (yi << PerlinNoise3D.PERLIN_YWRAPB) +
        (zi << PerlinNoise3D.PERLIN_ZWRAPB);

      const rxf = noise_fsc(xf);
      const ryf = noise_fsc(yf);

      let n1 = this.perlin[of & PerlinNoise3D.PERLIN_SIZE]!;
      n1 += rxf * (this.perlin[(of + 1) & PerlinNoise3D.PERLIN_SIZE]! - n1);
      let n2 =
        this.perlin[
          (of + PerlinNoise3D.PERLIN_YWRAP) & PerlinNoise3D.PERLIN_SIZE
        ]!;
      n2 +=
        rxf *
        (this.perlin[
          (of + PerlinNoise3D.PERLIN_YWRAP + 1) & PerlinNoise3D.PERLIN_SIZE
        ]! -
          n2);
      n1 += ryf * (n2 - n1);

      of += PerlinNoise3D.PERLIN_ZWRAP;
      n2 = this.perlin[of & PerlinNoise3D.PERLIN_SIZE]!;
      n2 += rxf * (this.perlin[(of + 1) & PerlinNoise3D.PERLIN_SIZE]! - n2);
      let n3 =
        this.perlin[
          (of + PerlinNoise3D.PERLIN_YWRAP) & PerlinNoise3D.PERLIN_SIZE
        ]!;
      n3 +=
        rxf *
        (this.perlin[
          (of + PerlinNoise3D.PERLIN_YWRAP + 1) & PerlinNoise3D.PERLIN_SIZE
        ]! -
          n3);
      n2 += ryf * (n3 - n2);

      n1 += noise_fsc(zf) * (n2 - n1);

      r += n1 * ampl;
      ampl *= this.perlin_amp_falloff;
      xi <<= 1;
      xf *= 2;
      yi <<= 1;
      yf *= 2;
      zi <<= 1;
      zf *= 2;

      if (xf >= 1.0) {
        xi++;
        xf--;
      }
      if (yf >= 1.0) {
        yi++;
        yf--;
      }
      if (zf >= 1.0) {
        zi++;
        zf--;
      }
    }
    return r;
  }
}
