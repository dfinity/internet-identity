declare module 'buffer-pipe' {
  import { Buffer } from 'buffer/';

  class BufferPipe {
    readonly buffer: Buffer;

    /**
     * Creates a new instance of a pipe
     * @param {Buffer} buf - an optional buffer to start with
     */
    constructor(buf?: Buffer);

    /**
     * read `num` number of bytes from the pipe
     * @param {Number} num
     * @return {Buffer}
     */
    read(num: number): Buffer;

    /**
     * Wites a buffer to the pipe
     * @param {Buffer} buf
     */
    write(buf: Buffer | number[]): void;

    /**
     * Whether or not there is more data to read from the buffer
     * returns {Boolean}
     */
    get end(): boolean;

    /**
     * returns the number of bytes read from the stream
     * @return {Integer}
     */
    get bytesRead(): number;

    /**
     * returns the number of bytes wrote to the stream
     * @return {Integer}
     */
    get bytesWrote(): number;
  }

  export = BufferPipe;
}
